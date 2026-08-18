# MCP Server Forwarding Lifecycle
# Owns forwarded-request queuing, response correlation, and timeout recovery.
# Operates on the mcprServer private environment while preserving R6 delegate seams.

#' @include mcpr-server-daemon.R
#' @include protocol.R
#' @include utils.R
NULL

# Detect whether an active secondary-session request lost its worker pipe.
# A missing socket or a connected socket with zero pipes marks the worker dead.
# Idle sessions are ignored because they have no client request to fail.
mcpr_forwarding_daemon_pipe_dropped <- function(private, client_id) {
  state <- private$.pending_requests[[client_id]]
  if (is.null(state) || is.null(state$active)) {
    return(FALSE)
  }
  sock <- the$daemon_sockets[[client_id]]
  if (is.null(sock)) {
    return(TRUE)
  }
  pipes <- tryCatch(nanonext::stat(sock, "pipes"), error = function(e) NA_integer_)
  isTRUE(!is.na(pipes) && pipes == 0L)
}

# Correlate an attached-session response with its active forwarded request.
# Restores the client request ID, drops terminal or foreign responses, and advances the queue.
# Unkeyed responses from the primary session pass directly to the MCP client.
mcpr_forwarding_handle_message <- function(private, data, session_key = NULL) {
  if (!is.character(data)) {
    return()
  }
  private$log_comm("FROM SESSION", data)

  if (!is.null(session_key)) {
    parsed <- tryCatch(jsonlite::parse_json(data), error = function(e) NULL)
    if (!is.list(parsed) || is.null(parsed$id) || length(parsed$id) != 1L ||
          !is.atomic(parsed$id) || is.na(parsed$id)) {
      private$log_warn(sprintf(
        "Dropping malformed response from session '%s': missing scalar id",
        session_key
      ))
      return()
    }
    resp_id <- as.character(parsed$id)
    if (nzchar(resp_id) && resp_id %in% private$.terminal_wire_ids) {
      private$log_debug(sprintf(
        "Dropping late response for terminal forwarded request id=%s (session '%s')",
        resp_id, session_key
      ))
      private$.terminal_wire_ids <- setdiff(private$.terminal_wire_ids, resp_id)
      return()
    }
    # Clear the active record now that its valid response arrived, then promote
    # the next queued request (if any) onto this session's current socket.
    state <- private$.pending_requests[[session_key]]
    active <- state$active
    active_wire_id <- active$wire_request_id %||% as.character(active$client_request_id %||% "")
    if (!is.null(active) && identical(active_wire_id, resp_id)) {
      parsed$id <- active$client_request_id
      data <- to_json(parsed)
      state$active <- NULL
      if (length(state$waiting) == 0L) {
        private$.pending_requests[[session_key]] <- NULL
      } else {
        private$.pending_requests[[session_key]] <- state
        private$dispatch_next(session_key, private$session_socket_for(session_key))
      }
    } else {
      private$log_warn(sprintf(
        "Dropping response id=%s that is not owned by the active request for session '%s'",
        resp_id, session_key
      ))
      return()
    }
  }

  nanonext::write_stdout(data)
}

# Validate a forwarded tool request before it enters the pending queue.
# Adds the resolved tool function and captures request-scoped MCP client context.
# Validation errors are returned unchanged so callers never create phantom queue records.
mcpr_forwarding_prepare_request <- function(private, data) {
  prepared <- private$append_tool_fn(data)
  if (is.list(prepared) && !is.null(prepared$error)) {
    return(prepared)
  }
  prepared$mcpr_request_context <- as_mcpr_request_context(
    mcp_apps_supported = private$.mcp_apps_supported,
    interface = private$.client_interface,
    client_name = private$.client_name
  )
  prepared
}

# Send the active queue record and retain its asynchronous send handle.
# Immediate send failures produce a client error and promote the next waiting request.
# Returns invisibly whether the active request was accepted by nanonext.
mcpr_forwarding_send_active <- function(private, session_key, sock, label = "TO TARGET") {
  state <- private$.pending_requests[[session_key]]
  record <- state$active
  if (is.null(record)) {
    return(invisible(FALSE))
  }
  private$log_comm(label, jsonlite::toJSON(record$data))
  send <- tryCatch(
    nanonext::send_aio(sock, record$data, mode = "serial", timeout = 1000L),
    error = function(e) e
  )
  if (inherits(send, "error")) {
    private$fail_pending_send(record, session_key, conditionMessage(send))
    state$active <- NULL
    if (length(state$waiting) == 0L) {
      private$.pending_requests[[session_key]] <- NULL
    } else {
      private$.pending_requests[[session_key]] <- state
      private$dispatch_next(session_key, sock)
    }
    return(invisible(FALSE))
  }
  state$active$send_aio <- send
  private$.pending_requests[[session_key]] <- state
  invisible(TRUE)
}

# Add a forwarded request to one session's bounded FIFO queue.
# Assigns a unique wire ID and permits only one active evaluation per mutable R session.
# Returns TRUE for immediate dispatch, FALSE for queuing, or NA after client-visible rejection.
mcpr_forwarding_enqueue <- function(private, data, session_key) {
  timeout_secs <- as.integer(
    data$params$arguments$timeout %||% private$.execution_timeout_secs
  )
  if (length(timeout_secs) != 1L || is.na(timeout_secs) || timeout_secs < 1L) {
    cat_json(jsonrpc_response(
      data$id,
      error = list(code = -32602L, message = "timeout must be a positive integer")
    ))
    return(NA)
  }
  private$.forward_sequence <- private$.forward_sequence + 1
  wire_request_id <- sprintf("mcpr-%0.f", private$.forward_sequence)
  client_request_id <- data$id
  data$id <- wire_request_id
  record <- list(
    client_request_id = client_request_id,
    wire_request_id = wire_request_id,
    session_key = session_key,
    data = data,
    timeout_secs = timeout_secs,
    sent_at = NULL,
    send_aio = NULL
  )
  state <- private$.pending_requests[[session_key]] %||% list(active = NULL, waiting = list())
  if (is.null(state$active)) {
    record$sent_at <- Sys.time()
    state$active <- record
    private$.pending_requests[[session_key]] <- state
    return(TRUE)
  }
  if (length(state$waiting) >= private$.max_waiting_per_session) {
    cat_json(jsonrpc_response(
      client_request_id,
      error = list(
        code = -32000L,
        message = sprintf(
          "Session '%s' request queue is full; retry after an active call completes.",
          session_key
        )
      )
    ))
    return(NA)
  }
  state$waiting <- c(state$waiting, list(record))
  private$.pending_requests[[session_key]] <- state
  FALSE
}

# Promote the oldest waiting request and send it through the current session socket.
# A vanished socket fails the complete queue iteratively to avoid recursive stack growth.
# The promoted request receives its timeout deadline only when it becomes active.
mcpr_forwarding_dispatch_next <- function(private, session_key, sock) {
  state <- private$.pending_requests[[session_key]]
  if (is.null(state) || length(state$waiting) == 0) {
    return(invisible(NULL))
  }
  record <- state$waiting[[1L]]
  state$waiting <- state$waiting[-1L]
  if (is.null(sock)) {
    # Socket vanished before promotion. Fail the complete remainder in one
    # pass so a large bounded queue cannot recurse through the R stack.
    for (pending in c(list(record), state$waiting)) {
      private$fail_pending_dead(pending, session_key)
    }
    private$.pending_requests[[session_key]] <- NULL
    return(invisible(NULL))
  }
  record$sent_at <- Sys.time()
  state$active <- record
  private$.pending_requests[[session_key]] <- state
  private$send_active_request(session_key, sock, label = "TO TARGET")
  invisible(NULL)
}

# Resolve the live socket currently associated with a forwarding session key.
# Secondary sessions use the daemon registry while human sessions use integer IDs.
# Returns NULL when neither registry contains a usable transport.
mcpr_forwarding_session_socket <- function(private, session_key) {
  sock <- the$daemon_sockets[[session_key]]
  if (!is.null(sock)) {
    return(sock)
  }
  get_user_session(suppressWarnings(as.integer(session_key)))
}

# Report that an attached session died before completing a pending request.
# Preserves the original client request ID and suggests the supported recovery controls.
# The caller remains responsible for clearing or advancing the affected queue state.
mcpr_forwarding_fail_dead <- function(private, record, session_key) {
  cat_json(jsonrpc_response(
    record$client_request_id,
    error = list(
      code = -32603L,
      message = sprintf(
        paste0(
          "Session '%s' is no longer responding - the R process may have exited. ",
          "Run manage_r_sessions(action='list') to see active sessions, ",
          "or manage_r_sessions(action='start') to open a new one."
        ),
        session_key
      )
    )
  ))
}

# Report that an active request could not be placed on its attached-session socket.
# Preserves the client request ID and includes the transport failure reason.
# Queue cleanup and promotion remain owned by the sending helper.
mcpr_forwarding_fail_send <- function(private, record, session_key, reason) {
  cat_json(jsonrpc_response(
    record$client_request_id,
    error = list(
      code = -32603L,
      message = sprintf("Could not send request to session '%s': %s", session_key, reason)
    )
  ))
}

# Retire a failed attached-session transport according to its ownership model.
# MCPR-owned secondary workers are unregistered; human sessions are detached without termination.
# Listener state is cleared before the registry cleanup runs.
mcpr_forwarding_retire_transport <- function(private, session_key) {
  if (is_secondary_session_key(session_key) ||
        !is.null(the$daemon_sockets[[session_key]])) {
    private$.daemon_listeners[[session_key]] <- NULL
    unregister_daemon(session_key)
    return(invisible(NULL))
  }
  session_id <- suppressWarnings(as.integer(session_key))
  if (!is.na(session_id)) {
    private$.user_listeners[[as.character(session_id)]] <- NULL
    unregister_user_session(session_id)
  }
  invisible(NULL)
}

# Resolve one daemon or human-session listener completion.
# Transport errors retire the session and fail every outstanding request on its queue.
# Character responses continue through request-ID correlation and normal queue advancement.
mcpr_forwarding_handle_listener <- function(private, data, session_key, session_type) {
  if (!is.character(data)) {
    private$log_warn(sprintf(
      "Session '%s' (%s) connection closed (nanonext error %s)",
      session_key, session_type, as.character(data)
    ))
    state <- private$.pending_requests[[session_key]]
    private$retire_session_transport(session_key)
    private$.session_manager$mark_dead(session_key)
    # The socket is gone: every outstanding request on it is dead. Fail the
    # active record and every queued one so none is silently lost.
    if (!is.null(state)) {
      if (!is.null(state$active)) {
        private$fail_pending_dead(state$active, session_key)
      }
      for (record in state$waiting) {
        private$fail_pending_dead(record, session_key)
      }
      private$.pending_requests[[session_key]] <- NULL
    }
    return()
  }
  private$handle_message_from_session(data, session_key)
}

# Sweep active forwarded requests for send failures and elapsed execution deadlines.
# Timed-out secondary workers may be recycled, with waiting work migrated to the fresh socket.
# Unrecoverable queues are failed completely and late wire responses are remembered for rejection.
mcpr_forwarding_sweep_pending <- function(private) {
  now <- Sys.time()
  for (key in names(private$.pending_requests)) {
    state <- private$.pending_requests[[key]]
    req <- state$active
    # Only the active record has a deadline; queued records are armed when promoted.
    if (is.null(req)) {
      next
    }
    if (!is.null(req$send_aio) && !nanonext::unresolved(req$send_aio)) {
      send_result <- req$send_aio$result
      if (nanonext::is_error_value(send_result)) {
        private$handle_session_listener_resolved(send_result, key, "send")
        next
      }
      state$active$send_aio <- NULL
      private$.pending_requests[[key]] <- state
      req <- state$active
    }
    elapsed <- as.numeric(difftime(now, req$sent_at, units = "secs"))
    if (elapsed <= req$timeout_secs) {
      next
    }
    private$log_warn(sprintf(
      "Request %s to session '%s' timed out after %ds",
      req$client_request_id, key, req$timeout_secs
    ))
    recovery <- private$.session_manager$recover_timeout(key)
    recovery_text <- switch(
      recovery$action %||% "none",
      recycled = sprintf(
        "The timed-out worker was recycled automatically; future calls will use session '%s'.",
        recovery$new_session_id
      ),
      closed = "The timed-out worker was closed automatically. Start a new session to continue attached execution.",
      detached = "MCPR detached from the timed-out human-owned session to avoid killing user work.",
      marked_dead = "The timed-out session was marked dead.",
      "The timed-out session state was cleared."
    )
    cat_json(jsonrpc_response(
      req$client_request_id,
      error = list(
        code = -32603L,
        message = sprintf(
          paste0(
            "Code execution timed out after %ds in session '%s'. ",
            "For long-running computations, pass a larger timeout= value. ",
            "%s"
          ),
          req$timeout_secs, key, recovery_text
        )
      )
    ))
    # Track this id so any late response that eventually arrives is dropped.
    # Cap at 500 to prevent unbounded growth over long server runs.
    terminal_wire_id <- req$wire_request_id %||% as.character(req$client_request_id)
    private$.terminal_wire_ids <- utils::tail(
      c(private$.terminal_wire_ids, terminal_wire_id),
      500L
    )

    waiting <- state$waiting
    private$.pending_requests[[key]] <- NULL
    if (identical(recovery$action, "recycled")) {
      # The worker was recycled onto a NEW key + socket. Migrate the queue to
      # the new active binding and dispatch its head to the fresh worker.
      new_key <- private$.session_manager$active_binding()$key %||% recovery$key
      if (length(waiting) > 0) {
        existing <- private$.pending_requests[[new_key]] %||% list(active = NULL, waiting = list())
        existing$waiting <- c(existing$waiting, waiting)
        private$.pending_requests[[new_key]] <- existing
        private$dispatch_next(new_key, private$session_socket_for(new_key))
      }
    } else {
      # No usable replacement (closed/detached/marked_dead/none): every queued
      # request on the dead session is unrecoverable.
      for (record in waiting) {
        private$fail_pending_dead(record, key)
      }
    }
  }
}
