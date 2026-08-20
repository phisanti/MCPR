# MCPR Session Manager
# Owns per-server active session binding state for MCPR runtime execution.
# Coordinates private, human, and secondary session attachment lifecycle.

#' MCPR Session Manager
#'
#' @description Server-owned runtime boundary for active session state.
#' @noRd
mcprSessionManager <- R6::R6Class("mcprSessionManager",
  public = list(
    #' @description Initialize a session manager.
    #' @param enabled Logical. Whether session-management capability is enabled.
    #' @param local_executor Function used for private/local tool execution.
    #' @param server Owning server instance.
    #' @param callbacks Named list of server lifecycle/transport callbacks.
    #' @return A new mcprSessionManager instance.
    initialize = function(enabled = FALSE, local_executor = NULL, server = NULL,
                          callbacks = list()) {
      if (!is.logical(enabled) || length(enabled) != 1L || is.na(enabled)) {
        cli::cli_abort("enabled must be a single TRUE or FALSE value")
      }
      if (!is.null(local_executor) && !is.function(local_executor)) {
        cli::cli_abort("local_executor must be a function or NULL")
      }

      private$.enabled <- enabled
      private$.local_executor <- local_executor
      private$.server <- server
      private$.callbacks <- callbacks %||% list()
      private$.private_binding <- private$new_binding(
        type = "local",
        session_id = NULL,
        key = "private",
        owned = TRUE,
        label = "private (local)"
      )
      private$.active_binding <- private$.private_binding
    },

    #' @description Return whether session management is enabled.
    #' @return Logical scalar.
    is_enabled = function() {
      private$.enabled
    },

    #' @description Return the active binding object.
    #' @return An environment describing the active binding.
    active_binding = function() {
      private$.active_binding
    },

    #' @description Return a human-readable active binding label.
    #' @return Character scalar.
    active_label = function() {
      private$.active_binding$label %||% "private (local)"
    },

    #' @description Execute a prepared JSON-RPC tool call against the active binding.
    #' @param data Parsed JSON-RPC request data.
    #' @return Result of the active binding executor.
    execute = function(data) {
      binding <- private$.active_binding
      if (identical(binding$type, "local")) {
        if (is.null(private$.local_executor)) {
          cli::cli_abort("Local executor is not configured", .internal = TRUE)
        }
        return(private$.local_executor(data))
      }

      tryCatch(
        private$forward_to_binding(data, binding),
        error = function(e) {
          self$mark_dead(binding$key %||% binding$session_id)
          cli::cli_abort(conditionMessage(e))
        }
      )
    },

    #' @description Handle session-control actions.
    #' @param action Control action.
    #' @param session Optional session identifier.
    #' @return User-facing status text.
    handle_control = function(action = "list", session = NULL) {
      if (!private$.enabled) {
        cli::cli_abort("Session management is not enabled for this server")
      }

      action <- tolower(as.character(action %||% "list"))
      if (identical(action, "stop")) {
        action <- "close"
      }

      if (action %in% c("detach", "local", "private")) {
        return(self$detach())
      }

      if (identical(action, "list")) {
        return(self$list_sessions())
      }

      if (identical(action, "join")) {
        return(self$join(session))
      }

      if (identical(action, "start")) {
        return(self$start_secondary())
      }

      if (identical(action, "close")) {
        return(self$close(session))
      }

      cli::cli_abort("action must be one of: 'list', 'join', 'start', 'detach', 'close'")
    },

    #' @description List private, active, and available attached sessions.
    #' @return User-facing status text.
    list_sessions = function() {
      private$discover_human_sessions()

      parts <- c(
        "Private session: ok",
        sprintf("Active session: %s", self$active_label())
      )
      if (!is.null(private$.last_dead_notice)) {
        parts <- c(parts, private$.last_dead_notice)
        private$.last_dead_notice <- NULL
      }

      rows <- private$session_rows()
      parts <- c(parts, "", "Available sessions:")
      if (length(rows) == 0L) {
        parts <- c(parts, "None.")
      } else {
        parts <- c(parts, rows)
      }

      paste(parts, collapse = "\n")
    },

    #' @description Attach a human or known session by numeric id.
    #' @param session Session id.
    #' @return User-facing status text.
    join = function(session) {
      session_id <- private$validate_session_id(session, "join")
      private$discover_human_sessions()
      previous <- private$.active_binding

      # A worker this server started is attached directly. Falling through to
      # join_human here would dial a second socket to our own secondary, label
      # it human, and leave close() unable to reset the active binding.
      binding <- private$.sessions[[private$secondary_key(session_id)]]
      if (is.null(binding) || !isTRUE(binding$owned)) {
        key <- private$human_key(session_id)
        binding <- private$.sessions[[key]]
        if (is.null(binding) || is.null(binding$socket)) {
          joined <- private$call_callback("join_human", session_id)
          binding <- private$new_binding(
            type = "human",
            session_id = session_id,
            key = key,
            socket = joined$socket,
            owned = FALSE,
            label = sprintf("%d (attached human)", session_id)
          )
          private$.sessions[[key]] <- binding
        }
      }

      private$.active_binding <- binding
      sprintf(
        "Attached to session %d.%s\nActive session: %s",
        session_id,
        private$displacement_notice(previous, binding),
        self$active_label()
      )
    },

    #' @description Start an MCPR-owned secondary session. The new session is
    #' attached only when nothing is attached yet; an existing attachment is
    #' never displaced silently.
    #' @return User-facing status text.
    start_secondary = function() {
      started <- private$call_callback("start_secondary", working_dir = getwd())
      session_id <- private$validate_session_id(started$session_id, "start")
      key <- started$key %||% private$secondary_key(session_id)
      binding <- private$new_binding(
        type = "secondary",
        session_id = session_id,
        key = key,
        socket = started$socket,
        process = started$process,
        owned = TRUE,
        label = sprintf("%d (attached secondary)", session_id)
      )
      private$.sessions[[key]] <- binding

      # Discovery may already have recorded this id as an available human
      # session before the worker existed. This server owns it now, so the
      # stale entry has to go or it will shadow the binding again.
      private$.sessions[[private$human_key(session_id)]] <- NULL

      # The active binding is server-wide. Stealing it here would silently
      # reroute an attachment another caller made explicitly, so a start only
      # attaches when nothing is attached yet.
      if (!private$is_private_active()) {
        return(sprintf(
          paste(
            "Secondary session %d started but not attached.",
            "Active session is unchanged: %s",
            "Use action='join' with session=%d to run code in the new session.",
            sep = "\n"
          ),
          session_id,
          self$active_label(),
          session_id
        ))
      }

      private$.active_binding <- binding
      sprintf(
        "Secondary session %d started and attached.\nActive session: %s",
        session_id,
        self$active_label()
      )
    },

    #' @description Return execution to the private/local session.
    #' @return User-facing status text.
    detach = function() {
      private$.active_binding <- private$.private_binding
      "Detached. Active session: private (local)"
    },

    #' @description Close an MCPR-owned secondary session.
    #' @param session Session id.
    #' @return User-facing status text.
    close = function(session) {
      session_id <- private$validate_session_id(session, "close")
      binding <- private$binding_for_session(session_id)

      if (is.null(binding)) {
        cli::cli_abort("No attached or secondary session {session_id} is known.")
      }
      if (!identical(binding$type, "secondary") || !isTRUE(binding$owned)) {
        if (identical(private$.active_binding$key, binding$key)) {
          private$.active_binding <- private$.private_binding
        }
        cli::cli_abort(
          "Session {session_id} is human-owned and will not be closed. Detached to private/local."
        )
      }

      private$call_callback("close_secondary", binding)
      private$.sessions[[binding$key]] <- NULL
      if (identical(private$.active_binding$key, binding$key)) {
        private$.active_binding <- private$.private_binding
      }

      sprintf(
        "Secondary session %d closed.\nActive session: %s",
        session_id,
        self$active_label()
      )
    },

    #' @description Reset active state after an attached session dies.
    #' @param key_or_session Session key or id.
    #' @return TRUE if the active binding was reset.
    mark_dead = function(key_or_session) {
      key <- private$normalize_key(key_or_session)
      if (is.null(key)) {
        return(FALSE)
      }

      binding <- private$.sessions[[key]]
      if (!is.null(binding)) {
        private$.sessions[[key]] <- NULL
      }

      was_active <- identical(private$.active_binding$key, key)
      if (was_active) {
        private$.active_binding <- private$.private_binding
        label <- if (!is.null(binding) && !is.null(binding$session_id)) {
          as.character(binding$session_id)
        } else {
          as.character(key_or_session)
        }
        private$.last_dead_notice <- sprintf(
          "Previous active session %s is no longer responding.",
          label
        )
      }

      was_active
    },

    #' @description Recover from a timed-out attached session.
    #' @param key_or_session Session key or id.
    #' @return A list describing the recovery action.
    recover_timeout = function(key_or_session) {
      key <- private$normalize_key(key_or_session)
      if (is.null(key)) {
        return(list(action = "none"))
      }

      binding <- private$.sessions[[key]]
      if (is.null(binding)) {
        self$mark_dead(key_or_session)
        return(list(action = "marked_dead", key = key))
      }

      old_session_id <- binding$session_id
      was_active <- identical(private$.active_binding$key, key)
      private$.sessions[[key]] <- NULL
      if (was_active) {
        private$.active_binding <- private$.private_binding
      }

      if (identical(binding$type, "secondary") && isTRUE(binding$owned)) {
        close_error <- tryCatch(
          {
            private$call_callback("close_secondary", binding)
            NULL
          },
          error = function(e) e
        )
        if (inherits(close_error, "error")) {
          cli::cli_warn(
            "Failed to close timed-out secondary session {old_session_id}: {conditionMessage(close_error)}"
          )
        }

        if (was_active) {
          started <- tryCatch(self$start_secondary(), error = function(e) e)
          if (!inherits(started, "error")) {
            new_binding <- private$.active_binding
            private$.last_dead_notice <- sprintf(
              "Previous active session %s timed out and was recycled as session %s.",
              old_session_id,
              new_binding$session_id
            )
            return(list(
              action = "recycled",
              old_session_id = old_session_id,
              new_session_id = new_binding$session_id,
              key = key
            ))
          }
          cli::cli_warn(
            "Failed to restart timed-out secondary session {old_session_id}: {conditionMessage(started)}"
          )
        }

        private$.last_dead_notice <- sprintf(
          "Previous active session %s timed out and was closed.",
          old_session_id
        )
        return(list(action = "closed", old_session_id = old_session_id, key = key))
      }

      if (identical(binding$type, "human") && !is.null(binding$socket)) {
        detach_error <- tryCatch(
          {
            private$call_callback("detach_human", binding)
            NULL
          },
          error = function(e) e
        )
        if (inherits(detach_error, "error")) {
          cli::cli_warn(
            "Failed to detach timed-out human session {old_session_id}: {conditionMessage(detach_error)}"
          )
        }
      }

      private$.last_dead_notice <- sprintf(
        "Previous active session %s timed out and was detached.",
        old_session_id
      )
      list(action = "detached", old_session_id = old_session_id, key = key)
    },

    #' @description Clean up sessions owned or joined by this manager.
    #' @return Invisibly TRUE.
    # Tear down only the bindings this manager owns or has joined.
    # Human sessions are detached, secondary sessions are closed, then the
    # active binding resets back to the private local session.
    cleanup_owned = function() {
      for (key in names(private$.sessions)) {
        binding <- private$.sessions[[key]]
        if (is.null(binding)) {
          next
        }
        if (identical(binding$type, "secondary") && isTRUE(binding$owned)) {
          private$call_callback("close_secondary", binding)
        } else if (identical(binding$type, "human") && !is.null(binding$socket)) {
          private$call_callback("detach_human", binding)
        }
        private$.sessions[[key]] <- NULL
      }
      private$.active_binding <- private$.private_binding
      invisible(TRUE)
    }
  ),
  private = list(
    .enabled = FALSE,
    .local_executor = NULL,
    .server = NULL,
    .callbacks = list(),
    .private_binding = NULL,
    .active_binding = NULL,
    .sessions = list(),
    .last_dead_notice = NULL,

    new_binding = function(type, session_id = NULL, key = NULL, socket = NULL,
                           process = NULL, owned = FALSE, label = NULL) {
      binding <- new.env(parent = emptyenv())
      binding$type <- type
      binding$session_id <- session_id
      binding$key <- key %||% type
      binding$socket <- socket
      binding$process <- process
      binding$owned <- owned
      binding$label <- label %||% type
      binding
    },

    # All transport and lifecycle side effects flow through server callbacks.
    # This keeps the manager decoupled from the actual socket/process owners.
    call_callback = function(name, ...) {
      callback <- private$.callbacks[[name]]
      if (!is.function(callback)) {
        cli::cli_abort("Session manager callback {.val {name}} is not configured", .internal = TRUE)
      }
      callback(...)
    },

    validate_session_id = function(session, action) {
      if (is.null(session)) {
        cli::cli_abort("session parameter is required when action='{action}'")
      }
      if (!is.numeric(session) || length(session) != 1L || is.na(session)) {
        cli::cli_abort("session must be a single integer")
      }
      as.integer(session)
    },

    is_private_active = function() {
      identical(private$.active_binding$key, private$.private_binding$key)
    },

    # Attachment changes are server-wide, so any caller displaced by one has to
    # be able to see it in the response text.
    displacement_notice = function(previous, binding) {
      if (is.null(previous)) {
        return("")
      }
      unchanged <- identical(previous$key, binding$key)
      from_private <- identical(previous$key, private$.private_binding$key)
      if (unchanged || from_private) {
        return("")
      }
      sprintf(
        paste(
          " This replaced previously attached session %s,",
          "which was active for every caller on this server."
        ),
        previous$session_id
      )
    },

    human_key = function(session_id) {
      sprintf("human-%d", as.integer(session_id))
    },

    secondary_key = function(session_id) {
      secondary_session_key(session_id)
    },

    normalize_key = function(key_or_session) {
      if (is.null(key_or_session)) {
        return(NULL)
      }
      if (is.character(key_or_session) && length(key_or_session) == 1L) {
        if (key_or_session %in% names(private$.sessions)) {
          return(key_or_session)
        }
        if (grepl("^\\d+$", key_or_session)) {
          return(private$normalize_key(as.integer(key_or_session)))
        }
        return(key_or_session)
      }
      session_id <- suppressWarnings(as.integer(key_or_session))
      if (is.na(session_id)) {
        return(NULL)
      }
      keys <- c(private$secondary_key(session_id), private$human_key(session_id))
      existing <- keys[keys %in% names(private$.sessions)]
      if (length(existing) > 0L) {
        existing[[1L]]
      } else {
        keys[[1L]]
      }
    },

    # An owned secondary binding is authoritative for its session id: this
    # server started that worker, so it wins over any human binding discovery
    # may have recorded for the same id.
    binding_for_session = function(session_id) {
      secondary <- private$.sessions[[private$secondary_key(session_id)]]
      if (!is.null(secondary)) {
        return(secondary)
      }
      private$.sessions[[private$human_key(session_id)]]
    },

    # Refresh the cache of attachable human sessions from the server callback.
    # Discovery only adds missing bindings; it never assumes ownership, and it
    # never shadows a secondary session this manager already owns.
    discover_human_sessions = function() {
      discover <- private$.callbacks$discover_human
      if (!is.function(discover)) {
        return(invisible(NULL))
      }
      discovered <- discover()
      if (length(discovered) == 0L) {
        return(invisible(NULL))
      }
      for (session_id in as.integer(discovered)) {
        if (is.na(session_id)) {
          next
        }
        owned <- private$.sessions[[private$secondary_key(session_id)]]
        if (!is.null(owned) && isTRUE(owned$owned)) {
          next
        }
        key <- private$human_key(session_id)
        if (is.null(private$.sessions[[key]])) {
          private$.sessions[[key]] <- private$new_binding(
            type = "human",
            session_id = session_id,
            key = key,
            owned = FALSE,
            label = sprintf("%d (available human)", session_id)
          )
        }
      }
      invisible(NULL)
    },

    session_rows = function() {
      if (length(private$.sessions) == 0L) {
        return(character(0))
      }
      vapply(private$.sessions, function(binding) {
        active <- if (identical(private$.active_binding$key, binding$key)) " active" else ""
        type <- switch(
          binding$type,
          human = "human",
          secondary = "secondary",
          binding$type
        )
        sprintf("- %d: %s%s", binding$session_id, type, active)
      }, character(1))
    },

    # Dispatch by binding type so the private session stays local while
    # attached human and secondary sessions go back through server callbacks.
    forward_to_binding = function(data, binding) {
      if (identical(binding$type, "human")) {
        return(private$call_callback("forward_human", data, binding))
      }
      if (identical(binding$type, "secondary")) {
        return(private$call_callback("forward_secondary", data, binding))
      }
      cli::cli_abort("Unknown active session binding type: {.val {binding$type}}", .internal = TRUE)
    }
  )
)
