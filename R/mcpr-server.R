# MCP Server Implementation
# Core server class implementing Model Context Protocol for private R execution.
# Handles JSON-RPC communication, tool discovery, and optional active session attachment.

#' @include mcp-resource-registry.R
#' @include mcpr-base.R
#' @include mcpr-server-tools.R
#' @include mcpr-session-manager.R
#' @include mcpr-server-daemon.R
#' @include mcpr-server-handlers.R
#' @include protocol.R
#' @include tool-register.R
#' @include utils.R
NULL

#' Detect MCP Apps support from client initialize params
#'
#' Checks capabilities.experimental.mcpApps first (spec-driven),
#' then falls back to clientInfo.name matching known UI hosts.
#'
#' @param params The params object from the initialize request
#' @return Logical indicating MCP Apps support
#' @noRd
detect_mcp_apps_support <- function(params) {
  # Spec-driven: check capabilities.experimental.mcpApps
  if (isTRUE(params$capabilities$experimental$mcpApps)) {
    return(TRUE)
  }

  # MCP Apps UI extension (Claude Desktop sends this)
  ui_ext <- params$capabilities$extensions[["io.modelcontextprotocol/ui"]]
  if (!is.null(ui_ext)) {
    return(TRUE)
  }

  # Fallback: match known UI host names (exact match, not substring)
  client_name <- tolower(trimws(params$clientInfo$name %||% ""))
  known_ui_hosts <- c("claude desktop", "claude-ai", "zed", "cline")

  client_name %in% known_ui_hosts
}

#' MCP Server
#' @description Implements a Model Context Protocol server with a private R runtime.
#' Ordinary tools execute in the server process by default. When `manage_r_sessions`
#' is registered, the server can optionally attach to human or secondary R sessions.
#' @details Server operates through layered message handling:
#' \itemize{
#'   \item \strong{Client Layer}: Handles JSON-RPC communication with MCP clients
#'   \item \strong{Server Layer}: Manages tool execution and active session state
#'   \item \strong{Session Layer}: Optionally forwards requests to attached R sessions
#' }
#'
#' @param registry A ToolRegistry instance for tool discovery and management
#' @param log_file Optional log file path for the server logger
#' @param .tools_dir Internal parameter for specifying tools directory path
#' @examples
#' \dontrun{
#' # Basic server initialization
#' server <- mcprServer$new()
#' server$start() # Blocking call
#'
#' # Server with tools discovered from a package or directory
#' registry <- ToolRegistry$new(tools_dir = "path/to/tools")
#' registry$search_tools()
#' server <- mcprServer$new(registry = registry)
#' server$start()
#'
#' # Using convenience function
#' registry <- ToolRegistry$new(tools_dir = "path/to/tools")
#' mcpr_server(registry = registry)
#' }
#' @export
mcprServer <- R6::R6Class("mcprServer",
  inherit = BaseMCPR,
  public = list(
    #' @description Initialize the MCP server with optional tools
    #' @param registry A ToolRegistry instance to use for tool discovery
    #' @param .tools_dir Internal parameter for specifying tools directory path
    #' @param log_file Optional log file path for the server logger
    #' @param execution_timeout_secs Default seconds before a forwarded request is considered
    #'   timed out (default: 300). Override per-call via the `timeout` argument in tools like
    #'   `execute_r_code`.
    #' @param resource_registry An MCPResourceRegistry instance. If NULL (default), a built-in
    #'   registry with the MCPR plot viewer is created automatically.
    #' @return A new mcprServer instance
    initialize = function(registry = NULL, .tools_dir = NULL, log_file = NULL, execution_timeout_secs = 300L,
                          resource_registry = NULL) {
      self$initialize_base("SERVER", log_file = log_file)
      private$.mcpr_version <- mcpr_package_version()

      private$.execution_timeout_secs <- as.integer(execution_timeout_secs)

      if (!is.null(registry) && !inherits(registry, "ToolRegistry")) {
        error_msg <- "registry must be a ToolRegistry instance"
        cli::cli_abort(error_msg)
      }
      if (is.null(registry)) {
        pkg_tools_dir <- if (!is.null(.tools_dir)) .tools_dir else find.package("MCPR")
        if (dir.exists(pkg_tools_dir)) {
          registry <- ToolRegistry$new(
            tools_dir = pkg_tools_dir,
            pattern = "tool-.*\\.R$",
            recursive = FALSE,
            verbose = FALSE
          )
          registry$search_tools()
        }
      }
      set_server_tools(registry = registry)
      private$.tools <- get_mcptools_tools()
      private$.session_manager <- mcprSessionManager$new(
        enabled = private$has_session_capability(),
        local_executor = function(data) private$handle_request(data),
        server = self,
        callbacks = private$session_manager_callbacks()
      )

      if (!is.null(resource_registry) && !inherits(resource_registry, "MCPResourceRegistry")) {
        cli::cli_abort("resource_registry must be an {.cls MCPResourceRegistry} instance")
      }
      if (is.null(resource_registry)) {
        resource_registry <- make_default_mcp_resource_registry(private$.mcpr_version)
      }
      private$.resource_registry <- resource_registry
    },

    #' @description Start the MCP server and begin listening for connections
    #' @note This method should only be called in non-interactive contexts because it blocks execution
    #' @return No return value (blocking call)
    start = function() {
      check_not_interactive()

      # Session sockets still fan in on this cv; stdin no longer rides it.
      private$.cv <- nanonext::cv()

      # Lock-free stdin reader (raw read() on STDIN_FILENO, no fgetc/flockfile),
      # replacing nanonext::read_stdin() whose fgetc-based thread deadlocks R's
      # main thread on the stdin FILE lock under any event-yielding user code.
      stdin_started <- .Call("mcpr_stdin_start", PACKAGE = "MCPR")
      if (!isTRUE(stdin_started)) {
        cli::cli_abort("Could not start the native stdin reader", .internal = TRUE)
      }
      self$register_cleanup(function() .Call("mcpr_stdin_stop", PACKAGE = "MCPR"), "stdin_reader")
      on.exit({
        if (private$.running) self$stop(timeout_ms = 0L) else self$cleanup_all()
      }, add = TRUE)
      private$log_info("stdin reader started")

      private$setup_session_transport()

      # Orphan guard: record the launcher (parent) PID at startup. If the client
      # process dies without a clean stdin EOF, the server reparents to PID 1;
      # nanonext never delivers that EOF, so poll for it here each loop tick.
      launcher_pid <- private$parent_pid()

      # Out-of-loop backstop: a native watchdog thread force-exits this process if
      # it is orphaned, even when the R main thread is wedged in non-yielding user
      # code and cannot reach the is_orphaned() check below (see harness S4b).
      watchdog_started <- .Call(
        "mcpr_watchdog_start", as.integer(launcher_pid), private$.stdin_tick_ms,
        PACKAGE = "MCPR"
      )
      if (!isTRUE(watchdog_started)) {
        self$cleanup_all()
        cli::cli_abort("Could not start the native parent watchdog", .internal = TRUE)
      }
      self$register_cleanup(
        function() .Call("mcpr_watchdog_stop", PACKAGE = "MCPR"),
        "parent_watchdog"
      )

      private$.running <- TRUE
      while (TRUE) {
        # Block up to the tick for a stdin line (immediate condvar wake on arrival);
        # this bounds every iteration so orphan/EOF/sweep run at least this often.
        line <- .Call("mcpr_stdin_poll", private$.stdin_tick_ms, PACKAGE = "MCPR")

        # Parent-death guard: reparented to init (PID 1) or launcher gone -> exit.
        if (private$is_orphaned(launcher_pid)) {
          self$stop()
          break
        }

        # Drain all buffered stdin lines non-blocking, then handle EOF.
        drained <- 0L
        while (is.character(line) && drained < 64L) {
          private$handle_message_from_client(line)
          drained <- drained + 1L
          if (drained >= 64L) break
          line <- .Call("mcpr_stdin_poll", 0L, PACKAGE = "MCPR")
        }
        # FALSE sentinel means stdin closed (EOF from the reader thread).
        if (isFALSE(line)) {
          self$stop(timeout_ms = 0L)
          break
        }

        if (!is.null(private$.session_reader) &&
            !nanonext::unresolved(private$.session_reader)) {
          private$handle_message_from_session(private$.session_reader$data)
          private$.session_reader <- private$arm_session_listener(previous = private$.session_reader)
        }
        # Check daemon listeners for responses
        for (cid in names(private$.daemon_listeners)) {
          dl <- private$.daemon_listeners[[cid]]
          if (!is.null(dl) && !nanonext::unresolved(dl)) {
            private$handle_session_listener_resolved(dl$data, cid, "daemon")
            if (is.character(dl$data)) {
              # Re-arm only on valid response; dead sockets are dropped, not re-armed
              sock <- the$daemon_sockets[[cid]]
              if (!is.null(sock)) {
                private$arm_daemon_listener(cid, sock, previous = dl)
              }
            } else {
              private$.daemon_listeners[[cid]] <- NULL
            }
          } else if (!is.null(dl) && private$daemon_pipe_dropped(cid)) {
            # Worker died without replying (e.g. SIGKILL): the recv_aio never
            # resolves, so detect the lost pipe and fail every outstanding request.
            private$handle_session_listener_resolved(NA, cid, "daemon")
            private$.daemon_listeners[[cid]] <- NULL
          }
        }
        # Check user session listeners for responses
        for (sid_key in names(private$.user_listeners)) {
          ul <- private$.user_listeners[[sid_key]]
          if (!is.null(ul) && !nanonext::unresolved(ul)) {
            private$handle_session_listener_resolved(ul$data, sid_key, "user")
            if (is.character(ul$data)) {
              # Re-arm only on valid response; dead sockets are dropped, not re-armed
              sock <- get_user_session(as.integer(sid_key))
              if (!is.null(sock)) {
                private$arm_user_session_listener(sid_key, sock, previous = ul)
              }
            } else {
              private$.user_listeners[[sid_key]] <- NULL
            }
          }
        }
        if (length(private$.pending_requests) > 0) {
          private$sweep_pending_requests()
        }
        if (!private$.running) break
      }
    },

    #' Stop the running server with graceful shutdown and resource cleanup
    #' @param timeout_ms Timeout in milliseconds for graceful shutdown (default: 5000)
    #' @return The server instance (invisibly) for method chaining
    stop = function(timeout_ms = 5000) {
      was_running <- private$.running
      private$.running <- FALSE

      # Graceful shutdown with timeout for condition variable resolution
      if (was_running && !is.null(private$.cv)) {
        start_time <- Sys.time()
        while (as.numeric(difftime(Sys.time(), start_time, units = "secs")) < (timeout_ms / 1000)) {
          Sys.sleep(0.1)
          if (nanonext::unresolved(private$.cv) == 0) break
        }
      }

      private$.session_manager$cleanup_owned()
      private$.daemon_listeners <- list()
      private$.user_listeners <- list()

      self$cleanup_all()

      # Reset condition variable
      private$.cv <- NULL
      private$.session_reader <- NULL

      invisible(self)
    },

    #' @description Check if the server is currently running
    #' @return TRUE if server is running, FALSE otherwise
    is_running = function() {
      private$.running
    },

    #' @description Get server tools in the specified format
    #' @param format Character string specifying output format: "list" (default) or "json"
    #' @return For "list": named list of ToolDef objects. For "json": list suitable for JSON serialization
    get_tools = function(format = c("list", "json")) {
      format <- match.arg(format)

      if (format == "json") {
        tools <- lapply(unname(private$.tools), tool_as_json)
        return(compact(tools))
      }

      # Default to list format
      res <- private$.tools
      stats::setNames(res, vapply(res, \(x) x$name, character(1)))
    },

    #' @description Get server capabilities for MCP protocol
    #' @param version Protocol version (if NULL, uses latest supported version)
    #' @return List of server capabilities
    get_capabilities = function(version = NULL) {
      # Thin wrapper around create_capabilities from protocol.R
      create_capabilities(
        version = version %||% max(SUPPORTED_VERSIONS),
        server_name = "R MCPR server",
        server_version = private$.mcpr_version
      )
    },

    #' @description Check if the connected client supports MCP Apps
    #' @return Logical indicating MCP Apps support
    mcp_apps_supported = function() {
      private$.mcp_apps_supported
    },

    #' @description Return the server-owned session manager.
    #' @return A mcprSessionManager instance.
    session_manager = function() {
      private$.session_manager
    },

    #' @description Return whether this server exposes session-management capability.
    #' @return Logical scalar.
    session_management_enabled = function() {
      private$.session_manager$is_enabled()
    },

    #' @description Return the active session binding owned by this server.
    #' @return An environment describing the active binding.
    active_session_binding = function() {
      private$.session_manager$active_binding()
    },

    #' @description Return the active session label owned by this server.
    #' @return Character scalar.
    active_session_label = function() {
      private$.session_manager$active_label()
    }
  ),
  private = list(
    .cv = NULL,
    .stdin_tick_ms = 250L,  # max time the loop blocks per iteration waiting for a stdin line
    .running = FALSE,
    .protocol_version = NULL,  # Negotiated protocol version for this connection
    .mcp_apps_supported = FALSE,
    .mcpr_version = "unknown",
    .resource_registry = NULL,
    .tools = list(),
    .client_name = "unknown",
    .client_interface = "unknown",
    .session_manager = NULL,
    .session_reader = NULL,
    .daemon_listeners = list(),
    .user_listeners = list(),
    .pending_requests = list(),     # session_key -> list(active = <record>|NULL, waiting = list(<record>))
    .terminal_wire_ids = character(0), # internal forwarded ids whose late responses must be dropped
    .forward_sequence = 0,         # monotonically increasing source for internal forwarded request ids
    .max_waiting_per_session = 64L, # bounded FIFO depth behind the one active request
    .execution_timeout_secs = 300L, # server-level default execution timeout

    # Current parent (launcher) PID via a C-level ps call (no subprocess).
    # Returns NA if it cannot be read; the caller then falls back to its belt.
    parent_pid = function() {
      ppid <- tryCatch(ps::ps_ppid(), error = function(e) NA_integer_)
      if (length(ppid) != 1L) NA_integer_ else as.integer(ppid)
    },

    # Orphan detection: TRUE once the launcher is gone. Primary signal is
    # reparent-to-init (current PPID == 1), robust against PID reuse. Belt:
    # the recorded launcher PID no longer accepts signal 0.
    is_orphaned = function(launcher_pid) {
      ppid <- private$parent_pid()
      if (!is.na(ppid) && ppid <= 1L) return(TRUE)
      if (!is.na(launcher_pid) && launcher_pid > 1L &&
          !isTRUE(tools::pskill(launcher_pid, signal = 0L))) {
        return(TRUE)
      }
      FALSE
    },

    has_session_capability = function() {
      "manage_r_sessions" %in% names(private$.tools)
    },

    # Build the server-owned callback bridge for session routing and cleanup.
    # The manager stays policy-only; these closures own the actual socket and
    # process operations for human and secondary sessions.
    session_manager_callbacks = function() {
      list(
        start_secondary = function(working_dir = getwd()) {
          session_id <- find_daemon_port()
          key <- secondary_session_key(session_id)
          private$log_info(sprintf("Starting secondary session %d", session_id))
          proc <- spawn_daemon(key, session_id, working_dir = working_dir)
          register_daemon(key, session_id)

          sock <- private$connect_daemon_socket(session_id, key, timeout_ms = 15000L)
          if (is.null(sock)) {
            unregister_daemon(key)
            cli::cli_abort("Secondary session {session_id} failed to connect within timeout")
          }

          the$daemon_sockets[[key]] <- sock
          private$arm_daemon_listener(key, sock)
          list(session_id = session_id, key = key, socket = sock, process = proc)
        },
        join_human = function(session_id) {
          existing_sock <- get_user_session(session_id)
          if (!is.null(existing_sock)) {
            unregister_user_session(session_id)
          }

          sock <- connect_ipc_socket(session_id, timeout_ms = 15000L)
          if (is.null(sock)) {
            cli::cli_abort("Could not connect to session {session_id}. Is the session running?")
          }

          register_user_session(session_id, sock)
          if (!is.null(private$.cv)) {
            private$arm_user_session_listener(as.character(session_id), sock)
          }
          list(session_id = session_id, socket = sock)
        },
        discover_human = function() {
          private$discover_human_session_ids()
        },
        forward_human = function(data, binding) {
          sock <- binding$socket %||% get_user_session(binding$session_id)
          if (is.null(sock)) {
            cli::cli_abort("User session {binding$session_id} socket not found")
          }
          private$forward_request_to_user(data, binding$session_id, sock)
        },
        forward_secondary = function(data, binding) {
          if (is.null(the$daemon_sockets[[binding$key]])) {
            cli::cli_abort("Secondary session {binding$session_id} socket not found")
          }
          private$forward_request_to_daemon(data, binding$key)
        },
        close_secondary = function(binding) {
          private$.daemon_listeners[[binding$key]] <- NULL
          unregister_daemon(binding$key)
        },
        detach_human = function(binding) {
          sid_key <- as.character(binding$session_id)
          private$.user_listeners[[sid_key]] <- NULL
          current_sock <- get_user_session(binding$session_id)
          if (!is.null(current_sock) && identical(current_sock, binding$socket)) {
            unregister_user_session(binding$session_id)
          }
        }
      )
    },

    # Arm a listener for secondary session responses.
    # Must pass cv so the main event loop wakes on secondary replies.
    arm_daemon_listener = function(client_id, sock, previous = NULL) {
      if (!is.null(previous) && nanonext::is_aio(previous)) {
        nanonext::stop_aio(previous)
      }
      reader <- nanonext::recv_aio(sock, mode = "string", cv = private$.cv)
      private$.daemon_listeners[[client_id]] <- reader
      reader
    },

    # Detect a secondary worker that died without replying. nanonext's recv_aio
    # does not resolve on a SIGKILLed peer, so we watch the socket's pipe count:
    # once a connected daemon socket drops to zero pipes while a request is still
    # outstanding on it, the worker is gone and the request must be failed.
    daemon_pipe_dropped = function(client_id) {
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
    },

    # Forward a tool call request to a secondary session's socket.
    # Resolves the socket from the legacy registry and enters the bounded queue.
    forward_request_to_daemon = function(data, client_id) {
      sock <- the$daemon_sockets[[client_id]]
      if (is.null(sock)) {
        return(cat_json(jsonrpc_response(
          data$id,
          error = list(code = -32603, message = "Secondary session socket not found")
        )))
      }
      prepared <- private$prepare_forward_request(data)
      if (is.list(prepared) && !is.null(prepared$error)) {
        return(cat_json(prepared))
      }
      queued <- private$enqueue_pending_request(prepared, client_id)
      if (isTRUE(queued)) {
        private$send_active_request(client_id, sock, label = "TO DAEMON")
      } else if (identical(queued, FALSE)) {
        private$log_debug(sprintf(
          "Queued request %s behind active request on session '%s'",
          data$id, client_id
        ))
      }
    },

    # Connect to a secondary session using pipe_notify + until (mirai pattern).
    # Dials the secondary session's IPC URL with autostart = TRUE so nanonext retries
    # automatically. When the secondary session calls listen(), the pipe is established
    # and the CV is signalled immediately - no polling, no Sys.sleep.
    # Returns a connected socket on success, or NULL on timeout / dead process.
    connect_daemon_socket = function(session_id, client_id = NULL, timeout_ms = 15000L) {
      url <- sprintf("%s%d", get_system_socket_url(), as.integer(session_id))
      sock <- nanonext::socket("poly")
      cv <- nanonext::cv()
      nanonext::pipe_notify(sock, cv, add = TRUE)
      nanonext::dial(sock, url = url, fail = "none")  # autostart = TRUE (default)

      connected <- nanonext::until(cv, as.integer(timeout_ms))
      nanonext::pipe_notify(sock, NULL, add = TRUE)  # deregister notification

      if (!connected) {
        nanonext::reap(sock)
        return(NULL)
      }
      sock
    },

    # Attach the stdin-backed session transport only when session management
    # is enabled. The server dials its own socket 1 so the main CV can watch
    # attached-session replies alongside client traffic.
    setup_session_transport = function() {
      if (!private$.session_manager$is_enabled()) {
        private$log_info("Session transport disabled for local-only server")
        return(NULL)
      }

      server_socket <- self$create_socket("poly", "server_communication")
      self$state_set("server_socket", server_socket)
      nanonext::dial(server_socket, url = self$socket_url(1L))

      socket_info <- check_session_socket(verbose = FALSE)
      private$log_info(sprintf(
        "Session transport enabled - Socket: %s, Interactive: %s, Has Session: %s",
        socket_info$socket_number %||% "NULL",
        socket_info$is_interactive,
        socket_info$has_session
      ))

      private$.session_reader <- private$arm_session_listener()
      private$.session_reader
    },

    # Keep a single AIO listener on the server socket and stop the previous
    # one before re-arming. This avoids leaking readers across responses.
    arm_session_listener = function(previous = NULL) {
      if (!is.null(previous) && nanonext::is_aio(previous)) {
        nanonext::stop_aio(previous)
      }

      nanonext::recv_aio(
        self$state_get("server_socket"),
        mode = "string",
        cv = private$.cv
      )
    },

    # Probe the well-known socket range for attached human sessions.
    # Discovery is best-effort and only reflects currently listening peers.
    discover_human_session_ids = function() {
      socket_base <- get_system_socket_url()
      sock <- nanonext::socket("poly")
      on.exit(nanonext::reap(sock), add = TRUE)

      cv <- nanonext::cv()
      monitor <- nanonext::monitor(sock, cv)

      for (i in seq_len(1024L)) {
        if (
          nanonext::dial(
            sock,
            url = sprintf("%s%d", socket_base, i),
            autostart = NA,
            fail = "none"
          ) &&
            i > 8L
        ) {
          break
        }
      }

      pipes <- nanonext::read_monitor(monitor)
      if (length(pipes) == 0L) {
        return(integer(0))
      }

      responses <- lapply(
        pipes,
        function(x) nanonext::recv_aio(sock, mode = "string", timeout = 500L)
      )
      lapply(
        pipes,
        function(x) nanonext::send_aio(sock, character(), mode = "serial", pipe = x)
      )

      session_data <- as.character(nanonext::collect_aio_(responses))
      matches <- regmatches(session_data, regexec("^(\\d+):", session_data))
      ids <- vapply(matches, function(match) {
        if (length(match) >= 2L) as.integer(match[[2L]]) else NA_integer_
      }, integer(1))
      ids[!is.na(ids)]
    },

    # Handle incoming messages from MCP clients
    handle_message_from_client = function(line) {
      if (length(line) == 0) {
        return()
      }
      private$log_comm("FROM CLIENT", line)
      data <- tryCatch(
        jsonlite::parse_json(line),
        error = function(e) NULL
      )
      if (is.null(data)) {
        return()
      }

      if (!is.list(data) || is.null(data$method)) {
        return(cat_json(jsonrpc_response(
          data$id,
          error = list(code = -32600, message = "Invalid Request")
        )))
      }

      handlers <- make_mcpr_server_handlers(self, private)

      # Route message and send response
      response <- private$route_message(data, handlers)
      if (!is.null(response)) {
        cat_json(response)
      }
    },

    # Handle messages from R sessions. session_key is provided for daemon/user sessions
    # so we can correlate the response to a pending request and drop late responses.
    handle_message_from_session = function(data, session_key = NULL) {
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
    },

    # Handle tool execution requests locally on the server
    handle_request = function(data) {
      prepared <- private$append_tool_fn(data)
      result <- if (is.list(prepared) && !is.null(prepared$error)) {
        prepared
      } else {
        set_mcpr_request_context(as_mcpr_request_context(
          mcp_apps_supported = private$.mcp_apps_supported,
          interface = private$.client_interface,
          client_name = private$.client_name
        ))
        on.exit(clear_mcpr_request_context(), add = TRUE)
        execute_tool_call(prepared)
      }
      private$log_comm("FROM SERVER", to_json(result))
      cat_json(result)
    },

    # Arm a recv_aio listener on a user session socket, waking the main CV on arrival.
    arm_user_session_listener = function(sid_key, sock, previous = NULL) {
      if (!is.null(previous) && nanonext::is_aio(previous)) {
        nanonext::stop_aio(previous)
      }
      reader <- nanonext::recv_aio(sock, mode = "string", cv = private$.cv)
      private$.user_listeners[[sid_key]] <- reader
      reader
    },

    # Forward a tool call request to a registered user session socket.
    forward_request_to_user = function(data, session_id, sock) {
      sid_key <- as.character(session_id)
      if (is.null(sock)) {
        return(cat_json(jsonrpc_response(
          data$id,
          error = list(code = -32603, message = sprintf("User session %d socket not found", session_id))
        )))
      }
      if (is.null(private$.user_listeners[[sid_key]])) {
        private$arm_user_session_listener(sid_key, sock)
      }
      prepared <- private$prepare_forward_request(data)
      if (is.list(prepared) && !is.null(prepared$error)) {
        return(cat_json(prepared))
      }
      queued <- private$enqueue_pending_request(prepared, sid_key)
      if (isTRUE(queued)) {
        private$send_active_request(sid_key, sock, label = "TO USER SESSION")
      } else if (identical(queued, FALSE)) {
        private$log_debug(sprintf(
          "Queued request %s behind active request on user session '%s'",
          data$id, sid_key
        ))
      }
    },

    # Validate a forwarded request and add the tool function before it enters the
    # pending queue. Validation failures must never create phantom active records.
    prepare_forward_request = function(data) {
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
    },

    # Send the active record and retain its AIO until completion. Immediate send
    # failures resolve the record and promote the next queued request.
    send_active_request = function(session_key, sock, label = "TO TARGET") {
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
    },

    # Enqueue a forwarded request and report whether it should be sent now.
    # session_key is the attached secondary key or the human session sid_key.
    # Each session tracks one `active` evaluation plus a FIFO `waiting` queue: a
    # session evaluates one request at a time, so a second concurrent call is held
    # here (queued) rather than forwarded to the busy worker. Only the active
    # record has a deadline (sent_at); queued records are armed when promoted.
    # Returns TRUE when the request became active (caller should forward it now),
    # FALSE when it was queued (caller must not forward).
    enqueue_pending_request = function(data, session_key) {
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
    },

    # Promote the FIFO head of a session's queue to active and forward it to the
    # session's CURRENT socket. No-op if nothing is waiting. `sock` is resolved by
    # key so migrated (recycled) sessions reach the new worker.
    dispatch_next = function(session_key, sock) {
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
    },

    # Resolve the current socket for a session_key. Secondary sessions live in the
    # daemon registry (keyed by the binding key); human sessions in the user registry
    # (keyed by the integer session id). Returns NULL if neither has it.
    session_socket_for = function(session_key) {
      sock <- the$daemon_sockets[[session_key]]
      if (!is.null(sock)) {
        return(sock)
      }
      get_user_session(suppressWarnings(as.integer(session_key)))
    },

    # Send a terminal dead-session error to a pending record's client.
    fail_pending_dead = function(record, session_key) {
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
    },

    # Return a terminal error for a request that could not be placed on its socket.
    fail_pending_send = function(record, session_key, reason) {
      cat_json(jsonrpc_response(
        record$client_request_id,
        error = list(
          code = -32603L,
          message = sprintf("Could not send request to session '%s': %s", session_key, reason)
        )
      ))
    },

    # Retire a failed attached transport consistently. Secondary workers are
    # MCPR-owned and are killed; human sessions are detached without killing R.
    retire_session_transport = function(session_key) {
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
    },

    # Called when a daemon or user session listener resolves.
    # If data is a nanonext error value (peer closed connection), return a dead-session
    # error to the waiting client immediately. Otherwise dispatch normally.
    handle_session_listener_resolved = function(data, session_key, session_type) {
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
    },

    # Sweep all pending forwarded requests. For each:
    #   - Tier-2: return a timeout error if elapsed > timeout_secs.
    # Tier-1 (dead-session via nanonext error) is handled immediately in
    # handle_session_listener_resolved, so we only need the timeout sweep here.
    sweep_pending_requests = function() {
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
    },

    # Routes incoming JSON-RPC messages to appropriate handlers
    route_message = function(data, handlers) {
      method <- data$method

      if (method %in% names(handlers)) {
        handler <- handlers[[method]]
        return(handler(data))
      }

      # Default error response for unknown methods
      jsonrpc_response(
        data$id,
        error = list(code = -32601, message = "Method not found")
      )
    },

    # Validates tool existence and appends function reference to request data
    append_tool_fn = function(data) {
      if (!identical(data$method, "tools/call")) {
        return(data)
      }
      tool_name <- data$params$name
      if (!tool_name %in% names(private$.tools)) {
        return(jsonrpc_response(
          data$id,
          error = list(code = -32601, message = "Method not found")
        ))
      }
      tooldef <- private$.tools[[tool_name]]
      data$tool <- structure(
        tooldef$fun,
        mcpr_arguments = tooldef$arguments,
        mcpr_convert = tooldef$convert
      )
      data
    }
  )
)

#' Start MCP Server
#'
#' @title Start MCP Server
#' @description Convenience function to initialize and start MCP server in one call.
#' Creates mcprServer instance and begins listening for client connections through
#' blocking event loop with automatic tool discovery and registration.
#'
#' @param registry A ToolRegistry instance to use for tool discovery
#' @param resource_registry An MCPResourceRegistry instance for custom MCP resources.
#'   If NULL (default), the built-in plot viewer registry is used.
#' @param log_file Optional log file path for the server logger. If NULL, the logger
#'   falls back to the package default.
#' @param execution_timeout_secs Default seconds before a forwarded request is considered
#'   timed out (default: 300). Individual tools can override via their `timeout` argument.
#' @return The server instance (invisibly)
#' @examples
#' \dontrun{
#' MCPR::mcpr_server()
#' }
#' @export
mcpr_server <- function(registry = NULL, resource_registry = NULL, log_file = NULL,
                        execution_timeout_secs = 300L) {
  server <- mcprServer$new(
    registry               = registry,
    resource_registry      = resource_registry,
    log_file               = log_file,
    execution_timeout_secs = execution_timeout_secs
  )
  server$start()
  invisible(server)
}
