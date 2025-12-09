# MCP Session Management
# Provides R6 class for session lifecycle management with timeout and resource cleanup.
# Maintains compatibility with existing global state for server communication.

#' MCP Session Management Class
#'
#' @title MCP Session
#' @description R6 class for managing MCP session communication with timeout management.
#' Encapsulates session state while maintaining compatibility with existing global state
#' for server communication. Provides automatic resource cleanup and session lifecycle
#' management.
#' @details Handles session lifecycle:
#' \itemize{
#'   \item \strong{Socket Management}: Creates and binds nanonext sockets for communication
#'   \item \strong{Message Processing}: Async tool execution and response handling
#'   \item \strong{Timeout Management}: Automatic session cleanup after inactivity
#'   \item \strong{Resource Cleanup}: Proper socket and state resource management
#' }
#'
#' @param timeout_seconds Timeout in seconds before session cleanup
#'
#' @export
mcprSession <- R6::R6Class("mcprSession",
  inherit = BaseMCPR,
  public = list(
    #' @description Create new mcprSession instance
    #' @param timeout_seconds Timeout in seconds (default: 900)
    #' @return A new mcprSession instance
    initialize = function(timeout_seconds = 900) {
      self$initialize_base("SESSION")

      private$.timeout_seconds <- timeout_seconds
      private$.last_activity <- Sys.time()

      # Set up automatic cleanup
      reg.finalizer(self, function(x) x$stop(), onexit = TRUE)
    },

    #' @description Start the MCP session and bind to a socket
    #' @param session_id Optional integer socket ID to bind. Auto-selects when NULL.
    #' @param force Logical. If TRUE, allows starting in non-interactive sessions.
    #' @return Self (invisibly) for method chaining
    start = function(session_id = NULL, force = FALSE) {
      if (!rlang::is_interactive() && !isTRUE(force)) {
        return(invisible(self))
      }

      if (private$.is_running) {
        cli::cli_warn("Session is already running")
        private$log_warn("SESSION_ALREADY_RUNNING - Attempted to start running session")
        return(invisible(self))
      }

      self$state_set("session_logger", private$get_logger())

      session_socket <- self$create_socket("poly", "session_communication")
      self$state_set("session_socket", session_socket)

      # Find and bind to available port (explicit session_id when provided)
      private$.session_id <- private$find_available_port(session_id)
      self$state_set("session", private$.session_id) # CRITICAL: Global state for server compatibility

      private$.is_running <- TRUE

      # Log socket diagnostics
      socket_info <- check_session_socket(verbose = FALSE)
      private$log_info(sprintf(
        "MCP session started - Socket: %s, Interactive: %s, Has Session: %s",
        socket_info$socket_number %||% "NULL",
        socket_info$is_interactive,
        socket_info$has_session
      ))

      # Start async message loop
      private$start_listening()

      # Schedule periodic timeout checks
      private$schedule_timeout_check()

      invisible(self)
    },

    #' @description Check for session timeout and cleanup if expired
    #' @return Self (invisibly) for method chaining
    check_timeout = function() {
      if (!private$.is_running || is.null(private$.last_activity)) {
        return(invisible(self))
      }

      if (difftime(Sys.time(), private$.last_activity, units = "secs") > private$.timeout_seconds) {
        self$stop()
      } else {
        private$schedule_timeout_check()
      }

      invisible(self)
    },

    #' @description Stop session and cleanup resources
    #' @return Self (invisibly) for method chaining
    stop = function() {
      if (!private$.is_running) {
        return(invisible(self))
      }

      private$.is_running <- FALSE

      self$cleanup_all()

      # Clean up private state
      private$.session_id <- NULL
      private$.last_activity <- NULL

      invisible(self)
    },

    #' @description Get session information
    #' @return List with session details
    get_info = function() {
      list(
        session_id = private$.session_id,
        is_running = private$.is_running,
        last_activity = private$.last_activity,
        socket_active = self$state_has("session_socket")
      )
    }
  ),
  private = list(
    .session_id = NULL,
    .last_activity = NULL,
    .timeout_seconds = 900, # 15 minutes
    .is_running = FALSE,

    # Find available port using global socket
    find_available_port = function(requested_id = NULL) {
      session_socket <- self$state_get("session_socket")

      if (!is.null(requested_id)) {
        if (!is.numeric(requested_id) || length(requested_id) != 1 || is.na(requested_id) || requested_id < 1) {
          cli::cli_abort("{.arg session_id} must be a positive integer when provided.")
        }
        requested_id <- as.integer(requested_id)
        if (nanonext::listen(
          session_socket,
          url = self$socket_url(requested_id),
          fail = "none"
        )) {
          cli::cli_abort("Requested session ID {.val {requested_id}} is already in use.")
        }
        return(requested_id)
      }

      i <- 1L
      while (i < 1024L) {
        if (nanonext::listen(
          session_socket,
          url = self$socket_url(i),
          fail = "none"
        )) {
          i <- i + 1L
        } else {
          return(i)
        }
      }
      cli::cli_abort("No available socket ports found")
    },

    # Schedule delayed timeout check
    schedule_timeout_check = function() {
      if (private$.is_running) {
        later::later(function() self$check_timeout(), delay = private$.timeout_seconds)
      }
    },

    # Start async message reception loop
    start_listening = function() {
      if (!private$.is_running) {
        return(invisible(self))
      }

      session_socket <- self$state_get("session_socket")
      raio <- nanonext::recv_aio(session_socket, mode = "serial")
      self$state_set("raio", raio)

      promises::as.promise(raio)$then(
        function(data) private$handle_message(data)
      )$catch(
        function(e) {
          # Connection lost - cleanup and stop
          if (private$.is_running) self$stop()
        }
      )

      invisible(self)
    },

    # Process incoming messages
    handle_message = function(data) {
      if (!private$.is_running) {
        return(invisible(self))
      }

      raio <- self$state_get("raio")
      pipe <- nanonext::pipe_id(raio)
      private$.last_activity <- Sys.time()

      private$log_comm("RECEIVED_MESSAGE", paste("Session:", private$.session_id, "| Pipe:", pipe))

      # Schedule next message listen
      private$start_listening()

      # Handle discovery ping
      if (length(data) == 0) {
        private$log_comm("DISCOVERY_PING", paste("Session:", private$.session_id, "| Pipe:", pipe))
        private$send_response(describe_session(detailed = TRUE), pipe)
        return(invisible(self))
      }

      # Log incoming message (exclude tool function to prevent log pollution)
      # Create clean data for logging (remove tool function)
      log_data <- data
      if (!is.null(log_data$tool)) {
        log_data$tool <- paste0("<function: ", data$params$name %||% "unknown", ">")
      }
      private$log_comm("FROM SERVER", paste("Session:", private$.session_id, "| ID:", data$id %||% "unknown", "| Tool:", data$params$name %||% "unknown", "| Data:", jsonlite::toJSON(log_data, auto_unbox = TRUE)))

      # Process tool call or return error with timing
      start_time <- Sys.time()
      body <- if (data$method == "tools/call") {
        tool_name <- data$params$name %||% "unknown"
        private$log_comm("EXECUTING_TOOL", paste("Session:", private$.session_id, "| Tool:", tool_name, "| ID:", data$id %||% "unknown"))
        result <- execute_tool_call(data)
        execution_time <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000, 2)
        private$log_comm("TOOL_COMPLETED", paste("Session:", private$.session_id, "| Tool:", tool_name, "| Duration:", paste0(execution_time, "ms")))
        result
      } else {
        jsonrpc_response(
          data$id,
          error = list(code = -32601, message = "Method not found")
        )
      }

      # Send response
      response_info <- if (inherits(body, "list") && !is.null(body$result)) {
        if (body$result$isError) "ERROR" else "SUCCESS"
      } else {
        "UNKNOWN"
      }
      private$log_comm("TO SERVER", paste("Session:", private$.session_id, "| ID:", data$id %||% "unknown", "| Status:", response_info, "| Response:", to_json(body)))
      private$send_response(to_json(body), pipe)
      invisible(self)
    },

    # Send response using global socket
    send_response = function(response, pipe) {
      if (!private$.is_running || !self$state_has("session_socket")) {
        return(invisible(self))
      }

      session_socket <- self$state_get("session_socket")
      nanonext::send_aio(
        session_socket,
        response,
        mode = "raw",
        pipe = pipe
      )

      invisible(self)
    }
  )
)

#' Make R Session Available to MCP Server
#'
#' @title Make R Session Available to MCP Server
#' @description Makes interactive R session discoverable by MCP server for tool execution.
#' Creates nanonext socket and listens on unique URL for server communication.
#' Enables AI assistants to execute code and tools within specific R session
#' through persistent workspace collaboration.
#'
#' @param timeout_seconds Timeout in seconds before session cleanup (default: 900)
#' @return mcprSession instance (invisibly)
#' @export
mcpr_session_start <- function(timeout_seconds = 900) {
  if (!rlang::is_interactive()) {
    return(invisible())
  }

  # Create and store session instance in global environment for compatibility
  the$mcpr_session <- mcprSession$new(timeout_seconds = timeout_seconds)
  the$mcpr_session$start()

  invisible(the$mcpr_session)
}

#' Stop MCP Session
#'
#' @title Stop MCP Session
#' @description Stops active MCP session and cleans up resources including socket
#' connections and global state variables. Provides clean session termination
#' for resource management and session lifecycle control.
#'
#' @return None (invisible)
#' @export
mcpr_session_stop <- function() {
  if (!is.null(the$mcpr_session)) {
    the$mcpr_session$stop()
    the$mcpr_session <- NULL
    the$session <- NULL
  }
  invisible()
}

#' Launch Headless MCP Session
#'
#' @title Launch MCP Session
#' @description Starts an MCP session in the current process, optionally binding to a
#'   specific session ID and working directory. Designed for non-interactive contexts
#'   where the agent needs to provision an R session on demand.
#'
#' @param session_id Optional integer session/socket identifier to bind.
#' @param timeout_seconds Session timeout before auto-cleanup (default: 900 seconds).
#' @param working_dir Optional path to set as the working directory before launch.
#' @param daemon Logical. If TRUE, return immediately after starting session (for processx spawning).
#'   If FALSE, block in event loop until session stops (for interactive use). Default FALSE.
#'
#' @return The mcprSession instance (invisibly).
#' @export
mcp_session <- function(session_id = NULL, timeout_seconds = 900, working_dir = NULL, daemon = FALSE) {
  if (!is.null(working_dir)) {
    old_dir <- getwd()
    on.exit(setwd(old_dir), add = TRUE)
    setwd(working_dir)
  }

  the$mcpr_session <- mcprSession$new(timeout_seconds = timeout_seconds)
  on.exit(mcpr_session_stop(), add = TRUE)
  the$mcpr_session$start(session_id = session_id, force = TRUE)

  # Daemon mode: Keep process alive with event loop
  # Non-daemon mode: Legacy blocking loop for manual invocation
  if (daemon || !rlang::is_interactive()) {
    repeat {
      later::run_now(timeout = 0.5)
      info <- the$mcpr_session$get_info()
      if (!isTRUE(info$is_running)) {
        break
      }
    }
  }

  invisible(the$mcpr_session)
}
