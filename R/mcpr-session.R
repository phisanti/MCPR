#' R6 Class for MCP Session Management
#'
#' @description
#' MCPRSession provides a cohesive class-based approach to managing MCP session
#' communication with nanonext sockets. Encapsulates all session state and
#' provides proper resource management.
#'
#' @details
#' The class handles socket creation, async message processing, timeout management,
#' and graceful cleanup. It replaces the previous functional approach with
#' proper encapsulation and automatic resource management.
#'
#' @export
MCPRSession <- R6::R6Class("MCPRSession",
  private = list(
    .socket = NULL,
    .session_id = NULL,
    .raio = NULL,
    .last_activity = NULL,
    .timeout_seconds = 900,  # 15 minutes
    .is_running = FALSE,
    
    # Find available socket port
    find_available_port = function() {
      i <- 1L
      while (i < 1024L) {
        if (nanonext::listen(
          private$.socket,
          url = sprintf("%s%d", the$socket_url, i),
          fail = "none"
        )) {
          i <- i + 1L
        } else {
          return(i)
        }
      }
      stop("No available socket ports found", call. = FALSE)
    },
    
    # Schedule timeout check
    schedule_timeout_check = function() {
      if (private$.is_running) {
        later::later(function() self$check_timeout(), delay = private$.timeout_seconds)
      }
    }
  ),
  
  public = list(
    #' @description Create new MCPRSession instance
    #' @param timeout_seconds Timeout in seconds (default: 900)
    initialize = function(timeout_seconds = 900) {
      private$.timeout_seconds <- timeout_seconds
      private$.socket <- nanonext::socket("poly")
      private$.last_activity <- Sys.time()
      
      # Set up automatic cleanup
      reg.finalizer(self, function(x) x$stop(), onexit = TRUE)
    },
    
    #' @description Start the MCP session
    start = function() {
      if (!rlang::is_interactive()) {
        return(invisible(self))
      }
      
      if (private$.is_running) {
        warning("Session is already running")
        return(invisible(self))
      }
      
      # Find and bind to available port
      private$.session_id <- private$find_available_port()
      private$.is_running <- TRUE
      
      # Start async message loop
      self$start_listening()
      
      # Schedule periodic timeout checks
      private$schedule_timeout_check()
      
      invisible(self)
    },
    
    #' @description Start async message listening loop
    start_listening = function() {
      if (!private$.is_running) return(invisible(self))
      
      private$.raio <- nanonext::recv_aio(private$.socket, mode = "serial")
      promises::as.promise(private$.raio)$then(
        function(data) self$handle_message(data)
      )$catch(
        function(e) {
          # Connection lost - cleanup and stop
          if (private$.is_running) self$stop()
        }
      )
      
      invisible(self)
    },
    
    #' @description Handle incoming message from server
    #' @param data Message data from server
    handle_message = function(data) {
      if (!private$.is_running) return(invisible(self))
      
      pipe <- nanonext::pipe_id(private$.raio)
      private$.last_activity <- Sys.time()
      
      # Schedule next message listen
      self$start_listening()
      
      # Handle discovery ping
      if (length(data) == 0) {
        self$send_response(describe_session(), pipe)
        return(invisible(self))
      }
      
      # Process tool call or return error
      body <- if (data$method == "tools/call") {
        execute_tool_call(data)
      } else {
        jsonrpc_response(
          data$id,
          error = list(code = -32601, message = "Method not found")
        )
      }
      
      self$send_response(to_json(body), pipe)
      invisible(self)
    },
    
    #' @description Send response back to server
    #' @param response Response data
    #' @param pipe Pipe ID for response routing
    send_response = function(response, pipe) {
      if (!private$.is_running || is.null(private$.socket)) {
        return(invisible(self))
      }
      
      nanonext::send_aio(
        private$.socket,
        response,
        mode = "raw",
        pipe = pipe
      )
      
      invisible(self)
    },
    
    #' @description Check for session timeout
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
    stop = function() {
      if (!private$.is_running) {
        return(invisible(self))
      }
      
      private$.is_running <- FALSE
      
      if (!is.null(private$.socket)) {
        nanonext::reap(private$.socket)
        private$.socket <- NULL
      }
      
      private$.session_id <- NULL
      private$.raio <- NULL
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
        socket_active = !is.null(private$.socket)
      )
    }
  )
)

#' Make an R session available to the MCP server
#'
#' @description
#' Call this function in an interactive R session to make it discoverable by the
#' MCP server. This allows MCP clients (like AI assistants) to execute code and
#' tools within this specific R session.
#'
#' It is recommended to add `mcptools::mcp_session()` to your `.Rprofile` to
#' automatically make every interactive session available.
#'
#' @details
#' The function works by creating a `nanonext` socket and listening on a
#' unique URL. The MCP server can then connect to this socket to communicate
#' with the R session. It only runs in interactive sessions.
#'
#' @param timeout_seconds Timeout in seconds before session cleanup (default: 900)
#' @export
#' @examples
#' if (interactive()) {
#'   mcp_session()
#' }
mcp_session <- function(timeout_seconds = 900) {
  if (!rlang::is_interactive()) {
    return(invisible())
  }
  
  # Store session in global environment for compatibility
  the$mcpr_session <- MCPRSession$new(timeout_seconds = timeout_seconds)
  the$mcpr_session$start()
  
  # Legacy compatibility - store session ID
  the$session <- the$mcpr_session$get_info()$session_id
  
  invisible(the$mcpr_session)
}

#' Stop the MCP session and clean up resources
#' @export
mcp_session_stop <- function() {
  if (!is.null(the$mcpr_session)) {
    the$mcpr_session$stop()
    the$mcpr_session <- NULL
    the$session <- NULL
  }
  invisible()
}