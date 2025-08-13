#' MCP Session Management
#'
#' @title MCP Session
#' @description Provides class-based approach to managing MCP session communication.
#' Encapsulates session state through nanonext socket management with async message
#' processing and automatic resource cleanup. Enables persistent R session discovery
#' and tool execution coordination through proper state encapsulation.
#' @details Handles session lifecycle management:
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

  
  public = list(
    #' @description Create new mcprSession instance
    #' @param timeout_seconds Timeout in seconds (default: 900)
    #' @return A new mcprSession instance
    initialize = function(timeout_seconds = 900) {
      private$.timeout_seconds <- timeout_seconds
      private$.socket <- nanonext::socket("poly")
      private$.last_activity <- Sys.time()
      private$.messenger <- MessageHandler$new()
      
      # Set up automatic cleanup
      reg.finalizer(self, function(x) x$stop(), onexit = TRUE)
    },
    
    #' @description Start the MCP session in interactive contexts
    #' @return Self (invisibly) for method chaining
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
  ),
  private = list(
    .socket = NULL,
    .session_id = NULL,
    .raio = NULL,
    .last_activity = NULL,
    .timeout_seconds = 900,  # 15 minutes
    .is_running = FALSE,
    .messenger = NULL,
    
    # Iterates through available ports to find unused socket binding
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
    
    # Schedules delayed timeout check using later package for async execution
    schedule_timeout_check = function() {
      if (private$.is_running) {
        later::later(function() self$check_timeout(), delay = private$.timeout_seconds)
      }
    },
    
    # Initiates async message reception loop with promise-based error handling
    start_listening = function() {
      if (!private$.is_running) return(invisible(self))
      
      private$.raio <- nanonext::recv_aio(private$.socket, mode = "serial")
      promises::as.promise(private$.raio)$then(
        function(data) private$handle_message(data)
      )$catch(
        function(e) {
          # Connection lost - cleanup and stop
          if (private$.is_running) self$stop()
        }
      )
      
      invisible(self)
    },
    
    # Processes incoming messages and executes tool calls with response routing
    handle_message = function(data) {
      if (!private$.is_running) return(invisible(self))
      
      pipe <- nanonext::pipe_id(private$.raio)
      private$.last_activity <- Sys.time()
      
      # Schedule next message listen
      private$start_listening()
      
      # Handle discovery ping
      if (length(data) == 0) {
        private$send_response(describe_session(), pipe)
        return(invisible(self))
      }
      
      # Process tool call or return error
      body <- if (data$method == "tools/call") {
        execute_tool_call(data)
      } else {
        private$.messenger$create_error(
          data$id,
          code = -32601,
          message = "Method not found"
        )
      }
      
      private$send_response(to_json(body), pipe)
      invisible(self)
    },
    
    # Sends response data back to server through specified pipe channel
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
mcpr_session <- function(timeout_seconds = 900) {
  if (!rlang::is_interactive()) {
    return(invisible())
  }
  
  # Store session in global environment for compatibility
  the$mcpr_session <- mcprSession$new(timeout_seconds = timeout_seconds)
  the$mcpr_session$start()
  
  # Legacy compatibility - store session ID
  the$session <- the$mcpr_session$get_info()$session_id
  
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
mcpr_session_stop <- function() {
  if (!is.null(the$mcpr_session)) {
    the$mcpr_session$stop()
    the$mcpr_session <- NULL
    the$session <- NULL
  }
  invisible()
}

#' Legacy Naming Assignment for MCP Session
#'
#' @title Legacy Naming Assignment for MCP Session
#' @description Direct assignment alias for backward compatibility.
#' Provides the old function name as a direct reference to the new function.
#'
#' @export
mcp_session <- mcpr_session
