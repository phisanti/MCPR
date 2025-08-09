#' @include mcpr-server-tools.R
#' @include tool-execution.R

# MCP Server Implementation
# Core server class for handling MCP protocol communication and tool execution.
# Manages three-layer architecture: client communication, tool routing, and session forwarding.
#'
#' @title MCP Server
#' @description Implements Model Context Protocol server for persistent R session management.
#' Operates through nanonext sockets for non-blocking message handling between JSON-RPC 
#' clients and R sessions, enabling tool execution routing and workspace state persistence.
#' @details Server operates through layered message handling:
#' \itemize{
#'   \item \strong{Client Layer}: Handles JSON-RPC communication with MCP clients
#'   \item \strong{Server Layer}: Manages tool execution and session routing
#'   \item \strong{Session Layer}: Forwards requests to active R sessions
#' }
#'
#' @param registry A ToolRegistry instance for tool discovery and management
#' @param .tools_dir Internal parameter for specifying tools directory path
#' @examples
#' \dontrun{
#' # Basic server initialization
#' server <- mcprServer$new()
#' server$start()  # Blocking call
#' 
#' # Server with custom tools
#' my_tool <- list(
#'   name = "mean",
#'   description = "Calculate arithmetic mean",
#'   fun = mean,
#'   arguments = list(x = "numeric")
#' )
#' registry <- ToolRegistry$new(); registry$add_tool(my_tool)
#' server <- mcprServer$new(registry = registry)
#' server$start()
#' 
#' # Using convenience function
#' registry <- ToolRegistry$new(tools_dir = "path/to/tools")
#' mcpr_server(registry = registry)
#' }
#'
#' @export
mcprServer <- R6::R6Class("mcprServer",
  public = list(
    #' @description Initialize the MCP server with optional tools
    #' @param registry A ToolRegistry instance to use for tool discovery
    #' @param .tools_dir Internal parameter for specifying tools directory path
    #' @return A new mcprServer instance
    initialize = function(registry = NULL, .tools_dir = NULL) {
      if (!is.null(registry) && !inherits(registry, "ToolRegistry")) {
        cli::cli_abort("registry must be a ToolRegistry instance")
      }
      
      # Initialize messenger for JSON-RPC communication
      private$.messenger <- MessageHandler$new(logger = logcat)
      if (is.null(registry)) {
        pkg_tools_dir <- if (!is.null(.tools_dir)) {
          .tools_dir
        } else {
          # If the package is installed, the inst/ folder is off-loaded to 
          # the package directory
          find.package("MCPR") 
        }
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
    },

    #' @description Start the MCP server and begin listening for connections
    #' @note This method should only be called in non-interactive contexts because it blocks execution
    #' @return No return value (blocking call)
    start = function() {
      check_not_interactive()

      private$.cv <- nanonext::cv()
      private$.reader_socket <- nanonext::read_stdin()
      on.exit(nanonext::reap(private$.reader_socket), add = TRUE)
      nanonext::pipe_notify(private$.reader_socket, private$.cv, remove = TRUE, flag = TRUE)

      the$server_socket <- nanonext::socket("poly")
      on.exit(nanonext::reap(the$server_socket), add = TRUE)
      # TODO: Make session discovery more robust than dialing a fixed number
      nanonext::dial(the$server_socket, url = sprintf("%s%d", the$socket_url, 1L))

      client <- nanonext::recv_aio(private$.reader_socket, mode = "string", cv = private$.cv)
      session <- nanonext::recv_aio(the$server_socket, mode = "string", cv = private$.cv)

      private$.running <- TRUE
      while (nanonext::wait(private$.cv)) {
        if (!nanonext::unresolved(session)) {
          private$handle_message_from_session(session$data)
          session <- nanonext::recv_aio(the$server_socket, mode = "string", cv = private$.cv)
        }
        if (!nanonext::unresolved(client)) {
          private$handle_message_from_client(client$data)
          client <- nanonext::recv_aio(private$.reader_socket, mode = "string", cv = private$.cv)
        }
      }
    },

    #' @description Stop the running server with graceful shutdown and resource cleanup
    #' @param timeout_ms Timeout in milliseconds for graceful shutdown (default: 5000)
    #' @return The server instance (invisibly) for method chaining
    stop = function(timeout_ms = 5000) {
      if (!private$.running) {
        return(invisible(self))
      }
      
      private$.running <- FALSE
      
      # Graceful shutdown with timeout
      start_time <- Sys.time()
      while (!is.null(private$.cv) && 
             as.numeric(difftime(Sys.time(), start_time, units = "secs")) < (timeout_ms/1000)) {
        Sys.sleep(0.1)
        if (nanonext::unresolved(private$.cv) == 0) break
      }
      
      # Close and cleanup sockets
      if (!is.null(private$.reader_socket)) {
        nanonext::reap(private$.reader_socket)
        private$.reader_socket <- NULL
      }
      
      if (!is.null(the$server_socket)) {
        nanonext::reap(the$server_socket)
        the$server_socket <- NULL
      }
      
      # Reset condition variable
      private$.cv <- NULL
      
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
        tools <- lapply(the$server_tools, tool_as_json)
        return(compact(tools))
      }
      
      # Default to list format
      res <- the$server_tools  
      stats::setNames(res, vapply(res, \(x) x$name, character(1)))
    }
  ),

  private = list(
    .reader_socket = NULL,
    .cv = NULL,
    .running = FALSE,
    .messenger = NULL,

    # === MESSAGE HANDLERS ===

    # Parses and routes incoming JSON-RPC messages from MCP clients to appropriate handlers
    handle_message_from_client = function(line) {
      if (length(line) == 0) {
        return()
      }
      
      data <- private$.messenger$parse_message(line)
      if (is.null(data)) return()

      # Define method handlers
      handlers <- list(
        "initialize" = function(data) {
          private$.messenger$create_response(data$id, private$.messenger$create_capabilities())
        },
        "tools/list" = function(data) {
          private$.messenger$create_response(
            data$id,
            list(tools = self$get_tools("json"))
          )
        },
        "resources/list" = function(data) {
          private$.messenger$create_response(
            data$id,
            list(resources = list())
          )
        },
        "prompts/list" = function(data) {
          private$.messenger$create_response(
            data$id,
            list(prompts = list())
          )
        },
        "tools/call" = function(data) {
          tool_name <- data$params$name
          # Execute server-side tools directly, or if no session is active
          if (tool_name %in% c("manage_r_sessions", "execute_r_code") ||
              !nanonext::stat(the$server_socket, "pipes")) {
            private$handle_request(data)
          } else {
            private$forward_request(data)
          }
        },
        "notifications/initialized" = function(data) {
          # Notification, no response needed
          NULL
        }
      )

      # Route message and send response
      response <- private$.messenger$route_message(data, handlers)
      if (!is.null(response)) {
        private$.messenger$output_json(response)
      }
    },

    # Forwards responses from R sessions directly back to MCP clients
    handle_message_from_session = function(data) {
      if (!is.character(data)) {
        return()
      }
      logcat(c("FROM SESSION: ", data))
      # Forward response directly to the client
      nanonext::write_stdout(data)
    },

    # Executes tool requests locally on server and sends results to client
    handle_request = function(data) {
      prepared <- private$append_tool_fn(data)
      result <- if (inherits(prepared, "jsonrpc_error")) {
        prepared
      } else {
        execute_tool_call(prepared)
      }
      log_result <- if (inherits(result, "jsonrpc_error")) unclass(result) else result
      logcat(c("FROM SERVER: ", to_json(log_result)))
      if (inherits(result, "jsonrpc_error")) {
        nanonext::write_stdout(to_json(unclass(result)))
      } else {
        nanonext::write_stdout(to_json(result))
      }
    },

    # Forwards validated tool requests to active R sessions for execution
    forward_request = function(data) {
      logcat(c("TO SESSION: ", jsonlite::toJSON(data)))
      prepared <- private$append_tool_fn(data)
      if (inherits(prepared, "jsonrpc_error")) {
        return(nanonext::write_stdout(to_json(unclass(prepared))))
      }
      nanonext::send_aio(the$server_socket, prepared, mode = "serial")
    },

    # Validates tool existence and appends function reference to request data
    append_tool_fn = function(data) {
      if (!identical(data$method, "tools/call")) {
        return(data)
      }
      tool_name <- data$params$name
      if (!tool_name %in% names(self$get_tools())) {
        return(structure(
          jsonrpc_response(
            data$id,
            error = list(code = -32601, message = "Method not found")
          ),
          class = "jsonrpc_error"
        ))
      }
      data$tool <- self$get_tools()[[tool_name]]$fun
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
#' @return The server instance (invisibly)
#' @export
mcpr_server <- function(registry = NULL) {
  # Auto-discovery logic is now handled in mcprServer$initialize()
  server <- mcprServer$new(registry = registry)
  server$start()
  invisible(server)
}