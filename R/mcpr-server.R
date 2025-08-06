#' @include mcpr-server-tools.R
#' @include tool-execution.R
#' @title MCP Server R6 Class
#'
#' @description
#' The `mcprServer` class implements the Model Context Protocol (MCP) server for 
#' persistent R session management. It enables AI coding assistants to connect 
#' to and interact with running R sessions, maintaining workspace state across 
#' multiple interactions.
#'
#' This class encapsulates server management, tool discovery, and protocol 
#' communication between MCP clients and R sessions.
#'
#' @section Architecture:
#' The server operates in three layers:
#' \itemize{
#'   \item **Client Layer**: Handles JSON-RPC communication with MCP clients
#'   \item **Server Layer**: Manages tool execution and session routing
#'   \item **Session Layer**: Forwards requests to active R sessions
#' }
#'
#' @section Key Features:
#' \itemize{
#'   \item Non-blocking message handling between clients and R sessions
#'   \item Tool registration and discovery via MCP protocol
#'   \item Session management and request routing
#'   \item Built-in server-side tools (session listing, selection, R code execution)
#' }
#'
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
  private = list(
    .reader_socket = NULL,
    .cv = NULL,
    .running = FALSE,
    .messenger = NULL,

    # === MESSAGE HANDLERS ===

    # Handle incoming messages from MCP clients
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
        "tools/call" = function(data) {
          tool_name <- data$params$name
          # Execute server-side tools directly, or if no session is active
          if (tool_name %in% c("list_r_sessions", "select_r_session", "execute_r_code") ||
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

    # Handle messages from R sessions
    handle_message_from_session = function(data) {
      if (!is.character(data)) {
        return()
      }
      logcat(c("FROM SESSION: ", data))
      # Forward response directly to the client
      nanonext::write_stdout(data)
    },

    # Handle tool execution requests locally on the server
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

    # Forward requests to an R session for execution
    forward_request = function(data) {
      logcat(c("TO SESSION: ", jsonlite::toJSON(data)))
      prepared <- private$append_tool_fn(data)
      if (inherits(prepared, "jsonrpc_error")) {
        return(nanonext::write_stdout(to_json(unclass(prepared))))
      }
      nanonext::send_aio(the$server_socket, prepared, mode = "serial")
    },

    # Append the tool function to the request data
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
  ),

  public = list(
    #' @description
    #' Initialize the MCP server with optional tools.
    #' 
    #' @param registry A ToolRegistry instance to use for tool discovery. If provided,
    #'   takes precedence over the `tools` parameter.
    #' @param .tools_dir Internal parameter for specifying tools directory path.
    #' @return A new `mcprServer` instance
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

    #' @description
    #' Start the MCP server and begin listening for connections.
    #' 
    #' @details
    #' This method blocks execution and runs the server's main event loop.
    #' It handles incoming messages from MCP clients and forwards requests
    #' to available R sessions. The server will continue running until
    #' manually stopped or interrupted.
    #' 
    #' @note This method should only be called in non-interactive contexts.
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

    #' @description
    #' Stop the running server.
    #' 
    #' @return The server instance (invisibly) for method chaining
    stop = function() {
      private$.running <- FALSE
      # TODO: Add more graceful shutdown logic
      invisible(self)
    },

    #' @description
    #' Check if the server is currently running.
    #' 
    #' @return `TRUE` if server is running, `FALSE` otherwise
    is_running = function() {
      private$.running
    },

    #' @description
    #' Get server tools in the specified format.
    #' 
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
  )
)

#' Start MCP Server (Convenience Function)
#'
#' @description
#' Convenience function to initialize and start an MCP server in one call.
#' Equivalent to creating a new `mcprServer` instance and calling `start()`.
#'
#' @param registry A ToolRegistry instance to use for tool discovery.
#' 
#' @details
#' This function is designed for non-interactive use in background processes
#' or command-line scripts. It will block execution until the server is stopped.
#'
#' @section Built-in Tools:
#' The server includes these essential tools:
#' \itemize{
#'   \item `list_r_sessions`: Discover available R sessions
#'   \item `select_r_session`: Connect to a specific R session
#'   \item `execute_r_code_tool`: Execute R code in the active session
#' }
#'
#' @section Integration Workflow:
#' 1. **Setup**: Initialize server with custom tools
#' 2. **Discovery**: Client queries available tools via MCP protocol
#' 3. **Connection**: Client selects and connects to R session
#' 4. **Execution**: Client sends tool calls, server routes to session
#' 5. **Response**: Results flow back through server to client
#'
#' @examples
#' \dontrun{
#' # Minimal server
#' mcpr_server()
#' 
#' # Server with custom tools from file
#' # Server with ToolRegistry from directory
#' registry <- ToolRegistry$new(tools_dir = "inst/tools")
#' mcpr_server(registry = registry)
#' 
#' # Server with ToolRegistry
#' registry <- ToolRegistry$new()
#' mcpr_server(registry = registry)
#' }
#' 
#' @return The server instance (invisibly)
#' @export
mcpr_server <- function(registry = NULL) {
  # Auto-discovery logic is now handled in mcprServer$initialize()
  server <- mcprServer$new(registry = registry)
  server$start()
  invisible(server)
}