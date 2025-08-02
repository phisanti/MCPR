#' @title MCP Server R6 Class
#'
#' @description
#' The `mcpServer` class implements the Model Context Protocol (MCP) server for 
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
#' server <- mcpServer$new()
#' server$start()  # Blocking call
#' 
#' # Server with custom tools
#' my_tool <- list(
#'   name = "mean",
#'   description = "Calculate arithmetic mean",
#'   fun = mean,
#'   arguments = list(x = "numeric")
#' )
#' server <- mcpServer$new(tools = list(my_tool))
#' server$start()
#' 
#' # Using convenience function
#' mcp_server(tools = "path/to/tools.R")
#' }
#'
#' @export
mcpServer <- R6::R6Class("mcpServer",
  private = list(
    .reader_socket = NULL,
    .cv = NULL,
    .running = FALSE,

    # === MESSAGE HANDLERS ===

    # Handle incoming messages from MCP clients
    handle_message_from_client = function(line) {
      if (length(line) == 0) {
        return()
      }
      logcat(c("FROM CLIENT: ", line))
      data <- tryCatch(
        jsonlite::parse_json(line),
        error = function(e) NULL
      )
      if (is.null(data)) return()

      if (!is.list(data) || is.null(data$method)) {
        return(cat_json(jsonrpc_response(
          data$id,
          error = list(code = -32600, message = "Invalid Request")
        )))
      }

      # Route based on method
      switch(data$method,
        "initialize" = {
          cat_json(jsonrpc_response(data$id, capabilities()))
        },
        "tools/list" = {
          cat_json(jsonrpc_response(
            data$id,
            list(tools = get_mcptools_tools_as_json())
          ))
        },
        "tools/call" = {
          tool_name <- data$params$name
          # Execute server-side tools directly, or if no session is active
          if (tool_name %in% c("list_r_sessions", "select_r_session", "execute_r_code_tool") ||
              !nanonext::stat(the$server_socket, "pipes")) {
            private$handle_request(data)
          } else {
            private$forward_request(data)
          }
        },
        "notifications/initialized" = {
          # Notification, no response needed
        },
        # Default for unknown methods
        cat_json(jsonrpc_response(
          data$id,
          error = list(code = -32601, message = "Method not found")
        ))
      )
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
      logcat(c("FROM SERVER: ", to_json(result)))
      cat_json(result)
    },

    # Forward requests to an R session for execution
    forward_request = function(data) {
      logcat(c("TO SESSION: ", jsonlite::toJSON(data)))
      prepared <- private$append_tool_fn(data)
      if (inherits(prepared, "jsonrpc_error")) {
        return(cat_json(prepared))
      }
      nanonext::send_aio(the$server_socket, prepared, mode = "serial")
    },

    # Append the tool function to the request data
    append_tool_fn = function(data) {
      if (!identical(data$method, "tools/call")) {
        return(data)
      }
      tool_name <- data$params$name
      if (!tool_name %in% names(get_mcptools_tools())) {
        return(structure(
          jsonrpc_response(
            data$id,
            error = list(code = -32601, message = "Method not found")
          ),
          class = "jsonrpc_error"
        ))
      }
      data$tool <- get_mcptools_tools()[[tool_name]]$fun
      data
    }
  ),

  public = list(
    #' @description
    #' Initialize the MCP server with optional tools.
    #' 
    #' @param tools Either:
    #'   \itemize{
    #'     \item A list of MCPR tool objects
    #'     \item A character path to an R file returning a list of tools
    #'     \item `NULL` to use only built-in tools
    #'   }
    #' @param registry A ToolRegistry instance to use for tool discovery. If provided,
    #'   takes precedence over the `tools` parameter.
    #' @param .tools_dir Internal parameter for specifying tools directory path.
    #' @return A new `mcpServer` instance
    initialize = function(tools = NULL, registry = NULL, .tools_dir = NULL) {
      if (!is.null(registry) && !inherits(registry, "ToolRegistry")) {
        cli::cli_abort("registry must be a ToolRegistry instance")
      }
      if (is.null(tools) && is.null(registry)) {
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
      tools = NULL
      set_server_tools(tools, registry = registry)
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
    }
  )
)

#' Start MCP Server (Convenience Function)
#'
#' @description
#' Convenience function to initialize and start an MCP server in one call.
#' Equivalent to creating a new `mcpServer` instance and calling `start()`.
#'
#' @param tools Tools specification. See `mcpServer$new()` for details.
#' @param registry A ToolRegistry instance to use for tool discovery. If provided,
#'   takes precedence over the `tools` parameter.
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
#' mcp_server()
#' 
#' # Server with custom tools from file
#' mcp_server(tools = "inst/tools/analysis_tools.R")
#' 
#' # Server with inline tools
#' summary_tool <- list(
#'   name = "summary",
#'   description = "Generate object summary",
#'   fun = summary,
#'   arguments = list(object = "any")
#' )
#' mcp_server(tools = list(summary_tool))
#' 
#' # Server with ToolRegistry
#' registry <- ToolRegistry$new()
#' mcp_server(registry = registry)
#' }
#' 
#' @return The server instance (invisibly)
#' @export
mcp_server <- function(tools = NULL, registry = NULL) {
  # Auto-discovery logic is now handled in mcpServer$initialize()
  server <- mcpServer$new(tools = tools, registry = registry)
  server$start()
  invisible(server)
}