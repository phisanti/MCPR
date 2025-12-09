# MCP Server Implementation
# Core server class implementing Model Context Protocol for persistent R session management.
# Handles JSON-RPC communication, tool discovery, and routing between MCP clients and R sessions.

#' MCP Server
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
#' server$start() # Blocking call
#'
#' # Server with custom tools
#' my_tool <- tool(
#'   function(x) mean(x),
#'   name = "mean",
#'   description = "Calculate arithmetic mean",
#'   arguments = list(x = "number")
#' )
#' registry <- ToolRegistry$new()
#' registry$add_tool(my_tool)
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
    #' @return A new mcprServer instance
    initialize = function(registry = NULL, .tools_dir = NULL) {
      self$initialize_base("SERVER")

      if (!is.null(registry) && !inherits(registry, "ToolRegistry")) {
        error_msg <- "registry must be a ToolRegistry instance"
        private$log_error(error_msg)
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
    },

    #' @description Start the MCP server and begin listening for connections
    #' @note This method should only be called in non-interactive contexts because it blocks execution
    #' @return No return value (blocking call)
    start = function() {
      check_not_interactive()

      private$.cv <- nanonext::cv()
      private$.reader_socket <- nanonext::read_stdin()
      self$register_cleanup(function() nanonext::reap(private$.reader_socket), "reader_socket")
      nanonext::pipe_notify(private$.reader_socket, private$.cv, remove = TRUE, flag = TRUE)

      server_socket <- self$create_socket("poly", "server_communication")
      self$state_set("server_socket", server_socket)
      nanonext::dial(server_socket, url = self$socket_url(1L))

      # Log socket diagnostics for troubleshooting
      socket_info <- check_session_socket(verbose = FALSE)
      private$log_info(sprintf(
        "MCP server started - Socket: %s, Interactive: %s, Has Session: %s",
        socket_info$socket_number %||% "NULL",
        socket_info$is_interactive,
        socket_info$has_session
      ))

      client <- nanonext::recv_aio(private$.reader_socket, mode = "string", cv = private$.cv)
      server_socket <- self$state_get("server_socket")
      session <- nanonext::recv_aio(server_socket, mode = "string", cv = private$.cv)

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

    #' Stop the running server with graceful shutdown and resource cleanup
    #' @param timeout_ms Timeout in milliseconds for graceful shutdown (default: 5000)
    #' @return The server instance (invisibly) for method chaining
    stop = function(timeout_ms = 5000) {
      if (!private$.running) {
        return(invisible(self))
      }

      private$.running <- FALSE

      # Graceful shutdown with timeout for condition variable resolution
      if (!is.null(private$.cv)) {
        start_time <- Sys.time()
        while (as.numeric(difftime(Sys.time(), start_time, units = "secs")) < (timeout_ms / 1000)) {
          Sys.sleep(0.1)
          if (nanonext::unresolved(private$.cv) == 0) break
        }
      }

      self$cleanup_all()

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
        tools <- lapply(unname(get_mcptools_tools()), tool_as_json)
        return(compact(tools))
      }

      # Default to list format
      res <- get_mcptools_tools()
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
        server_version = "1.0.0"
      )
    }
  ),
  private = list(
    .reader_socket = NULL,
    .cv = NULL,
    .running = FALSE,
    .protocol_version = NULL,  # Negotiated protocol version for this connection

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

      # Define method handlers
      handlers <- list(
        "initialize" = function(data) {
          # Extract client's requested protocol version
          client_version <- data$params$protocolVersion

          # Negotiate protocol version
          negotiated <- negotiate_protocol_version(client_version)

          # Store negotiated version for this connection
          private$.protocol_version <- negotiated

          # Log negotiation for debugging
          private$log_info(sprintf(
            "Protocol negotiation: client=%s, negotiated=%s",
            client_version %||% "NULL",
            negotiated
          ))

          # Return capabilities for negotiated version
          jsonrpc_response(data$id, self$get_capabilities(version = negotiated))
        },
        "tools/list" = function(data) {
          jsonrpc_response(
            data$id,
            list(tools = self$get_tools("json"))
          )
        },
        "resources/list" = function(data) {
          jsonrpc_response(
            data$id,
            list(resources = list())
          )
        },
        "prompts/list" = function(data) {
          jsonrpc_response(
            data$id,
            list(prompts = list())
          )
        },
        "tools/call" = function(data) {
          tool_name <- data$params$name
          if (tool_name %in% c("list_r_sessions", "select_r_session", "manage_r_sessions") ||
            !nanonext::stat(the$server_socket, "pipes")) {
            private$handle_request(data)

            # Log socket state AFTER tool execution for socket-changing tools
            if (tool_name %in% c("select_r_session", "manage_r_sessions")) {
              socket_info <- check_session_socket(verbose = FALSE)
              private$log_info(sprintf(
                "Socket state after %s - Socket: %s, Interactive: %s, Has Session: %s",
                tool_name,
                socket_info$socket_number %||% "NULL",
                socket_info$is_interactive,
                socket_info$has_session
              ))
            }
            return(NULL) # Response handled in handle_request
          } else {
            private$forward_request(data)
            return(NULL) # Response handled in forward_request
          }
        },
        "notifications/initialized" = function(data) {
          # Notification, no response needed
          NULL
        }
      )

      # Route message and send response
      response <- private$route_message(data, handlers)
      if (!is.null(response)) {
        cat_json(response)
      }
    },

    # Handle messages from R sessions
    handle_message_from_session = function(data) {
      if (!is.character(data)) {
        return()
      }
      private$log_comm("FROM SESSION", data)
      nanonext::write_stdout(data)
    },

    # Handle tool execution requests locally on the server
    handle_request = function(data) {
      prepared <- private$append_tool_fn(data)
      result <- if (is.list(prepared) && !is.null(prepared$error)) {
        prepared
      } else {
        execute_tool_call(prepared)
      }
      private$log_comm("FROM SERVER", to_json(result))
      cat_json(result)
    },

    # Forward requests to an R session for execution
    forward_request = function(data) {
      private$log_comm("TO SESSION", jsonlite::toJSON(data))
      prepared <- private$append_tool_fn(data)
      if (is.list(prepared) && !is.null(prepared$error)) {
        return(cat_json(prepared))
      }
      server_socket <- self$state_get("server_socket")
      nanonext::send_aio(server_socket, prepared, mode = "serial")
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
      if (!tool_name %in% names(get_mcptools_tools())) {
        return(jsonrpc_response(
          data$id,
          error = list(code = -32601, message = "Method not found")
        ))
      }
      data$tool <- get_mcptools_tools()[[tool_name]]$fun
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
