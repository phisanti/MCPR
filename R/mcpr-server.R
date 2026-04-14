# MCP Server Implementation
# Core server class implementing Model Context Protocol for persistent R session management.
# Handles JSON-RPC communication, tool discovery, and routing between MCP clients and R sessions.

# MIME type for MCP App HTML resources
MCPR_MCP_APP_MIME <- "text/html;profile=mcp-app"

#' Detect MCP Apps support from client initialize params
#'
#' @include mcpr-base.R
#' @include mcpr-server-tools.R
#' @include daemon-utils.R
#' @include protocol.R
#' @include tool-register.R
#' @include utils.R
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
      private$.mcpr_version <- mcpr_package_version()

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
      private$.session_reader <- private$arm_session_listener()

      private$.running <- TRUE
      while (nanonext::wait(private$.cv)) {
        if (!nanonext::unresolved(private$.session_reader)) {
          private$handle_message_from_session(private$.session_reader$data)
          private$.session_reader <- private$arm_session_listener(previous = private$.session_reader)
        }
        # Check daemon listeners for responses
        for (cid in names(private$.daemon_listeners)) {
          dl <- private$.daemon_listeners[[cid]]
          if (!is.null(dl) && !nanonext::unresolved(dl)) {
            private$handle_message_from_session(dl$data)
            sock <- the$daemon_sockets[[cid]]
            if (!is.null(sock)) {
              private$arm_daemon_listener(cid, sock, previous = dl)
            }
          }
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

      # Clean up daemon sessions (reuse unregister_daemon for each)
      for (cid in names(the$daemon_sessions)) {
        unregister_daemon(cid)
      }
      private$.daemon_listeners <- list()

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
        server_version = private$.mcpr_version
      )
    },

    #' @description Check if the connected client supports MCP Apps
    #' @return Logical indicating MCP Apps support
    mcp_apps_supported = function() {
      private$.mcp_apps_supported
    }
  ),
  private = list(
    .reader_socket = NULL,
    .cv = NULL,
    .running = FALSE,
    .protocol_version = NULL,  # Negotiated protocol version for this connection
    .mcp_apps_supported = FALSE,
    .mcpr_version = "unknown",
    .viewer_content_cache = NULL,
    .client_name = "unknown",
    .client_interface = "unknown",
    .session_reader = NULL,
    .daemon_listeners = list(),

    # Returns a client identifier for daemon session routing.
    # stdio server always uses "default"; HTTP server would override per-connection.
    get_client_id = function() {
      "default"
    },

    # Arm a listener for daemon session responses.
    # Must pass cv so the main event loop wakes on daemon replies.
    arm_daemon_listener = function(client_id, sock, previous = NULL) {
      if (!is.null(previous) && nanonext::is_aio(previous)) {
        nanonext::stop_aio(previous)
      }
      reader <- nanonext::recv_aio(sock, mode = "string", cv = private$.cv)
      private$.daemon_listeners[[client_id]] <- reader
      reader
    },

    # Forward a tool call request to a daemon session's socket.
    # Resolves the socket from the daemon registry and delegates to forward_to_socket.
    forward_request_to_daemon = function(data, client_id) {
      sock <- the$daemon_sockets[[client_id]]
      if (is.null(sock)) {
        return(cat_json(jsonrpc_response(
          data$id,
          error = list(code = -32603, message = "Daemon socket not found")
        )))
      }
      private$forward_to_socket(data, sock, label = "TO DAEMON")
    },

    # Ensure a daemon session exists for the given client with a usable socket.
    # If a daemon is registered but has no socket (e.g., spawned by action="start"
    # tool), connects to it. If no daemon exists, spawns one.
    ensure_daemon_session = function(client_id) {
      existing <- get_daemon_session(client_id)

      # Case: daemon registered but no server-side socket yet (tool spawned it)
      if (!is.null(existing) && is.null(the$daemon_sockets[[client_id]])) {
        private$log_info(sprintf(
          "Connecting to existing daemon session %d for client '%s'", existing, client_id
        ))
        sock <- await_daemon_ready(existing, timeout_ms = 15000)
        if (is.null(sock)) {
          # Daemon died or never started - clean up and re-spawn below
          private$log_warn(sprintf("Daemon session %d unreachable, re-spawning", existing))
          unregister_daemon(client_id)
        } else {
          the$daemon_sockets[[client_id]] <- sock
          private$arm_daemon_listener(client_id, sock)
          return(existing)
        }
      }

      # Case: daemon registered and socket exists - nothing to do
      if (!is.null(existing) && !is.null(the$daemon_sockets[[client_id]])) {
        return(existing)
      }

      # Case: no daemon - spawn one, then connect
      session_id <- find_daemon_port()
      private$log_info(sprintf(
        "Spawning daemon session %d for client '%s'", session_id, client_id
      ))

      spawn_daemon(client_id, session_id, working_dir = getwd())

      sock <- await_daemon_ready(session_id, timeout_ms = 15000)
      if (is.null(sock)) {
        private$log_error(sprintf("Daemon session %d failed to start", session_id))
        cli::cli_abort("Daemon session failed to start within timeout")
      }

      the$daemon_sockets[[client_id]] <- sock
      register_daemon(client_id, session_id)
      private$arm_daemon_listener(client_id, sock)

      private$log_info(sprintf(
        "Daemon session %d ready for client '%s'", session_id, client_id
      ))
      session_id
    },

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

    should_refresh_session_listener = function(data) {
      if (!identical(data$method, "tools/call")) {
        return(FALSE)
      }

      tool_name <- data$params$name %||% ""
      if (identical(tool_name, "select_r_session")) {
        return(TRUE)
      }
      if (!identical(tool_name, "manage_r_sessions")) {
        return(FALSE)
      }

      identical(as.character(data$params$arguments$action %||% ""), "join")
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
            "Protocol negotiation: client=%s, negotiated=%s, mcpr_version=%s",
            client_version %||% "NULL",
            negotiated,
            private$.mcpr_version
          ))

          # Detect MCP Apps support from client capabilities or name
          private$.client_name <- as.character(data$params$clientInfo$name %||% "unknown")
          private$.mcp_apps_supported <- detect_mcp_apps_support(data$params)
          private$.client_interface <- if (private$.mcp_apps_supported) "mcp_app" else "cli"
          private$log_info(sprintf(
            "Client runtime: name=%s interface=%s mcp_apps_supported=%s mcpr_version=%s",
            private$.client_name,
            private$.client_interface,
            private$.mcp_apps_supported,
            private$.mcpr_version
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
          resources <- list()
          if (private$.mcp_apps_supported) {
            resources <- list(list(
              uri = "ui://mcpr/plots",
              name = "MCPR Plot Viewer",
              description = "Interactive plot viewer for R visualizations",
              mimeType = MCPR_MCP_APP_MIME
            ))
          }
          jsonrpc_response(data$id, list(resources = resources))
        },
        "resources/read" = function(data) {
          uri <- data$params$uri
          if (identical(uri, "ui://mcpr/plots")) {
            if (is.null(private$.viewer_content_cache)) {
              viewer_path <- system.file("mcp_app/plot-viewer.html", package = "MCPR")
              if (!nzchar(viewer_path) || !file.exists(viewer_path)) {
                return(jsonrpc_response(
                  data$id,
                  error = list(code = -32002, message = "Plot viewer resource not found")
                ))
              }
              viewer_content <- paste(readLines(viewer_path, warn = FALSE), collapse = "\n")
              private$.viewer_content_cache <- gsub(
                "__MCPR_VERSION__", private$.mcpr_version, viewer_content, fixed = TRUE
              )
            }
            jsonrpc_response(data$id, list(
              contents = list(list(
                uri = uri,
                mimeType = MCPR_MCP_APP_MIME,
                text = private$.viewer_content_cache
              ))
            ))
          } else {
            jsonrpc_response(
              data$id,
              error = list(code = -32002, message = paste("Resource not found:", uri))
            )
          }
        },
        "prompts/list" = function(data) {
          jsonrpc_response(
            data$id,
            list(prompts = list())
          )
        },
        "tools/call" = function(data) {
          tool_name <- data$params$name

          # Path 1: Session-management tools always run locally
          if (tool_name %in% c("list_r_sessions", "select_r_session", "manage_r_sessions")) {
            private$handle_request(data)
            if (private$should_refresh_session_listener(data)) {
              private$log_debug(sprintf("Refreshing session listener after %s", tool_name))
              private$.session_reader <- private$arm_session_listener(previous = private$.session_reader)
              socket_info <- check_session_socket(verbose = FALSE)
              private$log_info(sprintf(
                "Socket state after %s - Socket: %s, Interactive: %s, Has Session: %s",
                tool_name,
                socket_info$socket_number %||% "NULL",
                socket_info$is_interactive,
                socket_info$has_session
              ))
            }
            return(NULL)
          }

          # Path 2: Joined to a user session? Forward there
          if (nanonext::stat(the$server_socket, "pipes") > 0) {
            private$forward_request(data)
            return(NULL)
          }

          # Path 3+4: Use existing or auto-spawn daemon session
          # Check for explicit session routing (session parameter in tool arguments)
          session_arg <- data$params$arguments$session
          if (!is.null(session_arg)) {
            daemon_key <- sprintf("daemon-%d", as.integer(session_arg))
            # Remove session from arguments before forwarding (routing-only param)
            data$params$arguments$session <- NULL
            private$ensure_daemon_session(daemon_key)
            private$forward_request_to_daemon(data, daemon_key)
          } else {
            client_id <- private$get_client_id()
            private$ensure_daemon_session(client_id)
            private$forward_request_to_daemon(data, client_id)
          }
          return(NULL)
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

    # Forward requests to an R session for execution
    forward_request = function(data) {
      server_socket <- self$state_get("server_socket")
      private$forward_to_socket(data, server_socket, label = "TO SESSION")
    },

    # Shared forwarding logic: prepare tool call and send to a nanonext socket.
    forward_to_socket = function(data, sock, label = "TO TARGET") {
      private$log_comm(label, jsonlite::toJSON(data))
      prepared <- private$append_tool_fn(data)
      if (is.list(prepared) && !is.null(prepared$error)) {
        return(cat_json(prepared))
      }
      prepared$mcpr_request_context <- as_mcpr_request_context(
        mcp_apps_supported = private$.mcp_apps_supported,
        interface = private$.client_interface,
        client_name = private$.client_name
      )
      nanonext::send_aio(sock, prepared, mode = "serial")
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
      tooldef <- get_mcptools_tools()[[tool_name]]
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
#' @return The server instance (invisibly)
#' @export
mcpr_server <- function(registry = NULL) {
  # Auto-discovery logic is now handled in mcprServer$initialize()
  server <- mcprServer$new(registry = registry)
  server$start()
  invisible(server)
}
