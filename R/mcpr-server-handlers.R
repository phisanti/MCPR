# MCP Server Client Handlers
# Builds JSON-RPC handler closures for mcprServer client dispatch.
# Keeps protocol method bodies separate from the server event loop and routing shell.

#' @include protocol.R
#' @include utils.R
NULL

make_mcpr_server_handlers <- function(self, private) {
  list(
    "initialize" = function(data) {
      handle_mcpr_server_initialize(data, self, private)
    },
    "tools/list" = function(data) {
      jsonrpc_response(
        data$id,
        list(tools = self$get_tools("json"))
      )
    },
    "resources/list" = function(data) {
      resources <- private$.resource_registry$list(private$.mcp_apps_supported)
      jsonrpc_response(data$id, list(resources = resources))
    },
    "resources/read" = function(data) {
      handle_mcpr_server_resource_read(data, private)
    },
    "prompts/list" = function(data) {
      jsonrpc_response(
        data$id,
        list(prompts = list())
      )
    },
    "tools/call" = function(data) {
      handle_mcpr_server_tool_call(data, private)
    },
    "notifications/initialized" = function(data) {
      NULL
    }
  )
}

handle_mcpr_server_initialize <- function(data, self, private) {
  client_version <- data$params$protocolVersion
  negotiated <- negotiate_protocol_version(client_version)

  private$.protocol_version <- negotiated
  private$log_info(sprintf(
    "Protocol negotiation: client=%s, negotiated=%s, mcpr_version=%s",
    client_version %||% "NULL",
    negotiated,
    private$.mcpr_version
  ))

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

  jsonrpc_response(data$id, self$get_capabilities(version = negotiated))
}

handle_mcpr_server_resource_read <- function(data, private) {
  uri <- data$params$uri
  result <- tryCatch(
    private$.resource_registry$read(uri, private$.mcp_apps_supported),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    private$log_error(sprintf(
      "Resource read failed for %s: %s", uri, conditionMessage(result)
    ))
    return(jsonrpc_response(
      data$id,
      error = list(code = -32603L, message = "Resource read failed",
                   data = list(uri = uri))
    ))
  }
  if (is.null(result)) {
    return(jsonrpc_response(
      data$id,
      error = list(code = -32002L, message = paste("Resource not found:", uri),
                   data = list(uri = uri))
    ))
  }

  jsonrpc_response(data$id, result)
}

handle_mcpr_server_tool_call <- function(data, private) {
  tool_name <- data$params$name

  # Server-owned session management must remain available even when the
  # active attached session has failed.
  if (identical(tool_name, "manage_r_sessions") &&
      private$.session_manager$is_enabled()) {
    handle_mcpr_server_session_control(data, private)
    return(NULL)
  }

  if (tool_name %in% c("list_r_sessions", "select_r_session")) {
    private$handle_request(data)
    return(NULL)
  }

  # Phase 1 runtime cleanup: ordinary tools no longer expose per-call session
  # routing. Route through the server-owned manager's active binding.
  tryCatch({
    private$.session_manager$execute(data)
  }, error = function(e) {
    cat_json(jsonrpc_response(data$id, error = list(
      code = -32603L,
      message = conditionMessage(e)
    )))
  })

  NULL
}

handle_mcpr_server_session_control <- function(data, private) {
  args <- data$params$arguments %||% list()
  result <- tryCatch(
    private$.session_manager$handle_control(
      action = args$action %||% "list",
      session = args$session
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    cat_json(jsonrpc_response(data$id, error = list(
      code = -32603L,
      message = conditionMessage(result)
    )))
  } else {
    cat_json(jsonrpc_response(data$id, result = list(
      content = list(list(type = "text", text = result)),
      isError = FALSE
    )))
  }
}
