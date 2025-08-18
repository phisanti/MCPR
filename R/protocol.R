# JSON-RPC Protocol Implementation
# Functions for handling JSON-RPC 2.0 protocol messages and MCP capability definitions.
# Provides standardized message creation, response handling, and protocol compliance.

#' Define server capabilities for the MCP 'initialize' handshake
#'
#' @return A list describing the server's capabilities.
capabilities <- function() {
  list(
    protocolVersion = "2024-11-05",
    capabilities = list(
      prompts = named_list(
        listChanged = FALSE
      ),
      resources = named_list(
        subscribe = FALSE,
        listChanged = FALSE
      ),
      tools = named_list(
        listChanged = FALSE
      )
    ),
    serverInfo = list(
      name = "R mcptools server",
      version = "0.0.1"
    ),
    instructions = "This provides information about a running R session."
  )
}

#' Output a JSON-formatted object to stdout
#'
#' @param x The object to convert to JSON and print.
cat_json <- function(x) {
  nanonext::write_stdout(to_json(x))
}

#' Create MCP Tool Call Request
#'
#' @title Create MCP Tool Call Request
#' @description Creates JSON-RPC request for MCP tool execution with proper parameter handling.
#' Constructs standardized tool call requests for MCP protocol communication with optional
#' argument passing. Enables structured tool invocation through JSON-RPC 2.0 compliance.
#'
#' @param id Request ID for response matching
#' @param tool Tool name to execute
#' @param arguments Tool arguments (default: empty list)
#' @return JSON-RPC request list for tool execution
create_tool_request <- function(id, tool, arguments = list()) {
  params <- if (length(arguments) == 0) {
    list(name = tool)
  } else {
    list(name = tool, arguments = arguments)
  }

  list(
    jsonrpc = "2.0",
    id = id,
    method = "tools/call",
    params = params
  )
}

#' Create MCP Capabilities Response
#'
#' @title Create MCP Capabilities Response
#' @description Creates capabilities response for MCP initialization handshake with protocol version.
#' Provides server capability information for MCP client negotiation including supported
#' features and protocol compliance. Enables proper MCP protocol establishment.
#'
#' @return Capabilities list with protocol version and feature support
create_capabilities <- function() {
  list(
    protocolVersion = "2024-11-05",
    capabilities = list(
      prompts = named_list(listChanged = FALSE),
      resources = named_list(subscribe = FALSE, listChanged = FALSE),
      tools = named_list(listChanged = FALSE)
    ),
    serverInfo = list(
      name = "R mcptools server",
      version = "0.0.1"
    ),
    instructions = "This provides information about a running R session."
  )
}


#' Create JSON-RPC 2.0 Response Object
#'
#' @title Create JSON-RPC 2.0 Response Object
#' @description Creates properly formatted JSON-RPC 2.0 response object with result or error.
#' Handles protocol compliance for MCP communication through structured response creation.
#' Validates mutual exclusivity of result and error fields while maintaining protocol
#' standards for reliable client-server communication.
#'
#' @param id Request ID for response matching
#' @param result Success result of method execution (mutually exclusive with error)
#' @param error Error object if method execution failed (mutually exclusive with result)
#' @return List representing JSON-RPC 2.0 response
jsonrpc_response <- function(id, result = NULL, error = NULL) {
  if (!xor(is.null(result), is.null(error))) {
    warning("Either `result` or `error` must be provided, but not both.")
  }

  drop_nulls(list(
    jsonrpc = "2.0",
    id = id,
    result = result,
    error = error
  ))
}

#' Convert JSON Types to R Objects
#'
#' @title Convert JSON Types to R Objects
#' @description Simple wrapper around from_mcpr_json for compatibility with ToolDef
#' @param args Named list of arguments from JSON
#' @return List with converted R objects
convert_json_types <- function(args) {
  # Simple pass-through for now - could be enhanced later
  if (is.list(args)) {
    lapply(args, function(x) {
      if (is.character(x) && length(x) == 1) {
        # Try to parse as JSON, but fallback to original value
        tryCatch(
          {
            from_mcpr_json(x)
          },
          error = function(e) {
            x
          }
        )
      } else {
        x
      }
    })
  } else {
    args
  }
}

#' Create MCP Initialization Request
#'
#' @title Create MCP Initialization Request
#' @description Creates initialization request for MCP client-server handshake with client information.
#' Establishes MCP protocol connection with version negotiation and capability exchange.
#' Enables proper client identification and protocol establishment.
#'
#' @param client_name Client name for identification (default: "MCP Test Client")
#' @param client_version Client version string (default: "0.1.0")
#' @return Initialization request list for MCP protocol handshake
create_initialize_request <- function(client_name = "MCP Test Client", client_version = "0.1.0") {
  list(
    jsonrpc = "2.0",
    id = 1,
    method = "initialize",
    params = list(
      protocolVersion = "2024-11-05",
      capabilities = list(tools = list(listChanged = FALSE)),
      clientInfo = list(name = client_name, version = client_version)
    )
  )
}

#' Create Tools List Request
#'
#' @title Create Tools List Request
#' @description Creates request for MCP tools discovery with proper JSON-RPC formatting.
#' Enables client discovery of available server tools through standardized protocol.
#' Provides foundation for tool-based MCP interactions.
#'
#' @param id Request ID for response matching (default: 2)
#' @return Tools list request for MCP tool discovery
create_tools_list_request <- function(id = 2) {
  list(
    jsonrpc = "2.0",
    id = id,
    method = "tools/list"
  )
}
