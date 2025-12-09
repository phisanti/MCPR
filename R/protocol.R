# JSON-RPC Protocol Implementation
# Functions for handling JSON-RPC 2.0 protocol messages and MCP capability definitions.
# Provides standardized message creation, response handling, and protocol compliance.


#' Supported MCP Protocol Versions
#'
#' Vector of MCP protocol versions supported by this server, in chronological order.
#' New versions should be appended to maintain version history.
#' @noRd
SUPPORTED_VERSIONS <- c("2024-11-05", "2025-03-26", "2025-06-18", "2025-11-25")

#' Version-Capability Feature Matrix
#'
#' Maps protocol versions to supported MCP features and capabilities.
#' Each version entry defines what capabilities the server advertises for that protocol version.
#'
#' @noRd
VERSION_CAPABILITIES <- list(
  "2024-11-05" = list(
    prompts = list(listChanged = FALSE),
    resources = list(subscribe = FALSE, listChanged = FALSE),
    tools = list(listChanged = FALSE),
    supports_tasks = FALSE,
    supports_elicitation = FALSE
  ),
  "2025-03-26" = list(
    prompts = list(listChanged = FALSE),
    resources = list(subscribe = FALSE, listChanged = FALSE),
    tools = list(listChanged = FALSE),
    supports_tasks = FALSE,
    supports_elicitation = FALSE
  ),
  "2025-06-18" = list(
    prompts = list(listChanged = FALSE),
    resources = list(subscribe = FALSE, listChanged = FALSE),
    tools = list(listChanged = FALSE),
    supports_tasks = FALSE,
    supports_elicitation = FALSE
  ),
  "2025-11-25" = list(
    prompts = list(listChanged = FALSE),
    resources = list(subscribe = FALSE, listChanged = FALSE),
    tools = list(listChanged = FALSE),
    supports_tasks = TRUE,
    supports_elicitation = TRUE
  )
)


#' Negotiate MCP Protocol Version
#'
#' Determines the appropriate protocol version to use based on client's request
#' and server's supported versions. Implements fallback strategies for version
#' compatibility and provides informative logging.
#'
#' @param client_version Version string from client's initialize request (may be NULL)
#' @param supported_versions Vector of versions this server supports (default: SUPPORTED_VERSIONS)
#' @return Negotiated version string (highest version both client and server support)
#'
#' @details
#' Negotiation strategy:
#' - If client version is NULL: default to "2024-11-05" (backward compatibility)
#' - If client version exactly matches a supported version: use it
#' - If client version is newer than all supported: use max supported version
#' - If client version is older: use client version if supported, else closest match
#'
#' @noRd
negotiate_protocol_version <- function(client_version,
                                       supported_versions = SUPPORTED_VERSIONS) {
  # Handle missing client version (backward compatibility with old clients)
  if (is.null(client_version) || !is.character(client_version) || length(client_version) == 0) {
    cli::cli_warn("Client did not specify protocolVersion, defaulting to 2024-11-05")
    return("2024-11-05")
  }

  # If client requests exact match we support, use it
  if (client_version %in% supported_versions) {
    cli::cli_inform("Protocol version negotiated: {client_version}")
    return(client_version)
  }

  # Client requested a version we don't support
  # Strategy: Use highest version <= client version, or lowest if all are higher

  # Find versions <= client version
  compatible_versions <- supported_versions[supported_versions <= client_version]

  if (length(compatible_versions) > 0) {
    # Use highest compatible version
    negotiated <- max(compatible_versions)
    cli::cli_inform(
      "Protocol version negotiated: {negotiated} (client requested {client_version})"
    )
    return(negotiated)
  }

  # All supported versions are newer than client's request
  # Use our oldest supported version
  negotiated <- min(supported_versions)
  cli::cli_warn(
    "Client requested {client_version}, but minimum supported version is {negotiated}. Using {negotiated}."
  )
  return(negotiated)
}


#' Output a JSON-formatted object to stdout
#'
#' @param x The object to convert to JSON and print.
#' @noRd
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
#' @noRd
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
#' @param version Protocol version string (e.g., "2025-11-25"). Must be in SUPPORTED_VERSIONS.
#' @param server_name Server name for serverInfo (default: "R MCPR server")
#' @param server_version Server version for serverInfo (default: "1.0.0")
#' @return Capabilities list with protocol version and feature support
#' @noRd
create_capabilities <- function(version,
                                server_name = "R MCPR server",
                                server_version = "1.0.0") {
  # Validate version
  if (!version %in% names(VERSION_CAPABILITIES)) {
    cli::cli_abort(
      "Unsupported protocol version: {version}. Supported versions: {paste(SUPPORTED_VERSIONS, collapse = ', ')}"
    )
  }

  # Get version-specific capabilities
  caps <- VERSION_CAPABILITIES[[version]]

  # Build response matching mcprServer$get_capabilities() structure
  list(
    protocolVersion = version,
    capabilities = list(
      prompts = caps$prompts,
      resources = caps$resources,
      tools = caps$tools
    ),
    serverInfo = list(
      name = server_name,
      version = server_version
    ),
    instructions = "This provides information about a running R session."
  )
}

#' Create MCP Client Capabilities
#'
#' @title Create MCP Client Capabilities
#' @description Creates client capabilities object for MCP initialization request.
#' Returns version-appropriate client capabilities that may vary based on protocol version.
#' Client capabilities declare what features the client can handle/support.
#'
#' @param version Protocol version string (e.g., "2025-11-25")
#' @return List of client capabilities for the specified protocol version
#' @noRd
create_client_capabilities <- function(version) {
  # Client capabilities are currently simple and don't vary much by version
  # Future versions may add more capabilities (e.g., elicitation support)
  list(
    tools = list(
      listChanged = FALSE
    )
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
#' @noRd
jsonrpc_response <- function(id, result = NULL, error = NULL) {
  if (!xor(is.null(result), is.null(error))) {
    cli::cli_warn("Either `result` or `error` must be provided, but not both.")
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
#' @noRd
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
#' @param protocol_version Protocol version to request (default: latest supported version)
#' @return Initialization request list for MCP protocol handshake
#' @noRd
create_initialize_request <- function(client_name = "MCP Test Client",
                                      client_version = "0.1.0",
                                      protocol_version = max(SUPPORTED_VERSIONS)) {
  list(
    jsonrpc = "2.0",
    id = 1,
    method = "initialize",
    params = list(
      protocolVersion = protocol_version,
      capabilities = create_client_capabilities(protocol_version),
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
#' @noRd
create_tools_list_request <- function(id = 2) {
  list(
    jsonrpc = "2.0",
    id = id,
    method = "tools/list"
  )
}
