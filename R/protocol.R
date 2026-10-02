# JSON-RPC Protocol Implementation
# Functions for handling JSON-RPC 2.0 protocol messages and MCP capability definitions.
# Provides standardized message creation, response handling, and protocol compliance.


#' Supported MCP Protocol Versions
#'
#' @include type-conversion-from-json.R
#' @include utils.R
#' Vector of handshake-era MCP protocol versions negotiated through `initialize`,
#' in chronological order. Stateless versions live in `MODERN_VERSIONS`.
#' @noRd
SUPPORTED_VERSIONS <- c("2024-11-05", "2025-03-26", "2025-06-18", "2025-11-25")

#' Stateless MCP protocol versions (no `initialize` handshake).
#'
#' Requests carry their version, client info, and client capabilities in
#' `params$_meta`; servers advertise these versions through `server/discover`.
#' @noRd
MODERN_VERSIONS <- c("2026-07-28")

#' Reserved `_meta` keys used by stateless MCP versions.
#' @noRd
MCP_META_PROTOCOL_VERSION <- "io.modelcontextprotocol/protocolVersion"
MCP_META_CLIENT_INFO <- "io.modelcontextprotocol/clientInfo"
MCP_META_CLIENT_CAPABILITIES <- "io.modelcontextprotocol/clientCapabilities"
MCP_META_SERVER_INFO <- "io.modelcontextprotocol/serverInfo"

#' MCP error codes that differ between protocol eras.
#' @noRd
MCP_ERROR_UNSUPPORTED_PROTOCOL_VERSION <- -32022L
MCP_ERROR_RESOURCE_NOT_FOUND_LEGACY <- -32002L

#' Default cache hints for `CacheableResult` responses (2026-07-28+).
#' Tool, prompt, and resource listings are fixed for the server process lifetime.
#' @noRd
MCP_CACHE_TTL_MS <- 3600000L

MCPR_SERVER_NAME <- "R MCPR server"
MCPR_SERVER_INSTRUCTIONS <- "This provides information about a running R session."

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
  ),
  "2026-07-28" = list(
    prompts = list(listChanged = FALSE),
    resources = list(subscribe = FALSE, listChanged = FALSE),
    tools = list(listChanged = FALSE),
    supports_tasks = FALSE,  # tasks moved to the io.modelcontextprotocol/tasks extension
    supports_elicitation = FALSE
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
  nanonext::write_stdout(to_json(finalize_result_message(x)))
}

#' Stamp result-level fields required by stateless MCP versions
#'
#' Adds `resultType = "complete"` and `_meta["io.modelcontextprotocol/serverInfo"]`
#' to every JSON-RPC result. Both are additive, so handshake-era clients ignore them.
#'
#' @param x A parsed JSON-RPC message (list).
#' @param server_version MCPR version reported in serverInfo.
#' @return The message with result fields stamped; non-result messages unchanged.
#' @noRd
finalize_result_message <- function(x, server_version = mcpr_package_version()) {
  if (!is.list(x) || !"result" %in% names(x) || !is.list(x$result)) {
    return(x)
  }
  result <- x$result
  if (is.null(result$resultType)) {
    result$resultType <- "complete"
  }
  meta <- result[["_meta"]]
  if (!is.list(meta)) {
    meta <- list()
  }
  if (is.null(meta[[MCP_META_SERVER_INFO]])) {
    meta[[MCP_META_SERVER_INFO]] <- list(name = MCPR_SERVER_NAME, version = server_version)
  }
  result[["_meta"]] <- meta
  x$result <- result
  x
}

#' Extract the stateless protocol version from a request
#'
#' @param data A parsed JSON-RPC request.
#' @return The `_meta` protocol version string, or NULL for handshake-era requests.
#' @noRd
request_protocol_version <- function(data) {
  params <- if (is.list(data)) data$params else NULL
  meta <- if (is.list(params)) params[["_meta"]] else NULL
  version <- if (is.list(meta)) meta[[MCP_META_PROTOCOL_VERSION]] else NULL
  if (is.character(version) && length(version) == 1L && nzchar(version)) version else NULL
}

#' Mark a result as cacheable (2026-07-28 `CacheableResult`)
#'
#' @param result Result list for a list/read/discover response.
#' @param scope Either "public" or "private".
#' @param ttl_ms Freshness hint in milliseconds.
#' @return The result with `ttlMs` and `cacheScope` set.
#' @noRd
as_cacheable_result <- function(result, scope = "public", ttl_ms = MCP_CACHE_TTL_MS) {
  result$ttlMs <- ttl_ms
  result$cacheScope <- scope
  result
}

#' Create a `server/discover` result
#'
#' @param server_version MCPR version (serverInfo is stamped by `cat_json`).
#' @return A `DiscoverResult` list.
#' @noRd
create_discover_result <- function(server_version = mcpr_package_version()) {
  caps <- create_capabilities(max(MODERN_VERSIONS), server_version = server_version)
  as_cacheable_result(list(
    supportedVersions = as.list(MODERN_VERSIONS),
    capabilities = caps$capabilities,
    instructions = caps$instructions
  ))
}

#' Create an `UnsupportedProtocolVersionError` response
#'
#' @param id Request ID.
#' @param requested The protocol version the client asked for.
#' @return JSON-RPC error response list.
#' @noRd
unsupported_protocol_version_response <- function(id, requested) {
  jsonrpc_response(id, error = list(
    code = MCP_ERROR_UNSUPPORTED_PROTOCOL_VERSION,
    message = paste("Unsupported protocol version:", requested),
    data = list(supported = as.list(MODERN_VERSIONS), requested = requested)
  ))
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

#' Resolve MCPR package version
#'
#' @param default Fallback version string when the package version cannot be resolved.
#' @return Installed MCPR package version, or `default`.
#' @noRd
mcpr_package_version <- function(default = "unknown") {
  version <- tryCatch(
    as.character(getNamespaceVersion("MCPR")),
    error = function(e) NULL
  )
  if (!is.null(version) && nzchar(version)) {
    return(version)
  }

  version <- tryCatch(
    as.character(utils::packageDescription("MCPR", fields = "Version")),
    error = function(e) NULL
  )
  if (!is.null(version) && nzchar(version) && !identical(version, "NA")) {
    return(version)
  }

  default
}

#' Normalize MCPR request context
#'
#' @param context Optional list containing request-scoped metadata
#' @param mcp_apps_supported Logical fallback value
#' @param interface Character fallback interface
#' @param client_name Character fallback client name
#' @return Canonical request context list
#' @noRd
as_mcpr_request_context <- function(context = NULL,
                                    mcp_apps_supported = FALSE,
                                    interface = "unknown",
                                    client_name = "unknown") {
  context <- context %||% list()

  list(
    mcp_apps_supported = isTRUE(context$mcp_apps_supported %||% mcp_apps_supported),
    mcpr_interface = as.character(context$mcpr_interface %||% interface),
    mcpr_client_name = as.character(context$mcpr_client_name %||% client_name)
  )
}

#' Get the current MCPR request context
#'
#' @return Canonical request context list
#' @noRd
get_mcpr_request_context <- function() {
  as_mcpr_request_context(the$current_request)
}

#' Set the current MCPR request context
#'
#' @param context Canonical or partial request context
#' @return Canonical request context list invisibly
#' @noRd
set_mcpr_request_context <- function(context) {
  the$current_request <- as_mcpr_request_context(context)
  invisible(the$current_request)
}

#' Clear the current MCPR request context
#'
#' @return NULL invisibly
#' @noRd
clear_mcpr_request_context <- function() {
  the$current_request <- NULL
  invisible(NULL)
}

#' Create MCP Capabilities Response
#'
#' @title Create MCP Capabilities Response
#' @description Creates capabilities response for MCP initialization handshake with protocol version.
#' Provides server capability information for MCP client negotiation including supported
#' features and protocol compliance. Enables proper MCP protocol establishment.
#'
#' @param version Protocol version string (e.g., "2025-11-25"). Must be in SUPPORTED_VERSIONS.
#' @param server_name Server name for serverInfo (default: `MCPR_SERVER_NAME`)
#' @param server_version Server version for serverInfo (default: installed MCPR version)
#' @return Capabilities list with protocol version and feature support
#' @noRd
create_capabilities <- function(version,
                                server_name = MCPR_SERVER_NAME,
                                server_version = mcpr_package_version()) {
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
    instructions = MCPR_SERVER_INSTRUCTIONS
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
