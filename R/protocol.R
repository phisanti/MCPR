# Protocol compatibility layer for MCPR package
# This file maintains backward compatibility while delegating to MessageHandler

#' @include messenger.R

# Package environment for messenger instance
.pkg_env <- new.env(parent = emptyenv())

#' Get or create package messenger instance
#' @return MessageHandler instance
get_pkg_messenger <- function() {
  if (!exists(".messenger", envir = .pkg_env)) {
    .pkg_env$.messenger <- MessageHandler$new()
  }
  .pkg_env$.messenger
}

#' Create a JSON-RPC 2.0 response object (DEPRECATED)
#'
#' @description
#' DEPRECATED: Use MessageHandler$create_response() instead.
#' This function is maintained for backward compatibility.
#'
#' @param id The request ID.
#' @param result The success result of the method execution.
#' @param error An error object if the method execution failed.
#' @return A list representing the JSON-RPC response.
jsonrpc_response <- function(id, result = NULL, error = NULL) {
  get_pkg_messenger()$create_response(id, result, error)
}

#' Define server capabilities for the MCP 'initialize' handshake
#'
#' @return A list describing the server's capabilities.
capabilities <- function() {
  get_pkg_messenger()$create_capabilities()
}

#' Output a JSON-formatted object to stdout (DEPRECATED)
#'
#' @description
#' DEPRECATED: Use MessageHandler$output_json() instead.
#' This function is maintained for backward compatibility.
#'
#' @param x The object to convert to JSON and print.
cat_json <- function(x) {
  get_pkg_messenger()$output_json(x)
}

#' Create MCP Tool Call Request (DEPRECATED)
#'
#' @description
#' DEPRECATED: Use MessageHandler$create_tool_request() instead.
#' This function is maintained for backward compatibility.
#'
#' @param id Request ID
#' @param tool Tool name
#' @param arguments Tool arguments (should be type-encoded if needed)
#' @return JSON-RPC request object
#' @export
mcp_request_tool_call <- function(id, tool, arguments) {
  get_pkg_messenger()$create_tool_request(id, tool, arguments)
}

#' Send and Receive JSON-RPC Message via Process (DEPRECATED)
#'
#' @description
#' DEPRECATED: Use MessageHandler$send_receive() instead.
#' This function is maintained for backward compatibility.
#'
#' @param process Process object (processx)
#' @param message JSON-RPC message to send
#' @param client Client instance for logging (optional)
#' @param max_attempts Maximum retry attempts (default: 20)
#' @param retry_delay Delay between retries in seconds (default: 0.2)
#' @return Parsed JSON response or NULL on timeout
#' @export
send_and_receive_message <- function(process, message, client = NULL, 
                                     max_attempts = 20, retry_delay = 0.2) {
  # Create messenger with client logging if available
  messenger <- if (!is.null(client) && !is.null(client$log_communication)) {
    MessageHandler$new(logger = client$log_communication)
  } else {
    get_pkg_messenger()
  }
  
  messenger$send_receive(
    process = process,
    message = message,
    context = "CLIENT",
    max_attempts = max_attempts,
    retry_delay = retry_delay
  )
}
