# Functions for handling JSON-RPC 2.0 protocol messages

#' Create a JSON-RPC 2.0 response object
#'
#' @param id The request ID.
#' @param result The success result of the method execution.
#' @param error An error object if the method execution failed.
#' @return A list representing the JSON-RPC response.
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
#' @description
#' Creates JSON-RPC request for MCP tool calls according to protocol specifications.
#' Reusable across client and server contexts for consistent request formatting.
#'
#' @param id Request ID
#' @param tool Tool name
#' @param arguments Tool arguments (should be type-encoded if needed)
#' @return JSON-RPC request object
#' @export
mcp_request_tool_call <- function(id, tool, arguments) {
  if (length(arguments) == 0) {
    params <- list(name = tool)
  } else {
    params <- list(
      name = tool,
      arguments = arguments
    )
  }
  list(
    jsonrpc = "2.0",
    id = id,
    method = "tools/call",
    params = params
  )
}

#' Send and Receive JSON-RPC Message via Process
#'
#' @description
#' Handles JSON-RPC communication with external processes using processx.
#' Provides timeout handling and optional logging integration.
#' Reusable for any processx-based MCP communication.
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
  json_msg <- jsonlite::toJSON(message, auto_unbox = TRUE)
  
  # Log communication if client provided
  if (!is.null(client) && !is.null(client$log_communication)) {
    client$log_communication(paste("FROM CLIENT:", json_msg))
  }
  
  process$write_input(paste0(json_msg, "\n"))

  output <- NULL
  attempts <- 0

  while (length(output) == 0 && attempts < max_attempts) {
    Sys.sleep(retry_delay)
    output <- process$read_output_lines()
    attempts <- attempts + 1
  }

  if (!is.null(output) && length(output) > 0) {
    if (!is.null(client) && !is.null(client$log_communication)) {
      client$log_communication(paste("FROM SERVER:", output[1]))
    }
    return(jsonlite::parse_json(output[1]))
  }

  if (!is.null(client) && !is.null(client$log_communication)) {
    client$log_communication(paste("ALERT: No response received after", attempts, "attempts"))
  }
  return(NULL)
}
