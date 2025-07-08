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
