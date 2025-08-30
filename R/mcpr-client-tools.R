# MCP Client Tool Processing
# Tool argument processing and execution functions for MCP client operations.
# Handles JSON argument decoding, type reconstruction, and tool call execution.

#' @title Decode Tool Arguments
#'
#' @description
#' Processes JSON arguments from MCP clients, reconstructing R object types
#' that may have been serialized during transmission. Handles both legacy
#' argument formats and enhanced type-aware arguments.
#'
#' @details
#' This function addresses the challenge of maintaining R object types when
#' arguments are transmitted via JSON-RPC. It detects MCP type markers
#' (\_mcp_type) and reconstructs original R objects accordingly. For legacy
#' compatibility, it falls back to the original argument structure when no
#' type markers are present.
#'
#' @param arguments Named list of function arguments from JSON-RPC request
#'
#' @return Reconstructed R objects with proper types, or original arguments
#'   if no type reconstruction is needed
#'
#' @examples
#' \dontrun{
#' # Arguments with MCP type markers
#' args_with_types <- list(
#'   data = list(
#'     `_mcp_type` = "numeric",
#'     value = c(1, 2, 3, 4, 5)
#'   ),
#'   method = "mean"
#' )
#'
#' processed <- decode_tool_args(args_with_types)
#' # Returns: list(data = c(1, 2, 3, 4, 5), method = "mean")
#'
#' # Legacy arguments without type markers
#' legacy_args <- list(
#'   data = list(1, 2, 3, 4, 5), # unnamed list
#'   method = "mean"
#' )
#'
#' processed <- decode_tool_args(legacy_args)
#' # Applies legacy coercion for unnamed lists
#' }
#'
#' @keywords internal
#' @seealso \code{\link{from_mcpr_json}} for type reconstruction details
#' @noRd
decode_tool_args <- function(arguments) {
  if (is.list(arguments)) {
    # Check if any arguments have MCP type markers
    has_mcp_types <- any(sapply(arguments, function(x) {
      is.list(x) && !is.null(x[["_mcp_type"]])
    }))

    if (has_mcp_types) {
      return(from_mcpr_json(arguments))
    }
  }

  # Fallback
  return(arguments)
}

#' @title Encode Tool Results
#' @description
#' Formats R function results into MCP-compatible response structures,
#' preserving type information and handling various R object types
#' appropriately for JSON-RPC transmission.
#'
#' @details
#' This function handles multiple result types:
#' \itemize{
#'   \item Single character strings: Direct text content
#'   \item Character vectors: Joined with newlines
#'   \item Complex objects: Serialized with type preservation
#'   \item Error results: Marked with isError flag
#' }
#'
#' The function ensures all results are wrapped in proper MCP content
#' structures with appropriate type annotations.
#'
#' @param data Original request data containing request ID
#' @param result R object returned from tool execution
#'
#' @return JSON-RPC response object with:
#'   \itemize{
#'     \item \code{id}: Request identifier from original request
#'     \item \code{result$content}: Array of content objects
#'     \item \code{result$isError}: Boolean error flag
#'   }
#'
#' @examples
#' \dontrun{
#' # Simple text result
#' request_data <- list(id = 1)
#' result <- "Analysis complete"
#' response <- encode_tool_results(request_data, result)
#'
#' # Character vector result
#' result <- c("Line 1", "Line 2", "Line 3")
#' response <- encode_tool_results(request_data, result)
#'
#' # Complex object result
#' result <- data.frame(x = 1:5, y = letters[1:5])
#' response <- encode_tool_results(request_data, result)
#' }
#'
#' @keywords internal
#' @seealso \code{\link{mcpr_serialize}} for complex object serialization
#' @noRd
encode_tool_results <- function(data, result) {
  is_error <- FALSE

  # For simple text results
  if (is.character(result) && length(result) == 1) {
    return(jsonrpc_response(
      data$id,
      list(
        content = list(list(
          type = "text",
          text = result # Use result directly, not paste(result, collapse = "\n")
        )),
        isError = is_error
      )
    ))
  }

  # For character vectors, join with newlines
  if (is.character(result) && length(result) > 1) {
    return(jsonrpc_response(
      data$id,
      list(
        content = list(list(
          type = "text",
          text = paste(result, collapse = "\n")
        )),
        isError = is_error
      )
    ))
  }

  # For complex objects, use rich type conversion
  serialized_result <- mcpr_serialize(result, pretty = TRUE)

  jsonrpc_response(
    data$id,
    list(
      content = list(list(
        type = "text",
        text = serialized_result
      )),
      isError = is_error
    )
  )
}

#' Execute MCP Tool Call Request
#'
#' @description
#' Processes and executes a JSON-RPC tool call request within the MCPR framework.
#' This function serves as the primary execution engine for tool calls received
#' from MCP clients, handling argument processing, function invocation, and
#' error management.
#'
#' @details
#' The function performs several critical operations:
#' \itemize{
#'   \item Extracts tool name and arguments from the JSON-RPC request
#'   \item Applies argument coercion to handle JSON array structures
#'   \item Executes the tool function using \code{do.call}
#'   \item Wraps results in proper MCP response format
#'   \item Provides comprehensive error handling with JSON-RPC error codes
#' }
#'
#' The argument coercion step is essential for handling JSON arrays that arrive
#' as unnamed lists, converting them to vectors for R function compatibility.
#'
#' @param data A parsed JSON-RPC request object containing:
#'   \itemize{
#'     \item \code{$id}: Request identifier for response matching
#'     \item \code{$params$name}: Tool name to execute
#'     \item \code{$params$arguments}: Named list of function arguments
#'     \item \code{$tool}: Function object to execute
#'   }
#'
#' @return A JSON-RPC response object with either:
#'   \itemize{
#'     \item Success result containing tool output formatted for MCP protocol
#'     \item Error response with code -32603 (Internal Error) and error message
#'   }
#'
#' @examples
#' \dontrun{
#' # Typical usage within MCP server message handling
#' request_data <- list(
#'   id = 1,
#'   params = list(
#'     name = "summary_stats",
#'     arguments = list(data = c(1, 2, 3, 4, 5))
#'   ),
#'   tool = summary_stats_function
#' )
#'
#' response <- execute_tool_call(request_data)
#' # Returns formatted JSON-RPC response
#' }
#'
#' @keywords internal
#' @seealso \code{\link{encode_tool_results}} for result formatting
#' @noRd
execute_tool_call <- function(data) {
  tool_name <- data$params$name

  # Enhanced argument processing with type reconstruction
  args <- decode_tool_args(data$params$arguments)

  # Legacy fallback for compatibility
  if (identical(args, data$params$arguments)) {
    args <- lapply(args, function(x) {
      if (is.list(x) && is.null(names(x))) {
        unlist(x, use.names = FALSE)
      } else {
        x
      }
    })
  }

  tryCatch(
    encode_tool_results(data, do.call(data$tool, args)),
    error = function(e) {
      jsonrpc_response(
        data$id,
        error = list(code = -32603, message = conditionMessage(e))
      )
    }
  )
}
