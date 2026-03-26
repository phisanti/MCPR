# MCP Client Tool Processing
# Tool argument processing and execution functions for MCP client operations.
# Handles schema-targeted argument coercion and tool call execution.

#' @include type-conversion-from-json.R
#' @include utils.R

#' Coerce Tool Arguments by Schema
#'
#' Targeted safety net for arguments that arrive as the wrong JSON type.
#' Only coerces arguments whose declared schema type disagrees with the
#' received R type. Not a general-purpose JSON decoder.
#'
#' @param args Named list of arguments from JSON-RPC request
#' @param schema Named list of mcpr_type objects (from ToolDef$arguments)
#' @return args with targeted coercions applied
#' @noRd
coerce_args_by_schema <- function(args, schema) {
  for (name in intersect(names(args), names(schema))) {
    spec <- schema[[name]]
    if (!inherits(spec, "mcpr_type")) next
    val <- args[[name]]

    # String that should be an object or array — parse it
    if (is.character(val) && length(val) == 1 && spec$type %in% c("object", "array")) {
      parsed <- tryCatch(jsonlite::fromJSON(val, simplifyVector = FALSE), error = function(e) val)
      if (!identical(parsed, val)) args[[name]] <- parsed
    }

    # Unnamed list that should be a scalar vector — unlist it
    if (is.list(val) && is.null(names(val)) && spec$type %in% c("string", "number", "integer", "boolean")) {
      args[[name]] <- unlist(val, use.names = FALSE)
    }
  }
  args
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
apply_default_audience <- function(item, default_audience = "assistant") {
  if (is.null(item$annotations$audience)) {
    if (is.null(item$annotations)) {
      item$annotations <- list(audience = list(default_audience))
    } else {
      item$annotations$audience <- list(default_audience)
    }
  }
  item
}

encode_tool_results <- function(data, result) {
  is_error <- FALSE

  # structuredContent: tool returned list(content=<content-array>, structuredContent=...)

  # content goes to model, structuredContent goes only to MCP App viewer
  if (is.list(result) &&
      !is.null(result$structuredContent) &&
      is.list(result$structuredContent) &&
      length(result$structuredContent) > 0 &&
      length(names(result$structuredContent)) > 0 &&
      all(nzchar(names(result$structuredContent))) &&
      is.list(result$content)) {
    response_result <- list(
      content = result$content,
      structuredContent = result$structuredContent,
      isError = is_error
    )
    return(jsonrpc_response(data$id, response_result))
  }

  # Image content: tool returned list(type="image", data=..., mimeType=...)
  if (is.list(result) && identical(result$type, "image") && !is.null(result$data)) {
    default_audience <- if (!is.null(result$`_meta`)) "user" else "assistant"
    item <- compact(list(
      type = "image",
      data = result$data,
      mimeType = result$mimeType %||% "image/png"
    ))
    if (!is.null(result$annotations)) item$annotations <- result$annotations
    item <- apply_default_audience(item, default_audience)
    response_result <- list(
      content = list(item),
      isError = is_error
    )
    if (!is.null(result$`_meta`)) response_result[["_meta"]] <- result$`_meta`
    return(jsonrpc_response(data$id, response_result))
  }

  # Text content with metadata: tool returned list(type="text", content=..., _meta=...)
  if (is.list(result) && identical(result$type, "text") &&
      !is.null(result$content) && is.character(result$content)) {
    default_audience <- if (!is.null(result$`_meta`)) "user" else "assistant"
    item <- list(type = "text", text = result$content)
    if (!is.null(result$annotations)) item$annotations <- result$annotations
    item <- apply_default_audience(item, default_audience)
    response_result <- list(
      content = list(item),
      isError = is_error
    )
    if (!is.null(result$`_meta`)) response_result[["_meta"]] <- result$`_meta`
    return(jsonrpc_response(data$id, response_result))
  }

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
  args <- data$params$arguments %||% list()

  if (!is.null(data$arg_schema)) {
    args <- coerce_args_by_schema(args, data$arg_schema)
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
