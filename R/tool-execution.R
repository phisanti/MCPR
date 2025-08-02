# TOOL EXECUTION ENGINE
# Execution interface for MCP tool calls with type preservation
# Functions ordered from simple utilities to complex orchestrators

#' Check for MCP Type Markers (Deep)
#'
#' @description
#' Recursively checks for MCP type markers in nested structures.
#' Pure function with no dependencies on other functions in this file.
#'
#' @param obj Object to check
#' @return Logical indicating presence of MCP type markers
#' @keywords internal
has_mcp_types_deep <- function(obj) {
  if (is.list(obj)) {
    # Check current level for _mcp_type
    if (!is.null(obj[["_mcp_type"]])) {
      return(TRUE)
    }
    
    # Check recursively
    return(any(sapply(obj, has_mcp_types_deep, USE.NAMES = FALSE)))
  }
  
  FALSE
}

#' Create Tool Execution Context
#'
#' @description
#' Helper function to create properly structured execution contexts
#' for both local and remote tool execution. Simple constructor function.
#'
#' @param id Request ID
#' @param tool_name Name of tool to execute
#' @param arguments Named list of arguments
#' @param tool (Optional) Local R function
#' @param process (Optional) Remote server process
#' @param client (Optional) Client instance
#' @return Execution context object
#' @export
create_execution_context <- function(id, tool_name, arguments, 
                                     tool = NULL, process = NULL, client = NULL) {
  list(
    id = id,
    params = list(
      name = tool_name,
      arguments = arguments
    ),
    tool = tool,
    process = process,
    client = client
  )
}

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
#'   data = list(1, 2, 3, 4, 5),  # unnamed list
#'   method = "mean"
#' )
#' 
#' processed <- decode_tool_args(legacy_args)
#' # Applies legacy coercion for unnamed lists
#' }
#'
#' @keywords internal
#' @seealso \code{\link{from_mcp_json}} for type reconstruction details
decode_tool_args <- function(arguments) {
  if (is.list(arguments)) {
    # Check if any arguments have MCP type markers
    has_mcp_types <- any(sapply(arguments, function(x) {
      is.list(x) && !is.null(x[["_mcp_type"]])
    }))
    
    if (has_mcp_types) {
      return(from_mcp_json(arguments))
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
#' @seealso \code{\link{mcp_serialize}} for complex object serialization
encode_tool_results <- function(data, result) {
  is_error <- FALSE
  
  # For simple text results
  if (is.character(result) && length(result) == 1) {
    return(jsonrpc_response(
      data$id,
      list(
        content = list(list(
          type = "text",
          text = result  # Use result directly, not paste(result, collapse = "\n")
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
  serialized_result <- mcp_serialize(result, pretty = TRUE)
  
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

#' Decode Tool Response
#'
#' @description
#' Decodes MCP tool responses with type reconstruction.
#' Handles both simple text responses and complex type-preserved responses.
#' Uses has_mcp_types_deep() utility function.
#'
#' @param response_result Result portion of JSON-RPC response
#' @return Decoded R objects with proper types
#' @keywords internal
decode_tool_response <- function(response_result) {
  # Check if response contains MCP type markers
  if (is.list(response_result) && has_mcp_types_deep(response_result)) {
    return(from_mcp_json(response_result))
  }
  
  # Return as-is for simple responses
  response_result
}

#' Execute Local Tool (Server-side)
#'
#' @description
#' Executes local R functions as MCP tools. Handles argument decoding,
#' function invocation, and result encoding with full type preservation.
#' Uses decode_tool_args() and encode_tool_results().
#'
#' @param data Local execution context with $tool function
#' @return JSON-RPC response object
#' @keywords internal
execute_local_tool <- function(data) {
  tool_name <- data$params$name
  
  # Enhanced argument processing with type reconstruction
  args <- decode_tool_args(data$params$arguments)

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

#' Execute Remote Tool (Client-side)
#'
#' @description
#' Executes tools on remote MCP servers. Handles argument encoding,
#' JSON-RPC communication, and response decoding with type preservation.
#' Uses decode_tool_response() for type reconstruction.
#'
#' @param data Remote execution context with $process
#' @return Parsed response from remote server
#' @keywords internal
execute_remote_tool <- function(data) {
  # Encode arguments with type preservation
  encoded_args <- to_mcp_json(data$params$arguments, auto_unbox = TRUE)
  
  # Create JSON-RPC request (uses protocol.R functions)
  request <- mcp_request_tool_call(
    id = data$id,
    tool = data$params$name,
    arguments = encoded_args
  )
  
  # Send request and receive response (uses protocol.R functions)
  response <- send_and_receive_message(data$process, request, data$client)
  
  # Decode response with type reconstruction
  if (!is.null(response$result)) {
    response$result <- decode_tool_response(response$result)
  }
  
  response
}

#' Execute MCP Tool Call Request
#'
#' @description
#' Universal execution engine for MCP tool calls. Handles both local R function
#' execution (server-side) and remote MCP server tool calls (client-side).
#' This function serves as the primary execution interface for all tool operations.
#' Uses execute_local_tool() and execute_remote_tool().
#'
#' @details
#' The function automatically detects execution mode:
#' \itemize{
#'   \item \strong{Local execution}: When \code{data$tool} is an R function
#'   \item \strong{Remote execution}: When \code{data$process} is provided
#' }
#'
#' For local execution:
#' \itemize{
#'   \item Decodes arguments with type reconstruction
#'   \item Executes R function using \code{do.call}
#'   \item Encodes results with type preservation
#' }
#'
#' For remote execution:
#' \itemize{
#'   \item Encodes arguments with type information
#'   \item Sends JSON-RPC request to remote server
#'   \item Decodes response with type reconstruction
#' }
#'
#' @param data Execution context object containing:
#'   \itemize{
#'     \item \code{$id}: Request identifier for response matching
#'     \item \code{$params$name}: Tool name to execute
#'     \item \code{$params$arguments}: Named list of function arguments
#'     \item \code{$tool}: (Local) R function object to execute
#'     \item \code{$process}: (Remote) Process object for MCP server
#'     \item \code{$client}: (Remote) Client instance for communication
#'   }
#'
#' @return For local execution: JSON-RPC response object
#'         For remote execution: Parsed response from remote server
#'
#' @examples
#' \dontrun{
#' # Local tool execution (server-side)
#' local_data <- list(
#'   id = 1,
#'   params = list(
#'     name = "summary_stats",
#'     arguments = list(data = c(1, 2, 3, 4, 5))
#'   ),
#'   tool = summary_stats_function
#' )
#' response <- execute_tool_call(local_data)
#' 
#' # Remote tool execution (client-side)
#' remote_data <- list(
#'   id = 2,
#'   params = list(
#'     name = "read_file",
#'     arguments = list(path = "/file.txt")
#'   ),
#'   process = server_process,
#'   client = client_instance
#' )
#' response <- execute_tool_call(remote_data)
#' }
#'
#' @export
execute_tool_call <- function(data) {
  # Detect execution mode
  if (!is.null(data$tool) && is.function(data$tool)) {
    # Local execution (server-side)
    return(execute_local_tool(data))
  } else if (!is.null(data$process)) {
    # Remote execution (client-side)
    return(execute_remote_tool(data))
  } else {
    # Invalid execution context
    return(jsonrpc_response(
      data$id,
      error = list(
        code = -32602,
        message = "Invalid execution context: missing tool function or process"
      )
    ))
  }
}