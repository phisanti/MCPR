# TOOL EXECUTION ENGINE
# Execution interface for MCP tool calls with type preservation
# Functions ordered from simple utilities to complex orchestrators

#' Check for MCP Type Markers
#'
#' @title Check for MCP Type Markers
#' @description Recursively checks for MCP type markers in nested list structures.
#' Performs deep inspection of list elements to detect type preservation markers
#' throughout nested object hierarchies. Enables type-aware processing decisions
#' for MCP protocol data handling and reconstruction workflows.
#'
#' @param obj Object to check for type markers
#' @return Logical indicating presence of MCP type markers
#' @noRd
has_mcpr_types_deep <- function(obj) {
  if (is.list(obj)) {
    # Check current level for _mcp_type
    if (!is.null(obj[["_mcp_type"]])) {
      return(TRUE)
    }

    # Check recursively
    return(any(sapply(obj, has_mcpr_types_deep, USE.NAMES = FALSE)))
  }

  FALSE
}

#' Create Tool Execution Context
#'
#' @title Create Tool Execution Context
#' @description Creates properly structured execution contexts for local or remote tool execution.
#' Constructs standardized context object containing request metadata and execution targets.
#' Enables unified tool execution interface across different execution modes through
#' structured context preparation and parameter organization.
#'
#' @param id Request ID for response matching
#' @param tool_name Name of tool to execute
#' @param arguments Named list of function arguments
#' @param tool Optional local R function for server-side execution
#' @param process Optional remote server process for client-side execution
#' @param client Optional client instance for communication handling
#' @return Execution context object with structured parameters
#' @noRd
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
    has_mcpr_types <- any(sapply(arguments, function(x) {
      is.list(x) && !is.null(x[["_mcp_type"]])
    }))

    if (has_mcpr_types) {
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

#' Decode Tool Response
#'
#' @title Decode Tool Response
#' @description Decodes MCP tool responses with automatic type reconstruction for complex objects.
#' Detects type preservation markers and reconstructs original R object types from
#' JSON-RPC responses. Handles both simple text responses and complex type-preserved
#' responses through intelligent marker detection and reconstruction.
#'
#' @param response_result Result portion of JSON-RPC response
#' @return Decoded R objects with proper types or original response if no markers
#' @noRd
decode_tool_response <- function(response_result) {
  # Check if response contains MCP type markers
  if (is.list(response_result) && has_mcpr_types_deep(response_result)) {
    return(from_mcpr_json(response_result))
  }

  # Return as-is for simple responses
  response_result
}

#' Execute Local Tool
#'
#' @title Execute Local Tool
#' @description Executes local R functions as MCP tools with comprehensive type preservation.
#' Handles argument decoding from JSON-RPC format, function invocation with proper
#' parameter mapping, and result encoding for MCP protocol responses. Provides
#' error handling and structured response formatting for server-side execution.
#'
#' @param data Local execution context containing tool function and parameters
#' @return JSON-RPC response object with results or error information
#' @noRd
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

#' Execute Remote Tool
#'
#' @title Execute Remote Tool
#' @description Executes tools on remote MCP servers with comprehensive type preservation.
#' Handles argument encoding with type information, JSON-RPC communication through
#' message handlers, and response decoding with automatic type reconstruction.
#' Manages client-server communication and logging for remote tool execution.
#'
#' @param data Remote execution context containing process and client information
#' @return Parsed response from remote server with type reconstruction
#' @noRd
execute_remote_tool <- function(data) {
  # Encode arguments with type preservation
  encoded_args <- to_mcpr_json(data$params$arguments, auto_unbox = TRUE)

  # Create messenger with client logging if available
  messenger <- if (!is.null(data$client) && !is.null(data$client$log_communication)) {
    MessageHandler$new(logger = data$client$log_communication)
  } else {
    MessageHandler$new()
  }

  # Create JSON-RPC request
  request <- messenger$create_tool_request(
    id = data$id,
    tool = data$params$name,
    arguments = encoded_args
  )

  # Send request and receive response
  response <- messenger$send_receive(
    process = data$process,
    message = request,
    context = "CLIENT"
  )

  # Decode response with type reconstruction
  if (!is.null(response$result)) {
    response$result <- decode_tool_response(response$result)
  }

  response
}

#' Execute MCP Tool Call Request
#'
#' @title Execute MCP Tool Call Request
#' @description Universal execution engine for MCP tool calls supporting both local and remote execution.
#' Automatically detects execution mode and routes to appropriate handler with comprehensive
#' type preservation and error handling. Serves as primary execution interface for all
#' tool operations across MCPR framework components.
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
#' @noRd
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
