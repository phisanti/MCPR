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
#' @seealso \code{\link{as_tool_call_result}} for result formatting
execute_tool_call <- function(data) {
  tool_name <- data$params$name
  args <- data$params$arguments

  # Argument coercion
  args <- lapply(args, function(x) {
    if (is.list(x) && is.null(names(x))) {
      unlist(x, use.names = FALSE)
    } else {
      x
    }
  })

  tryCatch(
    as_tool_call_result(data, do.call(data$tool, args)),
    error = function(e) {
      jsonrpc_response(
        data$id,
        error = list(code = -32603, message = conditionMessage(e))
      )
    }
  )
}


#' Convert Tool Result to MCP Response Format
#'
#' @description
#' Transforms tool execution results into the standardized MCP \code{tools/call}
#' response format. This function ensures consistent response structure across
#' all tool executions and properly handles both successful results and error
#' conditions from \code{ellmer::ContentToolResult} objects.
#'
#' @details
#' The function processes different result types:
#' \itemize{
#'   \item \strong{Standard results}: Converted to text content blocks
#'   \item \strong{ellmer::ContentToolResult}: Extracts value or error appropriately
#'   \item \strong{Vector results}: Joined with newlines for readability
#' }
#'
#' The \code{isError} field in the response allows MCP clients to distinguish
#' between successful tool execution and tool-level errors, enabling appropriate
#' handling in AI agent workflows.
#'
#' @param data The original JSON-RPC request object containing the request ID
#' @param result The tool execution result, which can be:
#'   \itemize{
#'     \item Any R object (converted to character representation)
#'     \item An \code{ellmer::ContentToolResult} object with value/error slots
#'   }
#'
#' @return A JSON-RPC response object structured as:
#'   \itemize{
#'     \item \code{content}: List of content blocks with type and text
#'     \item \code{isError}: Boolean indicating if result represents an error
#'   }
#'
#' @examples
#' \dontrun{
#' # Standard successful result
#' request <- list(id = 1)
#' result <- c("Mean: 2.5", "SD: 1.3")
#' response <- as_tool_call_result(request, result)
#' 
#' # ellmer ContentToolResult with error
#' error_result <- ellmer::ContentToolResult(error = "Invalid input")
#' error_response <- as_tool_call_result(request, error_result)
#' }
#'
#' @keywords internal
#' @seealso \code{\link{execute_tool_call}} for the main execution workflow
as_tool_call_result <- function(data, result) {
  is_error <- FALSE
  if (inherits(result, "ellmer::ContentToolResult")) {
    is_error <- !is.null(result@error)
    result <- result@value %||% result@error
  }

  jsonrpc_response(
    data$id,
    list(
      content = list(
        list(
          type = "text",
          text = paste(result, collapse = "\n")
        )
      ),
      isError = is_error
    )
  )
}


#' Convert MCP Tool Schema to Ellmer Type Definitions
#'
#' @description
#' Transforms MCP tool input schemas into \code{ellmer} type definitions for
#' R function integration. This function bridges the gap between JSON Schema
#' specifications used in MCP and the structured type system required by
#' the \code{ellmer} package for AI tool integration.
#'
#' @details
#' The conversion process involves:
#' \itemize{
#'   \item Extracting properties and required fields from the input schema
#'   \item Mapping each property to an appropriate \code{ellmer} type
#'   \item Preserving parameter descriptions and requirement constraints
#'   \item Handling nested objects and array types recursively
#' }
#'
#' This function is essential for dynamic tool registration where MCP tool
#' definitions need to be converted to \code{ellmer} tools for R integration.
#'
#' @param tool A tool definition object containing:
#'   \itemize{
#'     \item \code{$inputSchema$properties}: Named list of parameter definitions
#'     \item \code{$inputSchema$required}: Character vector of required parameter names
#'   }
#'
#' @return A named list where:
#'   \itemize{
#'     \item Names correspond to parameter names from the input schema
#'     \item Values are \code{ellmer} type objects (e.g., \code{type_string}, \code{type_number})
#'   }
#'
#' @examples
#' \dontrun{
#' # MCP tool schema conversion
#' mcp_tool <- list(
#'   inputSchema = list(
#'     properties = list(
#'       filename = list(type = "string", description = "File path"),
#'       count = list(type = "integer", description = "Number of lines")
#'     ),
#'     required = c("filename")
#'   )
#' )
#' 
#' ellmer_types <- as_ellmer_types(mcp_tool)
#' # Returns list with ellmer type definitions
#' }
#'
#' @keywords internal
#' @seealso \code{\link{as_ellmer_type}} for individual property conversion
as_ellmer_types <- function(tool) {
  properties <- tool$inputSchema$properties
  required_fields <- tool$inputSchema$required
  result <- list()
  for (prop_name in names(properties)) {
    result[[prop_name]] <- as_ellmer_type(
      prop_name,
      properties[[prop_name]],
      required_fields
    )
  }
  result
}


#' Convert Single Property to Ellmer Type
#'
#' @description
#' Converts an individual JSON Schema property definition to the corresponding
#' \code{ellmer} type object. This function handles the detailed mapping between
#' JSON Schema type specifications and R's type system as implemented in the
#' \code{ellmer} package.
#'
#' @details
#' Supported type mappings include:
#' \itemize{
#'   \item \strong{string}: \code{ellmer::type_string}
#'   \item \strong{number}: \code{ellmer::type_number}
#'   \item \strong{integer}: \code{ellmer::type_integer}
#'   \item \strong{boolean}: \code{ellmer::type_boolean}
#'   \item \strong{array}: \code{ellmer::type_array} with recursive item type detection
#'   \item \strong{object}: \code{ellmer::type_object} with nested property handling
#' }
#'
#' The function preserves important metadata including descriptions, required
#' status, and nested structure constraints. For unsupported types, it defaults
#' to \code{type_string} to ensure compatibility.
#'
#' @param prop_name Character string representing the parameter name
#' @param prop_def List containing the JSON Schema property definition with:
#'   \itemize{
#'     \item \code{$type}: Type specification (string, number, integer, etc.)
#'     \item \code{$description}: Human-readable parameter description
#'     \item \code{$items}: For arrays, the schema for array elements
#'     \item \code{$properties}: For objects, nested property definitions
#'   }
#' @param required_fields Character vector of required parameter names for
#'   determining if this property is mandatory
#'
#' @return An \code{ellmer} type object appropriate for the specified JSON Schema type,
#'   or \code{NULL} if the type specification is invalid
#'
#' @examples
#' \dontrun{
#' # String parameter conversion
#' string_prop <- list(type = "string", description = "User name")
#' ellmer_string <- as_ellmer_type("username", string_prop, c("username"))
#' 
#' # Array parameter conversion
#' array_prop <- list(
#'   type = "array",
#'   description = "List of values",
#'   items = list(type = "number")
#' )
#' ellmer_array <- as_ellmer_type("values", array_prop, character())
#' }
#'
#' @keywords internal
#' @seealso \code{\link{as_ellmer_types}} for processing complete tool schemas
as_ellmer_type <- function(prop_name, prop_def, required_fields = character()) {
  type <- prop_def$type
  description <- prop_def$description
  is_required <- prop_name %in% required_fields
  if (length(type) == 0) return(NULL)

  switch(type,
    "string" = ellmer::type_string(description = description, required = is_required),
    "number" = ellmer::type_number(description = description, required = is_required),
    "integer" = ellmer::type_integer(description = description, required = is_required),
    "boolean" = ellmer::type_boolean(description = description, required = is_required),
    "array" = {
      items_type <- if (!is.null(prop_def$items)) {
        as_ellmer_type("", prop_def$items, required_fields)
      } else {
        ellmer::type_string()
      }
      ellmer::type_array(description = description, items = items_type, required = is_required)
    },
    "object" = {
      obj_args <- list(.description = description, .required = is_required)
      if (!is.null(prop_def$properties)) {
        for (obj_prop_name in names(prop_def$properties)) {
          obj_args[[obj_prop_name]] <- as_ellmer_type(
            obj_prop_name,
            prop_def$properties[[obj_prop_name]],
            required_fields
          )
        }
      }
      do.call(ellmer::type_object, obj_args)
    },
    # Default fallback
    ellmer::type_string(description = description, required = is_required)
  )
}
