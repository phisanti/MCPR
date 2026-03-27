# MCP Client Tool Processing
# Tool argument processing and execution functions for MCP client operations.
# Normalizes tool inputs using declared MCPR types before execution and response encoding.

#' @include type-conversion-from-json.R
#' @include utils.R

#' Normalize Tool Arguments by MCPR Type
#'
#' Prepares tool arguments according to declared MCPR container types before
#' execution. JSON container types are normalized into canonical `_mcp_type`
#' envelopes and reconstructed through `from_mcpr_json()`.
#'
#' @param args Named list of arguments from JSON-RPC request
#' @param schema Named list of mcpr_type objects (from ToolDef$arguments)
#' @return args with MCPR type normalization applied
#' @noRd
.mcpr_type_error <- function(spec, path, fallback) {
  if (!is.null(spec$error) && nzchar(spec$error)) {
    cli::cli_abort("Parameter {.field {path}} is invalid: {spec$error}")
  }

  desc <- spec$description %||% ""
  suffix <- if (nzchar(desc)) paste0(" ", desc) else ""
  cli::cli_abort(paste0(fallback, suffix), call = NULL)
}

#' @noRd
.parse_container_json <- function(value, expected_type, path, spec = NULL) {
  if (!(is.character(value) && length(value) == 1)) {
    return(value)
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(value, simplifyVector = FALSE),
    error = function(e) {
      if (!is.null(spec$error) && nzchar(spec$error)) {
        cli::cli_abort("Parameter {.field {path}} is invalid: {spec$error}", call = NULL)
      }
      cli::cli_abort("Parameter {.field {path}} should be {expected_type}, but the provided string is not valid JSON.", parent = e)
    }
  )

  parsed
}

#' @noRd
normalize_arg_by_type <- function(value, spec, path = "value") {
  if (!inherits(spec, "mcpr_type")) {
    return(value)
  }

  if (is.null(value)) {
    return(NULL)
  }

  if (is.list(value) && is.null(names(value)) && spec$type %in% c("string", "number", "integer", "boolean")) {
    value <- unlist(value, use.names = FALSE)
  }

  if (spec$type == "string") {
    if (!(is.character(value) && length(value) == 1 && !is.na(value))) {
      .mcpr_type_error(spec, path, paste0("Parameter ", path, " should be a string."))
    }
    return(value)
  }

  if (spec$type == "number") {
    if (!(is.numeric(value) && length(value) == 1 && !is.na(value))) {
      .mcpr_type_error(spec, path, paste0("Parameter ", path, " should be a number."))
    }
    return(as.numeric(value))
  }

  if (spec$type == "integer") {
    if (!(is.numeric(value) && length(value) == 1 && !is.na(value) && isTRUE(all.equal(value, round(value))))) {
      .mcpr_type_error(spec, path, paste0("Parameter ", path, " should be an integer."))
    }
    return(as.integer(round(value)))
  }

  if (spec$type == "boolean") {
    if (!(is.logical(value) && length(value) == 1 && !is.na(value))) {
      .mcpr_type_error(spec, path, paste0("Parameter ", path, " should be true or false."))
    }
    return(value)
  }

  if (spec$type == "enum") {
    if (!(is.character(value) && length(value) == 1 && !is.na(value) && value %in% spec$values)) {
      .mcpr_type_error(
        spec,
        path,
        paste0("Parameter ", path, " should be one of: ", paste(spec$values, collapse = ", "), ".")
      )
    }
    return(value)
  }

  if (spec$type == "json_object") {
    parsed <- .parse_container_json(value, "a JSON object / named list", path, spec)
    if (!is.list(parsed) || is.null(names(parsed))) {
      .mcpr_type_error(spec, path, paste0("Parameter ", path, " should be a JSON object / named list."))
    }
    return(from_mcpr_json(list(`_mcp_type` = "json_object", value = parsed)))
  }

  if (spec$type == "json_array") {
    parsed <- .parse_container_json(value, "a JSON array / list", path, spec)
    if (is.atomic(parsed) && !is.null(parsed)) {
      parsed <- as.list(parsed)
    }
    if (!is.list(parsed)) {
      .mcpr_type_error(spec, path, paste0("Parameter ", path, " should be a JSON array / list."))
    }
    return(from_mcpr_json(list(`_mcp_type` = "json_array", value = parsed)))
  }

  if (spec$type == "object") {
    parsed <- .parse_container_json(value, "an object with named fields", path, spec)
    parsed <- from_mcpr_json(parsed)
    if (!is.list(parsed) || is.null(names(parsed))) {
      .mcpr_type_error(spec, path, paste0("Parameter ", path, " should be an object with named fields."))
    }

    properties <- spec$properties %||% list()
    required_names <- names(properties)[vapply(properties, function(prop) isTRUE(prop$required), logical(1))]
    missing_required <- setdiff(required_names, names(parsed))
    if (length(missing_required) > 0) {
      cli::cli_abort(
        "Parameter {.field {path}} is missing required field(s): {.val {missing_required}}.",
        call = NULL
      )
    }

    extra_fields <- setdiff(names(parsed), names(properties))
    if (!isTRUE(spec$additional_properties) && length(extra_fields) > 0) {
      cli::cli_abort(
        "Parameter {.field {path}} contains unexpected field(s): {.val {extra_fields}}.",
        call = NULL
      )
    }

    for (name in intersect(names(parsed), names(properties))) {
      parsed[[name]] <- normalize_arg_by_type(parsed[[name]], properties[[name]], path = paste0(path, ".", name))
    }

    return(parsed)
  }

  if (spec$type == "array") {
    parsed <- .parse_container_json(value, "an array", path, spec)
    parsed <- from_mcpr_json(parsed)
    if (is.atomic(parsed) && !is.null(parsed)) {
      parsed <- as.list(parsed)
    }
    if (!is.list(parsed)) {
      .mcpr_type_error(spec, path, paste0("Parameter ", path, " should be an array."))
    }

    item_spec <- spec$items
    parsed <- lapply(seq_along(parsed), function(i) {
      normalize_arg_by_type(parsed[[i]], item_spec, path = paste0(path, "[", i, "]"))
    })

    if (inherits(item_spec, "mcpr_type") && item_spec$type %in% c("string", "number", "integer", "boolean")) {
      return(unlist(parsed, use.names = FALSE))
    }

    return(parsed)
  }

  value
}

#' @noRd
normalize_args_by_type <- function(args, schema) {
  if (!is.list(schema) || length(schema) == 0) {
    return(args)
  }

  required_names <- names(schema)[vapply(schema, function(spec) inherits(spec, "mcpr_type") && isTRUE(spec$required), logical(1))]
  missing_required <- setdiff(required_names, names(args))
  if (length(missing_required) > 0) {
    cli::cli_abort("Missing required parameter(s): {.val {missing_required}}.", call = NULL)
  }

  for (name in intersect(names(args), names(schema))) {
    args[[name]] <- normalize_arg_by_type(args[[name]], schema[[name]], path = name)
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
  tool <- data$tool
  tool_arguments <- attr(tool, "mcpr_arguments", exact = TRUE) %||% list()
  tool_convert <- attr(tool, "mcpr_convert", exact = TRUE)

  tryCatch(
    {
      if (isTRUE(tool_convert)) {
        args <- normalize_args_by_type(args, tool_arguments)
      }
      encode_tool_results(data, do.call(tool, args))
    },
    error = function(e) {
      jsonrpc_response(
        data$id,
        error = list(code = -32603, message = conditionMessage(e))
      )
    }
  )
}
