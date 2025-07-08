# Functions related to the definition, management, and execution of tools.

#' Set the tools that the MCP server will provide
#'
#' @param x A list of tools created with `ellmer::tool`, or a path to an R
#'   file that returns such a list. If `NULL`, default tools are used.
#' @param x_arg The unevaluated expression for `x`, for use in error messages.
#' @param call The calling environment.
set_server_tools <- function(x, x_arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (is.null(x)) {
    the$server_tools <- c(list(list_r_sessions_tool, select_r_session_tool, execute_r_code_tool))
    return()
  }

  force(x_arg)
  if (looks_like_r_file(x)) {
    x <- tryCatch(
      source_tools(x),
      error = function(err) {
        cli::cli_abort(
          "Sourcing the {.arg {x_arg}} file {.file x} failed.",
          parent = err,
          call = call
        )
      }
    )
  }

  if (!rlang::is_list(x) || !all(vapply(x, inherits, logical(1), "ellmer::ToolDef"))) {
    msg <- "{.arg {x_arg}} must be a list of tools created with {.fn ellmer::tool} or a .R file path that returns a list of ellmer tools when sourced."
    if (inherits(x, "ellmer::ToolDef")) {
      msg <- c(msg, "i" = "Did you mean to wrap {.arg {x_arg}} in `list()`?")
    }
    cli::cli_abort(msg, call = call)
  }

  reserved_names <- c("list_r_sessions", "select_r_session")
  if (any(vapply(x, \(tool) tool@name, character(1)) %in% reserved_names)) {
    cli::cli_abort(
      "The tool names {.field list_r_sessions} and {.field select_r_session} are reserved by {.pkg mcptools}.",
      call = call
    )
  }

  the$server_tools <- c(x, list(list_r_sessions_tool, select_r_session_tool))
}

#' Get the currently configured server tools
#'
#' @return A named list of `ellmer::ToolDef` objects.
get_mcptools_tools <- function() {
  res <- the$server_tools
  stats::setNames(res, vapply(res, \(x) x@name, character(1)))
}

#' Get server tools formatted as a JSON list for the MCP protocol
#'
#' @return A list of tool definitions suitable for JSON serialization.
get_mcptools_tools_as_json <- function() {
  tools <- lapply(unname(get_mcptools_tools()), tool_as_json)
  compact(tools)
}

#' Execute a tool call request
#'
#' @param data The parsed JSON-RPC request for a `tools/call` method.
#' @return A JSON-RPC response object with the result of the tool execution.
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

#' Convert a tool's result into a `tools/call` response format
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


#' Convert a tool definition to a JSON-compatible list
tool_as_json <- function(tool) {
  dummy_provider <- ellmer::Provider("dummy", "dummy", "dummy")
  as_json <- getNamespace("ellmer")[["as_json"]]
  inputSchema <- compact(as_json(dummy_provider, tool@arguments))
  inputSchema$description <- NULL # This field is not needed

  list(
    name = tool@name,
    description = tool@description,
    inputSchema = inputSchema
  )
}

#' Convert MCP tool schema to `ellmer` type definitions
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

#' Convert a single property to an `ellmer` type
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

# === Helpers for set_server_tools ===
looks_like_r_file <- function(x) {
  rlang::is_string(x) &&
    file.exists(x) &&
    grepl("\\.r$", x, ignore.case = TRUE)
}

source_tools <- function(x) {
  source(x, local = TRUE)$value
}
