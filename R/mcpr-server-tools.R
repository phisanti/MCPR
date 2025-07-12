# Functions related to the definition, management, and execution of tools.

#' Set the tools that the MCP server will provide
#'
#' @param x A list of tools created with `ellmer::tool`, or a path to an R
#'   file that returns such a list. If `NULL`, default tools are used.
#' @param registry A ToolRegistry instance to use for tool discovery. If provided,
#'   takes precedence over the `x` parameter.
#' @param x_arg The unevaluated expression for `x`, for use in error messages.
#' @param call The calling environment.
set_server_tools <- function(x, registry = NULL, x_arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  # Handle ToolRegistry parameter - takes precedence over x
  if (!is.null(registry)) {
    if (!inherits(registry, "ToolRegistry")) {
      cli::cli_abort("registry must be a ToolRegistry instance", call = call)
    }
    registry_tools <- registry$get_tools()
    the$server_tools <- c(registry_tools, list(list_r_sessions_tool, select_r_session_tool))

    return()
  }
  
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
  # COMMENT OUT FOR MIGRATION TO TOOL REGISTRY
  #reserved_names <- c("list_r_sessions", "select_r_session")
  #if (any(vapply(x, \(tool) tool@name, character(1)) %in% reserved_names)) {
  #  cli::cli_abort(
  #    "The tool names {.field list_r_sessions} and {.field select_r_session} are reserved by {.pkg mcptools}.",
  #    call = call
  #  )
  #}

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