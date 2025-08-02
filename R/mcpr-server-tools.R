#' @include tool-definition.R
# Functions related to the definition, management, and execution of tools.

#' Set the tools that the MCP server will provide
#'
#' @param x A list of tools created with `tool()`, or a path to an R
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
    the$server_tools <- registry_tools  # ONLY registry tools
    return()
  }
  
  # No registry provided - use empty tools (complete migration)
  the$server_tools <- list()
  
  # Warn about deprecated usage
  if (!is.null(x)) {
    cli::cli_warn(
      c(
        "The 'tools' parameter is deprecated.",
        "i" = "Use ToolRegistry instead: mcpServer$new(registry = ToolRegistry$new(tools_dir = 'path'))"
      ),
      call = call
    )
  }
}

#' Get the currently configured server tools
#'
#' @return A named list of `ToolDef` objects.
get_mcptools_tools <- function() {
  res <- the$server_tools
  stats::setNames(res, vapply(res, \(x) x$name, character(1)))
}

#' Get server tools formatted as a JSON list for the MCP protocol
#'
#' @return A list of tool definitions suitable for JSON serialization.
get_mcptools_tools_as_json <- function() {
  tools <- lapply(unname(get_mcptools_tools()), tool_as_json)
  compact(tools)
}


tool_as_json <- function(tool) {
  check_tool(tool)
  
  inputSchema <- convert_arguments_to_schema(tool$arguments)
  inputSchema$description <- NULL # This field is not needed

  list(
    name = tool$name,
    description = tool$description,
    inputSchema = inputSchema
  )
}