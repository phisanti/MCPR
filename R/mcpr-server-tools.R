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
    the$server_tools <- c(registry_tools)
    return()
  }
  
  if (is.null(x)) {
    the$server_tools <- c(list(list_r_sessions_tool, select_r_session_tool, execute_r_code_tool))
    return()
  }

  force(x_arg)
  
  # COMMENTED OUT FOR MIGRATION TO TOOL REGISTRY - Legacy tool assignment
  # the$server_tools <- c(x, list(list_r_sessions_tool, select_r_session_tool))
  
  # *** CHANGE: When legacy path is reached, only load built-in tools ***
  # This ensures that if someone tries to use the old system, they get a warning 
  # and only the core functionality, encouraging migration to ToolRegistry
  # the$server_tools <- c(list(list_r_sessions_tool, select_r_session_tool, execute_r_code_tool))
  
  cli::cli_warn(
    c(
      "Legacy tool loading is deprecated and will be removed.",
      "i" = "Please use ToolRegistry for tool discovery: registry <- ToolRegistry$new(); mcp_server(registry = registry)"
    ),
    call = call
  )
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