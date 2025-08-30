# MCP Server Tool Management
# Functions for defining, managing, and executing tools on the MCP server side.
# Handles tool registration, discovery, and server-side execution coordination.

# MCP Server Tools Management
# Functions for managing server-side tool registration and JSON conversion.
# Handles tool discovery, registration, and MCP protocol serialization.

#' Set MCP Server Tools
#'
#' @param registry A ToolRegistry instance to use for tool discovery. If NULL,
#'   an empty tools list is set.
#' @param call The calling environment.
#' @noRd
set_server_tools <- function(registry = NULL, call = rlang::caller_env()) {
  # Handle ToolRegistry parameter
  if (!is.null(registry)) {
    if (!inherits(registry, "ToolRegistry")) {
      error_msg <- "registry must be a ToolRegistry instance"
      MCPRLogger$new(component = "SERVER")$error(error_msg)
      cli::cli_abort(error_msg, call = call)
    }
    registry_tools <- registry$get_tools()
    the$server_tools <- registry_tools
    return()
  }

  # No registry provided - use empty tools list
  the$server_tools <- list()
}


#' Convert ToolDef to MCP JSON Format
#'
#' @return A named list of `ToolDef` objects.
#' @noRd
get_mcptools_tools <- function() {
  res <- the$server_tools
  stats::setNames(res, vapply(res, \(x) x$name, character(1)))
}

#' Get server tools formatted as a JSON list for the MCP protocol
#'
#' @param tool A ToolDef object containing name, description, and typed arguments
#' @return List with name, description, and inputSchema fields for MCP protocol
#' @noRd
tool_as_json <- function(tool) {
  # Convert tool arguments to JSON schema format
  inputSchema <- convert_arguments_to_schema(tool$arguments)

  compact(list(
    name = tool$name,
    description = tool$description,
    inputSchema = inputSchema
  ))
}

#' Convert tool arguments to JSON schema format
#' @param arguments Named list of argument specifications
#' @return JSON schema object
#' @noRd
convert_arguments_to_schema <- function(arguments) {
  if (length(arguments) == 0) {
    return(list(
      type = "object",
      properties = named_list() # Use named_list for empty properties
    ))
  }

  properties <- named_list() # Start with named list
  for (arg_name in names(arguments)) {
    arg_spec <- arguments[[arg_name]]
    if (is.list(arg_spec) && !is.null(arg_spec$type)) {
      properties[[arg_name]] <- list(
        type = arg_spec$type,
        description = arg_spec$description %||% ""
      )
    } else {
      # Fallback for simple specifications
      properties[[arg_name]] <- list(
        type = "string",
        description = as.character(arg_spec %||% "")
      )
    }
  }

  list(
    type = "object",
    properties = properties
  )
}
