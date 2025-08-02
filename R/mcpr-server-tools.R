#' @include tool-definition.R
# Functions related to the definition, management, and execution of tools.

#' Set the tools that the MCP server will provide
#'
#' @param registry A ToolRegistry instance to use for tool discovery. If provided,
#'   takes precedence over the `x` parameter.
#' @param call The calling environment.
set_server_tools <- function(registry = NULL, call = rlang::caller_env()) {
  # Handle ToolRegistry parameter - takes precedence over x
  if (!inherits(registry, "ToolRegistry")) {
    cli::cli_abort("registry must be a ToolRegistry instance", call = call)
  }

  the$server_tools <- registry$get_tools()

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


# Helper function to convert mcpr_type to JSON schema
.mcpr_type_to_schema <- function(type) {
  if (is.null(type)) return(list(type = "string"))
  
  schema <- list(type = type$type)
  if (!is.null(type$description)) schema$description <- type$description
  if (type$type == "array" && !is.null(type$items)) schema$items <- .mcpr_type_to_schema(type$items)
  if (type$type == "enum" && !is.null(type$values)) schema$enum <- type$values
  schema
}

tool_as_json <- function(tool) {
  check_tool(tool)
  
  if (length(tool$arguments) == 0) {
    inputSchema <- list(type = "object", properties = list())
  } else {
    properties <- list()
    for (name in names(tool$arguments)) {
      arg <- tool$arguments[[name]]
      if (inherits(arg, "mcpr_type")) {
        properties[[name]] <- .mcpr_type_to_schema(arg)
      } else {
        properties[[name]] <- .mcpr_type_to_schema(map_type_schema("character"))
      }
    }
    inputSchema <- list(type = "object", properties = properties)
  }
  inputSchema$description <- NULL # This field is not needed

  list(
    name = tool$name,
    description = tool$description,
    inputSchema = inputSchema
  )
}