#' @include tool-definition.R
#' @include tool-execution.R
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



tool_as_json <- function(tool) {
  check_tool(tool)
  
  if (length(tool$arguments) == 0) {
    inputSchema <- list(type = "object", properties = list())
  } else {
    properties <- list()
    for (name in names(tool$arguments)) {
      arg <- tool$arguments[[name]]
      if (inherits(arg, "mcpr_type")) {
        # Convert mcpr_type to JSON schema format
        schema <- list(type = arg$type)
        if (!is.null(arg$description)) schema$description <- arg$description
        if (arg$type == "array" && !is.null(arg$items)) {
          schema$items <- to_mcp_json(arg$items, auto_unbox = TRUE)
        }
        if (arg$type == "enum" && !is.null(arg$values)) {
          schema$enum <- arg$values
        }
        properties[[name]] <- schema
      } else {
        # Default to string type for non-mcpr_type arguments
        properties[[name]] <- list(type = "string")
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