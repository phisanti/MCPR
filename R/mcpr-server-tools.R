#' @include tool-definition.R
#' @include tool-execution.R
# Functions related to the definition, management, and execution of tools.

#' @title Set MCP server tools
#' @description Set the tools that the MCP server will provide
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


#' Convert ToolDef to MCP JSON format
#'
#' @description
#' Converts a ToolDef object to MCP protocol-compliant JSON structure.
#' Transforms MCPR type definitions to JSON Schema format for tool registration.
#' Used by mcpServer's get_tools() method to serialize tools for MCP client discovery.
#'
#' @param tool A ToolDef object containing name, description, and typed arguments
#' @return List with name, description, and inputSchema fields for MCP protocol
#' 
#' @examples
#' \dontrun{
#' # Convert simple tool without arguments
#' tool <- create_tool("hello", "Says hello", list())
#' tool_as_json(tool)
#' 
#' # Convert tool with typed arguments
#' tool <- create_tool("greet", "Greets user", list(name = mcpr_string()))
#' tool_as_json(tool)
#' }
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