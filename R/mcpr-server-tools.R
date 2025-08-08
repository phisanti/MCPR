#' @include tool-definition.R
#' @include tool-execution.R

# MCP Server Tools Management
# Functions for managing server-side tool registration and JSON conversion.
# Handles tool discovery, registration, and MCP protocol serialization.

#' Set MCP Server Tools
#'
#' @title Set MCP Server Tools
#' @description Configures tools available for MCP server through registry assignment.
#' Validates registry instance and updates global server tools collection. Enables
#' tool discovery and registration for client-server communication through centralized
#' tool management and validation.
#'
#' @param registry A ToolRegistry instance to use for tool discovery
#' @param call The calling environment
#' @return None (invisible)
set_server_tools <- function(registry = NULL, call = rlang::caller_env()) {
  # Handle ToolRegistry parameter - takes precedence over x
  if (!inherits(registry, "ToolRegistry")) {
    cli::cli_abort("registry must be a ToolRegistry instance", call = call)
  }

  the$server_tools <- registry$get_tools()

}


#' Convert ToolDef to MCP JSON Format
#'
#' @title Convert ToolDef to MCP JSON Format
#' @description Converts ToolDef object to MCP protocol-compliant JSON structure.
#' Transforms MCPR type definitions to JSON Schema format through schema mapping
#' and property serialization. Enables tool registration and discovery by converting
#' internal tool representations to standardized MCP protocol format.
#'
#' @param tool A ToolDef object containing name, description, and typed arguments
#' @return List with name, description, and inputSchema fields for MCP protocol
tool_as_json <- function(tool) {
  check_tool(tool)
  
  if (length(tool$arguments) == 0) {
    inputSchema <- list(type = "object", properties = named_list())
  } else {
    properties <- list()
    for (name in names(tool$arguments)) {
      arg <- tool$arguments[[name]]
      if (inherits(arg, "mcpr_type")) {
        # Convert mcpr_type to JSON schema format
        schema <- list(type = arg$type)
        if (!is.null(arg$description)) schema$description <- arg$description
        if (arg$type == "array" && !is.null(arg$items)) {
          schema$items <- to_mcpr_json(arg$items, auto_unbox = TRUE)
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