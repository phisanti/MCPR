# MCP Server Tool Management
# Functions for defining, managing, and executing tools on the MCP server side.
# Handles tool registration, discovery, and server-side execution coordination.

#' Set MCP Server Tools
#'
#' @include aaa.R
#' @include logger.R
#' @include tool-definition.R
#' @include tool-register.R
#' @include utils.R
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

  result <- compact(list(
    name = tool$name,
    description = tool$description,
    inputSchema = inputSchema
  ))

  annotations <- tool$annotations
  if (length(annotations) > 0) {
    # Extract _meta to top level (MCP Apps extension point)
    if (!is.null(annotations[["_meta"]])) {
      meta <- annotations[["_meta"]]
      # Also expose legacy flat key for compatibility
      if (!is.null(meta$ui$resourceUri)) {
        meta[["ui/resourceUri"]] <- meta$ui$resourceUri
      }
      result[["_meta"]] <- meta
    }

    # Remaining annotations (standard MCP annotations excluding _meta)
    standard_annotations <- annotations[setdiff(names(annotations), "_meta")]
    if (length(standard_annotations) > 0) {
      result$annotations <- standard_annotations
    }
  }

  result
}

#' Convert mcpr_type to JSON Schema
#'
#' Faithfully serializes an mcpr_type object to its JSON Schema equivalent,
#' including nested properties, items, additionalProperties, and enum values.
#'
#' @param spec An mcpr_type object or fallback value
#' @return A JSON Schema list
#' @noRd
mcpr_type_to_json_schema <- function(spec) {
  if (!inherits(spec, "mcpr_type")) {
    return(list(type = "string", description = as.character(spec %||% "")))
  }

  base <- compact(list(
    type = spec$type,
    description = spec$description
  ))

  switch(spec$type,
    "object" = {
      if (length(spec$properties) > 0) {
        base$properties <- lapply(spec$properties, mcpr_type_to_json_schema)
      }
      if (isTRUE(spec$additional_properties)) {
        base$additionalProperties <- TRUE
      }
      base
    },
    "array" = {
      if (!is.null(spec$items)) {
        base$items <- mcpr_type_to_json_schema(spec$items)
      }
      base
    },
    "enum" = {
      compact(list(type = "string", enum = spec$values, description = spec$description))
    },
    base
  )
}

#' Convert tool arguments to JSON Schema format
#'
#' Converts a named list of mcpr_type argument specs into a JSON Schema
#' object suitable for MCP tools/list inputSchema.
#'
#' @param arguments Named list of mcpr_type argument specifications
#' @return JSON Schema object with type, properties, and required fields
#'
#' @details Required properties are wrapped with `I()` before JSON
#' serialization. MCPR uses `jsonlite::toJSON(auto_unbox = TRUE)` globally for
#' protocol messages, and plain length-1 character vectors would otherwise be
#' serialized as invalid JSON Schema scalars instead of `required` arrays.
#' @noRd
convert_arguments_to_schema <- function(arguments) {
  if (length(arguments) == 0) {
    return(list(
      type = "object",
      properties = named_list()
    ))
  }

  properties <- named_list()
  required_args <- character()

  for (arg_name in names(arguments)) {
    arg_spec <- arguments[[arg_name]]
    properties[[arg_name]] <- mcpr_type_to_json_schema(arg_spec)
    if (isTRUE(arg_spec$required)) {
      required_args <- c(required_args, arg_name)
    }
  }

  compact(list(
    type = "object",
    properties = properties,
    # Prevent jsonlite::toJSON(auto_unbox = TRUE) from collapsing
    # single-element required vectors into invalid scalar JSON.
    required = if (length(required_args) > 0) I(required_args)
  ))
}
