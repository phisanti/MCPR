# Tool Registry Helper Functions
# Helper functions for converting roxygen2 documentation blocks into ToolDef objects.
# Processes parsed roxygen metadata to create structured tool definitions.

#' Create Tool from Roxygen Block
#'
#' @title Create Tool from Roxygen Block
#' @description Constructs ToolDef object from parsed roxygen block and associated function.
#' Extracts function metadata, validates function existence, and converts roxygen2
#' documentation into structured tool specification. Handles error cases and provides
#' comprehensive logging for tool creation workflow.
#'
#' @param block Roxygen2 block object containing function documentation
#' @param env Environment where the function is defined
#' @param file_path Path of file being parsed (for logging purposes)
#' @return ToolDef object or NULL on failure
create_tool_from_block <- function(block, env, file_path) {
  # Extract function name from the block object
  func_name <- block$object$alias
  
  if (is.null(func_name) || !exists(func_name, envir = env)) {
    cli::cli_warn("Function {.fn {func_name %||% 'unknown'}} not found in {.file {basename(file_path)}}")
    return(NULL)
  }
  
  func <- get(func_name, envir = env)
  if (!is.function(func)) {
    cli::cli_warn("{.fn {func_name}} is not a function")
    return(NULL)
  }
  
  # Extract description
  description <- extract_description(block)
  
  # Extract parameters
  param_tags <- Filter(function(tag) inherits(tag, "roxy_tag_param"), block$tags)
  mcpr_args <- convert_to_schema(param_tags)
  
  # Create the tool
  tryCatch({
    tool(
      fun = func, 
      description = description, 
      name = func_name,
      arguments = mcpr_args
    )
  }, error = function(e) {
    cli::cli_warn("Failed to create tool for {.fn {func_name}}: {conditionMessage(e)}")
    NULL
  })
}

#' Extract Description from Roxygen Block
#'
#' @title Extract Description from Roxygen Block
#' @description Extracts function description from roxygen2 documentation tags with fallback strategy.
#' Prioritizes @description tag content, falls back to @intro tag, and provides default
#' message for missing documentation. Ensures consistent description extraction for
#' tool specification creation through tag hierarchy processing.
#'
#' @param block Roxygen2 block object containing documentation tags
#' @return Character string with extracted description
extract_description <- function(block) {
  # Look for @description tag first
  desc_tag <- Find(function(tag) inherits(tag, "roxy_tag_description"), block$tags)
  if (!is.null(desc_tag)) {
    return(paste(desc_tag$val, collapse = " "))
  }
  
  # Fall back to title/introduction
  intro_tag <- Find(function(tag) inherits(tag, "roxy_tag_intro"), block$tags)
  if (!is.null(intro_tag)) {
    return(paste(intro_tag$val, collapse = " "))
  }
  
  # Default
  return("No description available")
}

#' Convert Roxygen Parameters to MCPR Types
#'
#' @title Convert Roxygen Parameters to MCPR Types
#' @description Converts roxygen2 @param tags into MCPR type definitions through heuristic analysis.
#' Analyzes parameter descriptions for type hints and maps to appropriate MCPR type
#' specifications. Provides automatic type inference for tool parameter validation
#' and MCP protocol compatibility through description keyword matching.
#'
#' @param param_tags List of roxy_tag_param objects from roxygen2 parsing
#' @return Named list of MCPR type objects for tool arguments
convert_to_schema <- function(param_tags) {
  mcpr_args <- list()
  
  for (param_tag in param_tags) {
    # The 'val' field of a roxy_tag_param is already parsed into name and description
    param_name <- param_tag$val$name
    param_desc <- param_tag$val$description
    
    # A more robust way to infer type is needed, but for now, we assume string
    # or look for hints in the description. This part remains a weak point.
    # A better convention would be {type} description, e.g., @param name [string] The name.
    type_str <- "string" # Default type
    
    # Simple heuristic to guess type from description
    if (grepl("\\b(numeric|number|integer|int)\\b", param_desc, ignore.case = TRUE)) {
      type_str <- "numeric"
    } else if (grepl("\\b(logical|boolean|bool)\\b", param_desc, ignore.case = TRUE)) {
      type_str <- "boolean"
    } else if (grepl("\\b(list|array|vector)\\b", param_desc, ignore.case = TRUE)) {
      type_str <- "array"
    }

    mcpr_args[[param_name]] <- map_type_schema(type_str, param_desc)
  }
  
  mcpr_args
}