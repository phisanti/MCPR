# Tool Registry Helper Functions
# Helper functions for tool discovery and roxygen2 parsing in the ToolRegistry system.
# Provides roxygen2 block processing, function extraction, and tool metadata conversion.

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
#' @noRd
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

  # Create the tool using new ToolDef system
  tryCatch(
    {
      tool(
        fun = func,
        name = func_name,
        description = description,
        arguments = mcpr_args
      )
    },
    error = function(e) {
      cli::cli_warn("Failed to create tool for {.fn {func_name}}: {conditionMessage(e)}")
      NULL
    }
  )
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
#' @noRd
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
#' @noRd
convert_to_schema <- function(param_tags) {
  mcpr_args <- list()

  for (param_tag in param_tags) {
    # Parse the val field which contains "param_name type description"
    val_str <- paste(param_tag$val, collapse = " ")
    val_parts <- trimws(strsplit(val_str, "\\s+", perl = TRUE)[[1]])

    if (length(val_parts) < 2) {
      next
    }

    param_name <- val_parts[1]
    type_and_desc <- paste(val_parts[-1], collapse = " ")

    # Extract type from beginning of type_and_desc
    type_pattern <- "^(character|string|numeric|number|integer|int|logical|boolean|bool|list|array)\\s+"
    type_match <- regexpr(type_pattern, type_and_desc, ignore.case = TRUE)

    if (type_match != -1) {
      type_str <- regmatches(type_and_desc, type_match)
      type_str <- trimws(gsub("\\s+$", "", type_str))
      param_desc <- sub(type_pattern, "", type_and_desc, ignore.case = TRUE)
    } else {
      type_str <- "string" # default
      param_desc <- type_and_desc
    }

    # Create proper mcpr_type objects directly
    mcpr_args[[param_name]] <- map_type_schema(type_str, description = param_desc, input_type = "definition")
  }

  mcpr_args
}
