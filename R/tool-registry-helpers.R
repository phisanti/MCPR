#' Create a Tool from a Roxygen Block
#' @description Constructs a `ToolDef` from a parsed roxygen block and its function.
#' @param block A roxygen2 block object.
#' @param env The environment where the function is defined.
#' @param file_path The path of the file being parsed (for logging).
#' @return A `ToolDef` object or NULL on failure.
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
#' @description Extracts the function description from `@description` or `@intro` tags.
#' @param block A roxygen2 block object.
#' @return A character string with the description.
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

#' Convert Roxygen Params to MCPR Types
#' @description Converts a list of `@param` tags into a list of MCPR type definitions.
#' @param param_tags A list of `roxy_tag_param` objects.
#' @return A named list of MCPR type objects.
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