#' Create a Tool from a Roxygen Block
#' @description Constructs an `ellmer::tool` from a parsed roxygen block and its function.
#' @param block A roxygen2 block object.
#' @param env The environment where the function is defined.
#' @param file_path The path of the file being parsed (for logging).
#' @return An `ellmer::tool` object or NULL on failure.
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
  ellmer_args <- convert_to_schema(param_tags)
  
  # Create the tool
  tryCatch({
    tool_args <- c(
      list(.fun = func, .description = description, .name = func_name),
      ellmer_args
    )
    do.call(ellmer::tool, tool_args)
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

#' Convert Roxygen Params to Ellmer Types
#' @description Converts a list of `@param` tags into a list of `ellmer` type definitions.
#' @param param_tags A list of `roxy_tag_param` objects.
#' @return A named list of `ellmer` type objects.
convert_to_schema <- function(param_tags) {
  ellmer_args <- list()
  
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
      type_str <- "string"  # default
      param_desc <- type_and_desc
    }
    
    ellmer_args[[param_name]] <- map_type_schema(type_str, param_desc)
  }
  
  ellmer_args
}

#' Convert Type String to Ellmer Type
#' @description Converts a string (e.g., "numeric") to an `ellmer` type object.
#' @param type_str The type string (e.g., "character", "numeric").
#' @param description The parameter description.
#' @return An `ellmer` type object.
map_type_schema <- function(type_str, description) {
  switch(tolower(type_str),
    "character" = , "string" = ellmer::type_string(description = description),
    "numeric" = , "number" = {
      # If description mentions "vector" or "array", create an array type
      if (grepl("vector|array", description, ignore.case = TRUE)) {
        ellmer::type_array(description = description, items = ellmer::type_number())
      } else {
        ellmer::type_number(description = description)
      }
    },
    "integer" = , "int" = ellmer::type_integer(description = description),
    "logical" = , "boolean" = , "bool" = ellmer::type_boolean(description = description),
    "list" = , "array" = ellmer::type_array(description = description, items = ellmer::type_string()),
    ellmer::type_string(description = description)
  )
}