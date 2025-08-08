# Tool Definition Validators
# Specialized validators for ToolDef properties to ensure proper tool construction

#' Validate Tool Arguments
#'
#' @title Validate Tool Arguments
#' @description Validates arguments parameter for ToolDef construction ensuring proper list
#' structure and type compliance. Checks for named list format when non-empty and validates
#' each argument as mcpr_type object. Provides comprehensive error messaging for tool
#' definition validation through structured argument inspection.
#'
#' @param value The value to validate
#' @param property_name Name of the property for error messages
#' @return NULL if valid, throws error if invalid
#' @export
validate_tool_arguments <- function(value, property_name = "arguments") {
  # Must be a list
  if (!is.list(value)) {
    cli::cli_abort("Property {.field {property_name}} must be a list, not {obj_type_friendly(value)}")
  }
  
  # Check that if arguments exist, they are named
  if (length(value) > 0 && !rlang::is_named(value)) {
    cli::cli_abort("Property {.field {property_name}} must be a named list when non-empty")
  }
  
  # Validate each argument is a valid type object
  for (i in seq_along(value)) {
    arg_name <- names(value)[i]
    arg <- value[[i]]
    if (!is.null(arg) && !inherits(arg, "mcpr_type")) {
      cli::cli_abort(
        "Property {.field {property_name}} element {.val {arg_name}} must be a type object or NULL, not {obj_type_friendly(arg)}"
      )
    }
  }
}

#' Validate Tool Name
#'
#' @title Validate Tool Name
#' @description Validates name parameter for ToolDef ensuring single string format and character
#' compliance. Checks for alphanumeric characters with underscore and dash support through
#' regex pattern matching. Prevents missing values and enforces naming conventions for
#' tool identification and MCP protocol compatibility.
#'
#' @param value The value to validate
#' @param property_name Name of the property for error messages
#' @return NULL if valid, throws error if invalid
#' @export
validate_tool_name <- function(value, property_name = "name") {
  if (length(value) != 1) {
    cli::cli_abort("Property {.field {property_name}} must be a single string, not {obj_type_friendly(value)}")
  } else if (is.na(value)) {
    cli::cli_abort("Property {.field {property_name}} must not be missing")
  }
  
  if (!grepl("^[a-zA-Z0-9_-]+$", value)) {
    cli::cli_abort("Property {.field {property_name}} must contain only letters, numbers, - and _, got {.val {value}}")
  }
}

#' Validate Tool Description
#'
#' @title Validate Tool Description
#' @description Validates description parameter for ToolDef ensuring single string format
#' and non-missing content. Enforces character type and length requirements for tool
#' documentation. Provides error handling for missing or invalid description values
#' to ensure proper tool metadata for MCP protocol communication.
#'
#' @param value The value to validate
#' @param property_name Name of the property for error messages
#' @return NULL if valid, throws error if invalid
#' @export
validate_tool_description <- function(value, property_name = "description") {
  if (!is.character(value) || length(value) != 1) {
    cli::cli_abort("Property {.field {property_name}} must be a single string, not {obj_type_friendly(value)}")
  } 
  
  if (is.na(value)) {
    cli::cli_abort("Property {.field {property_name}} must not be missing")
  }
}

#' Validate Tool Function
#'
#' @title Validate Tool Function
#' @description Validates function parameter for ToolDef ensuring callable function object.
#' Checks for proper function type to enable tool execution within MCP framework.
#' Provides type validation for executable tool implementation through function
#' object verification and error reporting.
#'
#' @param value The value to validate
#' @param property_name Name of the property for error messages
#' @return NULL if valid, throws error if invalid
#' @export
validate_tool_fun <- function(value, property_name = "fun") {
  if (!is.function(value)) {
    cli::cli_abort("Property {.field {property_name}} must be a function, not {obj_type_friendly(value)}")
  }
}

#' Get Friendly Object Type Name
#'
#' @title Get Friendly Object Type Name
#' @description Provides user-friendly object type names for error messaging and debugging.
#' Converts R object types to readable format for validation error reporting through
#' type inspection and string formatting. Enhances error message clarity for tool
#' definition validation feedback.
#'
#' @param x Object to get type name for
#' @return Character string with friendly type name
obj_type_friendly <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.function(x)) return("a function")
  paste("a", class(x)[1])
}