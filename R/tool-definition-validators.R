# Tool Definition Validators
# Specialized validators for ToolDef properties to ensure proper tool construction

#' Validate arguments parameter for ToolDef
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

#' Validate name parameter for ToolDef
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

#' Validate description parameter for ToolDef
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

#' Validate function parameter for ToolDef
#' @param value The value to validate
#' @param property_name Name of the property for error messages
#' @return NULL if valid, throws error if invalid
#' @export
validate_tool_fun <- function(value, property_name = "fun") {
  if (!is.function(value)) {
    cli::cli_abort("Property {.field {property_name}} must be a function, not {obj_type_friendly(value)}")
  }
}

# Helper function to get friendly object type name
obj_type_friendly <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.function(x)) return("a function")
  paste("a", class(x)[1])
}