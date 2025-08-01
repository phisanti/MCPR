# Validation functions for ToolDef properties
# These functions provide comprehensive validation equivalent to the original S7 property validators

# Helper function to get friendly object type name (equivalent to obj_type_friendly)
obj_type_friendly <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.function(x)) return("a function")
  paste("a", class(x)[1])
}

# Helper function equivalent to names2 (ensures all elements have names)
names2 <- function(x) {
  names <- names(x)
  if (is.null(names)) {
    rep("", length(x))
  } else {
    names[is.na(names)] <- ""
    names
  }
}

#' Validate string property (equivalent to prop_string)
#' @param value The value to validate
#' @param property_name Name of the property for error messages
#' @param allow_null Whether to allow NULL values
#' @param allow_na Whether to allow NA values
#' @return NULL if valid, throws error if invalid
#' @export
validate_prop_string <- function(value, property_name = "string", allow_null = FALSE, allow_na = FALSE) {
  if (allow_null && is.null(value)) {
    return()
  }
  
  if (length(value) != 1) {
    cli::cli_abort("Property {.field {property_name}} must be a single string, not {obj_type_friendly(value)}")
  } else if (!allow_na && is.na(value)) {
    cli::cli_abort("Property {.field {property_name}} must not be missing")
  }
}

#' Validate boolean property (equivalent to prop_bool)
#' @param value The value to validate
#' @param property_name Name of the property for error messages
#' @param allow_null Whether to allow NULL values
#' @param allow_na Whether to allow NA values
#' @return NULL if valid, throws error if invalid
#' @export
validate_prop_bool <- function(value, property_name = "bool", allow_null = FALSE, allow_na = FALSE) {
  if (allow_null && is.null(value)) {
    return()
  }
  
  if (length(value) != 1) {
    if (allow_na) {
      cli::cli_abort("Property {.field {property_name}} must be a single TRUE or FALSE, not {obj_type_friendly(value)}")
    } else {
      cli::cli_abort("Property {.field {property_name}} must be a single TRUE, FALSE or NA, not {obj_type_friendly(value)}")
    }
  } else if (!allow_na && is.na(value)) {
    cli::cli_abort("Property {.field {property_name}} must be a TRUE or FALSE, not NA")
  }
}

#' Validate list of specific objects (equivalent to prop_list_of)
#' @param value The value to validate
#' @param class_name Expected class name for list elements
#' @param property_name Name of the property for error messages
#' @param names_requirement One of "any", "all", or "none" for naming requirements
#' @return NULL if valid, throws error if invalid
#' @export
validate_prop_list_of <- function(value, class_name, property_name = "list", names_requirement = "any") {
  if (!is.list(value)) {
    cli::cli_abort("Property {.field {property_name}} must be a list, not {obj_type_friendly(value)}")
  }
  
  for (i in seq_along(value)) {
    val <- value[[i]]
    if (!inherits(val, class_name)) {
      cli::cli_abort(
        "Property {.field {property_name}} must be a list of <{class_name}>s. Element {i} is {obj_type_friendly(val)}"
      )
    }
  }
  
  if (names_requirement == "all" && any(names2(value) == "")) {
    cli::cli_abort("Property {.field {property_name}} must be a named list")
  } else if (names_requirement == "none" && any(names2(value) != "")) {
    cli::cli_abort("Property {.field {property_name}} must be an unnamed list")
  }
}

#' Validate whole number property (equivalent to prop_number_whole)
#' @param value The value to validate
#' @param property_name Name of the property for error messages
#' @param min Minimum allowed value
#' @param max Maximum allowed value
#' @param allow_null Whether to allow NULL values
#' @param allow_na Whether to allow NA values
#' @return NULL if valid, throws error if invalid
#' @export
validate_prop_number_whole <- function(value, property_name = "number", min = NULL, max = NULL, allow_null = FALSE, allow_na = FALSE) {
  if (allow_null && is.null(value)) {
    return()
  }
  
  if (length(value) != 1) {
    cli::cli_abort("Property {.field {property_name}} must be a whole number, not {obj_type_friendly(value)}")
  } else if (!is.numeric(value)) {
    cli::cli_abort("Property {.field {property_name}} must be a whole number, not {obj_type_friendly(value)}")
  } else if (value != trunc(value)) {
    cli::cli_abort("Property {.field {property_name}} must be a whole number, not {obj_type_friendly(value)}")
  } else if (!is.null(min) && value < min) {
    cli::cli_abort("Property {.field {property_name}} must be at least {min}, not {value}")
  } else if (!is.null(max) && value > max) {
    cli::cli_abort("Property {.field {property_name}} must be at most {max}, not {value}")
  } else if (!allow_na && is.na(value)) {
    cli::cli_abort("Property {.field {property_name}} must not be missing")
  }
}

# ToolDef-specific validators using the generic prop validators

#' Validate function parameter for ToolDef
#' @param value The value to validate
#' @param property_name Name of the property for error messages
#' @return NULL if valid, throws error if invalid
validate_tool_fun <- function(value, property_name = "fun") {
  if (!is.function(value)) {
    cli::cli_abort("Property {.field {property_name}} must be a function, not {obj_type_friendly(value)}")
  }
}

#' Validate name parameter for ToolDef
#' @param value The value to validate
#' @param property_name Name of the property for error messages
#' @return NULL if valid, throws error if invalid
validate_tool_name <- function(value, property_name = "name") {
  validate_prop_string(value, property_name, allow_null = FALSE, allow_na = FALSE)
  if (!grepl("^[a-zA-Z0-9_-]+$", value)) {
    cli::cli_abort("Property {.field {property_name}} must contain only letters, numbers, - and _, got {.val {value}}")
  }
}

#' Validate description parameter for ToolDef
#' @param value The value to validate
#' @param property_name Name of the property for error messages
#' @return NULL if valid, throws error if invalid
validate_tool_description <- function(value, property_name = "description") {
  validate_prop_string(value, property_name, allow_null = FALSE, allow_na = FALSE)
}

#' Validate arguments parameter for ToolDef (equivalent to S7 TypeObject)
#' @param value The value to validate
#' @param property_name Name of the property for error messages
#' @return NULL if valid, throws error if invalid
validate_tool_arguments <- function(value, property_name = "arguments") {
  # Must be a list (equivalent to S7 TypeObject requirement)
  if (!is.list(value)) {
    cli::cli_abort("Property {.field {property_name}} must be a list, not {obj_type_friendly(value)}")
  }
  
  # Check that if arguments exist, they are named (S7 TypeObject is typically named)
  if (length(value) > 0 && !rlang::is_named(value)) {
    cli::cli_abort("Property {.field {property_name}} must be a named list when non-empty")
  }
  
  # Validate each argument is a valid type object (allowing NULL for optional args)
  # This matches the original S7 TypeObject behavior
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

#' Validate convert parameter for ToolDef
#' @param value The value to validate
#' @param property_name Name of the property for error messages
#' @return NULL if valid, throws error if invalid
validate_tool_convert <- function(value, property_name = "convert") {
  validate_prop_bool(value, property_name, allow_null = FALSE, allow_na = FALSE)
}

#' Validate annotations parameter for ToolDef (equivalent to S7 class_list)
#' @param value The value to validate
#' @param property_name Name of the property for error messages
#' @return NULL if valid, throws error if invalid
validate_tool_annotations <- function(value, property_name = "annotations") {
  # Must be a list (equivalent to S7 class_list requirement)
  if (!is.list(value)) {
    cli::cli_abort("Property {.field {property_name}} must be a list, not {obj_type_friendly(value)}")
  }
  
  # Additional validation to ensure list elements are appropriate annotation types
  # This provides more comprehensive validation than the original S7 class_list
  if (length(value) > 0) {
    for (i in seq_along(value)) {
      element_name <- names(value)[i] %||% paste0("element_", i)
      element <- value[[i]]
      
      # Allow basic R types for annotations (string, logical, numeric, list)
      if (!is.null(element) && 
          !is.character(element) && 
          !is.logical(element) && 
          !is.numeric(element) && 
          !is.list(element)) {
        cli::cli_abort(
          "Property {.field {property_name}} element {.val {element_name}} must be a basic R type (character, logical, numeric, or list), not {obj_type_friendly(element)}"
        )
      }
    }
  }
}