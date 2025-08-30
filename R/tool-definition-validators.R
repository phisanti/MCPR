# Tool Definition Validators
# Comprehensive validators for ToolDef properties with full mcpr_type validation
# Validates tool arguments, names, descriptions, and mcpr_type structure compliance

#' Validate Tool Arguments
#'
#' @title Validate Tool Arguments
#' @description Validates arguments parameter for ToolDef construction ensuring proper list
#' structure and mcpr_type compliance. Validates each argument specification for proper
#' type definitions and structure according to the mcpr_type system.
#'
#' @param value The value to validate
#' @param property_name Name of the property for error messages
#' @return NULL if valid, throws error if invalid
#' @noRd
validate_tool_arguments <- function(value, property_name = "arguments") {
  # Must be a list
  if (!is.list(value)) {
    cli::cli_abort("Property {.field {property_name}} must be a list, not {obj_type_friendly(value)}")
  }

  # Check that if arguments exist, they are named
  if (length(value) > 0 && !rlang::is_named(value)) {
    cli::cli_abort("Property {.field {property_name}} must be a named list when non-empty")
  }

  # Validate each argument has proper mcpr_type structure
  for (arg_name in names(value)) {
    arg_spec <- value[[arg_name]]
    validate_mcpr_type_structure(arg_spec, paste0(property_name, "$", arg_name))
  }
}

#' Validate MCPR Type Structure
#'
#' @title Validate MCPR Type Structure
#' @description Validates that an argument specification follows proper mcpr_type structure.
#' Checks for required fields and valid type definitions according to the mcpr_type system.
#'
#' @param arg_spec The argument specification to validate
#' @param property_name Name of the property for error messages
#' @return NULL if valid, throws error if invalid
#' @noRd
validate_mcpr_type_structure <- function(arg_spec, property_name = "argument") {
  # Must be a proper mcpr_type object
  if (!inherits(arg_spec, "mcpr_type")) {
    cli::cli_abort("Property {.field {property_name}} must be an mcpr_type object created with type_*() functions")
  }

  # Validate mcpr_type object structure
  validate_mcpr_type_object(arg_spec, property_name)
}

#' Validate MCPR Type Object
#'
#' @title Validate MCPR Type Object
#' @description Validates mcpr_type objects for proper structure and required fields.
#'
#' @param mcpr_obj The mcpr_type object to validate
#' @param property_name Name of the property for error messages
#' @return NULL if valid, throws error if invalid
#' @noRd
validate_mcpr_type_object <- function(mcpr_obj, property_name = "argument") {
  # Check required type field
  if (is.null(mcpr_obj$type)) {
    cli::cli_abort("Property {.field {property_name}} mcpr_type object must have a 'type' field")
  }

  # Validate type field
  valid_types <- c("boolean", "integer", "number", "string", "enum", "array", "object")
  if (!mcpr_obj$type %in% valid_types) {
    cli::cli_abort("Property {.field {property_name}} has invalid type '{mcpr_obj$type}'. Valid types: {.val {valid_types}}")
  }

  # Type-specific validation
  switch(mcpr_obj$type,
    "enum" = {
      if (is.null(mcpr_obj$values) || !is.character(mcpr_obj$values)) {
        cli::cli_abort("Property {.field {property_name}} enum type must have 'values' field with character vector")
      }
    },
    "array" = {
      if (is.null(mcpr_obj$items)) {
        cli::cli_abort("Property {.field {property_name}} array type must have 'items' field")
      }
      # Recursively validate items type
      validate_mcpr_type_structure(mcpr_obj$items, paste0(property_name, "$items"))
    },
    "object" = {
      if (!is.null(mcpr_obj$properties)) {
        if (!is.list(mcpr_obj$properties)) {
          cli::cli_abort("Property {.field {property_name}} object type 'properties' must be a list")
        }
        # Recursively validate each property
        for (prop_name in names(mcpr_obj$properties)) {
          validate_mcpr_type_structure(mcpr_obj$properties[[prop_name]], paste0(property_name, "$properties$", prop_name))
        }
      }
    }
  )
}

#' Validate Tool Name
#'
#' @title Validate Tool Name
#' @description Validates name parameter for ToolDef ensuring single string format and character
#' compliance. Checks for alphanumeric characters with underscore and dash support through
#' regex pattern matching.
#'
#' @param value The value to validate
#' @param property_name Name of the property for error messages
#' @return NULL if valid, throws error if invalid
#' @noRd
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
#' and non-missing content.
#'
#' @param value The value to validate
#' @param property_name Name of the property for error messages
#' @return NULL if valid, throws error if invalid
#' @noRd
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
#'
#' @param value The value to validate
#' @param property_name Name of the property for error messages
#' @return NULL if valid, throws error if invalid
#' @noRd
validate_tool_fun <- function(value, property_name = "fun") {
  if (!is.function(value)) {
    cli::cli_abort("Property {.field {property_name}} must be a function, not {obj_type_friendly(value)}")
  }
}

#' Get Friendly Object Type Name
#'
#' @title Get Friendly Object Type Name
#' @description Provides user-friendly object type names for error messaging and debugging.
#'
#' @param x Object to get type name for
#' @return Character string with friendly type name
#' @noRd
obj_type_friendly <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  if (is.function(x)) {
    return("a function")
  }
  paste("a", class(x)[1])
}
