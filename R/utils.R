# General utility functions for the MCPR package

# Given a vector or list, drop all the NULL items in it
drop_nulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

# Create a named list, ensuring that it's a named list, even if empty.
named_list <- function(...) {
  res <- list(...)
  if (length(res) == 0) {
    # A way of creating an empty named list
    res <- list(a = 1)[0]
  }
  res
}

# Convert an R object to a JSON string.
to_json <- function(x, ...) {
  jsonlite::toJSON(x, ..., auto_unbox = TRUE)
}

# This process will be launched by the MCP client, so stdout/stderr aren't
# visible. This function will log output to the `logfile` so that you can view
# it.
logcat <- function(x, ..., append = TRUE) {
  log_file <- mcptools_log_file()
  cat(x, "\n", sep = "", append = append, file = log_file)
}

mcptools_log_file <- function() {
  Sys.getenv("MCPTOOLS_LOG_FILE", tempfile(fileext = ".txt"))
}

# Check if the current session is interactive.
check_not_interactive <- function(call = rlang::caller_env()) {
  if (rlang::is_interactive()) {
    cli::cli_abort(
      c(
        "This function is not intended for interactive use.",
        "i" = "See {.help {.fn mcp_server}} for instructions on configuring this
       function with applications"
      ),
      call = call
    )
  }
}

# Filter out empty elements from a list.
compact <- function(.x) {
  Filter(length, .x)
}

# Infer the current IDE.
infer_ide <- function() {
  first_cmd_arg <- commandArgs()[1]
  switch(
    first_cmd_arg,
    ark = "Positron",
    RStudio = "RStudio",
    first_cmd_arg
  )
}

#' Null coalescing operator
#'
#' Returns the left-hand side if it is not NULL, otherwise returns the right-hand side.
#' @param x Left-hand side value
#' @param y Right-hand side value (used if x is NULL)
#' @return x if not NULL, otherwise y
#' @export
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Additional utility functions for MCPR migration

#' Check if object is a function
#' @param x Object to check
#' @param arg Argument name for error messages
#' @param call Calling environment
check_function <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.function(x)) {
    cli::cli_abort("{.arg {arg}} must be a function, not {.obj_type_friendly {x}}", call = call)
  }
}

#' Check if object is a string
#' @param x Object to check
#' @param allow_null Whether to allow NULL values
#' @param arg Argument name for error messages
#' @param call Calling environment
check_string <- function(x, allow_null = FALSE, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (allow_null && is.null(x)) return()
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single string, not {.obj_type_friendly {x}}", call = call)
  }
}

#' Check if object is a boolean
#' @param x Object to check
#' @param allow_null Whether to allow NULL values
#' @param arg Argument name for error messages
#' @param call Calling environment
check_bool <- function(x, allow_null = FALSE, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (allow_null && is.null(x)) return()
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single logical value, not {.obj_type_friendly {x}}", call = call)
  }
}

#' Convert JSON types to R types
#' @param args List of arguments to convert
#' @return List with converted types
convert_json_types <- function(args) {
  # Simple conversion for now - could be expanded
  args
}

#' Convert arguments to JSON schema
#' @param arguments List of arguments
#' @return JSON schema representation
convert_arguments_to_schema <- function(arguments) {
  if (length(arguments) == 0) {
    return(list(type = "object", properties = list()))
  }
  
  properties <- list()
  for (name in names(arguments)) {
    arg <- arguments[[name]]
    
    # Use enhanced type conversion for better R object support
    if (inherits(arg, "mcpr_type")) {
      # Keep existing behavior for mcpr_type objects
      properties[[name]] <- type_to_schema(arg)
    } else {
      # Use to_mcp_json for other R objects to infer schema
      json_representation <- to_mcp_json(arg)
      properties[[name]] <- .infer_schema_from_json(json_representation)
    }
  }
  
  list(
    type = "object",
    properties = properties
  )
}

# Helper function to infer JSON schema from to_mcp_json output
.infer_schema_from_json <- function(json_obj) {
  if (is.null(json_obj)) {
    return(list(type = "null"))
  }
  
  # Check for MCP type markers
  if (is.list(json_obj) && !is.null(json_obj$`_mcp_type`)) {
    mcp_type <- json_obj$`_mcp_type`
    
    switch(mcp_type,
      "Date" = list(
        type = "object",
        properties = list(
          values = list(type = "string", format = "date"),
          `_mcp_type` = list(type = "string", enum = list("Date"))
        ),
        required = list("values", "_mcp_type")
      ),
      "POSIXct" = list(
        type = "object", 
        properties = list(
          values = list(type = "string", format = "date-time"),
          timezone = list(type = "string"),
          `_mcp_type` = list(type = "string", enum = list("POSIXct"))
        ),
        required = list("values", "_mcp_type")
      ),
      "factor" = list(
        type = "object",
        properties = list(
          levels = list(type = "array", items = list(type = "string")),
          values = list(type = "array", items = list(type = "integer")),
          `_mcp_type` = list(type = "string", enum = list("factor"))
        ),
        required = list("levels", "values", "_mcp_type")
      ),
      "matrix" = list(
        type = "object",
        properties = list(
          data = list(type = "array"),
          dim = list(type = "array", items = list(type = "integer")),
          `_mcp_type` = list(type = "string", enum = list("matrix"))
        ),
        required = list("data", "dim", "_mcp_type")
      ),
      "complex" = list(
        type = "object",
        properties = list(
          real = list(type = "number"),
          imaginary = list(type = "number"),
          `_mcp_type` = list(type = "string", enum = list("complex"))
        ),
        required = list("real", "imaginary", "_mcp_type")
      ),
      "raw" = list(
        type = "object",
        properties = list(
          data = list(type = "string", format = "base64"),
          `_mcp_type` = list(type = "string", enum = list("raw"))
        ),
        required = list("data", "_mcp_type")
      ),
      # Default for unknown MCP types
      list(
        type = "object",
        properties = list(
          `_mcp_type` = list(type = "string", enum = list(mcp_type))
        ),
        additionalProperties = TRUE
      )
    )
  } else {
    # Infer basic JSON schema from the converted object
    .infer_basic_schema(json_obj)
  }
}

# Helper function to infer basic JSON schema
.infer_basic_schema <- function(obj) {
  if (is.null(obj)) {
    list(type = "null")
  } else if (is.logical(obj)) {
    list(type = "boolean")
  } else if (is.numeric(obj)) {
    if (all(obj == as.integer(obj), na.rm = TRUE)) {
      list(type = "integer")
    } else {
      list(type = "number")
    }
  } else if (is.character(obj)) {
    list(type = "string")
  } else if (is.list(obj)) {
    if (length(obj) == 0) {
      list(type = "array", items = list())
    } else if (is.null(names(obj))) {
      # Unnamed list - treat as array
      list(type = "array", items = list(type = "string"))
    } else {
      # Named list - treat as object
      list(type = "object", additionalProperties = TRUE)
    }
  } else {
    # Fallback
    list(type = "string")
  }
}


#' Convert type object to schema
#' @param type Type object
#' @return Schema representation
type_to_schema <- function(type) {
  if (is.null(type)) {
    return(list(type = "string"))
  }
  
  # Handle our simple mcpr_type objects
  if (inherits(type, "mcpr_type")) {
    schema <- list(type = type$type)
    if (!is.null(type$description)) {
      schema$description <- type$description
    }
    if (type$type == "array" && !is.null(type$items)) {
      schema$items <- type_to_schema(type$items)
    }
    if (type$type == "enum" && !is.null(type$values)) {
      schema$enum <- type$values
    }
    return(schema)
  }
  
  # Default fallback
  list(type = "string")
}


#' Compact list - remove NULL values
#' @param x List to compact
#' @return List with NULL values removed
compact_list <- function(x) {
  Filter(Negate(is.null), x)
}

# Mocking for testing
interactive <- NULL
basename <- NULL
getwd <- NULL
commandArgs <- NULL