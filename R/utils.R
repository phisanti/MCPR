# General Utility Functions
# Common utility functions supporting MCPR package operations and workflows.
# Provides NULL handling, JSON conversion, validation, and helper functions.

#' Drop NULL Values from Vector or List
#'
#' @title Drop NULL Values from Vector or List
#' @description Removes all NULL elements from vector or list through logical filtering.
#' Provides clean data structure processing by eliminating NULL entries for consistent
#' data handling workflows. Maintains non-NULL elements while preserving original
#' data structure characteristics.
#'
#' @param x Vector or list to process
#' @return Vector or list with NULL values removed
drop_nulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

#' Create Named List
#'
#' @title Create Named List
#' @description Creates named list ensuring proper named structure even when empty.
#' Handles edge case of empty list creation to maintain named list properties for
#' consistent data structure handling. Provides reliable named list construction
#' for MCPR framework data processing requirements.
#'
#' @param ... Elements to include in named list
#' @return Named list with provided elements
named_list <- function(...) {
  res <- list(...)
  if (length(res) == 0) {
    # A way of creating an empty named list
    res <- list(a = 1)[0]
  }
  res
}

#' Convert R Object to JSON String
#'
#' @title Convert R Object to JSON String
#' @description Converts R object to JSON string format with automatic unboxing for scalar values.
#' Provides consistent JSON serialization interface for MCPR package data transmission.
#' Simplifies JSON conversion with sensible defaults for MCP protocol communication.
#'
#' @param x R object to convert to JSON
#' @param ... Additional arguments passed to jsonlite::toJSON
#' @return JSON string representation of R object
to_json <- function(x, ...) {
  jsonlite::toJSON(x, ..., auto_unbox = TRUE)
}

#' Log Output to File
#'
#' @title Log Output to File
#' @description Logs output to file for MCP client processes where stdout/stderr aren't visible.
#' Enables debugging and monitoring of MCP server processes through file-based logging.
#' Provides essential diagnostic capability for background process troubleshooting
#' and development workflows.
#'
#' @param x Content to log
#' @param ... Additional arguments passed to cat function
#' @param append Whether to append to existing log file (default: TRUE)
#' @return None (writes to log file)
logcat <- function(x, ..., append = TRUE) {
  log_file <- mcptools_log_file()
  cat(x, "\n", sep = "", append = append, file = log_file)
}

#' Get MCP Tools Log File Path
#'
#' @title Get MCP Tools Log File Path
#' @description Returns log file path from environment variable or creates temporary file.
#' Provides consistent log file location for MCP tools debugging and monitoring.
#' Enables configurable logging through MCPTOOLS_LOG_FILE environment variable
#' with fallback to temporary file creation.
#'
#' @return Character string with log file path
mcptools_log_file <- function() {
  Sys.getenv("MCPTOOLS_LOG_FILE", tempfile(fileext = ".txt"))
}

#' Check Session is Not Interactive
#'
#' @title Check Session is Not Interactive
#' @description Validates that current session is not interactive for server functions.
#' Prevents server operations in interactive contexts where they're inappropriate.
#' Provides clear error messaging for proper usage guidance and workflow enforcement
#' in MCP server deployment scenarios.
#'
#' @param call Calling environment for error reporting
#' @return None (throws error if interactive)
check_not_interactive <- function(call = rlang::caller_env()) {
  if (rlang::is_interactive()) {
    cli::cli_abort(
      c(
        "This function is not intended for interactive use.",
        "i" = "See {.help {.fn mcpr_server}} for instructions on configuring this
       function with applications"
      ),
      call = call
    )
  }
}

#' Remove Empty Elements from List
#'
#' @title Remove Empty Elements from List
#' @description Filters out empty elements from list based on length criterion.
#' Provides data cleaning utility for list processing workflows by removing
#' zero-length elements. Maintains non-empty elements for consistent data
#' structure handling in MCPR framework operations.
#'
#' @param .x List to process
#' @return List with empty elements removed
compact <- function(.x) {
  Filter(length, .x)
}

#' Compact List by Removing NULL Values
#'
#' @title Compact List by Removing NULL Values
#' @description Removes NULL values from list through negated NULL check filtering.
#' Provides clean data structure processing for consistent list handling workflows.
#' Maintains non-NULL elements while preserving list structure for MCPR framework
#' data processing requirements.
#'
#' @param x List to compact
#' @return List with NULL values removed
compact_list <- function(x) {
  Filter(Negate(is.null), x)
}


#' Infer Current IDE
#'
#' @title Infer Current IDE
#' @description Infers current IDE from command line arguments for environment detection.
#' Recognizes RStudio, Positron, and other IDE environments through command argument
#' analysis. Enables context-aware behavior adaptation based on development environment
#' for enhanced user experience in different IDE contexts.
#'
#' @return Character string with IDE name
infer_ide <- function() {
  first_cmd_arg <- commandArgs()[1]
  switch(
    first_cmd_arg,
    ark = "Positron",
    RStudio = "RStudio",
    first_cmd_arg
  )
}

#' Null Coalescing Operator
#'
#' @title Null Coalescing Operator
#' @description Returns left-hand side if not NULL, otherwise returns right-hand side value.
#' Provides convenient NULL value handling for default value assignment and conditional
#' logic. Enables clean code patterns for NULL checking and fallback value provision
#' throughout MCPR framework operations.
#'
#' @param x Left-hand side value to check
#' @param y Right-hand side value (used if x is NULL)
#' @return x if not NULL, otherwise y
#' @export
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Additional utility functions for MCPR migration

#' Check Object is Function
#'
#' @title Check Object is Function
#' @description Validates that object is a function with informative error messaging.
#' Provides type checking utility for function parameter validation in MCPR framework.
#' Ensures proper function types for tool definition and execution workflows through
#' comprehensive validation with clear error reporting.
#'
#' @param x Object to check for function type
#' @param arg Argument name for error messages
#' @param call Calling environment for error reporting
#' @return None (throws error if not function)
check_function <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.function(x)) {
    cli::cli_abort("{.arg {arg}} must be a function, not {.obj_type_friendly {x}}", call = call)
  }
}

#' Check Object is String
#'
#' @title Check Object is String
#' @description Validates that object is single non-NA string with optional NULL allowance.
#' Provides string type checking utility for parameter validation in MCPR framework.
#' Ensures proper string types for tool names, descriptions, and configuration values
#' through comprehensive validation with clear error reporting.
#'
#' @param x Object to check for string type
#' @param allow_null Whether to allow NULL values (default: FALSE)
#' @param arg Argument name for error messages
#' @param call Calling environment for error reporting
#' @return None (throws error if not valid string)
check_string <- function(x, allow_null = FALSE, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (allow_null && is.null(x)) return()
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single string, not {.obj_type_friendly {x}}", call = call)
  }
}

#' Check Object is Boolean
#'
#' @title Check Object is Boolean
#' @description Validates that object is single non-NA logical value with optional NULL allowance.
#' Provides boolean type checking utility for parameter validation in MCPR framework.
#' Ensures proper logical types for configuration flags and boolean parameters through
#' comprehensive validation with clear error reporting.
#'
#' @param x Object to check for boolean type
#' @param allow_null Whether to allow NULL values (default: FALSE)
#' @param arg Argument name for error messages
#' @param call Calling environment for error reporting
#' @return None (throws error if not valid boolean)
check_bool <- function(x, allow_null = FALSE, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (allow_null && is.null(x)) return()
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single logical value, not {.obj_type_friendly {x}}", call = call)
  }
}

#' Create JSON-RPC 2.0 Response Object
#'
#' @title Create JSON-RPC 2.0 Response Object
#' @description Creates properly formatted JSON-RPC 2.0 response object with result or error.
#' Handles protocol compliance for MCP communication through structured response creation.
#' Validates mutual exclusivity of result and error fields while maintaining protocol
#' standards for reliable client-server communication.
#'
#' @param id Request ID for response matching
#' @param result Success result of method execution (mutually exclusive with error)
#' @param error Error object if method execution failed (mutually exclusive with result)
#' @return List representing JSON-RPC 2.0 response
jsonrpc_response <- function(id, result = NULL, error = NULL) {
  if (!xor(is.null(result), is.null(error))) {
    warning("Either `result` or `error` must be provided, but not both.")
  }

  drop_nulls(list(
    jsonrpc = "2.0",
    id = id,
    result = result,
    error = error
  ))
}

# Mocking for testing
interactive <- NULL
basename <- NULL
getwd <- NULL
commandArgs <- NULL