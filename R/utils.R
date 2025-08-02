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