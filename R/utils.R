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
  if (interactive()) {
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

# Mocking for testing
interactive <- NULL
basename <- NULL
getwd <- NULL
commandArgs <- NULL