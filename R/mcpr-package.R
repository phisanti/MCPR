# nocov start

# MCPR Package Configuration
# Package-level documentation and initialization for Model Context Protocol in R.
# Configures MCP server settings and provides package overview.

#' Model Context Protocol for R
#'
#' @title MCPR Package
#' @description Configures platform-specific socket URLs when package loads into R session
#'
#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  the$socket_url <- switch(Sys.info()[["sysname"]],
    Linux = "abstract://MCPR-socket",
    Windows = "ipc://MCPR-socket",
    "ipc:///tmp/MCPR-socket"
  )

  # Initialize diagnostic logger for execution tracing
  tryCatch(
    {
      initialize_diagnostic_logger()
    },
    error = function(e) {
      # Silently continue if logger initialization fails
    }
  )
}
# nocov end
