# MCPR Package Configuration
# Package-level documentation and initialization for Model Context Protocol in R.
# Configures MCP server settings and provides package overview.

# nocov start

#' Model Context Protocol for R
#'
#' @title MCPR Package
#' @include aaa.R
#' @include utils.R
#' @include logger.R
#' @description Configures platform-specific socket URLs when package loads into R session
#'
#' @importFrom base64enc dataURI
#' @importFrom processx process
#' @importFrom promises as.promise
#' @importFrom roxygen2 parse_file
#'
#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  the$socket_url <- default_socket_url()

  # Cache Rscript path for daemon spawning (mirai pattern)
  the$rscript_path <- switch(Sys.info()[["sysname"]],
    Windows = file.path(R.home("bin"), "Rscript.exe"),
    file.path(R.home("bin"), "Rscript")
  )
}
# nocov end
