# nocov start
#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  the$socket_url <- switch(
    Sys.info()[["sysname"]],
    Linux = "abstract://MCPR-socket",
    Windows = "ipc://MCPR-socket",
    "ipc:///tmp/MCPR-socket"
  )
}
# nocov end
