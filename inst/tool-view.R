# View Tool
# Main dispatcher for viewing R session state, terminal output, and workspace information.
# Provides focused inspection of specific aspects of the current R environment.

#* @mcp_tool
#' View R session information and workspace state
#'
#' @description View specific aspects of your R session including session info, terminal output, errors, packages, workspace files, search path, and warnings. This tool provides focused inspection of different components of your R environment. Use this to check session state, see recent commands, examine errors, review installed packages, browse workspace files, check the search path, or view recent warnings.
#' @param what character What to view. Options: "session" (R objects and session info), "terminal" (recent commands and output), "last_error" (most recent error details), "installed_packages" (installed R packages), "workspace" (current directory structure), "search_path" (package search path), "warnings" (recent warnings)
#' @param max_lines integer Maximum number of lines to display in output (default: 100). Controls output length for terminal history, error traces, package lists, file listings, etc.
#' @keywords mcpr_tool
#' @return Formatted information about the requested aspect of the R session
view <- function(what = "session", max_lines = 100) {
  # Input validation and argument matching
  if (!is.character(what) || length(what) != 1) {
    stop("'what' must be a single character string")
  }

  if (nchar(trimws(what)) == 0) {
    stop("'what' cannot be empty")
  }

  valid_options <- c(
    "session", "terminal", "last_error", "installed_packages",
    "workspace", "search_path", "warnings"
  )

  what <- match.arg(what, valid_options)

  if (!is.numeric(max_lines) || length(max_lines) != 1 || max_lines <= 0) {
    stop("'max_lines' must be a positive integer")
  }

  max_lines <- as.integer(max_lines)

  # Dispatch to appropriate view function using package namespace
  result <- switch(what,
    "session" = MCPR:::view_session(max_lines),
    "terminal" = MCPR:::view_terminal(max_lines),
    "last_error" = MCPR:::view_last_error(max_lines),
    "installed_packages" = MCPR:::view_installed_packages(max_lines),
    "workspace" = MCPR:::view_workspace(max_lines),
    "search_path" = MCPR:::view_search_path(max_lines),
    "warnings" = MCPR:::view_warnings(max_lines),
    stop("Unexpected error in view dispatch")
  )

  # Format final response
  if (is.character(result) && length(result) > 0) {
    paste0("View completed: ", what, "\n\n", paste(result, collapse = "\n"))
  } else {
    paste0("View completed, but no information available for: ", what)
  }
}

#' @export
view <- view
