# View Tool
# Main dispatcher for viewing R session state, terminal output, and workspace information.
# Provides focused inspection of specific aspects of the current R environment.

#' View R session information and workspace state
#'
#' @description View specific aspects of your R session including session info, terminal output, errors, packages, workspace files, search path, warnings, last computed value, and help documentation. This tool provides focused inspection of different components of your R environment. Use this for system and session state. For deep analysis of specific R objects (data frames, functions, models, lists), use inspect_object instead.
#' @param what character What to view. Options: "session" (R objects and session info), "terminal" (recent commands and output), "last_error" (most recent error details), "installed_packages" (installed R packages), "workspace" (current directory structure), "search_path" (package search path), "warnings" (recent warnings), "last_value" (inspect last computed R result), "help" (parsed help page, requires topic parameter), "vignette" (package vignette source, requires topic parameter)
#' @param max_lines integer Maximum number of lines to display in output (default: 100). Controls output length for terminal history, error traces, package lists, file listings, etc.
#' @param topic character Topic to look up. Required when what="help" or what="vignette". For what="help", supports "function_name" or "package::function_name" format. For what="vignette", supports three depths: "pkg" (index of all vignettes in the package), "pkg::name" (full raw source of one vignette), or "pkg::name#Section" (a single section of one vignette).
#' @keywords mcpr_tool
#' @return Formatted information about the requested aspect of the R session
view <- function(what = "session", max_lines = 100, topic = NULL) {
  # Input validation and argument matching
  if (!is.character(what) || length(what) != 1) {
    cli::cli_abort("'what' must be a single character string")
  }

  if (nchar(trimws(what)) == 0) {
    cli::cli_abort("'what' cannot be empty")
  }

  valid_options <- c(
    "session", "terminal", "last_error", "installed_packages",
    "workspace", "search_path", "warnings", "last_value", "help", "vignette"
  )

  what <- match.arg(what, valid_options)

  if (!is.numeric(max_lines) || length(max_lines) != 1 || max_lines <= 0) {
    cli::cli_abort("'max_lines' must be a positive integer")
  }

  max_lines <- as.integer(max_lines)

  # Validate topic parameter for help/vignette
  if (what == "help" || what == "vignette") {
    if (is.null(topic) || !is.character(topic) || length(topic) != 1 || nchar(trimws(topic)) == 0) {
      cli::cli_abort("'topic' is required when what='{what}'. Provide a function or package::function name.")
    }
    topic <- trimws(topic)
  }

  # Dispatch to appropriate view function using package namespace
  result <- switch(what,
    "session" = MCPR:::view_session(max_lines),
    "terminal" = MCPR:::view_terminal(max_lines),
    "last_error" = MCPR:::view_last_error(max_lines),
    "installed_packages" = MCPR:::view_installed_packages(max_lines),
    "workspace" = MCPR:::view_workspace(max_lines),
    "search_path" = MCPR:::view_search_path(max_lines),
    "warnings" = MCPR:::view_warnings(max_lines),
    "last_value" = MCPR:::view_last_value(max_lines),
    "help" = MCPR:::view_help(topic, max_lines),
    "vignette" = MCPR:::view_vignette(topic, max_lines),
    cli::cli_abort("Unexpected error in view dispatch", .internal = TRUE)
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
