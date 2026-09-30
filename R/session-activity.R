# Session Activity Log
# Unified timeline of top-level R commands from the user and code run by the agent.
# Fed by a task callback in joined user sessions and by execute_r_code; read via view("activity").

#' Maximum number of activity entries retained
#' @noRd
ACTIVITY_LOG_MAX <- 200L

#' Name of the task callback installed in joined user sessions
#' @noRd
ACTIVITY_CALLBACK_NAME <- "MCPR.activity"

#' Record one activity entry
#'
#' @param source "user" (typed at the console) or "agent" (execute_r_code)
#' @param code Character code that ran
#' @param ok Logical; FALSE when the code raised an error
#' @param value_class Class of the result, or NULL
#' @param plots Integer plot store ids drawn or updated by this code
#' @param error Error message when `ok` is FALSE
#' @return The entry, invisibly
#' @noRd
activity_record <- function(source, code, ok = TRUE, value_class = NULL,
                            plots = integer(), error = NULL) {
  code <- paste(code, collapse = "\n")
  if (nchar(code) > 500L) {
    code <- paste0(substr(code, 1L, 497L), "...")
  }
  entry <- list(
    time = Sys.time(),
    source = source,
    code = code,
    ok = isTRUE(ok),
    value_class = if (length(value_class)) value_class[[1L]] else NULL,
    plots = as.integer(plots),
    error = error
  )
  log <- c(the$activity_log, list(entry))
  if (length(log) > ACTIVITY_LOG_MAX) {
    log <- utils::tail(log, ACTIVITY_LOG_MAX)
  }
  the$activity_log <- log
  invisible(entry)
}

#' Clear the activity log
#' @noRd
activity_reset <- function() {
  the$activity_log <- NULL
  invisible(NULL)
}

#' Task callback recording each completed top-level user command
#'
#' Runs only for console commands; tool calls run inside event-loop callbacks
#' and never reach it. Also snapshots any plot the command drew.
#'
#' @return TRUE so the callback stays registered
#' @noRd
activity_task_callback <- function(expr, value, ok, visible) {
  tryCatch({
    plot_id <- plot_store_snapshot("user")
    activity_record(
      source = "user",
      code = deparse(expr, width.cutoff = 500L, nlines = 20L),
      ok = ok,
      value_class = if (isTRUE(visible)) class(value) else NULL,
      plots = if (is.null(plot_id)) integer() else plot_id
    )
  }, error = function(e) NULL)
  TRUE
}

#' Start recording the user's console activity and plots
#'
#' Idempotent: an existing MCPR callback is replaced, never stacked.
#' @noRd
activity_tracking_start <- function() {
  activity_tracking_stop()
  plot_hooks_install()
  addTaskCallback(activity_task_callback, name = ACTIVITY_CALLBACK_NAME)
  invisible(NULL)
}

#' Stop recording console activity and remove MCPR's plot hooks
#' @noRd
activity_tracking_stop <- function() {
  if (ACTIVITY_CALLBACK_NAME %in% getTaskCallbackNames()) {
    removeTaskCallback(ACTIVITY_CALLBACK_NAME)
  }
  plot_hooks_uninstall()
  invisible(NULL)
}

#' Format the activity timeline for the view tool
#'
#' @param max_lines Maximum number of entries to show (most recent last)
#' @return Character vector of lines
#' @noRd
view_activity <- function(max_lines = 100) {
  log <- the$activity_log
  if (!length(log)) {
    return(c(
      "Session Activity",
      "No activity recorded yet.",
      "User console commands are recorded while this session is joined via mcpr_session_start()."
    ))
  }
  log <- utils::tail(log, max_lines)
  lines <- vapply(log, function(e) {
    code <- gsub("\n", "\n              ", e$code, fixed = TRUE)
    head <- sprintf("[%s] %-5s > %s", format(e$time, "%H:%M:%S"), e$source, code)
    details <- character()
    if (!e$ok) {
      details <- c(details, paste0("error: ", e$error %||% "(unknown)"))
    } else if (!is.null(e$value_class)) {
      details <- c(details, paste0("-> <", e$value_class, ">"))
    }
    if (length(e$plots)) {
      details <- c(details, paste0("plots: ", paste0("plot:", e$plots, collapse = ", ")))
    }
    if (length(details)) {
      head <- paste0(head, "\n              ", paste(details, collapse = " | "))
    }
    head
  }, character(1))
  c(
    sprintf("Session Activity (last %d of %d entries)", length(log), length(the$activity_log)),
    "Sources: user = typed at the R console; agent = execute_r_code.",
    "",
    lines
  )
}
