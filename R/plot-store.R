# Plot Store and Owned Graphics Device
# Records every plot page a session draws as a replayable recordedplot, addressable as plot:N.
# Also owns one persistent httpgd device per session; invisible pdf(NULL) fallback without httpgd.

#' Maximum number of plot pages retained by the store
#' @noRd
PLOT_STORE_MAX <- 50L

#' Pattern matching a plot store reference such as `plot:3` or `plot:last`
#' @noRd
PLOT_REF_PATTERN <- "^plot:([0-9]+|last)$"

# ---- store ----

#' Get the plot store environment, creating it on first use
#'
#' Entries are kept in insertion order; `open_id` names the page that further
#' drawing on `open_device` updates in place rather than appending.
#' `page_device` is the device on which a page has started since the last
#' page boundary; display lists without a started page (e.g. only a `par()`
#' call) are never recorded.
#'
#' @return Environment with `entries`, `seq`, `open_id`, `open_device`,
#'   `last_dl`, `page_device`
#' @noRd
plot_store <- function() {
  if (is.null(the$plot_store)) {
    store <- new.env(parent = emptyenv())
    store$entries <- list()
    store$seq <- 0L
    store$open_id <- NULL
    store$open_device <- NULL
    store$last_dl <- NULL
    store$page_device <- NULL
    the$plot_store <- store
  }
  the$plot_store
}

#' Clear every stored plot page
#' @noRd
plot_store_reset <- function() {
  the$plot_store <- NULL
  invisible(NULL)
}

#' Resolve a plot reference to a store entry
#'
#' @param ref `plot:N`, `plot:last`, or an integer id
#' @return The entry list (`id`, `record`, `source`, `time`, `device`) or NULL
#' @noRd
plot_store_get <- function(ref) {
  store <- plot_store()
  if (!length(store$entries)) {
    return(NULL)
  }
  if (is.character(ref)) {
    key <- sub(PLOT_REF_PATTERN, "\\1", ref)
    if (identical(key, "last")) {
      return(store$entries[[length(store$entries)]])
    }
    ref <- suppressWarnings(as.integer(key))
  }
  store$entries[[as.character(ref)]]
}

#' List stored plot pages without their display lists
#' @return data.frame with `id`, `source`, `time`, `device`
#' @noRd
plot_store_list <- function() {
  entries <- plot_store()$entries
  if (!length(entries)) {
    return(data.frame(
      id = integer(), source = character(), time = character(),
      device = character(), stringsAsFactors = FALSE
    ))
  }
  data.frame(
    id = vapply(entries, function(e) e$id, integer(1)),
    source = vapply(entries, function(e) e$source, character(1)),
    time = vapply(entries, function(e) format(e$time, "%H:%M:%S"), character(1)),
    device = vapply(entries, function(e) e$device, character(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' Ids recorded during a capture that are still retained
#'
#' @param touched Integer vector of ids recorded while the code ran
#' @return Sorted unique ids that still exist in the store
#' @noRd
plot_store_touched <- function(touched) {
  ids <- sort(unique(as.integer(touched)))
  ids[as.character(ids) %in% names(plot_store()$entries)]
}

# ---- recording ----

#' Snapshot the current device page into the store
#'
#' Records the active device's display list. A page that is still open on the
#' same device is updated in place, so `plot(x); abline(...)` stays one entry.
#' Devices without a display list (e.g. file devices) are skipped silently.
#'
#' @param source "agent" or "user"
#' @return The id recorded or updated, or NULL when nothing changed
#' @noRd
plot_store_snapshot <- function(source = the$plot_capture_source %||% "user") {
  dev <- grDevices::dev.cur()
  if (dev == 1L || plot_recording_suspended()) {
    return(NULL)
  }
  store <- plot_store()
  if (!identical(store$page_device, unname(dev))) {
    return(NULL)
  }
  record <- tryCatch(grDevices::recordPlot(), error = function(e) NULL)
  if (is.null(record) || !length(record[[1L]])) {
    return(NULL)
  }

  if (!identical(store$open_device, unname(dev))) {
    store$open_id <- NULL
    store$last_dl <- NULL
  }
  if (!is.null(store$open_id) && identical(record[[1L]], store$last_dl)) {
    return(NULL)
  }

  id <- store$open_id
  if (is.null(id) || is.null(store$entries[[as.character(id)]])) {
    store$seq <- store$seq + 1L
    id <- store$seq
  }
  store$entries[[as.character(id)]] <- list(
    id = id,
    record = record,
    source = source,
    time = Sys.time(),
    device = names(dev) %||% "unknown"
  )
  if (length(store$entries) > PLOT_STORE_MAX) {
    store$entries <- utils::tail(store$entries, PLOT_STORE_MAX)
  }
  store$open_id <- id
  store$open_device <- unname(dev)
  store$last_dl <- record[[1L]]
  if (!is.null(the$plot_capture_touched)) {
    the$plot_capture_touched <- c(the$plot_capture_touched, id)
  }
  id
}

#' Close the open page so the next drawing starts a new store entry
#' @noRd
plot_store_close_page <- function() {
  store <- plot_store()
  store$open_id <- NULL
  store$last_dl <- NULL
  store$page_device <- NULL
  invisible(NULL)
}

#' Evaluate an internal render without recording it
#'
#' MCPR's own temporary devices (agent PNGs, MCP App images) must not be
#' recorded or disturb page tracking of the session's real device.
#'
#' @param expr Expression to evaluate
#' @return The value of `expr`
#' @noRd
without_plot_recording <- function(expr) {
  old <- the$plot_recording_suspended
  the$plot_recording_suspended <- TRUE
  on.exit(the$plot_recording_suspended <- old, add = TRUE)
  expr
}

#' Whether recording hooks should currently do nothing
#' @noRd
plot_recording_suspended <- function() {
  isTRUE(the$plot_recording_suspended)
}

#' plot.new / grid.newpage hook: a page has started on the current device
#' @noRd
plot_hook_page_started <- function() {
  if (plot_recording_suspended()) {
    return(invisible(NULL))
  }
  store <- plot_store()
  store$page_device <- unname(grDevices::dev.cur())
  invisible(NULL)
}

#' MCPR's graphics hooks as hook name -> function
#' @noRd
plot_hook_table <- function() {
  list(
    before.plot.new = plot_hook_before_base,
    before.grid.newpage = plot_hook_before_grid,
    plot.new = plot_hook_page_started,
    grid.newpage = plot_hook_page_started
  )
}

#' before.plot.new hook: finish the previous page when a new one starts
#'
#' Multi-panel layouts (`par(mfrow)`) call plot.new per panel but only start a
#' new page when `par("page")` is TRUE; those panels stay one entry.
#' @noRd
plot_hook_before_base <- function() {
  if (plot_recording_suspended()) {
    return(invisible(NULL))
  }
  tryCatch({
    if (grDevices::dev.cur() != 1L && isTRUE(graphics::par("page"))) {
      plot_store_snapshot()
      plot_store_close_page()
    }
  }, error = function(e) NULL)
  invisible(NULL)
}

#' before.grid.newpage hook: finish the previous page when grid starts a new one
#'
#' Graphics hooks run inside try(); failures are swallowed here so recording
#' can never print errors into the user's console or break their plot.
#' @noRd
plot_hook_before_grid <- function() {
  if (plot_recording_suspended()) {
    return(invisible(NULL))
  }
  tryCatch({
    if (grDevices::dev.cur() != 1L) {
      plot_store_snapshot()
      plot_store_close_page()
    }
  }, error = function(e) NULL)
  invisible(NULL)
}

#' Install the page-boundary hooks once
#'
#' Hooks are appended, never replacing hooks installed by other code, and are
#' tracked by identity so uninstalling removes only MCPR's own callbacks.
#' @noRd
plot_hooks_install <- function() {
  hooks <- plot_hook_table()
  for (name in names(hooks)) {
    fn <- hooks[[name]]
    current <- getHook(name)
    if (!any(vapply(current, identical, logical(1), fn))) {
      setHook(name, fn, action = "append")
    }
  }
  invisible(NULL)
}

#' Remove MCPR's page-boundary hooks, keeping hooks from other code
#' @noRd
plot_hooks_uninstall <- function() {
  hooks <- plot_hook_table()
  for (name in names(hooks)) {
    fn <- hooks[[name]]
    current <- getHook(name)
    kept <- Filter(function(h) !identical(h, fn), current)
    if (length(kept) != length(current)) {
      setHook(name, if (length(kept)) kept else NULL, action = "replace")
    }
  }
  invisible(NULL)
}

#' Start capturing every plot page drawn until plot_capture_end()
#'
#' Used by execute_r_code. In a non-interactive process with no device policy
#' of its own, plots go to an invisible display-list device so they can be
#' recorded instead of being written to Rplots.pdf.
#'
#' @param source "agent" or "user"
#' @return A capture token for plot_capture_end()
#' @noRd
plot_capture_begin <- function(source = "agent") {
  plot_hooks_install()
  token <- list(
    source = the$plot_capture_source,
    touched = the$plot_capture_touched,
    options = NULL
  )
  the$plot_capture_source <- source
  the$plot_capture_touched <- integer()
  if (!interactive() && is.null(the$plot_state)) {
    token$options <- options(device = open_capture_device)
  }
  token
}

#' Finish a capture: snapshot the last page and restore prior capture state
#'
#' @param token Value returned by plot_capture_begin()
#' @return Integer ids recorded or updated while capturing
#' @noRd
plot_capture_end <- function(token) {
  on.exit({
    if (!is.null(token$options)) options(token$options)
    the$plot_capture_source <- token$source
    the$plot_capture_touched <- token$touched
  }, add = TRUE)
  plot_store_snapshot(the$plot_capture_source)
  plot_store_touched(the$plot_capture_touched)
}

#' Open an invisible device that keeps a display list for recording
#' @noRd
open_capture_device <- function(...) {
  grDevices::pdf(file = NULL, width = 7, height = 7, bg = "white")
  grDevices::dev.control(displaylist = "enable")
  invisible(grDevices::dev.cur())
}

#' Describe recorded plot ids for a tool response
#'
#' @param ids Integer ids from plot_capture_end()
#' @return Character scalar, or NULL when no plots were recorded
#' @noRd
format_plot_capture <- function(ids) {
  if (!length(ids)) {
    return(NULL)
  }
  refs <- paste0("plot:", ids)
  paste0(
    "Plots recorded: ", paste(refs, collapse = ", "), "\n",
    "Show with show_plot(plot = \"", refs[[length(refs)]],
    "\", target = \"agent\" or \"user\"); optional width/height re-render at any size."
  )
}

# ---- owned device ----

#' Mark this process as an MCPR-owned session with its own graphics policy
#'
#' Routes the default device to the owned device so direct calls such as
#' `plot(cars)` land in the same httpgd viewer history (or the invisible
#' fallback) instead of Rplots.pdf, and installs the page-boundary hooks.
#' @noRd
configure_owned_graphics <- function() {
  the$plot_state <- list(
    mode = "uninitialized", device = NULL, url = NULL, browser_opened = FALSE
  )
  options(device = function(...) owned_plot_device())
  plot_hooks_install()
  invisible(NULL)
}

#' Activate the owned plot device, creating it once
#'
#' With httpgd: one device and URL for the life of the session, recreated only
#' if it was actually closed. Without httpgd (or if it fails to start): an
#' invisible display-list device, and httpgd is never retried.
#'
#' @return The plot state list (mode, device, url, browser_opened)
#' @noRd
owned_plot_device <- function() {
  state <- the$plot_state %||% list(
    mode = "uninitialized", device = NULL, url = NULL, browser_opened = FALSE
  )
  devices <- grDevices::dev.list()

  if (!is.null(state$device) && state$device %in% devices) {
    grDevices::dev.set(state$device)
    return(state)
  }

  if (!identical(state$mode, "none") && requireNamespace("httpgd", quietly = TRUE)) {
    started <- tryCatch({
      httpgd::hgd(silent = TRUE)
      TRUE
    }, error = function(e) FALSE)
    if (started) {
      state$mode <- "httpgd"
      state$device <- unname(grDevices::dev.cur())
      state$url <- httpgd::hgd_url()
      state$browser_opened <- FALSE
      the$plot_state <- state
      return(state)
    }
  }

  state$mode <- "none"
  state$device <- open_capture_device()
  state$url <- NULL
  the$plot_state <- state
  state
}

#' Activate a persistent httpgd device for user-facing display
#'
#' Reuses, in order: the device MCPR owns, an httpgd device the user already
#' has active, or a new device that MCPR then owns. The browser opens only the
#' first time a given device is shown.
#'
#' @return list(url, open_browser) where open_browser is TRUE once per device,
#'   or NULL in an owned session whose display is disabled
#' @noRd
activate_display_device <- function() {
  if (!is.null(the$plot_state)) {
    state <- owned_plot_device()
    if (!identical(state$mode, "httpgd")) {
      return(NULL)
    }
    open_browser <- !isTRUE(state$browser_opened)
    state$browser_opened <- TRUE
    the$plot_state <- state
    return(list(url = state$url, open_browser = open_browser))
  }

  if (names(grDevices::dev.cur()) %in% c("httpgd", "unigd")) {
    return(list(url = httpgd::hgd_url(), open_browser = FALSE))
  }

  display <- the$display_device
  if (!is.null(display) && display$device %in% grDevices::dev.list()) {
    grDevices::dev.set(display$device)
    return(list(url = display$url, open_browser = FALSE))
  }

  httpgd::hgd(silent = TRUE)
  the$display_device <- list(
    device = unname(grDevices::dev.cur()),
    url = httpgd::hgd_url()
  )
  list(url = the$display_device$url, open_browser = TRUE)
}

#' Whether this process is an MCPR-owned session with no static display
#' @noRd
owned_display_disabled <- function() {
  state <- the$plot_state
  if (is.null(state)) {
    return(FALSE)
  }
  if (identical(state$mode, "uninitialized")) {
    state <- owned_plot_device()
  }
  identical(state$mode, "none")
}

#' Validate an optional render dimension in pixels
#' @noRd
validate_plot_dimension <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 100 || x > 4000) {
    cli::cli_abort("{.arg {name}} must be a single number between 100 and 4000 pixels.")
  }
  as.integer(round(x))
}
