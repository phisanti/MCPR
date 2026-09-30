# Unit Tests for Plot Store, Owned Device, and Activity Log
# Covers page recording semantics, plot:N resolution, resize replay, owned-device reuse,
# the activity timeline, and the owned-session loop servicing httpgd redraws.

# Alias for assignments; `.the$x <- v` is not a valid replacement target
.the <- MCPR:::the

local_plot_sandbox <- function(env = parent.frame()) {
  old_state <- MCPR:::the$plot_state
  old_display <- MCPR:::the$display_device
  old_log <- MCPR:::the$activity_log
  old_devices <- grDevices::dev.list()
  MCPR:::plot_store_reset()
  MCPR:::activity_reset()
  withr::defer({
    new_devices <- setdiff(grDevices::dev.list(), old_devices)
    for (d in rev(new_devices)) try(grDevices::dev.off(d), silent = TRUE)
    MCPR:::plot_hooks_uninstall()
    MCPR:::plot_store_reset()
    .the$plot_state <- old_state
    .the$display_device <- old_display
    .the$activity_log <- old_log
  }, envir = env)
  withr::local_options(device = MCPR:::open_capture_device, .local_envir = env)
}

run_captured <- function(expr) {
  token <- MCPR:::plot_capture_begin("agent")
  force(expr)
  MCPR:::plot_capture_end(token)
}

# ---- recording semantics ----

test_that("one base plot becomes one entry and later additions update it", {
  local_plot_sandbox()
  ids1 <- run_captured(plot(cars))
  ids2 <- run_captured(abline(h = 50))

  expect_equal(ids1, 1L)
  expect_equal(ids2, 1L)
  expect_equal(nrow(MCPR:::plot_store_list()), 1L)
})

test_that("each new page gets its own id; multi-panel pages stay one entry", {
  local_plot_sandbox()
  ids <- run_captured({
    op <- par(mfrow = c(2, 2))
    plot(lm(dist ~ speed, data = cars))
    par(op)
    plot(1:3)
    plot(3:1)
  })
  expect_equal(ids, 1:3)
})

test_that("grid and ggplot pages are recorded", {
  skip_if_not_installed("ggplot2")
  local_plot_sandbox()
  ids <- run_captured({
    print(ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point())
    print(ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) + ggplot2::geom_point())
  })
  expect_equal(ids, 1:2)
})

test_that("code without plots records nothing", {
  local_plot_sandbox()
  expect_length(run_captured(1 + 1), 0L)
})

test_that("plots drawn before an error are still recorded", {
  local_plot_sandbox()
  token <- MCPR:::plot_capture_begin("agent")
  try({ plot(cars); stop("boom") }, silent = TRUE)
  expect_equal(MCPR:::plot_capture_end(token), 1L)
})

test_that("store is bounded", {
  local_plot_sandbox()
  ids <- run_captured(for (i in seq_len(MCPR:::PLOT_STORE_MAX + 5L)) plot(i))
  listing <- MCPR:::plot_store_list()
  expect_equal(nrow(listing), MCPR:::PLOT_STORE_MAX)
  expect_equal(max(listing$id), MCPR:::PLOT_STORE_MAX + 5L)
})

test_that("hooks install once and uninstall only MCPR's own hook", {
  local_plot_sandbox()
  other <- function() invisible(NULL)
  setHook("before.plot.new", other, "append")
  withr::defer(setHook("before.plot.new", Filter(function(h) !identical(h, other), getHook("before.plot.new")), "replace"))

  MCPR:::plot_hooks_install()
  MCPR:::plot_hooks_install()
  hooks <- getHook("before.plot.new")
  expect_equal(sum(vapply(hooks, identical, logical(1), MCPR:::plot_hook_before_base)), 1L)

  MCPR:::plot_hooks_uninstall()
  hooks <- getHook("before.plot.new")
  expect_false(any(vapply(hooks, identical, logical(1), MCPR:::plot_hook_before_base)))
  expect_true(any(vapply(hooks, identical, logical(1), other)))
})

# ---- references and resize ----

test_that("plot:N and plot:last resolve; unknown refs return NULL", {
  local_plot_sandbox()
  run_captured({ plot(1:3); plot(3:1) })
  expect_equal(MCPR:::plot_store_get("plot:1")$id, 1L)
  expect_equal(MCPR:::plot_store_get("plot:last")$id, 2L)
  expect_null(MCPR:::plot_store_get("plot:99"))
  expect_true(grepl(MCPR:::PLOT_REF_PATTERN, "plot:last"))
  expect_false(grepl(MCPR:::PLOT_REF_PATTERN, "plot:x"))
})

test_that("recorded plots replay for the agent at the requested size", {
  local_plot_sandbox()
  run_captured(plot(cars))
  entry <- MCPR:::plot_store_get("plot:last")

  small <- MCPR::channel_plot(entry$record, target = "agent", width = 300, height = 200)
  large <- MCPR::channel_plot(entry$record, target = "agent", width = 1000, height = 700)

  expect_equal(small$metadata$dimensions, "300x200")
  expect_equal(large$metadata$dimensions, "1000x700")
  expect_gt(nchar(large$data), nchar(small$data))
})

test_that("channel_plot validates width and height", {
  local_plot_sandbox()
  run_captured(plot(cars))
  rec <- MCPR:::plot_store_get("plot:last")$record
  expect_error(MCPR::channel_plot(rec, target = "agent", width = 10), "between 100 and 4000")
  expect_error(MCPR::channel_plot(rec, target = "agent", height = "big"), "between 100 and 4000")
})

test_that("format_plot_capture names the refs", {
  expect_null(MCPR:::format_plot_capture(integer()))
  note <- MCPR:::format_plot_capture(c(2L, 3L))
  expect_match(note, "plot:2, plot:3")
  expect_match(note, "show_plot\\(plot = \"plot:3\"")
})

# ---- owned device ----

test_that("owned session without httpgd uses an invisible device and never retries", {
  local_plot_sandbox()
  .the$plot_state <- list(mode = "none", device = NULL, url = NULL, browser_opened = FALSE)

  s1 <- MCPR:::owned_plot_device()
  s2 <- MCPR:::owned_plot_device()
  expect_equal(s1$mode, "none")
  expect_equal(s1$device, s2$device)
  expect_true(MCPR:::owned_display_disabled())
  expect_equal(MCPR:::detect_local_device(), "none")

  res <- MCPR::channel_plot(MCPR::capture_plot(plot(cars)), target = "user")
  expect_match(res$content, "Plot display skipped")
})

test_that("owned httpgd device and URL are reused across plots", {
  skip_if_not_installed("httpgd")
  local_plot_sandbox()
  MCPR:::configure_owned_graphics()
  withr::local_options(device = function(...) MCPR:::owned_plot_device())

  plot(cars)
  first <- MCPR:::the$plot_state
  expect_equal(first$mode, "httpgd")

  grDevices::pdf(NULL)
  other <- grDevices::dev.cur()
  d1 <- MCPR:::activate_display_device()
  d2 <- MCPR:::activate_display_device()

  expect_equal(unname(grDevices::dev.cur()), first$device)
  expect_equal(d1$url, first$url)
  expect_equal(d2$url, first$url)
  expect_true(d1$open_browser)
  expect_false(d2$open_browser)
  grDevices::dev.off(other)
})

test_that("closed owned device is recreated", {
  skip_if_not_installed("httpgd")
  local_plot_sandbox()
  MCPR:::configure_owned_graphics()
  first <- MCPR:::owned_plot_device()
  grDevices::dev.off(first$device)
  second <- MCPR:::owned_plot_device()
  expect_equal(second$mode, "httpgd")
  expect_true(second$device %in% grDevices::dev.list())
})

test_that("showing an older plot to the user never overwrites a stored page", {
  local_plot_sandbox()
  run_captured(plot(1:3, main = "ONE"))
  run_captured(hist(rnorm(50), main = "TWO"))
  before <- MCPR:::plot_store_get("plot:2")$record

  MCPR:::render_static_plot(MCPR:::plot_store_get("plot:1")$record, "device")
  ids <- run_captured(x <- 1)

  expect_length(ids, 0L)
  expect_identical(MCPR:::plot_store_get("plot:2")$record[[1L]], before[[1L]])
  expect_equal(nrow(MCPR:::plot_store_list()), 2L)
})

test_that("httpgd keeps every replayed page renderable", {
  skip_if_not_installed("httpgd")
  local_plot_sandbox()
  run_captured(plot(1:3, main = "ONE"))
  run_captured(plot(3:1, main = "TWO"))
  run_captured(plot(2:4, main = "THREE"))

  MCPR:::start_httpgd_device()
  dev <- grDevices::dev.cur()
  for (ref in c("plot:1", "plot:2", "plot:3")) {
    MCPR:::draw_on_display_device(MCPR:::plot_store_get(ref)$record)
  }

  expect_equal(unigd::ugd_state(which = dev)$hsize, 3L)
  for (i in 1:3) {
    svg <- unigd::ugd_render(page = i, width = 400, height = 300, as = "svg", which = dev)
    expect_match(svg, c("ONE", "TWO", "THREE")[[i]], fixed = TRUE)
  }
  expect_equal(nrow(MCPR:::plot_store_list()), 3L)
})

test_that("owned_display_disabled checks without opening a device", {
  skip_if_not_installed("httpgd")
  local_plot_sandbox()
  MCPR:::configure_owned_graphics()
  before <- grDevices::dev.list()

  expect_false(MCPR:::owned_display_disabled())
  expect_identical(grDevices::dev.list(), before)
  expect_equal(MCPR:::the$plot_state$mode, "uninitialized")
})

test_that("a joined session falls back when httpgd fails to start", {
  local_plot_sandbox()
  .the$plot_state <- NULL
  .the$display_device <- NULL
  local_mocked_bindings(
    start_httpgd_device = function() cli::cli_abort("port in use"),
    .package = "MCPR"
  )

  expect_null(MCPR:::activate_display_device())
  res <- MCPR:::render_static_plot(MCPR::capture_plot(plot(cars)), "httpgd")
  expect_equal(res$channel, "none")
})

test_that("is_current_page detects the page already on the device", {
  local_plot_sandbox()
  run_captured(plot(cars))
  rec <- MCPR:::plot_store_get("plot:last")$record
  expect_true(MCPR:::is_current_page(rec))
  plot(1:3)
  expect_false(MCPR:::is_current_page(rec))
  expect_false(MCPR:::is_current_page(MCPR::capture_plot(plot(cars))))
})

# ---- activity log ----

test_that("activity log records entries, truncates code, and is bounded", {
  local_plot_sandbox()
  MCPR:::activity_record("agent", "x <- 1", ok = TRUE, value_class = "numeric")
  MCPR:::activity_record("user", strrep("a", 800), ok = FALSE, error = "bad")
  log <- MCPR:::the$activity_log
  expect_length(log, 2L)
  expect_equal(nchar(log[[2]]$code), 500L)

  for (i in seq_len(MCPR:::ACTIVITY_LOG_MAX)) MCPR:::activity_record("agent", "1")
  expect_length(MCPR:::the$activity_log, MCPR:::ACTIVITY_LOG_MAX)
})

test_that("task callback records the user's command and plot", {
  local_plot_sandbox()
  MCPR:::plot_hooks_install()
  plot(cars)
  expect_true(MCPR:::activity_task_callback(quote(plot(cars)), NULL, TRUE, FALSE))
  entry <- MCPR:::the$activity_log[[1]]
  expect_equal(entry$source, "user")
  expect_equal(entry$code, "plot(cars)")
  expect_equal(entry$plots, 1L)
  expect_equal(MCPR:::plot_store_get("plot:1")$source, "user")
})

test_that("activity tracking installs one named callback and removes it", {
  local_plot_sandbox()
  MCPR:::activity_tracking_start()
  MCPR:::activity_tracking_start()
  expect_equal(sum(getTaskCallbackNames() == MCPR:::ACTIVITY_CALLBACK_NAME), 1L)
  MCPR:::activity_tracking_stop()
  expect_false(MCPR:::ACTIVITY_CALLBACK_NAME %in% getTaskCallbackNames())
})

test_that("view_activity formats the timeline", {
  local_plot_sandbox()
  expect_match(paste(MCPR:::view_activity(), collapse = "\n"), "No activity recorded")
  MCPR:::activity_record("user", "df <- head(mtcars)", value_class = "data.frame")
  MCPR:::activity_record("agent", "plot(cars)", plots = 4L)
  MCPR:::activity_record("agent", "stop('x')", ok = FALSE, error = "x")
  out <- paste(MCPR:::view_activity(), collapse = "\n")
  expect_match(out, "user  > df <- head\\(mtcars\\)")
  expect_match(out, "-> <data.frame>")
  expect_match(out, "plots: plot:4")
  expect_match(out, "error: x")
})

# ---- owned-session loop services httpgd redraws ----

test_that("owned session loop serves sized httpgd renders", {
  skip_on_cran()
  skip_if_not_installed("httpgd")
  skip_if_not_installed("processx")
  skip_on_os("windows")

  url_file <- tempfile(fileext = ".txt")
  session_id <- 900L + sample.int(99L, 1L)
  script <- sprintf(
    paste(
      "later::later(function() { plot(cars);",
      "writeLines(httpgd::hgd_url(endpoint = 'plot', width = 1234, height = 777), '%s') }, 0.2);",
      "MCPR::mcp_session(session_id = %dL, timeout_seconds = 60, daemon = TRUE)"
    ),
    url_file, session_id
  )
  proc <- processx::process$new(
    file.path(R.home("bin"), "Rscript"), c("-e", script),
    stdout = "|", stderr = "|",
    env = c("current", MCPR_PARENT_PID = as.character(Sys.getpid()))
  )
  withr::defer(if (proc$is_alive()) proc$kill())

  deadline <- Sys.time() + 20
  while (!file.exists(url_file) && Sys.time() < deadline && proc$is_alive()) Sys.sleep(0.1)
  skip_if_not(file.exists(url_file), "owned session did not start")
  url <- readLines(url_file)

  for (w in c(1234L, 640L)) {
    sized <- sub("width=1234", paste0("width=", w), url, fixed = TRUE)
    svg_file <- tempfile(fileext = ".svg")
    res <- processx::run("curl", c("-s", "-m", "5", "-o", svg_file, "-w", "%{http_code}", sized),
                         error_on_status = FALSE)
    expect_equal(res$stdout, "200", info = paste("width", w))
    svg <- paste(readLines(svg_file, warn = FALSE), collapse = "")
    expect_match(svg, sprintf('width="%d.00"', w), info = paste("width", w))
  }
})
