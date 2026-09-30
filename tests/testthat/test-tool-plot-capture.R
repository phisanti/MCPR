# Tool Tests for Ambient Plot Capture
# execute_r_code reports plot:N refs and logs activity; show_plot resolves refs at any size;
# view(what = "activity") renders the unified user/agent timeline.

resolve_tool_path <- function(file) {
  candidates <- c(
    file.path("inst", file),
    file.path("..", "inst", file),
    file.path("..", "..", "inst", file),
    system.file(file, package = "MCPR")
  )
  existing <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(existing)) stop("Could not locate ", file, " for tests.")
  existing[[1]]
}

.capture_tools <- new.env(parent = asNamespace("MCPR"))
for (.f in c("tool-execute_r_code.R", "tool-show_plot.R", "tool-view.R")) {
  source(resolve_tool_path(.f), local = .capture_tools)
}
.the <- MCPR:::the

local_capture_sandbox <- function(env = parent.frame()) {
  old_devices <- grDevices::dev.list()
  old_log <- .the$activity_log
  MCPR:::plot_store_reset()
  MCPR:::activity_reset()
  withr::local_options(device = MCPR:::open_capture_device, .local_envir = env)
  withr::defer({
    for (d in rev(setdiff(grDevices::dev.list(), old_devices))) try(grDevices::dev.off(d), silent = TRUE)
    MCPR:::plot_hooks_uninstall()
    MCPR:::plot_store_reset()
    .the$activity_log <- old_log
  }, envir = env)
}

test_that("execute_r_code reports recorded plots and logs agent activity", {
  local_capture_sandbox()
  out <- .capture_tools$execute_r_code("plot(cars); plot(1:3)")
  expect_match(out, "Plots recorded: plot:1, plot:2")
  expect_match(out, "show_plot\\(plot = \"plot:2\"")

  entry <- .the$activity_log[[length(.the$activity_log)]]
  expect_equal(entry$source, "agent")
  expect_equal(entry$plots, 1:2)
  expect_true(entry$ok)
})

test_that("execute_r_code without plots adds no plot note; errors are logged", {
  local_capture_sandbox()
  out <- .capture_tools$execute_r_code("x <- 1 + 1")
  expect_false(grepl("Plots recorded", out))

  .capture_tools$execute_r_code("stop('kaboom')")
  entry <- .the$activity_log[[length(.the$activity_log)]]
  expect_false(entry$ok)
  expect_equal(entry$error, "kaboom")
})

test_that("show_plot renders plot:N for the agent at the requested size", {
  local_capture_sandbox()
  .capture_tools$execute_r_code("plot(cars)")
  res <- .capture_tools$show_plot("plot:1", target = "agent", width = 500, height = 300)
  expect_equal(res$type, "image")
  expect_equal(res$metadata$dimensions, "500x300")

  last <- .capture_tools$show_plot("plot:last", target = "agent")
  expect_equal(last$metadata$dimensions, "600x450")
})

test_that("show_plot reports unknown plot refs with the available ids", {
  local_capture_sandbox()
  expect_error(.capture_tools$show_plot("plot:1", target = "agent"), "No plots have been recorded")
  .capture_tools$execute_r_code("plot(cars)")
  expect_error(.capture_tools$show_plot("plot:9", target = "agent"), "plot:1")
})

test_that("view(what = 'activity') shows the timeline", {
  local_capture_sandbox()
  .capture_tools$execute_r_code("plot(cars)")
  out <- .capture_tools$view("activity")
  expect_match(out, "View completed: activity")
  expect_match(out, "agent > plot\\(cars\\)")
  expect_match(out, "plots: plot:1")
})
