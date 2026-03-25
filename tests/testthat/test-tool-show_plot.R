# Unit Tests for Show Plot Tool
# Tests the show_plot tool with target-based routing (user/agent)

library(testthat)
library(ggplot2)
library(jsonlite)

resolve_show_plot_tool_path <- function() {
  candidates <- c(
    "inst/tool-show_plot.R",
    "../inst/tool-show_plot.R",
    "../../inst/tool-show_plot.R",
    system.file("tool-show_plot.R", package = "MCPR", mustWork = TRUE)
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop("Could not locate tool-show_plot.R for tests.")
  }
  existing[[1]]
}

load_show_plot_tool_env <- function() {
  env <- new.env(parent = asNamespace("MCPR"))
  source(resolve_show_plot_tool_path(), local = env)
  env
}

# Capture MCPR namespace references BEFORE list2env injection, which can
# shadow names and break MCPR::: resolution under test_check().
.set_mcpr_ctx <- MCPR:::set_mcpr_request_context
.clear_mcpr_ctx <- MCPR:::clear_mcpr_request_context

set_test_request_context <- function(mcp_apps_supported, interface = "mcp_app", client_name = "claude-ai") {
  .set_mcpr_ctx(list(
    mcp_apps_supported = mcp_apps_supported,
    mcpr_interface = interface,
    mcpr_client_name = client_name
  ))
  invisible(NULL)
}

with_graphics_cleanup <- function() {
  start_device <- grDevices::dev.cur()
  start_name <- names(start_device)

  function() {
    repeat {
      current_device <- grDevices::dev.cur()
      current_name <- names(current_device)
      if (identical(current_device, start_device) || identical(current_name, start_name) || current_device == 1) {
        break
      }
      try(grDevices::dev.off(), silent = TRUE)
    }

    current_name <- names(grDevices::dev.cur())
    if (current_name %in% c("httpgd", "unigd")) {
      try(grDevices::dev.off(), silent = TRUE)
    }
  }
}

# Source the show_plot tool into a dedicated environment so helper functions
# remain accessible in tests.
.tool_env <- load_show_plot_tool_env()

# Re-export all names into the test file scope for test_that blocks
list2env(as.list(.tool_env, all.names = TRUE), envir = environment())

# --- Input validation ---

test_that("show_plot validates expr", {
  expect_error(show_plot(123), "Expression must be a single character string")
  expect_error(show_plot(c("a", "b")), "Expression must be a single character string")
  expect_error(show_plot("   "), "Expression cannot be empty")
})

test_that("show_plot validates target", {
  expect_error(show_plot("plot(1)", target = "nobody"), "Target must be one of")
})

test_that("show_plot agent target validates format and dimensions", {
  expect_error(show_plot("plot(1)", target = "agent", format = "gif"), "Format must be one of")
  expect_error(show_plot("plot(1)", target = "agent", width = -100), "Width must be a positive number")
  expect_error(show_plot("plot(1)", target = "agent", height = 0), "Height must be a positive number")
})

# --- Defaults ---

test_that("show_plot defaults to target='user'", {
  formals_list <- formals(show_plot)
  expect_equal(formals_list$target, "user")
})

test_that("show_plot exposes tool-level MCP App annotations", {
  expect_equal(.show_plot_annotations$`_meta`$ui$resourceUri, "ui://mcpr/plots")
})

test_that("show_plot agent defaults match optimized values", {
  formals_list <- formals(show_plot)
  expect_equal(formals_list$width, 600)
  expect_equal(formals_list$height, 450)
  expect_equal(formals_list$format, "png")
  expect_equal(formals_list$token_limit, 25000)
  expect_equal(formals_list$warn_threshold, 20000)
})

# --- Channel detection ---

test_that("detect_output_channel returns valid channel", {
  channel <- detect_output_channel()
  expect_true(channel %in% c("mcp_app", "httpgd", "device", "file"))
})

test_that("detect_output_channel prefers httpgd when available", {
  skip_if_not(requireNamespace("httpgd", quietly = TRUE), "httpgd not available")
  channel <- detect_output_channel()
  expect_equal(channel, "httpgd")
})

# --- User target ---

test_that("show_plot user target returns text confirmation", {
  on.exit(with_graphics_cleanup()(), add = TRUE)
  result <- show_plot("plot(1:10)", target = "user")
  expect_equal(result$type, "text")
  expect_true(grepl("Plot displayed to user|Plot saved to file", result$content))
})

test_that("show_plot user target does not return base64 image", {
  on.exit(with_graphics_cleanup()(), add = TRUE)
  result <- show_plot("plot(1:10)", target = "user")
  expect_false(grepl("^data:", result$content))
  expect_false(result$type == "image")
})

test_that("show_plot user target handles ggplot objects", {
  on.exit(with_graphics_cleanup()(), add = TRUE)
  result <- show_plot("ggplot(mtcars, aes(mpg, hp)) + geom_point()", target = "user")
  expect_equal(result$type, "text")
})

test_that("show_plot user target reports errors", {
  expect_error(show_plot("nonexistent_var + 1", target = "user"), "Error displaying plot")
})

# --- httpgd channel ---

test_that("show_plot_via_httpgd returns url in confirmation", {
  skip_if_not(requireNamespace("httpgd", quietly = TRUE), "httpgd not available")
  on.exit(with_graphics_cleanup()(), add = TRUE)
  result <- show_plot_via_httpgd("plot(1:10)")
  expect_equal(result$type, "text")
  expect_true(grepl("httpgd", result$content))
  expect_true(grepl("http", result$content))
})

# --- File fallback ---

test_that("show_plot_via_file saves png and returns path", {
  on.exit(with_graphics_cleanup()(), add = TRUE)
  result <- show_plot_via_file("plot(1:10)")
  expect_equal(result$type, "text")
  expect_true(grepl("Plot saved to file", result$content))
  expect_true(grepl("\\.png", result$content))
})

# --- Optimization suggestions (shared helper) ---

test_that("generate_optimization_suggestions works", {
  suggestions <- generate_optimization_suggestions(1200, 900, 50000, 25000, "png")
  expect_true(length(suggestions) > 0)
  expect_true(any(grepl("400x300|600x450", suggestions)))
  expect_true(any(grepl("%", suggestions)))

  # Test format-specific suggestions
  suggestions_png <- generate_optimization_suggestions(600, 450, 22000, 25000, "png")
  expect_true(any(grepl("JPEG", suggestions_png)))

  # Test different reduction levels
  suggestions_minor <- generate_optimization_suggestions(700, 525, 27000, 25000, "png")
  expect_true(any(grepl("20%", suggestions_minor)))
})

test_that("render-first approach function signature", {
  # Verify that estimate_plot_tokens function no longer exists
  expect_false(exists("estimate_plot_tokens", mode = "function"))

  # Verify generate_optimization_suggestions has updated signature
  formals_suggestions <- formals(generate_optimization_suggestions)
  expect_true("current_format" %in% names(formals_suggestions))
  expect_equal(formals_suggestions$current_format, "png")
})

# --- MCP App channel ---

test_that("detect_output_channel returns mcp_app when flag is set", {
  set_test_request_context(TRUE)
  on.exit(.clear_mcpr_ctx(), add = TRUE)

  channel <- detect_output_channel()
  expect_equal(channel, "mcp_app")
})

test_that(".mcp_apps_supported prioritizes direct request context", {
  set_test_request_context(TRUE)
  on.exit(.clear_mcpr_ctx(), add = TRUE)

  expect_true(.mcp_apps_supported())
})

test_that("detect_output_channel ignores mcp_app flag when FALSE", {
  set_test_request_context(FALSE, interface = "cli")
  on.exit(.clear_mcpr_ctx(), add = TRUE)

  channel <- detect_output_channel()
  expect_true(channel %in% c("httpgd", "device", "file"))
})

test_that("show_plot_via_mcp_app returns content array with structuredContent image", {
  set_test_request_context(TRUE)
  on.exit(.clear_mcpr_ctx(), add = TRUE)
  on.exit(with_graphics_cleanup()(), add = TRUE)

  result <- show_plot_via_mcp_app("plot(1:10)")

  expect_equal(result$content[[1]]$type, "text")
  expect_equal(result$content[[1]]$text, "This tool call rendered a plot in the viewer.")
  expect_equal(result$content[[1]]$annotations$audience, list("assistant"))
  expect_equal(result$structuredContent$kind, "image")
  expect_equal(result$structuredContent$mimeType, "image/png")
  expect_true(nchar(result$structuredContent$data) > 0)
  expect_null(result$`_meta`)
})

test_that("show_plot_via_mcp_app handles ggplot objects", {
  on.exit(with_graphics_cleanup()(), add = TRUE)
  result <- show_plot_via_mcp_app("ggplot(mtcars, aes(mpg, hp)) + geom_point()")

  expect_equal(result$structuredContent$kind, "image")
  expect_equal(result$structuredContent$mimeType, "image/png")
  expect_null(result$`_meta`)
})

test_that("show_plot_via_mcp_app returns mcp_app channel result via show_plot", {
  set_test_request_context(TRUE)
  on.exit(.clear_mcpr_ctx(), add = TRUE)
  on.exit(with_graphics_cleanup()(), add = TRUE)

  result <- show_plot("plot(1:10)", target = "user")

  expect_equal(result$content[[1]]$text, "This tool call rendered a plot in the viewer.")
  expect_equal(result$structuredContent$kind, "image")
  expect_null(result$`_meta`)
})

test_that("show_plot_via_mcp_app routes plotly widgets to viewer payloads", {
  skip_if_not_installed("plotly")
  on.exit(with_graphics_cleanup()(), add = TRUE)

  result <- show_plot_via_mcp_app(
    "plotly::plot_ly(mtcars, x = ~mpg, y = ~hp, type = 'scatter', mode = 'markers')"
  )

  expect_equal(result$content[[1]]$text, "This tool call rendered an interactive widget in the viewer.")
  expect_equal(result$content[[1]]$annotations$audience, list("assistant"))
  expect_equal(result$structuredContent$kind, "plotly")
  expect_true(!is.null(result$structuredContent$spec))
  expect_null(result$`_meta`)
})

test_that("show_plot_via_mcp_app aborts on empty plot output", {
  on.exit(with_graphics_cleanup()(), add = TRUE)
  expect_error(
    show_plot_via_mcp_app("invisible(NULL)"),
    "empty image file"
  )
})

test_that("encode_tool_results preserves audience annotation from structuredContent result", {
  data <- list(id = 50)
  result <- list(
    content = list(list(
      type = "text",
      text = "Rendered.",
      annotations = list(audience = list("assistant"))
    )),
    structuredContent = list(kind = "image", mimeType = "image/png", data = "abc")
  )

  response <- MCPR:::encode_tool_results(data, result)

  expect_equal(response$result$content[[1]]$annotations$audience, list("assistant"))
})

# --- Wire-format integration: _meta propagation ---

test_that("tool_as_json propagates _meta.ui.resourceUri from annotations", {
  # Create a ToolDef with the same annotations as show_plot
  tool <- MCPR:::ToolDef$new(
    fun = function(expr) expr,
    name = "test_tool",
    description = "test",
    annotations = .show_plot_annotations
  )

  json <- MCPR:::tool_as_json(tool)

  expect_equal(json[["_meta"]]$ui$resourceUri, "ui://mcpr/plots")
  # Legacy flat key should also be present

  expect_equal(json[["_meta"]][["ui/resourceUri"]], "ui://mcpr/plots")
})

test_that("encode_tool_results passes structuredContent image alongside content array", {
  data <- list(id = 42)
  result <- list(
    content = list(list(type = "text", text = "This tool call rendered a plot in the viewer.")),
    structuredContent = list(
      kind = "image",
      mimeType = "image/png",
      data = "iVBORw0KGgo="
    )
  )

  response <- MCPR:::encode_tool_results(data, result)

  expect_equal(response$id, 42)
  expect_equal(response$result$structuredContent$kind, "image")
  expect_equal(response$result$structuredContent$data, "iVBORw0KGgo=")
  expect_equal(response$result$content[[1]]$type, "text")
  expect_equal(response$result$content[[1]]$text, "This tool call rendered a plot in the viewer.")
  expect_null(response$result[["_meta"]])
})

test_that("encode_tool_results passes structuredContent plotly alongside content array", {
  data <- list(id = 99)
  result <- list(
    content = list(list(type = "text", text = "This tool call rendered an interactive widget in the viewer.")),
    structuredContent = list(
      kind = "plotly",
      spec = list(data = list(), layout = list(), config = list())
    )
  )

  response <- MCPR:::encode_tool_results(data, result)

  expect_equal(response$id, 99)
  expect_equal(response$result$structuredContent$kind, "plotly")
  expect_true(!is.null(response$result$structuredContent$spec))
  expect_equal(
    response$result$content[[1]]$text,
    "This tool call rendered an interactive widget in the viewer."
  )
  expect_null(response$result[["_meta"]])
})

test_that("encode_tool_results defaults to assistant audience without _meta", {
  data <- list(id = 7)
  result <- list(
    type = "image",
    data = "iVBORw0KGgo=",
    mimeType = "image/png"
  )

  response <- MCPR:::encode_tool_results(data, result)

  expect_equal(response$result$content[[1]]$annotations$audience, list("assistant"))
  expect_null(response$result[["_meta"]])
})

# --- Agent target (graphics device tests) ---

test_that("show_plot agent target integration tests", {
  skip("Graphics device tests skipped in test environment")

  # These tests would verify:
  # - Actual plot creation works with target='agent'
  # - Response type is 'image' with base64 content
  # - Token count is accurate
  # - Warning system triggers at correct thresholds
  # - Metadata includes actual token counts
})
