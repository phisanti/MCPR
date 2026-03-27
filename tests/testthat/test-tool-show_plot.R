# Unit Tests for Show Plot Tool
# Tests the show_plot tool with new variable-name-based interface and channel_plot delegation

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

test_that("show_plot validates plot argument type", {
  expect_error(show_plot(123), "`plot` must be a non-empty variable name string")
  expect_error(show_plot(c("a", "b")), "`plot` must be a non-empty variable name string")
  expect_error(show_plot("   "), "`plot` must be a non-empty variable name string")
})

test_that("show_plot validates target", {
  # Create a valid plot variable first
  .GlobalEnv$.test_plot_obj <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  on.exit(rm(".test_plot_obj", envir = .GlobalEnv), add = TRUE)

  expect_error(show_plot(".test_plot_obj", target = "nobody"), "Target must be one of")
})

test_that("show_plot returns guidance when passed an expression string", {
  result <- show_plot("ggplot(mtcars, aes(x, y))")
  expect_equal(result$type, "text")
  expect_true(grepl("variable name", result$content))
  expect_true(grepl("execute_r_code", result$content))
  expect_true(grepl("capture_plot", result$content))
})

test_that("show_plot returns guidance for various expression patterns", {
  # Contains +
  r1 <- show_plot("p + geom_point()")
  expect_equal(r1$type, "text")
  expect_true(grepl("variable name", r1$content))

  # Contains <-
  r2 <- show_plot("p <- ggplot(mtcars)")
  expect_equal(r2$type, "text")
  expect_true(grepl("variable name", r2$content))
})

test_that("show_plot errors for non-existent variable", {
  expect_error(
    show_plot("this_var_does_not_exist_xyz_abc"),
    "not found in the R session"
  )
})

# --- Defaults ---

test_that("show_plot defaults to target='user'", {
  formals_list <- formals(show_plot)
  expect_equal(formals_list$target, "user")
})

test_that("show_plot exposes tool-level MCP App annotations", {
  expect_equal(.show_plot_annotations$`_meta`$ui$resourceUri, "ui://mcpr/plots")
})

# --- Local device detection (via MCPR::: shared primitive) ---

test_that("detect_local_device returns valid channel", {
  channel <- MCPR:::detect_local_device()
  expect_true(channel %in% c("httpgd", "device", "file"))
})

test_that("detect_local_device prefers httpgd when available", {
  skip_if_not(requireNamespace("httpgd", quietly = TRUE), "httpgd not available")
  channel <- MCPR:::detect_local_device()
  expect_equal(channel, "httpgd")
})

# --- User target via variable name ---

test_that("show_plot user target returns text confirmation", {
  on.exit(with_graphics_cleanup()(), add = TRUE)

  .GlobalEnv$.test_plot_var <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  on.exit(rm(".test_plot_var", envir = .GlobalEnv), add = TRUE)

  result <- show_plot(".test_plot_var", target = "user")
  expect_equal(result$type, "text")
  expect_true(grepl("Plot displayed to user|Plot saved to file|Plot displayed on active", result$content))
})

test_that("show_plot user target does not return base64 image", {
  on.exit(with_graphics_cleanup()(), add = TRUE)

  .GlobalEnv$.test_plot_var2 <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  on.exit(rm(".test_plot_var2", envir = .GlobalEnv), add = TRUE)

  result <- show_plot(".test_plot_var2", target = "user")
  expect_false(grepl("^data:", result$content))
  expect_false(result$type == "image")
})

test_that("show_plot displays captured base graphics via variable name", {
  on.exit(with_graphics_cleanup()(), add = TRUE)

  .GlobalEnv$.test_base_plot <- MCPR::capture_plot(plot(cars))
  on.exit(rm(".test_base_plot", envir = .GlobalEnv), add = TRUE)

  result <- show_plot(".test_base_plot", target = "user")
  expect_equal(result$type, "text")
  expect_true(grepl("Plot displayed to user|Plot saved to file|Plot displayed on active", result$content))
})

# --- Optimization suggestions (now in MCPR::: shared primitives) ---

test_that("generate_optimization_suggestions works", {
  suggestions <- MCPR:::generate_optimization_suggestions(1200, 900, 50000, 25000, "png")
  expect_true(length(suggestions) > 0)
  expect_true(any(grepl("400x300|600x450", suggestions)))
  expect_true(any(grepl("%", suggestions)))

  # Test format-specific suggestions
  suggestions_png <- MCPR:::generate_optimization_suggestions(600, 450, 22000, 25000, "png")
  expect_true(any(grepl("JPEG", suggestions_png)))

  # Test different reduction levels
  suggestions_minor <- MCPR:::generate_optimization_suggestions(700, 525, 27000, 25000, "png")
  expect_true(any(grepl("20%", suggestions_minor)))
})

test_that("render-first approach function signature", {
  # Verify that estimate_plot_tokens function no longer exists
  expect_false(exists("estimate_plot_tokens", mode = "function"))

  # Verify generate_optimization_suggestions has updated signature (now in MCPR:::)
  formals_suggestions <- formals(MCPR:::generate_optimization_suggestions)
  expect_true("current_format" %in% names(formals_suggestions))
  expect_equal(formals_suggestions$current_format, "png")
})

# --- MCP App channel ---

test_that(".mcp_apps_supported returns TRUE when mcp_app context is set", {
  set_test_request_context(TRUE)
  on.exit(.clear_mcpr_ctx(), add = TRUE)

  expect_true(.mcp_apps_supported())
})

test_that(".mcp_apps_supported prioritizes direct request context", {
  set_test_request_context(TRUE)
  on.exit(.clear_mcpr_ctx(), add = TRUE)

  expect_true(.mcp_apps_supported())
})

test_that(".mcp_apps_supported returns FALSE when context is not mcp_app", {
  set_test_request_context(FALSE, interface = "cli")
  on.exit(.clear_mcpr_ctx(), add = TRUE)

  expect_false(.mcp_apps_supported())
})

test_that("show_plot routes to structuredContent when mcp_app context is set", {
  set_test_request_context(TRUE)
  on.exit(.clear_mcpr_ctx(), add = TRUE)
  on.exit(with_graphics_cleanup()(), add = TRUE)

  .GlobalEnv$.test_mcp_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  on.exit(rm(".test_mcp_plot", envir = .GlobalEnv), add = TRUE)

  result <- show_plot(".test_mcp_plot", target = "user")

  expect_equal(result$content[[1]]$text, "This tool call rendered a plot in the viewer.")
  expect_equal(result$structuredContent$kind, "image")
  expect_null(result$`_meta`)
})

test_that("show_plot routes captured side-effect plots to structuredContent when mcp_app context is set", {
  set_test_request_context(TRUE)
  on.exit(.clear_mcpr_ctx(), add = TRUE)
  on.exit(with_graphics_cleanup()(), add = TRUE)

  .GlobalEnv$.test_model_plot <- MCPR::capture_plot({
    model <- lm(dist ~ speed, data = cars)
    plot(model)
  })
  on.exit(rm(".test_model_plot", envir = .GlobalEnv), add = TRUE)

  result <- show_plot(".test_model_plot", target = "user")

  expect_equal(result$content[[1]]$text, "This tool call rendered a plot in the viewer.")
  expect_equal(result$structuredContent$kind, "image")
  expect_null(result$`_meta`)
})

test_that("show_plot routes plotly widget to plotly structuredContent", {
  skip_if_not_installed("plotly")
  set_test_request_context(TRUE)
  on.exit(.clear_mcpr_ctx(), add = TRUE)
  on.exit(with_graphics_cleanup()(), add = TRUE)

  .GlobalEnv$.test_plotly_obj <- plotly::plot_ly(mtcars, x = ~mpg, y = ~hp, type = "scatter", mode = "markers")
  on.exit(rm(".test_plotly_obj", envir = .GlobalEnv), add = TRUE)

  result <- show_plot(".test_plotly_obj", target = "user")

  expect_equal(result$content[[1]]$text, "This tool call rendered an interactive widget in the viewer.")
  expect_equal(result$content[[1]]$annotations$audience, list("assistant"))
  expect_equal(result$structuredContent$kind, "plotly")
  expect_false(is.null(result$structuredContent$spec))
  expect_null(result$`_meta`)
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
    fun = function(plot) plot,
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
