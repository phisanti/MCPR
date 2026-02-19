# Unit Tests for Show Plot Tool
# Tests the show_plot tool with target-based routing (user/agent)

library(testthat)
library(ggplot2)
library(jsonlite)

# Source the show_plot tool
source(system.file("tool-show_plot.R", package = "MCPR", mustWork = TRUE))

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
  expect_true(channel %in% c("httpgd", "device", "file"))
})

test_that("detect_output_channel prefers httpgd when available", {
  skip_if_not(requireNamespace("httpgd", quietly = TRUE), "httpgd not available")
  channel <- detect_output_channel()
  expect_equal(channel, "httpgd")
})

# --- User target ---

test_that("show_plot user target returns text confirmation", {
  result <- show_plot("plot(1:10)", target = "user")
  expect_equal(result$type, "text")
  expect_true(grepl("Plot displayed to user|Plot saved to file", result$content))
})

test_that("show_plot user target does not return base64 image", {
  result <- show_plot("plot(1:10)", target = "user")
  expect_false(grepl("^data:", result$content))
  expect_false(result$type == "image")
})

test_that("show_plot user target handles ggplot objects", {
  result <- show_plot("ggplot(mtcars, aes(mpg, hp)) + geom_point()", target = "user")
  expect_equal(result$type, "text")
})

test_that("show_plot user target reports errors", {
  expect_error(show_plot("nonexistent_var + 1", target = "user"), "Error displaying plot")
})

# --- httpgd channel ---

test_that("show_plot_via_httpgd returns url in confirmation", {
  skip_if_not(requireNamespace("httpgd", quietly = TRUE), "httpgd not available")
  result <- show_plot_via_httpgd("plot(1:10)")
  expect_equal(result$type, "text")
  expect_true(grepl("httpgd", result$content))
  expect_true(grepl("http", result$content))
  # Clean up httpgd device
  if (names(grDevices::dev.cur()) %in% c("httpgd", "unigd")) {
    grDevices::dev.off()
  }
})

# --- File fallback ---

test_that("show_plot_via_file saves png and returns path", {
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
