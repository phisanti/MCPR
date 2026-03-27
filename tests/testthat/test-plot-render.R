# ---- detect_local_device ----

test_that("detect_local_device returns a valid channel string", {
  result <- MCPR:::detect_local_device()
  expect_true(result %in% c("httpgd", "device", "file"))
})

test_that("detect_local_device returns 'httpgd' when httpgd is available", {
  skip_if_not_installed("httpgd")
  result <- MCPR:::detect_local_device()
  expect_equal(result, "httpgd")
})

test_that("detect_local_device returns 'file' in non-interactive without httpgd", {
  skip_if_not_installed("mockery")

  # Build a local wrapper that delegates to the internal function, so mockery
  # can intercept calls made inside it.
  local_detect <- MCPR:::detect_local_device
  environment(local_detect) <- environment()

  mockery::stub(local_detect, "requireNamespace", FALSE)
  mockery::stub(local_detect, "interactive", FALSE)
  mockery::stub(local_detect, "grDevices::dev.cur", stats::setNames(1L, "null device"))
  result <- local_detect()
  expect_equal(result, "file")
})

# ---- show_widget_in_browser ----

test_that("show_widget_in_browser creates an HTML file", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("mockery")

  # Create a minimal htmlwidget for testing
  skip_if_not_installed("plotly")
  skip_if_not_installed("ggplot2")

  widget <- plotly::ggplotly(
    ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
      ggplot2::geom_point()
  )

  # Mock browseURL so we don't actually open a browser
  mockery::stub(MCPR:::show_widget_in_browser, "utils::browseURL", function(url) invisible(NULL))

  result <- MCPR:::show_widget_in_browser(widget)
  expect_true(file.exists(result))
  expect_match(result, "\\.html$")

  # Clean up
  unlink(result)
})

test_that("show_widget_in_browser errors without htmlwidgets package", {
  skip_if_not_installed("mockery")
  local_show <- MCPR:::show_widget_in_browser
  environment(local_show) <- environment()

  mockery::stub(local_show, "requireNamespace", FALSE)
  expect_error(
    local_show(list()),
    "htmlwidgets package is required"
  )
})

# ---- capture_plot ----

test_that("capture_plot returns a deferred plotting object", {
  captured <- MCPR::capture_plot(plot(cars))

  expect_s3_class(captured, "captured_plot")
  expect_true(is.call(captured$expr))
  expect_true(is.environment(captured$env))
})

# ---- render_static_plot ----

test_that("render_static_plot with 'file' channel produces a PNG file", {
  skip_if_not_installed("ggplot2")
  plot_obj <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  result <- MCPR:::render_static_plot(plot_obj, "file")
  expect_true(is.list(result))
  expect_equal(result$channel, "file")
  expect_true(file.exists(result$info))
  expect_match(result$info, "\\.png$")
  expect_true(file.info(result$info)$size > 0)

  unlink(result$info)
})

test_that("render_static_plot with 'file' channel handles ggplot objects", {
  skip_if_not_installed("ggplot2")
  plot_obj <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  result <- MCPR:::render_static_plot(plot_obj, "file")
  expect_equal(result$channel, "file")
  expect_true(file.exists(result$info))
  expect_true(file.info(result$info)$size > 0)

  unlink(result$info)
})

test_that("render_static_plot with 'file' channel handles captured_plot objects", {
  captured <- MCPR::capture_plot(plot(cars))
  result <- MCPR:::render_static_plot(captured, "file")

  expect_equal(result$channel, "file")
  expect_true(file.exists(result$info))
  expect_true(file.info(result$info)$size > 0)

  unlink(result$info)
})

test_that("render_static_plot with 'device' channel returns confirmation", {
  skip_if_not_installed("ggplot2")
  plot_obj <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  result <- MCPR:::render_static_plot(plot_obj, "device")
  expect_equal(result$channel, "device")
  expect_true(is.character(result$info))
})

# ---- channel_plot ----

test_that("channel_plot rejects non-plot objects", {
  expect_error(
    MCPR::channel_plot(list(a = 1)),
    "Expected a plot object"
  )
  expect_error(
    MCPR::channel_plot("not a plot"),
    "Expected a plot object"
  )
  expect_error(
    MCPR::channel_plot(42L),
    "Expected a plot object"
  )
})

test_that("channel_plot accepts valid ggplot objects", {
  skip_if_not_installed("ggplot2")
  plot_obj <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()

  # Use file channel path (headless) — mcp_apps_supported=FALSE, target="user"
  on.exit({
    if (grDevices::dev.cur() != 1) try(grDevices::dev.off(), silent = TRUE)
  }, add = TRUE)

  result <- MCPR::channel_plot(plot_obj, mcp_apps_supported = FALSE, target = "user")
  expect_equal(result$type, "text")
  expect_true(grepl("Plot displayed|Plot saved", result$content))
})

test_that("channel_plot accepts captured side-effect plot objects", {
  captured <- MCPR::capture_plot({
    model <- lm(dist ~ speed, data = cars)
    plot(model)
  })

  result <- MCPR::channel_plot(captured, mcp_apps_supported = TRUE, target = "user")

  expect_equal(result$structuredContent$kind, "image")
  expect_equal(result$structuredContent$mimeType, "image/png")
  expect_true(nchar(result$structuredContent$data) > 0)
})

test_that("channel_plot target='user' mcp_apps_supported=TRUE returns structuredContent for ggplot", {
  skip_if_not_installed("ggplot2")
  plot_obj <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()

  on.exit({
    if (grDevices::dev.cur() != 1) try(grDevices::dev.off(), silent = TRUE)
  }, add = TRUE)

  result <- MCPR::channel_plot(plot_obj, mcp_apps_supported = TRUE, target = "user")
  expect_equal(result$structuredContent$kind, "image")
  expect_equal(result$structuredContent$mimeType, "image/png")
  expect_true(nchar(result$structuredContent$data) > 0)
})

test_that("channel_plot target='user' mcp_apps_supported=TRUE routes plotly to plotly viewer", {
  skip_if_not_installed("plotly")
  widget <- plotly::plot_ly(mtcars, x = ~mpg, y = ~hp, type = "scatter", mode = "markers")

  result <- MCPR::channel_plot(widget, mcp_apps_supported = TRUE, target = "user")
  expect_equal(result$structuredContent$kind, "plotly")
  expect_false(is.null(result$structuredContent$spec))
})

test_that("channel_plot target='user' mcp_apps_supported=FALSE routes interactive to browser", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("mockery")

  widget <- plotly::plot_ly(mtcars, x = ~mpg, y = ~hp, type = "scatter", mode = "markers")

  # Mock browseURL and saveWidget so we don't open a browser
  mockery::stub(MCPR:::show_widget_in_browser, "utils::browseURL", function(url) invisible(NULL))

  result <- MCPR::channel_plot(widget, mcp_apps_supported = FALSE, target = "user")
  expect_equal(result$type, "text")
  expect_true(grepl("Interactive widget opened in browser", result$content))
})

test_that("channel_plot target='agent' returns image response for ggplot", {
  skip_if_not_installed("ggplot2")
  plot_obj <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()

  on.exit({
    if (grDevices::dev.cur() != 1) try(grDevices::dev.off(), silent = TRUE)
  }, add = TRUE)

  result <- MCPR::channel_plot(plot_obj, target = "agent")
  expect_equal(result$type, "image")
  expect_match(result$content, "^data:image/png;base64,")
  expect_true(!is.null(result$metadata))
  expect_equal(result$metadata$format, "png")
  expect_equal(result$metadata$dimensions, "600x450")
})

# ---- setup_graphics_device ----

test_that("setup_graphics_device returns expected structure", {
  device_info <- MCPR:::setup_graphics_device("png", 400, 300)
  expect_true(is.list(device_info))
  expect_true(device_info$type %in% c("httpgd", "standard"))
  expect_match(device_info$file, "\\.png$")

  # Clean up the open device
  if (grDevices::dev.cur() != 1) {
    grDevices::dev.off()
  }
  unlink(device_info$file)
})

test_that("setup_graphics_device respects format parameter", {
  device_info <- MCPR:::setup_graphics_device("jpeg", 400, 300)
  expect_match(device_info$file, "\\.jpg$")

  if (grDevices::dev.cur() != 1) {
    grDevices::dev.off()
  }
  unlink(device_info$file)
})

# ---- generate_optimization_suggestions ----

test_that("generate_optimization_suggestions returns suggestions for large reduction", {
  result <- MCPR:::generate_optimization_suggestions(800, 600, 50000, 20000, "png")
  expect_true(is.character(result))
  expect_true(length(result) >= 1)
  expect_match(result[1], "400x300")
})

test_that("generate_optimization_suggestions returns suggestions for moderate reduction", {
  result <- MCPR:::generate_optimization_suggestions(800, 600, 30000, 20000, "png")
  expect_true(length(result) >= 1)
  expect_match(result[1], "600x450")
})

test_that("generate_optimization_suggestions returns suggestions for minor reduction", {
  result <- MCPR:::generate_optimization_suggestions(800, 600, 22000, 20000, "png")
  expect_true(length(result) >= 1)
  expect_match(result[1], "720x540")
})

test_that("generate_optimization_suggestions suggests JPEG for high PNG tokens", {
  result <- MCPR:::generate_optimization_suggestions(800, 600, 50000, 20000, "png")
  expect_true(any(grepl("JPEG", result)))
})

# ---- response_image ----

test_that("response_image creates base64 encoded response", {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 400, height = 400)
  plot(1:5)
  grDevices::dev.off()

  result <- MCPR:::response_image(tmp)
  expect_equal(result$type, "image")
  expect_match(result$content, "^data:image/png;base64,")

  unlink(tmp)
})

test_that("response_image errors for missing file", {
  expect_error(
    MCPR:::response_image("/nonexistent/file.png"),
    "Image file does not exist"
  )
})
