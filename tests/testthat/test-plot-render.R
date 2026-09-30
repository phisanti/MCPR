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

test_that("detect_local_device returns 'file' without httpgd/interactive", {
  skip_if_not_installed("mockery")

  # Build a local wrapper that delegates to the internal function, so
  # mockery can intercept calls made inside it.
  local_detect <- MCPR:::detect_local_device
  environment(local_detect) <- environment()

  mockery::stub(local_detect, "requireNamespace", FALSE)
  mockery::stub(local_detect, "interactive", FALSE)
  mockery::stub(
    local_detect, "grDevices::dev.cur",
    stats::setNames(1L, "null device")
  )
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
  mockery::stub(
    MCPR:::show_widget_in_browser, "utils::browseURL",
    function(url) invisible(NULL)
  )

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
  plot_obj <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
    ggplot2::geom_point()
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
  plot_obj <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
    ggplot2::geom_point()
  result <- MCPR:::render_static_plot(plot_obj, "file")
  expect_equal(result$channel, "file")
  expect_true(file.exists(result$info))
  expect_true(file.info(result$info)$size > 0)

  unlink(result$info)
})

test_that("render_static_plot 'file' channel handles captured_plot", {
  captured <- MCPR::capture_plot(plot(cars))
  result <- MCPR:::render_static_plot(captured, "file")

  expect_equal(result$channel, "file")
  expect_true(file.exists(result$info))
  expect_true(file.info(result$info)$size > 0)

  unlink(result$info)
})

test_that("render_static_plot with 'device' channel returns confirmation", {
  skip_if_not_installed("ggplot2")
  plot_obj <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
    ggplot2::geom_point()
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
  plot_obj <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
    ggplot2::geom_point()

  # Use file channel path (headless) — mcp_apps_supported=FALSE, target="user"
  on.exit({
    if (grDevices::dev.cur() != 1) try(grDevices::dev.off(), silent = TRUE)
  }, add = TRUE)

  result <- MCPR::channel_plot(
    plot_obj, mcp_apps_supported = FALSE, target = "user"
  )
  expect_equal(result$type, "text")
  expect_true(grepl("Plot displayed|Plot saved", result$content))
})

test_that("channel_plot accepts captured side-effect plot objects", {
  captured <- MCPR::capture_plot({
    model <- lm(dist ~ speed, data = cars)
    plot(model)
  })

  result <- MCPR::channel_plot(
    captured, mcp_apps_supported = TRUE, target = "user"
  )

  expect_equal(result$structuredContent$kind, "image")
  expect_equal(result$structuredContent$mimeType, "image/png")
  expect_true(nchar(result$structuredContent$data) > 0)
})

test_that(
  "channel_plot returns structuredContent for user+mcp_apps",
  {
    skip_if_not_installed("ggplot2")
    plot_obj <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
      ggplot2::geom_point()

    on.exit({
      if (grDevices::dev.cur() != 1) try(grDevices::dev.off(), silent = TRUE)
    }, add = TRUE)

    result <- MCPR::channel_plot(
      plot_obj, mcp_apps_supported = TRUE, target = "user"
    )
    expect_equal(result$structuredContent$kind, "image")
    expect_equal(result$structuredContent$mimeType, "image/png")
    expect_true(nchar(result$structuredContent$data) > 0)
  }
)

test_that(
  "channel_plot routes plotly to plotly viewer for user+mcp_apps",
  {
    skip_if_not_installed("plotly")
    widget <- plotly::plot_ly(
      mtcars, x = ~mpg, y = ~hp, type = "scatter", mode = "markers"
    )

    result <- MCPR::channel_plot(
      widget, mcp_apps_supported = TRUE, target = "user"
    )
    expect_equal(result$structuredContent$kind, "plotly")
    expect_false(is.null(result$structuredContent$spec))
  }
)

test_that(
  "channel_plot routes interactive to browser for user without mcp_apps",
  {
    skip_if_not_installed("plotly")
    skip_if_not_installed("htmlwidgets")
    skip_if_not_installed("mockery")

    widget <- plotly::plot_ly(
      mtcars, x = ~mpg, y = ~hp, type = "scatter", mode = "markers"
    )

    # Mock browseURL so we don't open a browser
    mockery::stub(
      MCPR:::show_widget_in_browser, "utils::browseURL",
      function(url) invisible(NULL)
    )

    result <- MCPR::channel_plot(
      widget, mcp_apps_supported = FALSE, target = "user"
    )
    expect_equal(result$type, "text")
    expect_true(grepl("Interactive widget opened in browser", result$content))
  }
)

test_that("channel_plot target='agent' returns image response for ggplot", {
  skip_if_not_installed("ggplot2")
  plot_obj <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
    ggplot2::geom_point()

  on.exit({
    if (grDevices::dev.cur() != 1) try(grDevices::dev.off(), silent = TRUE)
  }, add = TRUE)

  result <- MCPR::channel_plot(plot_obj, target = "agent")
  expect_equal(result$type, "image")
  expect_equal(result$mimeType, "image/png")
  expect_match(result$data, "^iVBORw0KGgo")
  expect_true(!is.null(result$metadata))
  expect_equal(result$metadata$format, "png")
  expect_equal(result$metadata$dimensions, "600x450")
})

# ---- setup_graphics_device ----

test_that("setup_graphics_device returns expected structure", {
  device_info <- MCPR:::setup_graphics_device("png", 400, 300)
  expect_true(is.list(device_info))
  expect_equal(device_info$type, "standard")
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
  expect_equal(result$mimeType, "image/png")
  expect_match(result$data, "^iVBORw0KGgo")

  unlink(tmp)
})

test_that("response_image errors for missing file", {
  expect_error(
    MCPR:::response_image("/nonexistent/file.png"),
    "Image file does not exist"
  )
})

# ---- encode_agent_image ----

test_that("encode_agent_image counts tokens from pixels, not base64 length", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  grDevices::png(tmp, width = 900, height = 650)
  plot(1:10)
  grDevices::dev.off()

  result <- MCPR:::encode_agent_image(tmp, 900, 650)

  expect_equal(result$type, "image")
  expect_equal(result$mimeType, "image/png")
  expect_false(grepl("^data:", result$data))
  # 900 * 650 / 750 -- independent of base64 payload size
  expect_equal(result$tokens, 780)
})

test_that("encode_agent_image aborts on an empty image file", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  file.create(tmp)
  expect_error(
    MCPR:::encode_agent_image(tmp, 600, 450), "empty image file"
  )
})

test_that("encode_agent_image aborts on an oversized payload", {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  writeBin(raw(6 * 1024^2), tmp)
  expect_error(MCPR:::encode_agent_image(tmp, 600, 450), "payload limit")
})

# ---- capture_widget_for_agent ----

# Each subprocess exit code must surface as its own actionable error rather
# than a generic failure -- the agent needs to know whether to install a
# browser or fall back to target = "user".
test_that("capture_widget_for_agent maps exit codes to distinct errors", {
  skip_if_not_installed("mockery")
  skip_if_not_installed("htmlwidgets")

  cases <- list(
    list(status = 2L, pattern = "No Chromium-based browser"),
    list(status = 5L, pattern = "chromote package is required"),
    list(status = 3L, pattern = "no plotly plot element appeared"),
    list(status = 4L, pattern = "did not settle"),
    list(status = 9L, pattern = "Render subprocess failed")
  )

  for (case in cases) {
    local_capture <- MCPR:::capture_widget_for_agent
    environment(local_capture) <- environment()
    mockery::stub(local_capture, "htmlwidgets::saveWidget", TRUE)
    mockery::stub(local_capture, "processx::run", list(
      status = case$status, timeout = FALSE, stdout = "", stderr = "boom"
    ))
    expect_error(
      local_capture(structure(list(), class = "plotly")),
      case$pattern
    )
  }
})

test_that("capture_widget_for_agent aborts on subprocess timeout", {
  skip_if_not_installed("mockery")
  skip_if_not_installed("htmlwidgets")

  local_capture <- MCPR:::capture_widget_for_agent
  environment(local_capture) <- environment()
  mockery::stub(local_capture, "htmlwidgets::saveWidget", TRUE)
  mockery::stub(local_capture, "processx::run", list(
    status = NA_integer_, timeout = TRUE, stdout = "", stderr = ""
  ))
  expect_error(
    local_capture(structure(list(), class = "plotly")), "timed out"
  )
})

# ---- channel_plot(target = "agent") on interactive widgets ----

test_that("channel_plot renders plotly to non-blank image for agent", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("chromote")
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("png")
  skip_if_not(chrome_available(), "No Chromium-based browser available")

  p <- plotly::plot_ly(
    x = 1:50, y = seq_len(50), type = "scatter", mode = "markers"
  )
  result <- MCPR::channel_plot(p, target = "agent")

  expect_equal(result$type, "image")
  expect_equal(result$metadata$dimensions, "900x650")
  expect_equal(result$tokens, 780)

  # A blank PNG is non-empty and well-formed -- it was the original bug.
  # Assert the image actually contains a drawing by counting pixels.
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  b64_str <- result$data
  writeBin(base64enc::base64decode(b64_str), tmp)
  pixels <- png::readPNG(tmp)
  expect_gt(length(unique(as.vector(pixels[, , 1]))), 10)
})

test_that("channel_plot leaves no temp files behind on the interactive path", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("chromote")
  skip_if_not_installed("htmlwidgets")
  skip_if_not(chrome_available(), "No Chromium-based browser available")

  before <- list.files(tempdir(), recursive = TRUE)
  MCPR::channel_plot(
    plotly::plot_ly(x = 1:20, y = 1:20, type = "scatter", mode = "markers"),
    target = "agent"
  )
  expect_length(setdiff(list.files(tempdir(), recursive = TRUE), before), 0)
})

# The readiness probe originally counted only `.point` + `canvas` nodes. Line,
# heatmap, box and pie traces render as path.js-line / <image> / path.box /
# path.surface and produce neither, so they never settled and every one of
# them hung until the deadline before failing with a misleading timeout.
test_that("channel_plot renders every common plotly trace type", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("png")
  skip_if_not(chrome_available(), "No Chromium-based browser available")

  fixtures <- list(
    markers = plotly::plot_ly(
      x = 1:20, y = 1:20, type = "scatter", mode = "markers"
    ),
    lines = plotly::plot_ly(
      x = 1:20, y = 1:20, type = "scatter", mode = "lines"
    ),
    bar = plotly::plot_ly(x = c("a", "b"), y = c(3, 5), type = "bar"),
    heatmap = plotly::plot_ly(z = matrix(1:100, 10), type = "heatmap"),
    box = plotly::plot_ly(y = seq_len(100), type = "box")
  )

  for (nm in names(fixtures)) {
    elapsed <- system.time(
      result <- MCPR::channel_plot(fixtures[[nm]], target = "agent")
    )[["elapsed"]]

    expect_equal(result$type, "image", info = nm)

    tmp <- tempfile(fileext = ".png")
    writeBin(
      base64enc::base64decode(result$data), tmp
    )
    pixels <- png::readPNG(tmp)
    unlink(tmp)
    expect_gt(length(unique(as.vector(pixels[, , 1]))), 10)

    # A wedged settle loop still "succeeds" eventually via the deadline path,
    # so assert it settled promptly rather than merely that it returned.
    expect_lt(elapsed, 30, label = paste0(nm, " render seconds"))
  }
})

test_that("non-plotly htmlwidgets are rejected with an actionable message", {
  widget <- structure(list(), class = c("htmlwidget"))
  expect_error(
    MCPR::channel_plot(widget, target = "agent"),
    "Only plotly widgets"
  )
})

# The mocked exit-code test above stubs processx::run away entirely, so it
# cannot see argv drift, a missing helper, or child-side breakage. This drives
# a real subprocess through the real contract.
test_that("render helper subprocess honours the exit-code contract", {
  skip_on_cran()
  helper <- system.file("scripts/render-widget.R", package = "MCPR")
  skip_if(!nzchar(helper), "renderer script not installed")

  rscript <- file.path(R.home("bin"), "Rscript")
  run_helper <- function(html, deadline_ms = 8000) {
    out <- tempfile(fileext = ".png")
    on.exit(unlink(out), add = TRUE)
    res <- processx::run(
      rscript,
      c("--no-save", "--no-restore", helper, html, out, "400", "300",
        as.character(deadline_ms)),
      env = c(
        "current",
        R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)
      ),
      timeout = 60, error_on_status = FALSE
    )
    list(status = res$status, token = trimws(res$stdout))
  }

  # A page with no widget must report NO_PLOT, not hang or crash.
  no_widget <- tempfile(fileext = ".html")
  writeLines("<html><body><p>no widget</p></body></html>", no_widget)
  on.exit(unlink(no_widget), add = TRUE)

  res <- run_helper(no_widget)
  expect_equal(res$status, 3L)
  expect_equal(res$token, "NO_PLOT")

  skip_if_not(chrome_available(), "No Chromium-based browser available")
  skip_if_not_installed("plotly")
  skip_if_not_installed("htmlwidgets")

  widget_html <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(
    plotly::plot_ly(x = 1:5, y = 1:5, type = "scatter", mode = "lines"),
    widget_html,
    selfcontained = FALSE
  )
  on.exit(unlink(widget_html), add = TRUE)

  ok <- run_helper(widget_html)
  expect_equal(ok$status, 0L)
  expect_equal(ok$token, "OK")
})
