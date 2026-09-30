# Plot Rendering Primitives and Public Routing API
# Pure rendering functions plus deferred capture helpers for plotting workflows.
# Exports channel_plot() and capture_plot() to keep routing device-agnostic.

# Plot classes that respond to print() on a graphics device. Interactive
# widgets (plotly/htmlwidget) are deliberately absent - they have no print
# method that draws to a device, which is why they need the browser path.
PRINTABLE_CLASSES <- c(
  "gg", "ggplot", "grob", "gtable", "trellis", "recordedplot", "captured_plot"
)

#' Detect the best available local graphics device
#'
#' Returns the device channel for static plot rendering in non-MCP-App contexts.
#' Checks httpgd availability, interactive session, or falls back to export.
#'
#' @return Character string: "none", "httpgd", "device", or "file"
#' @noRd
detect_local_device <- function() {
  # MCPR-owned session without httpgd: static display is deliberately skipped
  if (owned_display_disabled()) {
    return("none")
  }

  # httpgd already the active device?
  dev_name <- names(grDevices::dev.cur())
  if (dev_name %in% c("httpgd", "unigd")) {
    return("httpgd")
  }

  # httpgd available but not active?
  if (requireNamespace("httpgd", quietly = TRUE)) {
    return("httpgd")
  }

  # Interactive with a display?
  if (interactive()) {
    return("device")
  }

  # Headless fallback
  "file"
}

#' Capture a plotting expression for deferred display
#'
#' @description Capture an unevaluated plotting expression for later display on
#' the active graphics device. This is intended for base R graphics and other
#' side-effect plotting workflows that do not naturally return a reusable plot
#' object, such as `plot(cars)` or `{ model <- lm(dist ~ speed, data = cars);
#' plot(model) }`.
#'
#' The expression is not evaluated when captured. Instead, `print()` re-runs it
#' in the original calling environment when the plot is displayed through
#' `show_plot()` or `channel_plot()`.
#'
#' @param expr A plotting expression to defer for later rendering.
#' @return An object of class `captured_plot`.
#' @examples
#' captured <- capture_plot(plot(cars))
#' @export
capture_plot <- function(expr) {
  if (missing(expr)) {
    cli::cli_abort("`expr` must be supplied")
  }

  structure(
    list(
      expr = substitute(expr),
      env = parent.frame()
    ),
    class = "captured_plot"
  )
}

#' @export
print.captured_plot <- function(x, ...) {
  if (!is.list(x) || is.null(x$expr) || is.null(x$env)) {
    cli::cli_abort("Invalid `captured_plot` object", .internal = TRUE)
  }

  eval(x$expr, envir = x$env)
  invisible(x)
}

#' Display an htmlwidget in the user's default browser
#'
#' Saves the widget as a self-contained HTML file and opens it with browseURL.
#' Used for plotly and other htmlwidget objects in local (non-MCP-App) contexts.
#'
#' @param widget An htmlwidget object (e.g., plotly, leaflet, DT)
#' @return The path to the saved HTML file
#' @noRd
show_widget_in_browser <- function(widget) {
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    cli::cli_abort(
      "htmlwidgets package is required to display interactive widgets"
    )
  }

  tmp <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(widget, tmp, selfcontained = TRUE)
  utils::browseURL(tmp)
  tmp
}

#' Render a static plot object on a local device
#'
#' Encapsulates the prepare/print/finalize cycle for static plot objects.
#' Handles httpgd, native device, and file export paths. Always receives a
#' real plot object; no expression evaluation happens here.
#'
#' @param plot_obj A plot object (ggplot, recordedplot, grob, gtable, trellis,
#'   or captured_plot)
#' @param device_channel One of "none", "httpgd", "device", or "file"
#' @return A list with `channel` (character) and `info` (URL, path, or msg)
#' @noRd
render_static_plot <- function(plot_obj, device_channel) {
  switch(device_channel,
    none = {
      list(
        channel = "none",
        info = "Plot display skipped: httpgd is not installed in this session."
      )
    },

    httpgd = {
      # One persistent device per session: every plot joins the same viewer
      # history, and the browser opens only when the device is first shown.
      display <- activate_display_device()
      if (is.null(display)) {
        return(render_static_plot(plot_obj, "none"))
      }

      if (inherits(plot_obj, PRINTABLE_CLASSES) &&
            !is_current_page(plot_obj)) {
        draw_on_display_device(plot_obj)
      }

      if (isTRUE(display$open_browser)) {
        httpgd::hgd_browse()
      }

      list(channel = "httpgd", info = display$url)
    },

    device = {
      if (inherits(plot_obj, PRINTABLE_CLASSES)) {
        draw_on_display_device(plot_obj)
      }

      list(
        channel = "device",
        info = "Plot displayed on active graphics device."
      )
    },

    file = {
      tmp <- tempfile(fileext = ".png")
      without_plot_recording({
        grDevices::png(tmp, width = 800, height = 600)
        tryCatch(
          if (inherits(plot_obj, PRINTABLE_CLASSES)) print(plot_obj),
          finally = grDevices::dev.off()
        )
      })

      list(channel = "file", info = tmp)
    }
  )
}

#' Whether a recorded plot is already the active device's current page
#'
#' Plots drawn directly on the owned httpgd device are already in its history;
#' replaying them again would only add a duplicate page.
#'
#' @param plot_obj A plot object
#' @return Logical scalar
#' @noRd
is_current_page <- function(plot_obj) {
  if (!inherits(plot_obj, "recordedplot") || grDevices::dev.cur() == 1L) {
    return(FALSE)
  }
  current <- tryCatch(grDevices::recordPlot(), error = function(e) NULL)
  !is.null(current) && identical(current[[1L]], plot_obj[[1L]])
}

#' Display a static plot via MCP App inline viewer
#'
#' Captures the plot object as PNG and returns a single MCP content item with
#' structuredContent for the MCP App viewer. If the object is an htmlwidget or
#' plotly, delegates to show_plotly_via_mcp_app() for interactive rendering.
#'
#' @param plot_obj A plot object (ggplot, recordedplot, grob, gtable, trellis,
#'   plotly, htmlwidget, or captured_plot)
#' @param width,height Render size in pixels
#' @return A single content item descriptor for encode_tool_results()
#' @noRd
show_plot_via_mcp_app <- function(plot_obj, width = 800L, height = 600L) {
  # Delegate interactive plots to the plotly path
  if (inherits(plot_obj, c("htmlwidget", "plotly"))) {
    return(show_plotly_via_mcp_app(plot_obj))
  }

  tmp <- tempfile(fileext = ".png")
  device_open <- FALSE
  on.exit({
    if (device_open && grDevices::dev.cur() != 1) {
      try(grDevices::dev.off(), silent = TRUE)
    }
    unlink(tmp)
  }, add = TRUE)

  # Open device BEFORE printing so side-effect plots (recordedplot) are captured
  grDevices::png(tmp, width = width, height = height)
  device_open <- TRUE

  if (inherits(plot_obj, PRINTABLE_CLASSES)) {
    print(plot_obj)
  }

  grDevices::dev.off()
  device_open <- FALSE

  # Base64-encode the PNG - guard against empty/partial files
  file_size <- file.info(tmp)$size
  if (is.na(file_size) || file_size == 0) {
    cli::cli_abort(c(
      "Plot produced an empty image file.",
      "i" = "The plot object may not generate visible output."
    ))
  }
  raw_data <- readBin(tmp, "raw", file_size)
  b64_data <- base64enc::base64encode(raw_data)

  list(
    content = list(list(
      type = "text",
      text = "This tool call rendered a plot in the viewer.",
      annotations = list(audience = list("assistant"))
    )),
    structuredContent = list(
      kind = "image",
      mimeType = "image/png",
      data = b64_data
    )
  )
}

#' Display a plotly/htmlwidget via MCP App inline viewer
#'
#' Builds the plotly spec and returns it in `structuredContent` for the MCP App
#' viewer to render interactively.
#'
#' @param widget A plotly or htmlwidget object
#' @return A single content item descriptor for encode_tool_results()
#' @noRd
show_plotly_via_mcp_app <- function(widget) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    cli::cli_abort("plotly package is required for interactive chart rendering")
  }

  built <- plotly::plotly_build(widget)

  spec <- list(
    data   = built$x$data,
    layout = built$x$layout,
    config = built$x$config
  )

  list(
    content = list(list(
      type = "text",
      text = "This tool call rendered an interactive widget in the viewer.",
      annotations = list(audience = list("assistant"))
    )),
    structuredContent = list(
      kind = "plotly",
      spec = spec
    )
  )
}

#' Create standard graphics device for plot generation
#'
#' Uses standard R graphics devices for agent-facing static image capture.
#' The user-facing display path handles httpgd separately.
#'
#' @param format Output format: "png", "jpeg", "pdf", or "svg"
#' @param width Width for the device
#' @param height Height for the device
#' @return A list with `type` ("standard") and `file` (temp file path)
#' @noRd
setup_graphics_device <- function(format = "png", width = 800, height = 600) {
  file_ext <- switch(format,
    "png" = ".png",
    "jpeg" = ".jpg",
    "pdf" = ".pdf",
    "svg" = ".svg"
  )

  tmp <- tempfile(fileext = file_ext)

  switch(format,
    "png" = grDevices::png(tmp, width = width, height = height),
    "jpeg" = grDevices::jpeg(tmp, width = width, height = height, quality = 90),
    "pdf" = grDevices::pdf(tmp, width = width / 100, height = height / 100),
    "svg" = grDevices::svg(tmp, width = width / 100, height = height / 100)
  )
  list(type = "standard", file = tmp)
}

#' Get plot data from a graphics device with token calculation
#'
#' Retrieves the rendered plot from the device, encodes it as base64,
#' and calculates token usage.
#'
#' @param device_info Device information from setup_graphics_device
#' @param format Output format
#' @param width Width in pixels
#' @param height Height in pixels
#' @return Image response with base64 encoded plot and token count
#' @noRd
get_plot_data <- function(device_info, format = "png", width = 800,
                          height = 600) {
  # Register cleanup first so the temp file is always removed, even if
  # response_image() or base64 encoding throws before we reach the end.
  on.exit(unlink(device_info$file), add = TRUE)

  if (device_info$type == "httpgd") {
    tryCatch(
      {
        plot_data <- httpgd::ugd_render(
          width = width,
          height = height,
          renderer = format
        )

        writeBin(plot_data, device_info$file)
      },
      error = function(e) {
        switch(format,
          "png" = {
            grDevices::dev.copy(
              grDevices::png, device_info$file, width = width,
              height = height
            )
            grDevices::dev.off()
          },
          "jpeg" = {
            grDevices::dev.copy(
              grDevices::jpeg, device_info$file, width = width,
              height = height, quality = 90
            )
            grDevices::dev.off()
          },
          "pdf" = {
            grDevices::dev.copy(
              grDevices::pdf, device_info$file, width = width / 100,
              height = height / 100
            )
            grDevices::dev.off()
          },
          "svg" = {
            grDevices::dev.copy(
              grDevices::svg, device_info$file, width = width / 100,
              height = height / 100
            )
            grDevices::dev.off()
          }
        )
      }
    )
  } else {
    grDevices::dev.off()
  }

  encode_agent_image(device_info$file, width, height, format)
}

#' Encode a finished image file for agent delivery
#'
#' Takes an image file already written to disk (from a graphics device or a
#' subprocess renderer) and produces the base64 content item consumed by
#' `.channel_plot_agent()`. Token accounting is pixel-based - it reflects what
#' a vision model is actually charged for a rendered image block, not the
#' length of the base64 text produced by encoding it.
#'
#' @param file Path to the rendered image file
#' @param width Width in pixels the image was rendered at
#' @param height Height in pixels the image was rendered at
#' @param format Output format: "png", "jpeg", "pdf", or "svg"
#' @return A list with `type` ("image"), `data` (base64), `mimeType`, `tokens`
#' @noRd
encode_agent_image <- function(file, width, height, format = "png") {
  file_size <- file.info(file)$size
  if (is.na(file_size) || file_size == 0) {
    cli::cli_abort(c(
      "Plot produced an empty image file.",
      "i" = "The plot object may not generate visible output."
    ))
  }
  if (file_size > 5 * 1024^2) {
    cli::cli_abort(c(
      "Rendered image is {round(file_size / 1024^2, 1)} MB.",
      "i" = "Exceeds the 5 MB payload limit for agent delivery."
    ))
  }

  mime_type <- switch(format,
    "png" = "image/png",
    "jpeg" = "image/jpeg",
    "pdf" = "application/pdf",
    "svg" = "image/svg+xml"
  )

  image_response <- response_image(file, mime_type)
  image_response$tokens <- ceiling(width * height / 750)

  image_response
}

#' Generate optimization suggestions for token reduction
#'
#' Provides specific recommendations to reduce token consumption
#' based on current dimensions and token usage.
#'
#' @param current_width Current plot width
#' @param current_height Current plot height
#' @param current_tokens Actual current token count
#' @param target_tokens Target token limit
#' @param current_format Current format used
#' @return Character vector of suggestions
#' @noRd
generate_optimization_suggestions <- function(current_width, current_height,
                                              current_tokens, target_tokens,
                                              current_format = "png") {
  suggestions <- character()

  reduction_needed <- (current_tokens - target_tokens) / current_tokens

  if (reduction_needed > 0.5) {
    suggestions <- c(
      suggestions,
      sprintf("Reduce resolution to 400x300 (saves ~70%% tokens)")
    )
  } else if (reduction_needed > 0.3) {
    suggestions <- c(
      suggestions,
      sprintf("Reduce resolution to 600x450 (saves ~40%% tokens)")
    )
  } else {
    new_width <- round(current_width * 0.9)
    new_height <- round(current_height * 0.9)
    suggestions <- c(
      suggestions,
      sprintf(
        "Reduce resolution to %dx%d (saves ~20%% tokens)",
        new_width, new_height
      )
    )
  }

  if (current_format == "png" && current_tokens > target_tokens * 0.8) {
    suggestions <- c(
      suggestions,
      "Consider JPEG format for ~20% token savings (lower quality)"
    )
  }

  suggestions
}

#' Create an image response in base64 format
#'
#' @param file Path to the image file
#' @param mime_type MIME type of the image (default: "image/png")
#' @return A list with `type` ("image"), `data` (raw base64, no data-URI
#'   prefix) and `mimeType` - the shape `encode_tool_results()` turns into an
#'   MCP image content block
#' @noRd
response_image <- function(file, mime_type = "image/png") {
  if (!file.exists(file)) {
    cli::cli_abort("Image file does not exist: {file}")
  }

  list(
    type = "image",
    data = base64enc::base64encode(file),
    mimeType = mime_type
  )
}

#' Route a plot object to the appropriate display channel
#'
#' @description Central routing function for displaying R plot objects.
#' Validates the plot object class, then routes to the appropriate
#' rendering path based on target audience and MCP App support.
#'
#' @param plot_obj A plot object (ggplot, plotly, htmlwidget, recordedplot,
#'   grob, gtable, trellis, or captured_plot)
#' @param mcp_apps_supported Logical; MCP App structuredContent support
#' @param target Character; "user" (display) or "agent" (base64 analysis)
#' @param width,height Optional render size in pixels (100-4000) for the agent
#'   image and the MCP App viewer. Recorded plots (`recordedplot`) replay at
#'   any size. Ignored by the responsive httpgd viewer.
#' @return Varies by path: text, structuredContent list, or image response
#' @examples
#' \dontrun{
#' captured <- capture_plot(plot(cars))
#' channel_plot(captured)
#' }
#' @export
channel_plot <- function(plot_obj, mcp_apps_supported = FALSE,
                         target = c("user", "agent"),
                         width = NULL, height = NULL) {
  target <- match.arg(target)
  width <- validate_plot_dimension(width, "width")
  height <- validate_plot_dimension(height, "height")

  # Validate plot_obj class
  valid_classes <- c(
    "gg", "ggplot", "plotly", "htmlwidget", "recordedplot", "grob",
    "gtable", "trellis", "captured_plot"
  )
  if (!inherits(plot_obj, valid_classes)) {
    cli::cli_abort(
      "Expected a plot object ({paste(valid_classes, collapse = ', ')}), ",
      "got {paste(class(plot_obj), collapse = '/')}"
    )
  }

  is_interactive <- inherits(plot_obj, c("htmlwidget", "plotly"))

  # Temporary render devices below are not session plots: keep them out of
  # the plot store and away from the session device's page tracking.
  if (target == "agent") {
    return(without_plot_recording(
      .channel_plot_agent(plot_obj, is_interactive, width, height)
    ))
  }

  if (mcp_apps_supported) {
    if (is_interactive) {
      return(show_plotly_via_mcp_app(plot_obj))
    }
    return(without_plot_recording(
      show_plot_via_mcp_app(plot_obj, width %||% 800L, height %||% 600L)
    ))
  }

  # Local rendering
  if (is_interactive) {
    path <- show_widget_in_browser(plot_obj)
    return(list(
      type = "text",
      content = sprintf(
        "Interactive widget opened in browser: %s", path
      )
    ))
  }

  device_channel <- detect_local_device()
  render_result <- render_static_plot(plot_obj, device_channel)
  list(
    type = "text",
    content = switch(render_result$channel,
      httpgd = sprintf(
        "Plot displayed to user via httpgd at %s",
        render_result$info
      ),
      none = render_result$info,
      device = render_result$info,
      file = sprintf(
        "Plot saved to file: %s (headless environment).",
        render_result$info
      )
    )
  )
}

#' Capture an htmlwidget as a static PNG for agent analysis
#'
#' Renders a plotly/htmlwidget object in an isolated headless-Chrome
#' subprocess and returns the path to the resulting PNG. MCPR's own
#' `later`/nanonext event loop is never touched, and Chrome cannot outlive
#' the call. Chrome discovery honours `chromote::find_chrome()`; set
#' `CHROMOTE_CHROME` env var to point at a specific binary if needed.
#'
#' @param plot_obj A plotly object
#' @param width Screenshot width in pixels
#' @param height Screenshot height in pixels
#' @param format Image format written by the renderer (currently "png")
#' @param timeout Seconds to wait for render subprocess before aborting
#' @return Character scalar - path to the rendered PNG on disk
#' @noRd
capture_widget_for_agent <- function(plot_obj, width = 900L, height = 650L,
                                     format = "png", timeout = 45) {
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    cli::cli_abort(c(
      "htmlwidgets package required to render interactive plots.",
      "i" = "Use `target = \"user\"` to display the widget instead."
    ))
  }

  tmp_html <- tempfile(fileext = ".html")
  tmp_out <- tempfile(fileext = paste0(".", format))
  # tmp_out is the return value and is owned by the caller - cleaning it
  # up here would delete the PNG before the caller ever sees it.
  on.exit({
    unlink(tmp_html)
    unlink(
      paste0(tools::file_path_sans_ext(tmp_html), "_files"),
      recursive = TRUE
    )
  }, add = TRUE)

  htmlwidgets::saveWidget(plot_obj, tmp_html, selfcontained = FALSE)

  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  helper <- system.file("scripts/render-widget.R", package = "MCPR")
  # Rscript itself exits 2 when it cannot open its script file, which would
  # otherwise be indistinguishable from the helper's own NO_CHROME status.
  if (!nzchar(helper) || !file.exists(helper)) {
    cli::cli_abort(
      "Renderer script is missing from the installed MCPR package.",
      .internal = TRUE
    )
  }
  # Give the helper a shorter deadline than the process-level timeout so its
  # clean TIMEOUT exit wins the race against processx killing it outright.
  deadline_s <- max(5, timeout - 5)

  res <- processx::run(
    rscript,
    c(
      "--no-save", "--no-restore", helper, tmp_html, tmp_out,
      as.character(width), as.character(height),
      as.character(deadline_s * 1000)
    ),
    # --vanilla would imply --no-init-file, which stops renv activating from
    # .Rprofile and hides a renv-installed chromote from the child.
    env = c(
      "current",
      R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)
    ),
    timeout = timeout,
    # NB: cleanup_tree = TRUE would also reap an orphaned Chrome on a timeout
    # kill, but it errors on the normal path here ("failed to send signal")
    # when part of the tree has already exited. The helper's own
    # on.exit(b$close()) covers every non-SIGKILL exit; see MCPR-3uq.
    error_on_status = FALSE
  )

  # The child writes diagnostics to stderr on every failure path; surface them
  # rather than discarding the only description of what actually went wrong.
  stderr_txt <- if (is.null(res$stderr)) "" else trimws(res$stderr)
  detail <- if (nzchar(stderr_txt)) {
    c("i" = "Renderer said: {stderr_txt}")
  } else {
    character()
  }

  if (isTRUE(res$timeout)) {
    cli::cli_abort(c(
      "Rendering the interactive plot timed out after {timeout}s.",
      "i" = "Use `target = \"user\"` to display the widget instead.",
      detail
    ))
  }

  if (identical(res$status, 0L)) {
    if (file.exists(tmp_out) && file.info(tmp_out)$size > 0) {
      return(tmp_out)
    }
    cli::cli_abort(
      "Render subprocess reported success but produced no file.",
      .internal = TRUE
    )
  }

  msg <- switch(as.character(res$status),
    "2" = c(
      "No Chromium-based browser found.",
      "i" = "Check `chromote::find_chrome()` or set the CHROMOTE_CHROME env var.",
      "i" = "Or use `target = \"user\"` to display in a browser."
    ),
    "5" = c(
      "The chromote package is required to render interactive plots.",
      "i" = "Install it, or use `target = \"user\"` to display the widget."
    ),
    "3" = c(
      "Page loaded but no plotly plot element appeared.",
      "i" = "Only plotly widgets can be rendered for `target = \"agent\"`.",
      "i" = "Use `target = \"user\"` for other htmlwidget types."
    ),
    "4" = c(
      "Rendering did not settle within {deadline_s}s.",
      "i" = "Very large or complex plots may need `target = \"user\"`."
    ),
    c("Render subprocess failed (exit {res$status}).")
  )
  cli::cli_abort(c(msg, detail))
}

#' Render a plot for agent analysis as base64-encoded image
#'
#' Internal helper called by channel_plot() for agent target. Static plots
#' (ggplot, base R, etc.) render at 600x450 on a graphics device. plotly
#' objects render at 900x650 via an isolated headless Chrome subprocess.
#'
#' Token accounting is pixel-based, so the thresholds below are expressed in
#' those terms: 3000 tokens is roughly 1500x1500 px. They are deliberately
#' reachable - the previous base64-length limit of 25000 could not be crossed
#' by any dimensions this function produces.
#'
#' @param plot_obj A plot object
#' @param is_interactive Logical; whether plot_obj is an htmlwidget/plotly,
#'   as already determined by `channel_plot()`
#' @param width,height Optional render size overriding the defaults
#' @return Image response; `metadata` stays R-side, `optimization_warning` reaches the agent as text
#' @noRd
.channel_plot_agent <- function(plot_obj,
                                is_interactive = inherits(
                                  plot_obj, c("htmlwidget", "plotly")
                                ),
                                width = NULL, height = NULL) {
  format <- "png"
  token_limit <- 3000
  warn_threshold <- 2000

  # The renderer looks for `.js-plotly-plot`, so a leaflet/DT/visNetwork
  # widget would spin until the deadline and then be blamed for failing to
  # render. Reject it up front with a message naming the real limitation.
  if (is_interactive && !inherits(plot_obj, "plotly")) {
    cli::cli_abort(c(
      "Only plotly widgets can be rendered for `target = \"agent\"`.",
      "i" = "Got {.cls {class(plot_obj)[1]}}.",
      "i" = "Use `target = \"user\"` to display this widget interactively."
    ))
  }

  if (is_interactive) {
    width <- width %||% 900L
    height <- height %||% 650L
  } else {
    width <- width %||% 600L
    height <- height %||% 450L
  }

  tryCatch(
    {
      if (is_interactive) {
        out_file <- capture_widget_for_agent(plot_obj, width, height)
        on.exit(unlink(out_file), add = TRUE)
        image_response <- encode_agent_image(out_file, width, height, format)
      } else {
        device_info <- setup_graphics_device(format, width, height)

        # Print the plot object to the open device
        if (inherits(plot_obj, PRINTABLE_CLASSES)) {
          print(plot_obj)
        }

        image_response <- get_plot_data(device_info, format, width, height)
      }

      actual_tokens <- image_response$tokens

      if (actual_tokens > token_limit) {
        suggestions <- generate_optimization_suggestions(
          width, height, actual_tokens, token_limit, format
        )
        error_msg <- sprintf(
          c(
            "Plot too large for agent analysis: %s tokens exceeds %s.",
            "\nTry these optimizations:",
            "\n- %s",
            "\nOr use `target = 'user'` to display directly."
          ),
          format(actual_tokens, big.mark = ","),
          format(token_limit, big.mark = ","),
          paste(suggestions, collapse = "\n- ")
        )
        cli::cli_abort(error_msg)
      }

      optimization_warning <- NULL
      if (actual_tokens > warn_threshold) {
        suggestions <- generate_optimization_suggestions(
          width, height, actual_tokens, warn_threshold, format
        )
        optimization_warning <- sprintf(
          c(
            "HIGH TOKEN USAGE: %s tokens (%.1f%% of limit)",
            "\nConsider optimizations:",
            "\n- %s",
            "\nOr use `target = 'user'` to display directly."
          ),
          format(actual_tokens, big.mark = ","),
          (actual_tokens / token_limit) * 100,
          paste(suggestions, collapse = "\n- ")
        )
        cli::cli_warn(optimization_warning)
      }

      image_response$metadata <- list(
        actual_tokens = actual_tokens,
        dimensions = paste0(width, "x", height),
        format = format,
        optimization_applied = if (!is.null(optimization_warning))
          "Warning issued"
        else
          "None",
        token_efficiency = sprintf(
          "%.1f%% of limit used",
          (actual_tokens / token_limit) * 100
        )
      )

      if (!is.null(optimization_warning)) {
        image_response$optimization_warning <- optimization_warning
      }

      image_response
    },
    error = function(e) {
      # The interactive branch never opens a graphics device - closing "the
      # active device" here would be spurious and could clobber a device the
      # caller has open for unrelated reasons.
      if (!is_interactive && grDevices::dev.cur() != 1) {
        try(grDevices::dev.off(), silent = TRUE)
      }
      # Carry `e$body` through: the remediation bullets ("use target = user",
      # "set CHROMOTE_CHROME") live there, and rewrapping on the message alone
      # silently discards every actionable hint the agent needs.
      cli::cli_abort(
        c("Error creating plot: {conditionMessage(e)}", e$body),
        call = NULL
      )
    }
  )
}
