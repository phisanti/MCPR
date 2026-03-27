# Plot Rendering Primitives and Public Routing API
# Pure rendering functions plus deferred capture helpers for side-effect plotting workflows.
# Exports channel_plot() and capture_plot() to keep plot routing device-agnostic.

#' Detect the best available local graphics device
#'
#' Returns the device channel for static plot rendering in non-MCP-App contexts.
#' Checks httpgd availability, interactive session, or falls back to file export.
#'
#' @return Character string: "httpgd", "device", or "file"
#' @noRd
detect_local_device <- function() {
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
    cli::cli_abort("htmlwidgets package is required to display interactive widgets")
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
#' real plot object — no expression evaluation happens here.
#'
#' @param plot_obj A plot object (ggplot, recordedplot, grob, gtable, trellis,
#'   or captured_plot)
#' @param device_channel One of "httpgd", "device", or "file"
#' @return A list with `channel` (character) and `info` (URL, path, or confirmation)
#' @noRd
render_static_plot <- function(plot_obj, device_channel) {
  printable_classes <- c("gg", "ggplot", "grob", "gtable", "trellis", "recordedplot", "captured_plot")

  switch(device_channel,
    httpgd = {
      dev_name <- names(grDevices::dev.cur())
      already_active <- dev_name %in% c("httpgd", "unigd")

      if (!already_active) {
        httpgd::hgd(silent = TRUE)
      }

      if (inherits(plot_obj, printable_classes)) {
        print(plot_obj)
      }

      url <- httpgd::hgd_url()
      if (!already_active) {
        httpgd::hgd_browse()
      }

      list(channel = "httpgd", info = url)
    },

    device = {
      if (inherits(plot_obj, printable_classes)) {
        print(plot_obj)
      }

      list(channel = "device", info = "Plot displayed on active graphics device.")
    },

    file = {
      tmp <- tempfile(fileext = ".png")
      grDevices::png(tmp, width = 800, height = 600)
      on.exit(grDevices::dev.off(), add = TRUE)

      if (inherits(plot_obj, printable_classes)) {
        print(plot_obj)
      }

      list(channel = "file", info = tmp)
    }
  )
}

#' Display a static plot via MCP App inline viewer
#'
#' Captures the plot object as PNG and returns a single MCP content item with
#' structuredContent for the MCP App viewer. If the object is an htmlwidget or
#' plotly, delegates to show_plotly_via_mcp_app() for interactive rendering.
#'
#' @param plot_obj A plot object (ggplot, recordedplot, grob, gtable, trellis,
#'   plotly, htmlwidget, or captured_plot)
#' @return A single content item descriptor for encode_tool_results()
#' @noRd
show_plot_via_mcp_app <- function(plot_obj) {
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
  grDevices::png(tmp, width = 800, height = 600)
  device_open <- TRUE

  printable_classes <- c("gg", "ggplot", "grob", "gtable", "trellis", "recordedplot", "captured_plot")
  if (inherits(plot_obj, printable_classes)) {
    print(plot_obj)
  }

  grDevices::dev.off()
  device_open <- FALSE

  # Base64-encode the PNG — guard against empty/partial files
  file_size <- file.info(tmp)$size
  if (is.na(file_size) || file_size == 0) {
    cli::cli_abort("Plot produced an empty image file — the plot object may not generate visible output.")
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

#' Create graphics device for plot generation
#'
#' Uses httpgd if available, otherwise falls back to standard R graphics devices.
#'
#' @param format Output format: "png", "jpeg", "pdf", or "svg"
#' @param width Width for the device
#' @param height Height for the device
#' @return A list with `type` ("httpgd" or "standard") and `file` (temp file path)
#' @noRd
setup_graphics_device <- function(format = "png", width = 800, height = 600) {
  file_ext <- switch(format,
    "png" = ".png",
    "jpeg" = ".jpg",
    "pdf" = ".pdf",
    "svg" = ".svg"
  )

  tmp <- tempfile(fileext = file_ext)

  if (requireNamespace("httpgd", quietly = TRUE)) {
    httpgd::hgd(width = width, height = height, silent = TRUE)
    return(list(type = "httpgd", file = tmp))
  } else {
    switch(format,
      "png" = grDevices::png(tmp, width = width, height = height),
      "jpeg" = grDevices::jpeg(tmp, width = width, height = height, quality = 90),
      "pdf" = grDevices::pdf(tmp, width = width / 100, height = height / 100),
      "svg" = grDevices::svg(tmp, width = width / 100, height = height / 100)
    )
    return(list(type = "standard", file = tmp))
  }
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
get_plot_data <- function(device_info, format = "png", width = 800, height = 600) {
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
            grDevices::dev.copy(grDevices::png, device_info$file, width = width, height = height)
            grDevices::dev.off()
          },
          "jpeg" = {
            grDevices::dev.copy(grDevices::jpeg, device_info$file, width = width, height = height, quality = 90)
            grDevices::dev.off()
          },
          "pdf" = {
            grDevices::dev.copy(grDevices::pdf, device_info$file, width = width / 100, height = height / 100)
            grDevices::dev.off()
          },
          "svg" = {
            grDevices::dev.copy(grDevices::svg, device_info$file, width = width / 100, height = height / 100)
            grDevices::dev.off()
          }
        )
      }
    )
  } else {
    grDevices::dev.off()
  }

  mime_type <- switch(format,
    "png" = "image/png",
    "jpeg" = "image/jpeg",
    "pdf" = "application/pdf",
    "svg" = "image/svg+xml"
  )

  image_response <- response_image(device_info$file, mime_type)

  base64_content <- sub("^data:[^,]*,", "", image_response$content)
  actual_tokens <- ceiling(nchar(base64_content) / 4)

  image_response$tokens <- actual_tokens

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
generate_optimization_suggestions <- function(current_width, current_height, current_tokens, target_tokens, current_format = "png") {
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
      sprintf("Reduce resolution to %dx%d (saves ~20%% tokens)", new_width, new_height)
    )
  }

  if (current_format == "png" && current_tokens > target_tokens * 0.8) {
    suggestions <- c(suggestions, "Consider JPEG format for ~20% token savings (slightly lower quality)")
  }

  suggestions
}

#' Create an image response in base64 format
#'
#' @param file Path to the image file
#' @param mime_type MIME type of the image (default: "image/png")
#' @return A list with image content in base64 format
#' @noRd
response_image <- function(file, mime_type = "image/png") {
  if (!file.exists(file)) {
    cli::cli_abort("Image file does not exist: {file}")
  }

  list(
    type = "image",
    content = base64enc::dataURI(file = file, mime = mime_type)
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
#' @param mcp_apps_supported Logical; whether the client supports MCP App structuredContent
#' @param target Character; who sees the plot: "user" (display) or "agent" (base64 for analysis)
#' @return Varies by path — text confirmation, structuredContent list, or image response
#' @export
channel_plot <- function(plot_obj, mcp_apps_supported = FALSE,
                         target = c("user", "agent")) {
  target <- match.arg(target)

  # Validate plot_obj class
  valid_classes <- c("gg", "ggplot", "plotly", "htmlwidget", "recordedplot", "grob", "gtable", "trellis", "captured_plot")
  if (!inherits(plot_obj, valid_classes)) {
    cli::cli_abort(
      "Expected a plot object ({paste(valid_classes, collapse = ', ')}), got {paste(class(plot_obj), collapse = '/')}"
    )
  }

  is_interactive <- inherits(plot_obj, c("htmlwidget", "plotly"))

  if (target == "agent") {
    return(.channel_plot_agent(plot_obj))
  }

  # target == "user"
  if (mcp_apps_supported) {
    if (is_interactive) {
      return(show_plotly_via_mcp_app(plot_obj))
    }
    return(show_plot_via_mcp_app(plot_obj))
  }

  # Local rendering
  if (is_interactive) {
    path <- show_widget_in_browser(plot_obj)
    return(list(type = "text", content = sprintf("Interactive widget opened in browser: %s", path)))
  }

  device_channel <- detect_local_device()
  render_result <- render_static_plot(plot_obj, device_channel)
  list(
    type = "text",
    content = switch(render_result$channel,
      httpgd = sprintf("Plot displayed to user via httpgd at %s", render_result$info),
      device = render_result$info,
      file = sprintf("Plot saved to file: %s (headless environment, no display available).", render_result$info)
    )
  )
}

#' Render a plot for agent analysis as base64-encoded image
#'
#' Internal helper called by channel_plot() for agent target. Uses hardcoded
#' defaults (600x450, png, 25000 token limit, 20000 warn threshold).
#'
#' @param plot_obj A plot object
#' @return Image response with base64-encoded plot and optimization metadata
#' @noRd
.channel_plot_agent <- function(plot_obj) {
  width <- 600L
  height <- 450L
  format <- "png"
  token_limit <- 25000
  warn_threshold <- 20000

  tryCatch(
    {
      device_info <- setup_graphics_device(format, width, height)

      # Print the plot object to the open device
      printable_classes <- c("gg", "ggplot", "grob", "gtable", "trellis", "recordedplot", "captured_plot")
      if (inherits(plot_obj, printable_classes)) {
        print(plot_obj)
      }

      image_response <- get_plot_data(device_info, format, width, height)
      actual_tokens <- image_response$tokens

      if (actual_tokens > token_limit) {
        suggestions <- generate_optimization_suggestions(width, height, actual_tokens, token_limit, format)
        error_msg <- sprintf(
          "Plot too large for agent analysis: %s tokens exceeds %s token limit.\nTry these optimizations:\n- %s\nOr use show_plot with target='user' to display directly to the user instead.",
          format(actual_tokens, big.mark = ","),
          format(token_limit, big.mark = ","),
          paste(suggestions, collapse = "\n- ")
        )
        cli::cli_abort(error_msg)
      }

      optimization_warning <- NULL
      if (actual_tokens > warn_threshold) {
        suggestions <- generate_optimization_suggestions(width, height, actual_tokens, warn_threshold, format)
        optimization_warning <- sprintf(
          "WARNING: HIGH TOKEN USAGE: %s tokens (%.1f%% of limit)\nConsider optimizing for better efficiency:\n- %s\nOr use show_plot with target='user' to display directly to the user instead.",
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
        optimization_applied = if (!is.null(optimization_warning)) "Warning issued" else "None",
        token_efficiency = sprintf("%.1f%% of limit used", (actual_tokens / token_limit) * 100)
      )

      if (!is.null(optimization_warning)) {
        image_response$optimization_warning <- optimization_warning
      }

      image_response
    },
    error = function(e) {
      if (grDevices::dev.cur() != 1) {
        try(grDevices::dev.off(), silent = TRUE)
      }
      cli::cli_abort("Error creating plot: {e$message}")
    }
  )
}
