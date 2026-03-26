# Plot Rendering Primitives
# Pure rendering functions for plot device management, widget display, and static plot capture.
# Called from inst/tool-show_plot.R via MCPR::: — no MCP awareness; uses cli only for errors.

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

#' Evaluate an R expression string in .GlobalEnv
#'
#' Shared evaluator used by both MCP App and local rendering paths.
#' Returns the result object so callers can inspect its class.
#'
#' @param expr Character string containing an R expression
#' @return The result of evaluating the expression
#' @noRd
eval_plot_expr <- function(expr) {
  eval(parse(text = expr), envir = .GlobalEnv)
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

#' Render a static plot on a local device
#'
#' Encapsulates the prepare/eval/print/finalize cycle for static plots.
#' Handles httpgd, native device, and file export paths. When `result` is
#' provided (pre-evaluated), reuses it for printable objects (gg, grob, etc.)
#' to avoid double evaluation. Side-effect-only plots (base R) are re-evaluated
#' on the target device since the side effect IS the plot.
#'
#' @param expr Character string containing the plot expression
#' @param device_channel One of "httpgd", "device", or "file"
#' @param result Optional pre-evaluated result from a prior eval_plot_expr() call.
#'   If a printable object, it will be printed on the target device without re-eval.
#'   If NULL (default), the expression is evaluated fresh.
#' @return A list with `channel` (character) and `info` (URL, path, or confirmation)
#' @noRd
render_static_plot <- function(expr, device_channel, result = NULL) {
  printable_classes <- c("gg", "ggplot", "grob", "gtable", "trellis", "recordedplot")

  # show_plot_local() evals once for type detection (widget vs static), then
  # passes the result here. Printable objects (gg, grob, etc.) are reused via
  # print() — no re-eval needed. Side-effect-only plots (base R plot() returns
  # NULL) must be re-evaluated on the target device since the draw IS the side effect.
  needs_eval <- !inherits(result, printable_classes)

  switch(device_channel,
    httpgd = {
      dev_name <- names(grDevices::dev.cur())
      already_active <- dev_name %in% c("httpgd", "unigd")

      if (!already_active) {
        httpgd::hgd(silent = TRUE)
      }

      if (needs_eval) {
        result <- eval_plot_expr(expr)
      }
      if (inherits(result, printable_classes)) {
        print(result)
      }

      url <- httpgd::hgd_url()
      if (!already_active) {
        httpgd::hgd_browse()
      }

      list(channel = "httpgd", info = url)
    },

    device = {
      if (needs_eval) {
        result <- eval_plot_expr(expr)
      }
      if (inherits(result, printable_classes)) {
        print(result)
      }

      list(channel = "device", info = "Plot displayed on active graphics device.")
    },

    file = {
      tmp <- tempfile(fileext = ".png")
      grDevices::png(tmp, width = 800, height = 600)
      on.exit(grDevices::dev.off(), add = TRUE)

      if (needs_eval) {
        result <- eval_plot_expr(expr)
      }
      if (inherits(result, printable_classes)) {
        print(result)
      }

      list(channel = "file", info = tmp)
    }
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
