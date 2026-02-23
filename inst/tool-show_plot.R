# Show Plot Tool for MCPR
# Unified plotting tool with target-based routing.
# target="user" (default): prints the plot to the active graphics device for the user to see.
# target="agent": renders to base64 for agent analysis, with token management.

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

#' Create graphics device for plot generation
#'
#' Uses httpgd if available, otherwise falls back to standard R graphics devices
#'
#' @param format Output format: 'png', 'jpeg', 'pdf', or 'svg'
#' @param width Width for the device
#' @param height Height for the device
#' @return Temporary file path where plot will be saved
#' @noRd
setup_graphics_device <- function(format = "png", width = 800, height = 600) {
  # Create temporary file
  file_ext <- switch(format,
    "png" = ".png",
    "jpeg" = ".jpg",
    "pdf" = ".pdf",
    "svg" = ".svg"
  )

  tmp <- tempfile(fileext = file_ext)

  # Try httpgd first, fallback to standard devices
  if (requireNamespace("httpgd", quietly = TRUE)) {
    # Use httpgd if available
    httpgd::hgd(width = width, height = height, silent = TRUE)
    return(list(type = "httpgd", file = tmp))
  } else {
    # Fallback to standard R graphics devices
    switch(format,
      "png" = grDevices::png(tmp, width = width, height = height),
      "jpeg" = grDevices::jpeg(tmp, width = width, height = height, quality = 90),
      "pdf" = grDevices::pdf(tmp, width = width / 100, height = height / 100), # PDF uses inches
      "svg" = grDevices::svg(tmp, width = width / 100, height = height / 100) # SVG uses inches
    )
    return(list(type = "standard", file = tmp))
  }
}

#' Get plot data using appropriate method with token calculation
#'
#' @param device_info Device information from setup_graphics_device
#' @param format Output format
#' @param width Width in pixels
#' @param height Height in pixels
#' @return Image response with base64 encoded plot and token count
#' @noRd
get_plot_data <- function(device_info, format = "png", width = 800, height = 600) {
  if (device_info$type == "httpgd") {
    # Use httpgd rendering
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
        # Fallback: copy httpgd device to file using dev.copy
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
    # Standard device - just close it
    grDevices::dev.off()
  }

  # Determine MIME type
  mime_type <- switch(format,
    "png" = "image/png",
    "jpeg" = "image/jpeg",
    "pdf" = "application/pdf",
    "svg" = "image/svg+xml"
  )

  # Create response with token calculation
  image_response <- response_image(device_info$file, mime_type)

  # Calculate actual tokens from base64 content
  # Extract base64 content from dataURI (format: "data:image/png;base64,ACTUALDATA")
  base64_content <- sub("^data:[^,]*,", "", image_response$content)
  actual_tokens <- ceiling(nchar(base64_content) / 4)

  # Add token information to response
  image_response$tokens <- actual_tokens

  # Clean up
  on.exit(unlink(device_info$file))

  return(image_response)
}


#' Generate Optimization Suggestions
#'
#' Provides specific recommendations to reduce token consumption
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
    # Need major reduction
    suggestions <- c(
      suggestions,
      sprintf("Reduce resolution to 400x300 (saves ~70%% tokens)")
    )
  } else if (reduction_needed > 0.3) {
    # Need moderate reduction
    suggestions <- c(
      suggestions,
      sprintf("Reduce resolution to 600x450 (saves ~40%% tokens)")
    )
  } else {
    # Need minor reduction
    new_width <- round(current_width * 0.9)
    new_height <- round(current_height * 0.9)
    suggestions <- c(
      suggestions,
      sprintf("Reduce resolution to %dx%d (saves ~20%% tokens)", new_width, new_height)
    )
  }

  # Suggest format optimization if using PNG and tokens are high
  if (current_format == "png" && current_tokens > target_tokens * 0.8) {
    suggestions <- c(suggestions, "Consider JPEG format for ~20% token savings (slightly lower quality)")
  }

  return(suggestions)
}

#' Create and display R plots
#'
#' @description Create and display R plots. By default (target='user'), the plot
#' is shown to the user via the active graphics device — the user sees it
#' directly. Set target='agent' when YOU (the agent) need to see and analyze the
#' plot as an image — returns a base64-encoded image optimized for token
#' efficiency. The user does NOT see agent-targeted plots.
#'
#' The width, height, format, token_limit, and warn_threshold parameters only
#' apply when target='agent'. For target='user', the plot is simply printed to
#' the user's active graphics device at its default size.
#'
#' @param expr R code expression to generate the plot
#' @param target Who should see the plot: 'user' (default) prints to the active
#'   graphics device for the user to see; 'agent' returns a base64-encoded image
#'   for agent analysis
#' @param width Width in pixels (agent only, default: 600)
#' @param height Height in pixels (agent only, default: 450)
#' @param format Output format: 'png', 'jpeg', 'pdf', or 'svg' (agent only, default: 'png')
#' @param token_limit Maximum allowed tokens (agent only, default: 25000)
#' @param warn_threshold Token threshold for optimization warnings (agent only, default: 20000)
#' @keywords mcpr_tool
#' @return For target='user': a text confirmation. For target='agent': image
#'   response with base64-encoded plot and optimization metadata.
show_plot <- function(expr, target = "user", width = 600, height = 450, format = "png",
                      token_limit = 25000, warn_threshold = 20000) {
  # Validate inputs
  if (!is.character(expr) || length(expr) != 1) {
    cli::cli_abort("Expression must be a single character string")
  }

  if (nchar(trimws(expr)) == 0) {
    cli::cli_abort("Expression cannot be empty")
  }

  valid_targets <- c("user", "agent")
  if (!target %in% valid_targets) {
    cli::cli_abort("Target must be one of: {paste(valid_targets, collapse = ', ')}")
  }

  if (target == "user") {
    return(show_plot_user(expr))
  }

  # target == "agent"
  show_plot_agent(expr, width, height, format, token_limit, warn_threshold)
}

#' Detect the best available output channel for user-facing plots
#'
#' Checks for httpgd availability, interactive sessions with displays,
#' and falls back to file export for headless environments.
#'
#' @return Character string: "httpgd", "device", or "file"
#' @noRd
detect_output_channel <- function() {
  # 1. httpgd already the active device?
  dev_name <- names(grDevices::dev.cur())
  if (dev_name %in% c("httpgd", "unigd")) return("httpgd")

  # 2. httpgd available but not active? Start it.
  if (requireNamespace("httpgd", quietly = TRUE)) return("httpgd")

  # 3. Interactive with a display?
  if (interactive()) return("device")

  # 4. Headless fallback
  return("file")
}

#' Display a plot to the user via the best available channel
#'
#' Routes through httpgd (with browser), active graphics device, or
#' file export depending on the environment.
#'
#' @param expr R code expression to generate the plot
#' @return A text confirmation message
#' @noRd
show_plot_user <- function(expr) {
  channel <- detect_output_channel()

  tryCatch(
    switch(channel,
      httpgd = show_plot_via_httpgd(expr),
      device = show_plot_via_device(expr),
      file   = show_plot_via_file(expr)
    ),
    error = function(e) {
      cli::cli_abort("Error displaying plot: {e$message}")
    }
  )
}

#' Display a plot via httpgd and open in the browser
#'
#' Starts an httpgd device if needed, evaluates the plot expression,
#' and opens the httpgd viewer in the user's default browser.
#' If httpgd fails to start (e.g., memory constraints in MCP sessions),
#' falls back to device or file output.
#'
#' @param expr R code expression to generate the plot
#' @return A text confirmation message
#' @noRd
show_plot_via_httpgd <- function(expr) {
  dev_name <- names(grDevices::dev.cur())
  already_active <- dev_name %in% c("httpgd", "unigd")

  if (!already_active) {
    started <- tryCatch(
      { httpgd::hgd(silent = TRUE); TRUE },
      error = function(e) FALSE
    )
    if (!started) {
      # httpgd failed to start — fall back
      if (interactive()) return(show_plot_via_device(expr))
      return(show_plot_via_file(expr))
    }
  }

  result <- eval(parse(text = expr), envir = .GlobalEnv)
  if (inherits(result, c("gg", "ggplot", "grob", "gtable", "trellis", "recordedplot", "htmlwidget", "plotly"))) {
    print(result)
  }

  url <- httpgd::hgd_url()

  # Open in browser so the user can see it
  if (!already_active) {
    httpgd::hgd_browse()
  }

  list(
    type = "text",
    content = sprintf("Plot displayed to user via httpgd at %s", url)
  )
}

#' Display a plot via the active graphics device (print)
#'
#' @param expr R code expression to generate the plot
#' @return A text confirmation message
#' @noRd
show_plot_via_device <- function(expr) {
  result <- eval(parse(text = expr), envir = .GlobalEnv)
  if (inherits(result, c("gg", "ggplot", "grob", "gtable", "trellis", "recordedplot", "htmlwidget", "plotly"))) {
    print(result)
  }

  list(
    type = "text",
    content = "Plot displayed to user via active graphics device."
  )
}

#' Save a plot to a temp file for headless environments
#'
#' @param expr R code expression to generate the plot
#' @return A text confirmation with the file path
#' @noRd
show_plot_via_file <- function(expr) {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 800, height = 600)
  on.exit(grDevices::dev.off())

  result <- eval(parse(text = expr), envir = .GlobalEnv)
  if (inherits(result, c("gg", "ggplot", "grob", "gtable", "trellis", "recordedplot", "htmlwidget", "plotly"))) {
    print(result)
  }

  list(
    type = "text",
    content = sprintf("Plot saved to file: %s (headless environment, no display available).", tmp)
  )
}

#' Render a plot for agent analysis as base64-encoded image
#'
#' @param expr R code expression to generate the plot
#' @param width Width in pixels
#' @param height Height in pixels
#' @param format Output format
#' @param token_limit Maximum allowed tokens
#' @param warn_threshold Token threshold for optimization warnings
#' @return Image response with base64-encoded plot and optimization metadata
#' @noRd
show_plot_agent <- function(expr, width = 600, height = 450, format = "png",
                            token_limit = 25000, warn_threshold = 20000) {
  # Validate format
  valid_formats <- c("png", "jpeg", "pdf", "svg")
  if (!format %in% valid_formats) {
    cli::cli_abort("Format must be one of: {paste(valid_formats, collapse = ', ')}")
  }

  # Validate dimensions
  if (!is.numeric(width) || width <= 0) {
    cli::cli_abort("Width must be a positive number")
  }
  if (!is.numeric(height) || height <= 0) {
    cli::cli_abort("Height must be a positive number")
  }

  # Convert to integer
  width <- as.integer(width)
  height <- as.integer(height)

  tryCatch(
    {
      # Set up graphics device (httpgd if available, standard otherwise)
      device_info <- setup_graphics_device(format, width, height)

      # Execute the plotting code
      result <- eval(parse(text = expr), envir = .GlobalEnv)

      # Get the plot data with actual token count
      image_response <- get_plot_data(device_info, format, width, height)
      actual_tokens <- image_response$tokens

      # Check if plot exceeds token limits
      if (actual_tokens > token_limit) {
        suggestions <- generate_optimization_suggestions(width, height, actual_tokens, token_limit, format)

        error_msg <- sprintf(
          "Plot too large for agent analysis: %s tokens exceeds %s token limit.
          Try these optimizations:
          - %s
          Or use show_plot with target='user' to display directly to the user instead.",
          format(actual_tokens, big.mark = ","),
          format(token_limit, big.mark = ","),
          paste(suggestions, collapse = "\n- ")
        )

        cli::cli_abort(error_msg)
      }

      # Generate warning for high token usage
      optimization_warning <- NULL
      if (actual_tokens > warn_threshold) {
        suggestions <- generate_optimization_suggestions(width, height, actual_tokens, warn_threshold, format)

        optimization_warning <- sprintf(
          "WARNING: HIGH TOKEN USAGE: %s tokens (%.1f%% of limit)
        Consider optimizing for better efficiency:
        - %s
        Or use show_plot with target='user' to display directly to the user instead.",
          format(actual_tokens, big.mark = ","),
          (actual_tokens / token_limit) * 100,
          paste(suggestions, collapse = "\n- ")
        )

        cli::cli_warn(optimization_warning)
      }

      # Add optimization metadata to response
      image_response$metadata <- list(
        actual_tokens = actual_tokens,
        dimensions = paste0(width, "x", height),
        format = format,
        optimization_applied = if (!is.null(optimization_warning)) "Warning issued" else "None",
        token_efficiency = sprintf("%.1f%% of limit used", (actual_tokens / token_limit) * 100)
      )

      # Add warning to response if applicable
      if (!is.null(optimization_warning)) {
        image_response$optimization_warning <- optimization_warning
      }

      return(image_response)
    },
    error = function(e) {
      # Clean up any active devices on error
      if (grDevices::dev.cur() != 1) {
        try(grDevices::dev.off(), silent = TRUE)
      }
      cli::cli_abort("Error creating plot: {e$message}")
    }
  )
}

#' @export
show_plot <- show_plot
