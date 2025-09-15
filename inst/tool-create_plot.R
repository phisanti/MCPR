# Create Plot Tool for MCPR
# Unified plotting tool using httpgd backend for optimal performance and simplicity.
# Handles the primary data analyst workflow of creating and viewing R plots efficiently.

#' Create an image response in base64 format
#'
#' @param file Path to the image file
#' @param mime_type MIME type of the image (default: "image/png")
#' @return A list with image content in base64 format
#' @noRd
response_image <- function(file, mime_type = "image/png") {
  if (!file.exists(file)) {
    stop("Image file does not exist: ", file)
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

#' Create plots for agent analysis and inspection
#'
#' @description IMPORTANT: This tool is designed for AI agents to analyze plot contents,
#' NOT for displaying plots to users.
#' USE THIS TOOL WHEN:
#' - You need to see and analyze plot data visually
#' - Debugging plot generation issues
#' - Inspecting plot contents for data validation
#'
#' FOR USER-FACING PLOTS, USE INSTEAD:
#' - execute_r_code("print(your_plot)")  # Shows plot to user
#' - execute_r_code("ggsave('plot.png', plot)")  # Saves for artifacts
#'
#' This tool optimizes for token efficiency with agent-friendly defaults
#' (600x450px) and includes automatic size management for MCP protocol limits.
#'
#' @param expr R code expression to generate the plot
#' @param width Width in pixels (default: 600, optimized for agent analysis)
#' @param height Height in pixels (default: 450, optimized for agent analysis)
#' @param format Output format: 'png', 'jpeg', 'pdf', or 'svg' (default: 'png')
#' @param token_limit Maximum allowed tokens (default: 25000, MCP protocol limit)
#' @param warn_threshold Token threshold for optimization warnings (default: 20000)
#' @keywords mcpr_tool
#' @return Image response with the created plot, includes optimization metadata
create_plot <- function(expr, width = 600, height = 450, format = "png",
                        token_limit = 25000, warn_threshold = 20000) {
  # Validate inputs
  if (!is.character(expr) || length(expr) != 1) {
    stop("Expression must be a single character string")
  }

  if (nchar(trimws(expr)) == 0) {
    stop("Expression cannot be empty")
  }

  # Validate format
  valid_formats <- c("png", "jpeg", "pdf", "svg")
  if (!format %in% valid_formats) {
    stop("Format must be one of: ", paste(valid_formats, collapse = ", "))
  }

  # Validate dimensions
  if (!is.numeric(width) || width <= 0) {
    stop("Width must be a positive number")
  }
  if (!is.numeric(height) || height <= 0) {
    stop("Height must be a positive number")
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
          "Plot analysis failed: %s tokens exceeds %s token limit.
          REMINDER: This tool is for agent analysis only.
          For user-facing plots, use: execute_r_code('print(your_plot)')
          If you need this plot for analysis, try these optimizations
          - %s
          Note: Agent analysis plots prioritize token efficiency over visual quality.",
          format(actual_tokens, big.mark = ","),
          format(token_limit, big.mark = ","),
          paste(suggestions, collapse = "\n- ")
        )

        stop(error_msg)
      }

      # Generate warning for high token usage
      optimization_warning <- NULL
      if (actual_tokens > warn_threshold) {
        suggestions <- generate_optimization_suggestions(width, height, actual_tokens, warn_threshold, format)

        optimization_warning <- sprintf(
          "WARNING: HIGH TOKEN USAGE: %s tokens (%.1f%% of limit)
        Agent Analysis Mode: Consider optimizing for better efficiency:
        - %s
        For User Display: Use execute_r_code('print(plot)') instead",
          format(actual_tokens, big.mark = ","),
          (actual_tokens / token_limit) * 100,
          paste(suggestions, collapse = "\n- ")
        )

        # Print warning to console for user awareness
        message(optimization_warning)
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
      stop("Error creating plot: ", e$message)
    }
  )
}

#' @export
create_plot <- create_plot
