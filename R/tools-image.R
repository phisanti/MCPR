# This file contains tools that give AI agents the ability to create and "see"
# images, plots, and other graphical output from R.

# --- Image/Plotting Tools ---

#' Render an R plot from code and return it as an image
#'
#' @description
#' Executes R code that generates a plot and captures the output as an image.
#'
#' @param expr `character` R code expression to generate the plot.
#' @param width `integer` Width of the output image in pixels.
#' @param height `integer` Height of the output image in pixels.
#' @param device `character` Graphics device to use ('png', 'jpeg', 'pdf', 'svg').
#' @return A list representing the image in a format the client can render.
#' @export
render_plot <- function(expr, width = 800, height = 600, device = "png") {
  if (!is.character(expr) || length(expr) != 1 || nchar(trimws(expr)) == 0) {
    stop("Expression must be a non-empty character string.")
  }
  valid_devices <- c("png", "jpeg", "pdf", "svg")
  if (!device %in% valid_devices) {
    stop("Device must be one of: ", paste(valid_devices, collapse = ", "))
  }

  file_ext <- switch(device, "jpeg" = ".jpg", paste0(".", device))
  tmp <- tempfile(fileext = file_ext)
  on.exit(unlink(tmp), add = TRUE)

  tryCatch({
    device_call <- switch(device,
      "png"  = grDevices::png(tmp, width = width, height = height),
      "jpeg" = grDevices::jpeg(tmp, width = width, height = height, quality = 90),
      "pdf"  = grDevices::pdf(tmp, width = width/100, height = height/100),
      "svg"  = grDevices::svg(tmp, width = width/100, height = height/100)
    )
    on.exit(grDevices::dev.off(), add = TRUE)
    
    eval(parse(text = expr), envir = .GlobalEnv)
    grDevices::dev.off()
    on.exit(NULL) # remove dev.off()

    response_image(tmp, mime_type = switch(device,
      "png" = "image/png", "jpeg" = "image/jpeg",
      "pdf" = "application/pdf", "svg" = "image/svg+xml"
    ))
  }, error = function(e) {
    stop("Error rendering plot: ", e$message)
  })
}

#' Load and view an existing image file
#'
#' @param file_path `character` Path to the image file to load.
#' @return A list representing the image.
#' @export
view_image <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("Image file does not exist: ", file_path)
  }
  response_image(file_path)
}

#' Capture the current plot from the active R graphics device
#'
#' @param width `integer` Width of the output image in pixels.
#' @param height `integer` Height of the output image in pixels.
#' @param device `character` Graphics device to use.
#' @return A list representing the captured image.
#' @export
capture_current_plot <- function(width = 800, height = 600, device = "png") {
  if (grDevices::dev.cur() == 1) {
    stop("No active graphics device found. Create a plot first.")
  }
  
  file_ext <- switch(device, "jpeg" = ".jpg", paste0(".", device))
  tmp <- tempfile(fileext = file_ext)
  on.exit(unlink(tmp), add = TRUE)

  tryCatch({
    grDevices::dev.copy(
      device = switch(device,
        "png" = grDevices::png, "jpeg" = grDevices::jpeg,
        "pdf" = grDevices::pdf, "svg" = grDevices::svg
      ),
      filename = tmp,
      width = width,
      height = height
    )
    grDevices::dev.off()
    response_image(tmp)
  }, error = function(e) {
    stop("Error capturing plot: ", e$message)
  })
}

# --- Helpers ---

#' Create an image response object
#'
#' @param file Path to the image file.
#' @param mime_type The MIME type of the image. If NULL, it's inferred.
#' @return A list with the base64-encoded image content.
response_image <- function(file, mime_type = NULL) {
  if (is.null(mime_type)) {
    mime_type <- switch(tolower(tools::file_ext(file)),
      "png" = "image/png", "jpg" = "image/jpeg", "jpeg" = "image/jpeg",
      "gif" = "image/gif", "svg" = "image/svg+xml", "pdf" = "application/pdf",
      "bmp" = "image/bmp", "tiff" = "image/tiff", "image/png" # Default
    )
  }
  list(
    type = "image",
    content = base64enc::dataURI(file = file, mime = mime_type)
  )
}

# --- Ellmer Tool Definitions ---

render_plot_tool <- ellmer::tool(
  .fun = render_plot,
  .description = "Render an R plot from code and return it as an image for viewing.",
  expr = ellmer::type_string("R code expression to generate the plot.", required = TRUE),
  width = ellmer::type_integer("Width of the output image in pixels (default: 800)."),
  height = ellmer::type_integer("Height of the output image in pixels (default: 600)."),
  device = ellmer::type_string("Graphics device: 'png', 'jpeg', 'pdf', or 'svg' (default: 'png').")
)

view_image_tool <- ellmer::tool(
  .fun = view_image,
  .description = "Load and view an existing image file from the file system.",
  file_path = ellmer::type_string("Path to the image file to load and view.", required = TRUE)
)

capture_current_plot_tool <- ellmer::tool(
  .fun = capture_current_plot,
  .description = "Capture and view the current plot from the active R graphics device.",
  width = ellmer::type_integer("Width of the output image in pixels (default: 800)."),
  height = ellmer::type_integer("Height of the output image in pixels (default: 600)."),
  device = ellmer::type_string("Graphics device: 'png', 'jpeg', 'pdf', or 'svg' (default: 'png').")
)
