#' Capture Current Plot Tool for MCPR
#'
#' This file defines the capture_current_plot tool that allows AI agents to
#' capture whatever is currently displayed in the R graphics device.

#' Create an image response in base64 format
#'
#' @param file Path to the image file
#' @param mime_type MIME type of the image (default: "image/png")
#' @return A list with image content in base64 format
response_image <- function(file, mime_type = "image/png") {
  if (!file.exists(file)) {
    stop("Image file does not exist: ", file)
  }
  
  list(
    type = "image",
    content = base64enc::dataURI(file = file, mime = mime_type)
  )
}

#* @mcp_tool  
#* @description Capture and view the current plot from the active R graphics device. This tool is useful when you have already created a plot in R and want to view it without re-running the plotting code. It captures whatever is currently displayed in the graphics window and returns it as an image.
#* @param width integer Width of the output image in pixels (default: 800)
#* @param height integer Height of the output image in pixels (default: 600)
#* @param device character Graphics device: 'png', 'jpeg', 'pdf', or 'svg' (default: 'png')
#' Capture the current plot and return as image
#'
#' @param width Width of the output image in pixels (default: 800)
#' @param height Height of the output image in pixels (default: 600)
#' @param device Graphics device to use (default: "png")
#' @return Image response with the current plot
capture_current_plot <- function(width = 800, height = 600, device = "png") {
  # Check if there's an active graphics device
  if (dev.cur() == 1) {
    stop("No active graphics device found. Create a plot first.")
  }
  
  # Create temporary file
  file_ext <- switch(device,
    "png" = ".png",
    "jpeg" = ".jpg",
    "pdf" = ".pdf", 
    "svg" = ".svg"
  )
  
  tmp <- tempfile(fileext = file_ext)
  
  tryCatch({
    # Copy current device to file
    switch(device,
      "png" = {
        dev.copy(png, tmp, width = width, height = height)
        dev.off()
      },
      "jpeg" = {
        dev.copy(jpeg, tmp, width = width, height = height, quality = 90)
        dev.off()
      },
      "pdf" = {
        dev.copy(pdf, tmp, width = width/100, height = height/100)
        dev.off()
      },
      "svg" = {
        dev.copy(svg, tmp, width = width/100, height = height/100)
        dev.off()
      }
    )
    
    # Determine MIME type
    mime_type <- switch(device,
      "png" = "image/png",
      "jpeg" = "image/jpeg", 
      "pdf" = "application/pdf",
      "svg" = "image/svg+xml"
    )
    
    # Return the image
    image_response <- response_image(tmp, mime_type)
    
    # Clean up
    on.exit(unlink(tmp))
    
    return(image_response)
    
  }, error = function(e) {
    if (file.exists(tmp)) unlink(tmp)
    stop("Error capturing plot: ", e$message)
  })
}

# LEGACY ELLMER TOOL DEFINITION (commented out for Phase 2 migration)
# This will be removed in Phase 3 after full migration validation
#
# #' Capture Current Plot Tool
# #' 
# #' Capture whatever is currently displayed in the R graphics device
# capture_current_plot_tool <- ellmer::tool(
#   .fun = capture_current_plot,
#   .description = paste(
#     "Capture and view the current plot from the active R graphics device.",
#     "This tool is useful when you have already created a plot in R and want",
#     "to view it without re-running the plotting code. It captures whatever",
#     "is currently displayed in the graphics window and returns it as an image."
#   ),
#   width = ellmer::type_integer(
#     "Width of the output image in pixels (default: 800)",
#     required = FALSE
#   ),
#   height = ellmer::type_integer(
#     "Height of the output image in pixels (default: 600)",
#     required = FALSE
#   ),
#   device = ellmer::type_string(
#     "Graphics device: 'png', 'jpeg', 'pdf', or 'svg' (default: 'png')",
#     required = FALSE
#   )
# )

#' @export
capture_current_plot <- capture_current_plot
