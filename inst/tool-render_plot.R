#' Render Plot Tool for MCPR
#'
#' This file defines the render_plot tool that allows AI agents to create
#' plots from R code and return them as images for viewing.

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
#* @description Render an R plot from code and return it as an image for viewing. This tool allows you to create data visualizations, statistical plots, and graphics using R code. The plot will be generated and returned as an image that you can see and analyze. Use standard R plotting functions like plot(), ggplot2, or any other graphics package. Examples: plot(1:10), hist(rnorm(1000)), ggplot(mtcars, aes(x=mpg, y=hp)) + geom_point()
#* @param expr character R code expression to generate the plot. Use standard R plotting syntax.
#* @param width integer Width of the output image in pixels (default: 800)
#* @param height integer Height of the output image in pixels (default: 600)
#* @param device character Graphics device: 'png', 'jpeg', 'pdf', or 'svg' (default: 'png')
#' Render an R plot and return it as an image
#'
#' @param expr R expression or code to generate a plot
#' @param width Width of the output image in pixels (default: 800)
#' @param height Height of the output image in pixels (default: 600)
#' @param device Graphics device to use ("png", "jpeg", "pdf", "svg")
#' @return Image response with the rendered plot
render_plot <- function(expr, width = 800, height = 600, device = "png") {
  # Validate inputs
  if (!is.character(expr) || length(expr) != 1) {
    stop("Expression must be a single character string")
  }
  
  if (nchar(trimws(expr)) == 0) {
    stop("Expression cannot be empty")
  }
  
  # Validate device
  valid_devices <- c("png", "jpeg", "pdf", "svg")
  if (!device %in% valid_devices) {
    stop("Device must be one of: ", paste(valid_devices, collapse = ", "))
  }
  
  # Create temporary file with appropriate extension
  file_ext <- switch(device,
    "png" = ".png",
    "jpeg" = ".jpg", 
    "pdf" = ".pdf",
    "svg" = ".svg"
  )
  
  tmp <- tempfile(fileext = file_ext)
  
  # Set up graphics device
  tryCatch({
    switch(device,
      "png" = png(tmp, width = width, height = height),
      "jpeg" = jpeg(tmp, width = width, height = height, quality = 90),
      "pdf" = pdf(tmp, width = width/100, height = height/100), # PDF uses inches
      "svg" = svg(tmp, width = width/100, height = height/100)  # SVG uses inches
    )
    
    on.exit({
      if (dev.cur() != 1) dev.off()
    })
    
    # Execute the plotting code
    result <- eval(parse(text = expr), envir = .GlobalEnv)
    
    # Close the device
    dev.off()
    on.exit(NULL)
    
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
    # Clean up on error
    if (dev.cur() != 1) dev.off()
    if (file.exists(tmp)) unlink(tmp)
    stop("Error rendering plot: ", e$message)
  })
}

# LEGACY ELLMER TOOL DEFINITION (commented out for Phase 2 migration)
# This will be removed in Phase 3 after full migration validation
#
# #' Render R Plot Tool
# #' 
# #' Creates plots from R code and returns them as images that AI agents can view
# render_plot_tool <- ellmer::tool(
#   .fun = render_plot,
#   .description = paste(
#     "Render an R plot from code and return it as an image for viewing.",
#     "This tool allows you to create data visualizations, statistical plots,",
#     "and graphics using R code. The plot will be generated and returned as",
#     "an image that you can see and analyze. Use standard R plotting functions",
#     "like plot(), ggplot2, or any other graphics package.",
#     "Examples: plot(1:10), hist(rnorm(1000)), ggplot(mtcars, aes(x=mpg, y=hp)) + geom_point()"
#   ),
#   expr = ellmer::type_string(
#     "R code expression to generate the plot. Use standard R plotting syntax.",
#     required = TRUE
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
render_plot <- render_plot
