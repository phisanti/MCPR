#' View Image Tool for MCPR
#'
#' This file defines the view_image tool that allows AI agents to load
#' and view existing image files from the file system.

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
#* @description Load and view an existing image file from the file system. This tool allows you to examine images that already exist on disk, such as plots saved from previous analysis, photos, diagrams, or any other image files. Supports common image formats like PNG, JPEG, GIF, SVG, PDF, BMP, and TIFF. Provide the full or relative path to the image.
#* @param file_path character Path to the image file to load and view
#' Load and view an existing image file
#'
#' @param file_path Path to the image file to load
#' @return Image response with the loaded image
view_image <- function(file_path) {
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("File path must be a single character string")
  }
  
  if (!file.exists(file_path)) {
    stop("Image file does not exist: ", file_path)
  }
  
  # Detect MIME type from file extension
  file_ext <- tolower(tools::file_ext(file_path))
  mime_type <- switch(file_ext,
    "png" = "image/png",
    "jpg" = "image/jpeg",
    "jpeg" = "image/jpeg",
    "gif" = "image/gif",
    "svg" = "image/svg+xml",
    "pdf" = "application/pdf",
    "bmp" = "image/bmp",
    "tiff" = "image/tiff",
    "tif" = "image/tiff",
    # Default to PNG if unknown
    "image/png"
  )
  
  response_image(file_path, mime_type)
}

# LEGACY ELLMER TOOL DEFINITION (commented out for Phase 2 migration)
# This will be removed in Phase 3 after full migration validation
#
# #' View Image File Tool
# #' 
# #' Load and view existing image files from the file system
# view_image_tool <- ellmer::tool(
#   .fun = view_image,
#   .description = paste(
#     "Load and view an existing image file from the file system.",
#     "This tool allows you to examine images that already exist on disk,",
#     "such as plots saved from previous analysis, photos, diagrams, or",
#     "any other image files. Supports common image formats like PNG, JPEG,",
#     "GIF, SVG, PDF, BMP, and TIFF. Provide the full or relative path to the image."
#   ),
#   file_path = ellmer::type_string(
#     "Path to the image file to load and view",
#     required = TRUE
#   )
# )

#' @export
view_image <- view_image
