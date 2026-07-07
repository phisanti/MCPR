# JSON to R Object Reconstruction
# Functions for reconstructing R objects from JSON data with type restoration.
# Reverses MCP serialization process to restore original R object semantics.

.mcpr_json_scalar <- function(x) {
  if (is.list(x) && length(x) == 1) {
    return(x[[1]])
  }
  x
}

.mcpr_json_vector <- function(x) {
  if (is.list(x)) {
    return(unlist(x))
  }
  x
}

.mcpr_json_type <- function(obj) {
  mcp_type <- obj[["_mcp_type"]]
  .mcpr_json_scalar(mcp_type)
}

.mcpr_reconstruct_matrix <- function(obj) {
  data <- .mcpr_json_vector(obj$data)
  dims <- .mcpr_json_vector(obj$dim)
  mat <- matrix(data, nrow = dims[1], ncol = dims[2])
  if (!is.null(obj$dimnames)) {
    dimnames(mat) <- obj$dimnames
  }
  mat
}

.mcpr_reconstruct_array <- function(obj) {
  data <- .mcpr_json_vector(obj$data)
  dims <- .mcpr_json_vector(obj$dim)
  arr <- array(data, dim = dims)
  if (!is.null(obj$dimnames)) {
    dimnames(arr) <- obj$dimnames
  }
  arr
}

.mcpr_reconstruct_data_frame <- function(obj, reconstruct) {
  obj[["_mcp_type"]] <- NULL
  obj[["_mcp_nrow"]] <- NULL
  cols <- lapply(obj, reconstruct)
  cols <- lapply(cols, .mcpr_simplify_data_frame_column)
  as.data.frame(cols)
}

.mcpr_simplify_data_frame_column <- function(col) {
  if (is.list(col) && !is.data.frame(col)) {
    scalar_items <- vapply(col, function(item) {
      is.null(item) || (!is.list(item) && length(item) <= 1)
    }, logical(1))
    if (all(scalar_items)) {
      return(unlist(col, recursive = FALSE, use.names = FALSE))
    }
  }
  col
}

.mcpr_reconstruct_s3 <- function(obj, reconstruct) {
  mcp_class <- obj[["_mcp_class"]]
  obj[["_mcp_type"]] <- NULL
  obj[["_mcp_class"]] <- NULL
  result <- lapply(obj, reconstruct)
  class(result) <- mcp_class
  result
}

.mcpr_reconstruct_special_numeric <- function(obj) {
  val <- obj$value
  if (identical(val, "Inf")) {
    return(Inf)
  }
  if (identical(val, "-Inf")) {
    return(-Inf)
  }
  if (identical(val, "NaN")) {
    return(NaN)
  }
  as.numeric(val)
}

.mcpr_reconstruct_numeric_vector_special <- function(obj) {
  values <- obj$values
  result <- numeric(length(values))
  for (i in seq_along(values)) {
    result[i] <- .mcpr_reconstruct_numeric_item(values[[i]])
  }
  result
}

.mcpr_reconstruct_numeric_item <- function(value) {
  if (is.null(value)) {
    return(NA)
  }
  if (identical(value, "Inf")) {
    return(Inf)
  }
  if (identical(value, "-Inf")) {
    return(-Inf)
  }
  if (identical(value, "NaN")) {
    return(NaN)
  }
  as.numeric(value)
}

.mcpr_reconstruct_date <- function(obj) {
  values <- .mcpr_json_vector(obj$values)
  as.Date(values)
}

.mcpr_reconstruct_posixct <- function(obj) {
  values <- .mcpr_json_vector(obj$values)
  result <- as.POSIXct(values, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  if (!is.null(obj$timezone) && obj$timezone != "UTC") {
    attr(result, "tzone") <- obj$timezone
  }
  result
}

.mcpr_reconstruct_raw <- function(obj) {
  data <- .mcpr_json_scalar(obj$data)
  jsonlite::base64_dec(data)
}

.mcpr_reconstruct_formula <- function(obj) {
  formula_str <- .mcpr_json_scalar(obj$formula)
  stats::as.formula(formula_str)
}

.mcpr_reconstruct_environment_marker <- function(obj) {
  structure(list(name = obj$name), class = "mcp_environment_marker")
}

.mcpr_reconstruct_plot_marker <- function(obj) {
  structure(list(
    format = obj$format,
    plot_type = obj$plot_type,
    data = obj$data
  ), class = "mcp_plot_marker")
}

.mcpr_reconstruct_structural_marker <- function(obj, mcp_type, reconstruct) {
  switch(mcp_type,
    matrix = .mcpr_reconstruct_matrix(obj),
    array = .mcpr_reconstruct_array(obj),
    factor = factor(obj$values, levels = obj$levels),
    json_object = reconstruct(obj$value %||% list()),
    json_array = reconstruct(obj$value %||% list()),
    data.frame = .mcpr_reconstruct_data_frame(obj, reconstruct)
  )
}

.mcpr_reconstruct_object_marker <- function(obj, mcp_type, reconstruct) {
  switch(mcp_type,
    S3 = .mcpr_reconstruct_s3(obj, reconstruct),
    S4 = obj
  )
}

.mcpr_reconstruct_atomic_marker <- function(obj, mcp_type) {
  switch(mcp_type,
    special_numeric = .mcpr_reconstruct_special_numeric(obj),
    numeric_vector_special = .mcpr_reconstruct_numeric_vector_special(obj),
    Date = .mcpr_reconstruct_date(obj),
    POSIXct = .mcpr_reconstruct_posixct(obj),
    complex = complex(real = obj$real, imaginary = obj$imaginary),
    raw = .mcpr_reconstruct_raw(obj)
  )
}

.mcpr_reconstruct_marker_object <- function(obj, mcp_type) {
  switch(mcp_type,
    formula = .mcpr_reconstruct_formula(obj),
    language = parse(text = obj$expression)[[1]],
    environment = .mcpr_reconstruct_environment_marker(obj),
    plot = .mcpr_reconstruct_plot_marker(obj),
    large_object = structure(obj, class = "mcp_large_object_marker")
  )
}

.mcpr_reconstruct_mcp_marker <- function(obj, mcp_type, reconstruct) {
  if (mcp_type %in% c("matrix", "array", "factor", "json_object", "json_array", "data.frame")) {
    return(.mcpr_reconstruct_structural_marker(obj, mcp_type, reconstruct))
  }
  if (mcp_type %in% c("S3", "S4")) {
    return(.mcpr_reconstruct_object_marker(obj, mcp_type, reconstruct))
  }
  if (mcp_type %in% c("special_numeric", "numeric_vector_special", "Date", "POSIXct", "complex", "raw")) {
    return(.mcpr_reconstruct_atomic_marker(obj, mcp_type))
  }
  if (mcp_type %in% c("formula", "language", "environment", "plot", "large_object")) {
    return(.mcpr_reconstruct_marker_object(obj, mcp_type))
  }
  NULL
}

.mcpr_reconstruct_json_object <- function(obj) {
  if (is.null(obj)) {
    return(NULL)
  }

  if (!is.list(obj)) {
    return(obj)
  }

  mcp_type <- .mcpr_json_type(obj)
  if (!is.null(mcp_type)) {
    reconstructed <- .mcpr_reconstruct_mcp_marker(obj, mcp_type, .mcpr_reconstruct_json_object)
    if (!is.null(reconstructed)) {
      return(reconstructed)
    }
  }

  lapply(obj, .mcpr_reconstruct_json_object)
}

#' Convert JSON Data Back to R Objects
#'
#' @include utils.R
#' @title Convert JSON Data Back to R Objects
#' @description Reconstructs R objects from JSON data created with to_mcpr_json function.
#' Preserves comprehensive type information including dates, factors, matrices, and
#' special R types through reverse conversion pipeline. Enables faithful restoration
#' of R object semantics from MCP protocol transmission.
#'
#' @param json JSON string or already parsed JSON data
#' @return R object reconstructed with preserved type information
#'
#' @details
#' This function reverses the conversion done by \code{to_mcpr_json}, reconstructing:
#' \itemize{
#'   \item Special numeric values (Inf, -Inf, NaN)
#'   \item Date and POSIXct objects with timezones
#'   \item Factors with original levels
#'   \item Matrices and arrays with dimensions
#'   \item Data frames
#'   \item S3 objects with class information
#'   \item Complex numbers
#'   \item Raw vectors from base64
#'   \item Formulas and language objects
#' }
#'
#' Note: Environments cannot be reconstructed and are replaced with marker objects.
#'
#' @examples
#' # Simple JSON string
#' json_str <- '{"a": 1, "b": ["hello", "world"]}'
#' from_mcpr_json(json_str)
#'
#' # Round-trip conversion
#' original <- list(
#'   date = Sys.Date(),
#'   values = c(1, 2, Inf),
#'   factor = factor(c("a", "b", "a"))
#' )
#' json <- mcpr_serialize(original)
#' reconstructed <- from_mcpr_json(json)
#' @noRd
from_mcpr_json <- function(json) {
  if (is.character(json) && length(json) == 1) {
    x <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  } else {
    x <- json
  }

  .mcpr_reconstruct_json_object(x)
}
