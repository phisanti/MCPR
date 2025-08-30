# JSON to R Object Reconstruction
# Functions for reconstructing R objects from JSON data with type restoration.
# Reverses MCP serialization process to restore original R object semantics.

#' Convert JSON Data Back to R Objects
#'
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
  # If it's a string, parse it first
  if (is.character(json) && length(json) == 1) {
    x <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  } else {
    x <- json
  }

  # Recursive function to reconstruct R objects
  reconstruct <- function(obj) {
    if (is.null(obj)) {
      return(NULL)
    }

    # Check for MCP type markers
    if (is.list(obj)) {
      mcp_type <- obj[["_mcp_type"]]

      if (!is.null(mcp_type)) {
        if (mcp_type == "matrix") {
          # Reconstruct matrix
          data <- if (is.list(obj$data)) unlist(obj$data) else obj$data
          dims <- if (is.list(obj$dim)) unlist(obj$dim) else obj$dim
          mat <- matrix(data, nrow = dims[1], ncol = dims[2])
          if (!is.null(obj$dimnames)) {
            dimnames(mat) <- obj$dimnames
          }
          return(mat)
        } else if (mcp_type == "array") {
          # Reconstruct array
          data <- if (is.list(obj$data)) unlist(obj$data) else obj$data
          dims <- if (is.list(obj$dim)) unlist(obj$dim) else obj$dim
          arr <- array(data, dim = dims)
          if (!is.null(obj$dimnames)) {
            dimnames(arr) <- obj$dimnames
          }
          return(arr)
        } else if (mcp_type == "factor") {
          # Reconstruct factor
          return(factor(obj$values, levels = obj$levels))
        } else if (mcp_type == "data.frame") {
          # Reconstruct data frame
          obj[["_mcp_type"]] <- NULL
          obj[["_mcp_nrow"]] <- NULL
          df <- as.data.frame(lapply(obj, reconstruct))
          return(df)
        } else if (mcp_type == "S3") {
          # Reconstruct S3 object
          mcp_class <- obj[["_mcp_class"]]
          obj[["_mcp_type"]] <- NULL
          obj[["_mcp_class"]] <- NULL
          result <- lapply(obj, reconstruct)
          class(result) <- mcp_class
          return(result)
        } else if (mcp_type == "S4") {
          # S4 reconstruction is more complex and may not always work
          # For now, return as list with class info
          return(obj)
        } else if (mcp_type == "special_numeric") {
          # Reconstruct special numeric value
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
          return(as.numeric(val))
        } else if (mcp_type == "numeric_vector_special") {
          # Reconstruct numeric vector with special values
          values <- obj$values
          result <- numeric(length(values))
          for (i in seq_along(values)) {
            if (is.null(values[[i]])) {
              result[i] <- NA
            } else if (identical(values[[i]], "Inf")) {
              result[i] <- Inf
            } else if (identical(values[[i]], "-Inf")) {
              result[i] <- -Inf
            } else if (identical(values[[i]], "NaN")) {
              result[i] <- NaN
            } else {
              result[i] <- as.numeric(values[[i]])
            }
          }
          return(result)
        } else if (mcp_type == "Date") {
          # Reconstruct Date objects
          values <- if (is.list(obj$values)) unlist(obj$values) else obj$values
          return(as.Date(values))
        } else if (mcp_type == "POSIXct") {
          # Reconstruct POSIXct objects
          values <- if (is.list(obj$values)) unlist(obj$values) else obj$values
          result <- as.POSIXct(values, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
          if (!is.null(obj$timezone) && obj$timezone != "UTC") {
            attr(result, "tzone") <- obj$timezone
          }
          return(result)
        } else if (mcp_type == "complex") {
          # Reconstruct complex numbers
          return(complex(real = obj$real, imaginary = obj$imaginary))
        } else if (mcp_type == "raw") {
          # Reconstruct raw vectors from base64
          data <- if (is.list(obj$data)) obj$data[[1]] else obj$data
          return(jsonlite::base64_dec(data))
        } else if (mcp_type == "formula") {
          # Reconstruct formula
          formula_str <- if (is.list(obj$formula)) obj$formula[[1]] else obj$formula
          return(stats::as.formula(formula_str))
        } else if (mcp_type == "language") {
          # Reconstruct language objects
          return(parse(text = obj$expression)[[1]])
        } else if (mcp_type == "environment") {
          # Can't reconstruct environments - return a marker
          return(structure(list(name = obj$name), class = "mcp_environment_marker"))
        } else if (mcp_type == "plot") {
          # Can't reconstruct plots - return a marker with the image data
          return(structure(list(
            format = obj$format,
            plot_type = obj$plot_type,
            data = obj$data
          ), class = "mcp_plot_marker"))
        } else if (mcp_type == "large_object") {
          # Can't reconstruct large objects - return the summary
          return(structure(obj, class = "mcp_large_object_marker"))
        }
      }

      # Regular list - recursively process elements
      return(lapply(obj, reconstruct))
    }

    # Return as-is
    return(obj)
  }

  reconstruct(x)
}
