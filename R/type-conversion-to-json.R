# type-conversion-to-json.R
# Utility functions for converting R objects to JSON-compatible formats for MCP.
# Handles various R types (atomic, complex, dates, factors, formulas, plots, S3/S4, etc.)
# and preserves type information for robust serialization.

# Helper functions for type conversion ----------------------------------------

# Handle NULL values
.mcp_convert_null <- function(x, ...) {
  if (is.null(x)) return(NULL)
  NULL
}

# Handle custom serializers
.mcp_convert_custom <- function(x, custom_serializers, ...) {
  obj_class <- class(x)[1]
  if (obj_class %in% names(custom_serializers)) {
    return(custom_serializers[[obj_class]](x))
  }
  NULL
}

# Handle large objects
.mcp_convert_large_object <- function(x, auto_unbox = TRUE, size_limit = 1e6, 
                                      custom_serializers = list(), ...) {
  obj_size <- object.size(x)
  if (obj_size <= size_limit) return(NULL)

  preview <- NULL
  if (is.data.frame(x)) {
    preview <- list(
      nrow = nrow(x),
      ncol = ncol(x),
      columns = names(x),
      head = to_mcp_json(head(x, 5), auto_unbox = auto_unbox, 
                         size_limit = Inf, custom_serializers = custom_serializers)
    )
  } else if (is.atomic(x)) {
    preview <- list(
      length = length(x),
      type = typeof(x),
      head = head(x, 100)
    )
  }

  list(
    `_mcp_type` = "large_object",
    class = class(x),
    size = as.numeric(obj_size),
    size_human = format(obj_size, units = "auto"),
    summary = capture.output(summary(x)),
    preview = preview
  )
}

# Handle special numeric values (Inf, -Inf, NaN)
.mcp_convert_special_numeric <- function(x, auto_unbox = TRUE, ...) {
  if (!(is.atomic(x) && is.numeric(x))) return(NULL)
  if (!any(is.infinite(x) | is.nan(x), na.rm = TRUE)) return(NULL)

  x_converted <- x
  x_converted[is.infinite(x) & x > 0] <- "Inf"
  x_converted[is.infinite(x) & x < 0] <- "-Inf"
  x_converted[is.nan(x)] <- "NaN"
  x_converted[is.na(x) & !is.nan(x)] <- NA

  if (length(x) == 1) {
    return(list(
      value = if (auto_unbox) jsonlite::unbox(x_converted) else x_converted,
      `_mcp_type` = "special_numeric"
    ))
  }

  list(
    values = x_converted,
    special_indices = which(is.infinite(x) | is.nan(x)),
    `_mcp_type` = "numeric_vector_special"
  )
}

# Handle Date objects
.mcp_convert_date <- function(x, ...) {
  if (!inherits(x, "Date")) return(NULL)
  list(
    values = format(x, "%Y-%m-%d"),
    `_mcp_type` = "Date"
  )
}

# Handle POSIXct/POSIXlt datetime objects
.mcp_convert_posix <- function(x, ...) {
  if (!inherits(x, "POSIXt")) return(NULL)
  if (inherits(x, "POSIXlt")) x <- as.POSIXct(x)

  list(
    values = format(x, "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    timezone = attr(x, "tzone") %||% "UTC",
    `_mcp_type` = "POSIXct"
  )
}

# Handle complex numbers
.mcp_convert_complex <- function(x, ...) {
  if (!is.complex(x)) return(NULL)
  list(
    real = Re(x),
    imaginary = Im(x),
    `_mcp_type` = "complex"
  )
}

# Handle raw vectors
.mcp_convert_raw <- function(x, ...) {
  if (!is.raw(x)) return(NULL)
  list(
    data = jsonlite::base64_enc(x),
    `_mcp_type` = "raw"
  )
}

# Handle formula objects
.mcp_convert_formula <- function(x, auto_unbox = TRUE, ...) {
  if (!inherits(x, "formula")) return(NULL)
  formula_str <- deparse(x)
  if (length(formula_str) == 1 && auto_unbox) {
    formula_str <- jsonlite::unbox(formula_str)
  }
  list(
    formula = formula_str,
    environment = jsonlite::unbox(
      if (!identical(environment(x), globalenv())) "<non-global>" else "global"
    ),
    `_mcp_type` = jsonlite::unbox("formula")
  )
}

# Handle language objects (expressions, calls, symbols)
.mcp_convert_language <- function(x, auto_unbox = TRUE, ...) {
  if (!is.language(x)) return(NULL)
  
  # Safely deparse the expression without evaluating it
  expr_str <- tryCatch({
    deparse(x)
  }, error = function(e) {
    # Fallback to safer representation
    paste0("<", typeof(x), ">")
  })
  
  if (length(expr_str) == 1 && auto_unbox) {
    expr_str <- jsonlite::unbox(expr_str)
  }
  list(
    expression = expr_str,
    type = jsonlite::unbox(typeof(x)),
    `_mcp_type` = jsonlite::unbox("language")
  )
}

# Handle environments
.mcp_convert_environment <- function(x, ...) {
  if (!is.environment(x)) return(NULL)
  env_name <- environmentName(x)
  if (env_name == "") env_name <- capture.output(print(x))[1]
  list(
    name = jsonlite::unbox(env_name),
    `_mcp_type` = jsonlite::unbox("environment")
  )
}

# Handle ggplot2 objects
.mcp_convert_plot_gg <- function(x, ...) {
  if (!(inherits(x, "gg") || inherits(x, "ggplot"))) return(NULL)
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  requireNamespace("ggplot2", quietly = TRUE)
  ggplot2::ggsave(tmp, x, width = 8, height = 6, dpi = 150)

  list(
    `_mcp_type` = "plot",
    format = "image/png",
    plot_type = "ggplot2",
    data = jsonlite::base64_enc(readBin(tmp, "raw", file.info(tmp)$size))
  )
}

# Handle base R recorded plots
.mcp_convert_plot_recorded <- function(x, ...) {
  if (!inherits(x, "recordedplot")) return(NULL)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png(tmp, width = 800, height = 600)
  replayPlot(x)
  dev.off()

  list(
    `_mcp_type` = "plot",
    format = "image/png",
    plot_type = "base_r",
    data = jsonlite::base64_enc(readBin(tmp, "raw", file.info(tmp)$size))
  )
}

# Handle factor objects
.mcp_convert_factor <- function(x, ...) {
  if (!is.factor(x)) return(NULL)
  list(
    levels = levels(x),
    values = as.integer(x),
    `_mcp_type` = "factor"
  )
}

# Handle basic atomic types
.mcp_convert_atomic <- function(x, auto_unbox = TRUE, ...) {
  if (!(is.atomic(x) && !is.array(x) && !is.matrix(x))) return(NULL)

  if (!is.null(names(x))) return(as.list(x))
  if (length(x) == 1 && auto_unbox) return(jsonlite::unbox(x))
  x
}

# Handle matrices and arrays
.mcp_convert_matrix_array <- function(x, ...) {
  if (!(is.matrix(x) || is.array(x))) return(NULL)
  list(
    data = as.vector(x),
    dim = dim(x),
    dimnames = dimnames(x),
    `_mcp_type` = if (is.matrix(x)) "matrix" else "array"
  )
}

# Handle data frames
.mcp_convert_dataframe <- function(x, ...) {
  if (!is.data.frame(x)) return(NULL)
  result <- as.list(x)
  attr(result, "_mcp_type") <- "data.frame"
  attr(result, "_mcp_nrow") <- nrow(x)
  result
}

# Handle S4 objects
.mcp_convert_S4 <- function(x, auto_unbox = TRUE, size_limit = 1e6, 
                           custom_serializers = list(), ...) {
  if (!isS4(x)) return(NULL)
  slots <- slotNames(x)
  result <- list(`_mcp_type` = "S4", `_mcp_class` = class(x))
  for (s in slots) {
    result[[s]] <- to_mcp_json(slot(x, s), auto_unbox = auto_unbox, 
                               size_limit = size_limit, 
                               custom_serializers = custom_serializers)
  }
  result
}

# Handle S3 objects
.mcp_convert_S3 <- function(x, auto_unbox = TRUE, size_limit = 1e6, 
                           custom_serializers = list(), ...) {
  if (!is.object(x) || isS4(x)) return(NULL)
  result <- unclass(x)
  if (is.list(result)) {
    converted <- list()
    for (i in seq_along(result)) {
      converted[[names(result)[i]]] <- to_mcp_json(result[[i]], 
                                                   auto_unbox = auto_unbox, 
                                                   size_limit = size_limit, 
                                                   custom_serializers = custom_serializers)
    }
    result <- converted
  } else {
    result <- to_mcp_json(result, auto_unbox = auto_unbox, 
                          size_limit = size_limit, 
                          custom_serializers = custom_serializers)
  }
  attr(result, "_mcp_type") <- "S3"
  attr(result, "_mcp_class") <- class(x)
  result
}

# Handle lists
.mcp_convert_list <- function(x, auto_unbox = TRUE, size_limit = 1e6, 
                             custom_serializers = list(), ...) {
  if (!is.list(x)) return(NULL)
  lapply(x, to_mcp_json, auto_unbox = auto_unbox, size_limit = size_limit, 
         custom_serializers = custom_serializers)
}

# Main function for type conversion ----------------------------------------

#' @title Convert R objects to JSON-compatible format for MCP
#'
#' Converts various R objects to a JSON-compatible format, preserving type
#' information where possible. Handles special types like dates, factors,
#' matrices, and special numeric values (Inf, -Inf, NaN).
#'
#' @param x An R object to convert
#' @param auto_unbox Whether to automatically unbox single-element vectors
#' @param size_limit Maximum object size in bytes before using large object handling (default: 1MB)
#' @param custom_serializers List of custom serializers for specific classes
#' @return A JSON-compatible representation of the R object
#' @importFrom utils object.size capture.output head
#' @importFrom grDevices png dev.off replayPlot
#' @importFrom methods slotNames slot
#' @importFrom stats as.formula
#' 
#' @details
#' The function handles the following R types:
#' \itemize{
#'   \item Basic types: NULL, logical, numeric, character, integer
#'   \item Special numeric values: Inf, -Inf, NaN
#'   \item Date/time types: Date, POSIXct, POSIXlt
#'   \item Complex numbers
#'   \item Raw vectors (binary data)
#'   \item Factors (with levels preserved)
#'   \item Matrices and arrays (with dimensions)
#'   \item Data frames
#'   \item Lists (recursive conversion)
#'   \item S3 and S4 objects
#'   \item Formulas and language objects
#'   \item Environments (replaced with markers)
#' }
#' 
#' @export
#' @examples
#' # Basic types
#' to_mcp_json(list(a = 1, b = "hello"))
#' to_mcp_json(c(TRUE, FALSE, NA))
#' 
#' # Special numeric values
#' to_mcp_json(c(1, Inf, -Inf, NaN))
#' 
#' # Dates and times
#' to_mcp_json(Sys.Date())
#' to_mcp_json(Sys.time())
#' 
#' # Data frames
#' to_mcp_json(data.frame(x = 1:3, y = letters[1:3]))
#' 
#' # Complex types
#' to_mcp_json(matrix(1:6, nrow = 2))
#' to_mcp_json(factor(c("a", "b", "a")))
#' to_mcp_json(3 + 4i)
to_mcp_json <- function(x, auto_unbox = TRUE, size_limit = 1e6, custom_serializers = list()) {
  
  if (is.null(x)) {
    return(NULL)
  } else if (class(x)[1] %in% names(custom_serializers)) {
    return(custom_serializers[[class(x)[1]]](x))
  } else if (object.size(x) > size_limit) {
    return(.mcp_convert_large_object(x, auto_unbox = auto_unbox, size_limit = size_limit, custom_serializers = custom_serializers))
  } else if (is.atomic(x) && is.numeric(x) && any(is.infinite(x) | is.nan(x), na.rm = TRUE)) {
    return(.mcp_convert_special_numeric(x, auto_unbox = auto_unbox))
  } else if (inherits(x, "Date")) {
    return(.mcp_convert_date(x))
  } else if (inherits(x, "POSIXt")) {
    return(.mcp_convert_posix(x))
  } else if (is.complex(x)) {
    return(.mcp_convert_complex(x))
  } else if (is.raw(x)) {
    return(.mcp_convert_raw(x))
  } else if (inherits(x, "formula")) {
    return(.mcp_convert_formula(x, auto_unbox = auto_unbox))
  } else if (is.language(x)) {
    return(.mcp_convert_language(x, auto_unbox = auto_unbox))
  } else if (is.environment(x)) {
    return(.mcp_convert_environment(x))
  } else if (inherits(x, "gg") || inherits(x, "ggplot")) {
    return(.mcp_convert_plot_gg(x))
  } else if (inherits(x, "recordedplot")) {
    return(.mcp_convert_plot_recorded(x))
  } else if (is.factor(x)) {
    return(.mcp_convert_factor(x))
  } else if (is.matrix(x) || is.array(x)) {
    return(.mcp_convert_matrix_array(x))
  } else if (is.data.frame(x)) {
    return(.mcp_convert_dataframe(x))
  } else if (isS4(x)) {
    return(.mcp_convert_S4(x, auto_unbox = auto_unbox, size_limit = size_limit, custom_serializers = custom_serializers))
  } else if (is.object(x)) {
    return(.mcp_convert_S3(x, auto_unbox = auto_unbox, size_limit = size_limit, custom_serializers = custom_serializers))
  } else if (is.list(x)) {
    return(.mcp_convert_list(x, auto_unbox = auto_unbox, size_limit = size_limit, custom_serializers = custom_serializers))
  } else if (is.atomic(x) && !is.array(x) && !is.matrix(x)) {
    return(.mcp_convert_atomic(x, auto_unbox = auto_unbox))
  } else {
    return(x)
  }
}
