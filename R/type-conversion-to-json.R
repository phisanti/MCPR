# R to JSON Conversion Functions
# Comprehensive R object to JSON conversion with type preservation for MCP protocol.
# Handles diverse R types including special values, dates, factors, matrices, and custom objects.

# Helper functions for type conversion ----------------------------------------

# Handle NULL values
.mcpr_convert_null <- function(x, ...) {
  if (is.null(x)) {
    return(NULL)
  }
  NULL
}

# Handle custom serializers
.mcpr_convert_custom <- function(x, custom_serializers, ...) {
  obj_class <- class(x)[1]
  if (obj_class %in% names(custom_serializers)) {
    return(custom_serializers[[obj_class]](x))
  }
  NULL
}

# Handle large objects
.mcpr_convert_large_object <- function(x, auto_unbox = TRUE, size_limit = 1e6,
                                       custom_serializers = list(), ...) {
  obj_size <- utils::object.size(x)
  if (obj_size <= size_limit) {
    return(NULL)
  }

  preview <- NULL
  if (is.data.frame(x)) {
    preview <- list(
      nrow = nrow(x),
      ncol = ncol(x),
      columns = names(x),
      head = to_mcpr_json(utils::head(x, 5),
        auto_unbox = auto_unbox,
        size_limit = Inf, custom_serializers = custom_serializers
      )
    )
  } else if (is.atomic(x)) {
    preview <- list(
      length = length(x),
      type = typeof(x),
      head = utils::head(x, 100)
    )
  }

  list(
    `_mcp_type` = "large_object",
    class = class(x),
    size = as.numeric(obj_size),
    size_human = format(obj_size, units = "auto"),
    summary = utils::capture.output(summary(x)),
    preview = preview
  )
}

# Handle special numeric values (Inf, -Inf, NaN)
.mcpr_convert_special_numeric <- function(x, auto_unbox = TRUE, ...) {
  if (!(is.atomic(x) && is.numeric(x))) {
    return(NULL)
  }
  if (!any(is.infinite(x) | is.nan(x), na.rm = TRUE)) {
    return(NULL)
  }

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
.mcpr_convert_date <- function(x, ...) {
  if (!inherits(x, "Date")) {
    return(NULL)
  }
  list(
    values = format(x, "%Y-%m-%d"),
    `_mcp_type` = "Date"
  )
}

# Handle POSIXct/POSIXlt datetime objects
.mcpr_convert_posix <- function(x, ...) {
  if (!inherits(x, "POSIXt")) {
    return(NULL)
  }
  if (inherits(x, "POSIXlt")) x <- as.POSIXct(x)

  list(
    values = format(x, "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    timezone = attr(x, "tzone") %||% "UTC",
    `_mcp_type` = "POSIXct"
  )
}

# Handle complex numbers
.mcpr_convert_complex <- function(x, ...) {
  if (!is.complex(x)) {
    return(NULL)
  }
  real_part <- Re(x)
  imaginary_part <- Im(x)

  list(
    real = if (length(real_part) == 1) jsonlite::unbox(real_part) else real_part,
    imaginary = if (length(imaginary_part) == 1) jsonlite::unbox(imaginary_part) else imaginary_part,
    `_mcp_type` = "complex"
  )
}

# Handle raw vectors
.mcpr_convert_raw <- function(x, ...) {
  if (!is.raw(x)) {
    return(NULL)
  }
  list(
    data = jsonlite::base64_enc(x),
    `_mcp_type` = "raw"
  )
}

# Handle formula objects
.mcpr_convert_formula <- function(x, auto_unbox = TRUE, ...) {
  if (!inherits(x, "formula")) {
    return(NULL)
  }
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
.mcpr_convert_language <- function(x, auto_unbox = TRUE, ...) {
  if (!is.language(x)) {
    return(NULL)
  }

  # Safely deparse the expression without evaluating it
  expr_str <- tryCatch(
    {
      deparse(x)
    },
    error = function(e) {
      # Fallback to safer representation
      paste0("<", typeof(x), ">")
    }
  )

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
.mcpr_convert_environment <- function(x, ...) {
  if (!is.environment(x)) {
    return(NULL)
  }
  env_name <- environmentName(x)
  if (env_name == "") env_name <- utils::capture.output(print(x))[1]
  list(
    name = jsonlite::unbox(env_name),
    `_mcp_type` = jsonlite::unbox("environment")
  )
}

# Handle ggplot2 objects
.mcpr_convert_plot_gg <- function(x, ...) {
  if (!(inherits(x, "gg") || inherits(x, "ggplot"))) {
    return(NULL)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

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
.mcpr_convert_plot_recorded <- function(x, ...) {
  if (!inherits(x, "recordedplot")) {
    return(NULL)
  }

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp, width = 800, height = 600)
  grDevices::replayPlot(x)
  grDevices::dev.off()

  list(
    `_mcp_type` = "plot",
    format = "image/png",
    plot_type = "base_r",
    data = jsonlite::base64_enc(readBin(tmp, "raw", file.info(tmp)$size))
  )
}

# Handle factor objects
.mcpr_convert_factor <- function(x, ...) {
  if (!is.factor(x)) {
    return(NULL)
  }
  list(
    levels = levels(x),
    values = as.integer(x),
    `_mcp_type` = "factor"
  )
}

# Handle basic atomic types
.mcpr_convert_atomic <- function(x, auto_unbox = TRUE, ...) {
  if (!(is.atomic(x) && !is.array(x) && !is.matrix(x))) {
    return(NULL)
  }

  if (!is.null(names(x))) {
    return(as.list(x))
  }
  if (length(x) == 1 && auto_unbox) {
    return(jsonlite::unbox(x))
  }
  x
}

# Handle matrices and arrays
.mcpr_convert_matrix_array <- function(x, ...) {
  if (!(is.matrix(x) || is.array(x))) {
    return(NULL)
  }
  list(
    data = as.vector(x),
    dim = dim(x),
    dimnames = dimnames(x),
    `_mcp_type` = if (is.matrix(x)) "matrix" else "array"
  )
}

# Handle data frames
.mcpr_convert_dataframe <- function(x, auto_stream = TRUE, stream_threshold = 500, ...) {
  if (!is.data.frame(x)) {
    return(NULL)
  }

  # Auto-stream if dataframe size (rows * cols) exceeds threshold
  df_size <- nrow(x) * ncol(x)
  if (auto_stream && df_size > stream_threshold) {
    return(list(
      `_mcp_type` = "streamed_dataframe",
      nrow = nrow(x), ncol = ncol(x), columns = names(x),
      size = df_size, threshold = stream_threshold
    ))
  }

  result <- as.list(x)
  attr(result, "_mcp_type") <- "data.frame"
  attr(result, "_mcp_nrow") <- nrow(x)
  result
}

# Handle S4 objects
.mcpr_convert_S4 <- function(x, auto_unbox = TRUE, size_limit = 1e6,
                             custom_serializers = list(), ...) {
  if (!isS4(x)) {
    return(NULL)
  }
  slots <- methods::slotNames(x)
  result <- list(`_mcp_type` = "S4", `_mcp_class` = class(x))
  for (s in slots) {
    result[[s]] <- to_mcpr_json(methods::slot(x, s),
      auto_unbox = auto_unbox,
      size_limit = size_limit,
      custom_serializers = custom_serializers
    )
  }
  result
}

# Handle S3 objects
.mcpr_convert_S3 <- function(x, auto_unbox = TRUE, size_limit = 1e6,
                             custom_serializers = list(), ...) {
  if (!is.object(x) || isS4(x)) {
    return(NULL)
  }
  result <- unclass(x)
  if (is.list(result)) {
    converted <- list()
    for (i in seq_along(result)) {
      converted[[names(result)[i]]] <- to_mcpr_json(result[[i]],
        auto_unbox = auto_unbox,
        size_limit = size_limit,
        custom_serializers = custom_serializers
      )
    }
    result <- converted
  } else {
    result <- to_mcpr_json(result,
      auto_unbox = auto_unbox,
      size_limit = size_limit,
      custom_serializers = custom_serializers
    )
  }
  attr(result, "_mcp_type") <- "S3"
  attr(result, "_mcp_class") <- class(x)
  result
}

# Handle lists
.mcpr_convert_list <- function(x, auto_unbox = TRUE, size_limit = 1e6,
                               custom_serializers = list(), ...) {
  if (!is.list(x)) {
    return(NULL)
  }
  lapply(x, to_mcpr_json,
    auto_unbox = auto_unbox, size_limit = size_limit,
    custom_serializers = custom_serializers
  )
}

#' Convert R Objects to JSON-Compatible Format for MCP
#'
#' @title Convert R Objects to JSON-Compatible Format for MCP
#' @description Converts diverse R objects to JSON-compatible format with comprehensive type preservation.
#' Handles special types including dates, factors, matrices, and special numeric values through
#' sophisticated conversion pipeline. Maintains R object semantics while ensuring MCP protocol
#' compatibility through structured type markers and metadata preservation.
#'
#' @param x R object to convert to JSON-compatible format
#' @param auto_unbox Whether to automatically unbox single-element vectors
#' @param size_limit Maximum object size in bytes before large object handling (default: 1MB)
#' @param custom_serializers List of custom serializers for specific classes
#' @return JSON-compatible representation with preserved type information
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
#' @examples
#' # Basic types
#' to_mcpr_json(list(a = 1, b = "hello"))
#' to_mcpr_json(c(TRUE, FALSE, NA))
#'
#' # Special numeric values
#' to_mcpr_json(c(1, Inf, -Inf, NaN))
#'
#' # Dates and times
#' to_mcpr_json(Sys.Date())
#' to_mcpr_json(Sys.time())
#'
#' # Data frames
#' to_mcpr_json(data.frame(x = 1:3, y = letters[1:3]))
#'
#' # Complex types
#' to_mcpr_json(matrix(1:6, nrow = 2))
#' to_mcpr_json(factor(c("a", "b", "a")))
#' to_mcpr_json(3 + 4i)
#' @noRd
to_mcpr_json <- function(x, auto_unbox = TRUE, size_limit = 1e6, custom_serializers = list()) {
  if (is.null(x)) {
    return(NULL)
  } else if (class(x)[1] %in% names(custom_serializers)) {
    return(custom_serializers[[class(x)[1]]](x))
  } else if (utils::object.size(x) > size_limit) {
    return(.mcpr_convert_large_object(x, auto_unbox = auto_unbox, size_limit = size_limit, custom_serializers = custom_serializers))
  } else if (is.atomic(x) && is.numeric(x) && any(is.infinite(x) | is.nan(x), na.rm = TRUE)) {
    return(.mcpr_convert_special_numeric(x, auto_unbox = auto_unbox))
  } else if (inherits(x, "Date")) {
    return(.mcpr_convert_date(x))
  } else if (inherits(x, "POSIXt")) {
    return(.mcpr_convert_posix(x))
  } else if (is.complex(x)) {
    return(.mcpr_convert_complex(x))
  } else if (is.raw(x)) {
    return(.mcpr_convert_raw(x))
  } else if (inherits(x, "formula")) {
    return(.mcpr_convert_formula(x, auto_unbox = auto_unbox))
  } else if (is.language(x)) {
    return(.mcpr_convert_language(x, auto_unbox = auto_unbox))
  } else if (is.environment(x)) {
    return(.mcpr_convert_environment(x))
  } else if (inherits(x, "gg") || inherits(x, "ggplot")) {
    return(.mcpr_convert_plot_gg(x))
  } else if (inherits(x, "recordedplot")) {
    return(.mcpr_convert_plot_recorded(x))
  } else if (is.factor(x)) {
    return(.mcpr_convert_factor(x))
  } else if (is.matrix(x) || is.array(x)) {
    return(.mcpr_convert_matrix_array(x))
  } else if (is.data.frame(x)) {
    return(.mcpr_convert_dataframe(x))
  } else if (isS4(x)) {
    return(.mcpr_convert_S4(x, auto_unbox = auto_unbox, size_limit = size_limit, custom_serializers = custom_serializers))
  } else if (is.object(x)) {
    return(.mcpr_convert_S3(x, auto_unbox = auto_unbox, size_limit = size_limit, custom_serializers = custom_serializers))
  } else if (is.list(x)) {
    return(.mcpr_convert_list(x, auto_unbox = auto_unbox, size_limit = size_limit, custom_serializers = custom_serializers))
  } else if (is.atomic(x) && !is.array(x) && !is.matrix(x)) {
    return(.mcpr_convert_atomic(x, auto_unbox = auto_unbox))
  } else {
    return(x)
  }
}
