#' Convert R objects to JSON-compatible format for MCP
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
  # Handle NULL
  if (is.null(x)) {
    return(NULL)
  }
  
  # Check for custom serializers first
  obj_class <- class(x)[1]
  if (obj_class %in% names(custom_serializers)) {
    return(custom_serializers[[obj_class]](x))
  }
  
  # Check object size for large object handling
  obj_size <- object.size(x)
  if (obj_size > size_limit) {
    return(list(
      `_mcp_type` = "large_object",
      class = class(x),
      size = as.numeric(obj_size),
      size_human = format(obj_size, units = "auto"),
      summary = capture.output(summary(x)),
      preview = if (is.data.frame(x)) {
        list(
          nrow = nrow(x),
          ncol = ncol(x),
          columns = names(x),
          head = to_mcp_json(head(x, 5), auto_unbox = auto_unbox, size_limit = Inf)
        )
      } else if (is.atomic(x)) {
        list(
          length = length(x),
          type = typeof(x),
          head = head(x, 100)
        )
      } else {
        NULL
      }
    ))
  }
  
  # Handle special numeric values
  if (is.atomic(x) && is.numeric(x)) {
    # Check for special values
    if (any(is.infinite(x) | is.nan(x), na.rm = TRUE)) {
      # Convert special values to strings for JSON compatibility
      x_converted <- x
      x_converted[is.infinite(x) & x > 0] <- "Inf"
      x_converted[is.infinite(x) & x < 0] <- "-Inf"
      x_converted[is.nan(x)] <- "NaN"
      x_converted[is.na(x) & !is.nan(x)] <- NA
      
      # If it's a single value, return with type marker
      if (length(x) == 1) {
        return(list(
          value = if (auto_unbox) jsonlite::unbox(x_converted) else x_converted,
          `_mcp_type` = "special_numeric"
        ))
      }
      # For vectors, include original indices of special values
      return(list(
        values = x_converted,
        special_indices = which(is.infinite(x) | is.nan(x)),
        `_mcp_type` = "numeric_vector_special"
      ))
    }
  }
  
  # Handle Date objects
  if (inherits(x, "Date")) {
    return(list(
      values = format(x, "%Y-%m-%d"),
      `_mcp_type` = "Date"
    ))
  }
  
  # Handle POSIXct/POSIXlt datetime objects
  if (inherits(x, "POSIXt")) {
    # Convert POSIXlt to POSIXct for consistency
    if (inherits(x, "POSIXlt")) {
      x <- as.POSIXct(x)
    }
    return(list(
      values = format(x, "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
      timezone = attr(x, "tzone") %||% "UTC",
      `_mcp_type` = "POSIXct"
    ))
  }
  
  # Handle complex numbers
  if (is.complex(x)) {
    return(list(
      real = Re(x),
      imaginary = Im(x),
      `_mcp_type` = "complex"
    ))
  }
  
  # Handle raw vectors
  if (is.raw(x)) {
    # Convert to base64 for JSON compatibility
    return(list(
      data = jsonlite::base64_enc(x),
      `_mcp_type` = "raw"
    ))
  }
  
  # Handle formulas
  if (inherits(x, "formula")) {
    formula_str <- deparse(x)
    if (length(formula_str) == 1 && auto_unbox) {
      formula_str <- jsonlite::unbox(formula_str)
    }
    return(list(
      formula = formula_str,
      environment = jsonlite::unbox(if (!identical(environment(x), globalenv())) "<non-global>" else "global"),
      `_mcp_type` = jsonlite::unbox("formula")
    ))
  }
  
  # Handle language objects (expressions, calls, symbols)
  if (is.language(x)) {
    # For single-line expressions, unbox the string
    expr_str <- deparse(x)
    if (length(expr_str) == 1 && auto_unbox) {
      expr_str <- jsonlite::unbox(expr_str)
    }
    return(list(
      expression = expr_str,
      type = jsonlite::unbox(typeof(x)),
      `_mcp_type` = jsonlite::unbox("language")
    ))
  }
  
  # Handle environments (just return a marker, don't serialize contents)
  if (is.environment(x)) {
    env_name <- environmentName(x)
    if (env_name == "") {
      env_name <- capture.output(print(x))[1]
    }
    return(list(
      name = jsonlite::unbox(env_name),
      `_mcp_type` = jsonlite::unbox("environment")
    ))
  }
  
  # Handle plots
  if (inherits(x, "gg") || inherits(x, "ggplot")) {
    # ggplot2 plots
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      tmp <- tempfile(fileext = ".png")
      on.exit(unlink(tmp), add = TRUE)
      
      requireNamespace("ggplot2", quietly = TRUE)
      ggplot2::ggsave(tmp, x, width = 8, height = 6, dpi = 150)
      
      return(list(
        `_mcp_type` = "plot",
        format = "image/png",
        plot_type = "ggplot2",
        data = jsonlite::base64_enc(readBin(tmp, "raw", file.info(tmp)$size))
      ))
    }
  }
  
  # Handle base R recorded plots
  if (inherits(x, "recordedplot")) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)
    
    png(tmp, width = 800, height = 600)
    replayPlot(x)
    dev.off()
    
    return(list(
      `_mcp_type` = "plot",
      format = "image/png", 
      plot_type = "base_r",
      data = jsonlite::base64_enc(readBin(tmp, "raw", file.info(tmp)$size))
    ))
  }
  
  # Handle factors (moved after special type checks)
  if (is.factor(x)) {
    return(list(
      levels = levels(x),
      values = as.integer(x),
      `_mcp_type` = "factor"
    ))
  }
  
  # Handle basic atomic types
  if (is.atomic(x) && !is.array(x) && !is.matrix(x)) {
    # Preserve names if they exist
    if (!is.null(names(x))) {
      return(as.list(x))
    }
    # Single values should be unboxed
    if (length(x) == 1 && auto_unbox) {
      return(jsonlite::unbox(x))
    }
    return(x)
  }
  
  # Handle matrices and arrays
  if (is.matrix(x) || is.array(x)) {
    # Convert to list with dimension information
    result <- list(
      data = as.vector(x),
      dim = dim(x),
      dimnames = dimnames(x),
      `_mcp_type` = if (is.matrix(x)) "matrix" else "array"
    )
    return(result)
  }
  
  # Handle data frames
  if (is.data.frame(x)) {
    # Convert to list of columns
    result <- as.list(x)
    # Add metadata
    attr(result, "_mcp_type") <- "data.frame"
    attr(result, "_mcp_nrow") <- nrow(x)
    return(result)
  }
  
  
  # Handle S3/S4 objects before generic lists
  if (is.object(x)) {
    # Try to convert to list representation
    if (isS4(x)) {
      # S4 objects
      slots <- slotNames(x)
      result <- list(`_mcp_type` = "S4", `_mcp_class` = class(x))
      for (slot in slots) {
        result[[slot]] <- to_mcp_json(methods::slot(x, slot), auto_unbox = auto_unbox, size_limit = size_limit, custom_serializers = custom_serializers)
      }
      return(result)
    } else {
      # S3 objects - convert to list and preserve class
      result <- unclass(x)
      if (is.list(result)) {
        # Use a loop to preserve attributes
        converted <- list()
        for (i in seq_along(result)) {
          converted[[names(result)[i]]] <- to_mcp_json(result[[i]], auto_unbox = auto_unbox, size_limit = size_limit, custom_serializers = custom_serializers)
        }
        result <- converted
      } else {
        result <- to_mcp_json(result, auto_unbox = auto_unbox, size_limit = size_limit, custom_serializers = custom_serializers)
      }
      attr(result, "_mcp_type") <- "S3"
      attr(result, "_mcp_class") <- class(x)
      return(result)
    }
  }
  
  # Handle lists (after checking for S3/S4 objects)
  if (is.list(x)) {
    # Recursively convert list elements
    result <- lapply(x, to_mcp_json, auto_unbox = auto_unbox, size_limit = size_limit, custom_serializers = custom_serializers)
    return(result)
  }
  
  # Default: return as-is and hope jsonlite can handle it
  return(x)
}