# Object Inspection Functions
# Deep analysis of R objects for AI agent consumption.
# Each function inspects a specific object type and returns formatted plain text.

# ---- Type Detection and Dispatch ----

#' Detect object type and dispatch to appropriate inspector
#' @param obj The R object to inspect
#' @param object_name The name of the object (for display)
#' @return Formatted inspection result
#' @noRd
inspect_dispatch <- function(obj, object_name) {
  # Header with object name and memory size
  obj_size <- format(utils::object.size(obj), units = "auto")
  header <- paste0("Object: ", object_name, " (", obj_size, ")")

  # Type-specific dispatch in priority order
  body <- if (is.data.frame(obj)) {
    inspect_data_frame(obj)
  } else if (is.factor(obj)) {
    inspect_factor(obj)
  } else if (inherits(obj, "formula")) {
    inspect_formula(obj)
  } else if (inherits(obj, c("POSIXct", "POSIXlt", "Date"))) {
    inspect_datetime(obj)
  } else if (is.function(obj)) {
    inspect_function(obj)
  } else if (is.matrix(obj) || is.array(obj)) {
    inspect_matrix_array(obj)
  } else if (is.vector(obj) && !is.list(obj)) {
    inspect_vector(obj)
  } else if (inherits(obj, "R6")) {
    inspect_r6(obj)
  } else if (isS4(obj)) {
    inspect_s4(obj)
  } else if (is.environment(obj)) {
    inspect_environment(obj)
  } else if (is.list(obj)) {
    # Check for S3 model objects before generic list
    obj_class <- class(obj)
    if (length(obj_class) > 1 || obj_class[1] != "list") {
      inspect_s3(obj)
    } else {
      inspect_list(obj)
    }
  } else if (length(class(obj)) > 1 || class(obj)[1] != typeof(obj)) {
    inspect_s3(obj)
  } else {
    inspect_default(obj)
  }

  paste0(header, "\n", body)
}

# ---- Data Frames ----

#' @noRd
inspect_data_frame <- function(x) {
  n_rows <- nrow(x)
  n_cols <- ncol(x)

  result <- paste0("Data frame: ", n_rows, " rows x ", n_cols, " columns")

  if (n_cols == 0) {
    return(paste0(result, "\nEmpty data frame (no columns)"))
  }

  # Column info
  col_names <- names(x)
  col_types <- vapply(x, function(col) paste(class(col), collapse = "/"), character(1))
  col_info <- paste0(col_names, " (", col_types, ")")

  if (n_cols <= 10) {
    result <- paste0(result, "\nColumns: ", paste(col_info, collapse = ", "))
  } else {
    result <- paste0(result, "\nColumns (first 10): ", paste(col_info[1:10], collapse = ", "))
    result <- paste0(result, "\n... and ", n_cols - 10, " more columns")
  }

  # Missing values
  total_na <- sum(vapply(x, function(col) sum(is.na(col)), integer(1)))
  if (total_na > 0) {
    na_percent <- round(100 * total_na / (n_rows * n_cols), 1)
    result <- paste0(result, "\nMissing values: ", total_na, " (", na_percent, "%)")

    # Per-column NA breakdown (only columns with NAs)
    na_per_col <- vapply(x, function(col) sum(is.na(col)), integer(1))
    cols_with_na <- na_per_col[na_per_col > 0]
    if (length(cols_with_na) > 0 && length(cols_with_na) <= 10) {
      na_details <- paste0(names(cols_with_na), ": ", cols_with_na)
      result <- paste0(result, "\n  ", paste(na_details, collapse = ", "))
    }
  }

  # Data preview
  if (n_rows > 0) {
    if (n_rows <= 20 && n_cols <= 10) {
      # Small table: show everything
      result <- paste0(result, "\n\nFull data:")
      captured <- utils::capture.output(print(x, row.names = TRUE))
      result <- paste0(result, "\n", paste(captured, collapse = "\n"))
    } else {
      # Large table: head/tail
      n_show <- min(5, n_rows)
      result <- paste0(result, "\n\nFirst ", n_show, " rows:")
      captured_head <- utils::capture.output(print(utils::head(x, n_show), row.names = TRUE))
      result <- paste0(result, "\n", paste(captured_head, collapse = "\n"))

      if (n_rows > 2 * n_show) {
        result <- paste0(result, "\n... (", n_rows - 2 * n_show, " rows omitted)")
        result <- paste0(result, "\n\nLast ", n_show, " rows:")
        captured_tail <- utils::capture.output(print(utils::tail(x, n_show), row.names = TRUE))
        result <- paste0(result, "\n", paste(captured_tail, collapse = "\n"))
      }
    }

    # Numeric summary for numeric columns
    numeric_cols <- names(x)[vapply(x, is.numeric, logical(1))]
    if (length(numeric_cols) > 0) {
      n_summary <- min(8, length(numeric_cols))
      result <- paste0(result, "\n\nNumeric summary:")
      summary_out <- utils::capture.output(print(summary(x[, numeric_cols[1:n_summary], drop = FALSE])))
      result <- paste0(result, "\n", paste(summary_out, collapse = "\n"))
      if (length(numeric_cols) > n_summary) {
        result <- paste0(result, "\n... and ", length(numeric_cols) - n_summary, " more numeric columns")
      }
    }
  }

  result
}

# ---- Vectors ----

#' @noRd
inspect_vector <- function(x) {
  len <- length(x)
  type_info <- typeof(x)
  class_info <- class(x)

  result <- paste0("Vector (", type_info, ") of length ", len)
  if (length(class_info) > 1 || class_info[1] != type_info) {
    result <- paste0(result, " [class: ", paste(class_info, collapse = ", "), "]")
  }

  if (len == 0) {
    return(paste0(result, "\nEmpty vector"))
  }

  # Named vector info
  if (!is.null(names(x))) {
    n_named <- sum(nzchar(names(x)))
    result <- paste0(result, "\nNamed: ", n_named, " of ", len, " elements have names")
  }

  # Content preview
  if (len <= 20) {
    result <- paste0(result, "\nValues: ", paste(utils::capture.output(dput(x)), collapse = ""))
  } else {
    head_vals <- utils::head(x, 5)
    tail_vals <- utils::tail(x, 5)
    result <- paste0(result, "\nFirst 5: ", paste(head_vals, collapse = ", "))
    result <- paste0(result, "\nLast 5: ", paste(tail_vals, collapse = ", "))
  }

  # Statistics for numeric vectors
  if (is.numeric(x)) {
    non_na <- x[!is.na(x)]
    if (length(non_na) > 0) {
      result <- paste0(result, "\nRange: [", min(non_na), ", ", max(non_na), "]")
      result <- paste0(result, "\nMean: ", round(mean(non_na), 4),
                       " | Median: ", round(stats::median(non_na), 4),
                       " | SD: ", round(stats::sd(non_na), 4))
    }
    n_unique <- length(unique(non_na))
    result <- paste0(result, "\nUnique values: ", n_unique)
  } else if (is.character(x)) {
    n_unique <- length(unique(x))
    result <- paste0(result, "\nUnique values: ", n_unique)
    n_empty <- sum(!nzchar(x), na.rm = TRUE)
    if (n_empty > 0) {
      result <- paste0(result, "\nEmpty strings: ", n_empty)
    }
  } else if (is.logical(x)) {
    n_true <- sum(x, na.rm = TRUE)
    n_false <- sum(!x, na.rm = TRUE)
    result <- paste0(result, "\nTRUE: ", n_true, " | FALSE: ", n_false)
  }

  # Missing values
  na_count <- sum(is.na(x))
  if (na_count > 0) {
    result <- paste0(result, "\nMissing values: ", na_count, " (", round(100 * na_count / len, 1), "%)")
  }

  result
}

# ---- Factors ----

#' @noRd
inspect_factor <- function(x) {
  len <- length(x)
  levels_list <- levels(x)
  n_levels <- length(levels_list)
  is_ordered <- is.ordered(x)

  result <- paste0("Factor", if (is_ordered) " (ordered)" else "", " of length ", len,
                    " with ", n_levels, " levels")

  if (n_levels == 0) {
    return(paste0(result, "\n(no levels defined)"))
  }

  # Show levels
  if (n_levels <= 15) {
    result <- paste0(result, "\nLevels: ", paste(levels_list, collapse = ", "))
  } else {
    result <- paste0(result, "\nLevels (first 10): ", paste(levels_list[1:10], collapse = ", "))
    result <- paste0(result, "\n... and ", n_levels - 10, " more levels")
  }

  # Frequencies
  if (len > 0) {
    freq_table <- table(x, useNA = "ifany")
    sorted_freq <- sort(freq_table, decreasing = TRUE)
    n_show <- min(10, length(sorted_freq))

    result <- paste0(result, "\n\nFrequencies (top ", n_show, "):")
    for (i in seq_len(n_show)) {
      level_name <- names(sorted_freq)[i]
      if (is.na(level_name)) level_name <- "<NA>"
      count <- as.numeric(sorted_freq[i])
      percent <- round(100 * count / len, 1)
      result <- paste0(result, "\n  ", level_name, ": ", count, " (", percent, "%)")
    }

    if (length(sorted_freq) > n_show) {
      result <- paste0(result, "\n  ... and ", length(sorted_freq) - n_show, " more")
    }
  }

  # Missing values
  na_count <- sum(is.na(x))
  if (na_count > 0) {
    result <- paste0(result, "\nMissing values: ", na_count, " (", round(100 * na_count / len, 1), "%)")
  }

  result
}

# ---- Functions ----

#' @noRd
inspect_function <- function(x) {
  if (is.primitive(x)) {
    result <- paste0("Primitive function")
    result <- paste0(result, "\nType: ", typeof(x))

    # Try to get argument info
    args_info <- args(x)
    if (!is.null(args_info)) {
      arg_text <- deparse(args(x), width.cutoff = 80L)
      result <- paste0(result, "\nSignature: ", paste(arg_text, collapse = " "))
    }

    return(result)
  }

  # Closure
  formals_info <- formals(x)
  if (length(formals_info) == 0) {
    args_text <- "()"
  } else {
    arg_names <- names(formals_info)
    arg_display <- character(length(arg_names))

    for (i in seq_along(arg_names)) {
      name <- arg_names[i]
      default_val <- formals_info[[name]]

      if (missing(default_val) || identical(default_val, quote(expr = ))) {
        arg_display[i] <- name
      } else {
        default_text <- deparse(default_val, width.cutoff = 60L)[1]
        if (nchar(default_text) > 30) default_text <- paste0(substr(default_text, 1, 27), "...")
        arg_display[i] <- paste0(name, " = ", default_text)
      }
    }
    args_text <- paste0("(", paste(arg_display, collapse = ", "), ")")
  }

  result <- paste0("Function", args_text)
  result <- paste0(result, "\nArguments: ", length(formals_info))

  # Environment
  fn_env <- environment(x)
  env_name <- if (identical(fn_env, .GlobalEnv)) {
    "Global Environment"
  } else if (identical(fn_env, baseenv())) {
    "Base Environment"
  } else if (isNamespace(fn_env)) {
    paste0("Namespace: ", environmentName(fn_env))
  } else {
    format(fn_env)
  }
  result <- paste0(result, "\nEnvironment: ", env_name)

  # Source reference if available
  src_ref <- utils::getSrcref(x)
  if (!is.null(src_ref)) {
    src_file <- utils::getSrcFilename(x, full.names = TRUE)
    src_line <- src_ref[1]
    if (nzchar(src_file)) {
      result <- paste0(result, "\nSource: ", src_file, ":", src_line)
    }
  }

  # Body preview
  body_text <- deparse(body(x), width.cutoff = 80L)
  if (length(body_text) > 15) {
    result <- paste0(result, "\nBody (first 15 lines):\n", paste(body_text[1:15], collapse = "\n"))
    result <- paste0(result, "\n... (", length(body_text) - 15, " more lines)")
  } else {
    result <- paste0(result, "\nBody:\n", paste(body_text, collapse = "\n"))
  }

  result
}

# ---- Lists ----

#' @noRd
inspect_list <- function(x) {
  len <- length(x)

  result <- paste0("List of length ", len)

  if (len == 0) {
    return(paste0(result, "\nEmpty list"))
  }

  # Names
  nms <- names(x)
  if (!is.null(nms)) {
    n_named <- sum(nzchar(nms))
    result <- paste0(result, "\nNamed: ", n_named, " of ", len, " elements")
  }

  # Element types
  n_show <- min(20, len)
  result <- paste0(result, "\n\nElements:")

  for (i in seq_len(n_show)) {
    el <- x[[i]]
    el_name <- if (!is.null(nms) && nzchar(nms[i])) nms[i] else paste0("[[", i, "]]")
    el_class <- paste(class(el), collapse = "/")

    el_detail <- if (is.data.frame(el)) {
      paste0("data.frame [", nrow(el), " x ", ncol(el), "]")
    } else if (is.atomic(el) && length(el) == 1) {
      paste0(el_class, ": ", deparse(el, width.cutoff = 60L)[1])
    } else if (is.atomic(el)) {
      paste0(el_class, "[", length(el), "]")
    } else if (is.list(el)) {
      paste0("list[", length(el), "]")
    } else if (is.function(el)) {
      paste0("function(", length(formals(el)), " args)")
    } else {
      el_class
    }

    result <- paste0(result, "\n  $", el_name, " : ", el_detail)
  }

  if (len > n_show) {
    result <- paste0(result, "\n  ... and ", len - n_show, " more elements")
  }

  # Recursive structure summary
  str_out <- utils::capture.output(utils::str(x, max.level = 2, vec.len = 3))
  if (length(str_out) > 0) {
    n_str <- min(25, length(str_out))
    result <- paste0(result, "\n\nStructure:\n", paste(str_out[1:n_str], collapse = "\n"))
    if (length(str_out) > n_str) {
      result <- paste0(result, "\n... (", length(str_out) - n_str, " more lines)")
    }
  }

  result
}

# ---- Matrices / Arrays ----

#' @noRd
inspect_matrix_array <- function(x) {
  dims <- dim(x)
  n_dims <- length(dims)
  type_info <- typeof(x)

  if (n_dims == 2) {
    result <- paste0("Matrix (", type_info, "): ", dims[1], " rows x ", dims[2], " columns")
  } else {
    result <- paste0("Array (", type_info, "): ", paste(dims, collapse = " x "))
  }

  # Dimension names
  dn <- dimnames(x)
  if (!is.null(dn)) {
    for (i in seq_along(dn)) {
      if (!is.null(dn[[i]])) {
        dim_label <- if (!is.null(names(dn))) names(dn)[i] else paste0("dim", i)
        n_names <- length(dn[[i]])
        if (n_names <= 10) {
          result <- paste0(result, "\n  ", dim_label, ": ", paste(dn[[i]], collapse = ", "))
        } else {
          result <- paste0(result, "\n  ", dim_label, ": ", paste(dn[[i]][1:5], collapse = ", "),
                           " ... (", n_names, " total)")
        }
      }
    }
  }

  total_elements <- prod(dims)

  # Statistics for numeric matrices
  if (is.numeric(x)) {
    non_na <- x[!is.na(x)]
    if (length(non_na) > 0) {
      result <- paste0(result, "\nRange: [", min(non_na), ", ", max(non_na), "]")
      result <- paste0(result, "\nMean: ", round(mean(non_na), 4),
                       " | SD: ", round(stats::sd(non_na), 4))
    }
  }

  # Missing values
  na_count <- sum(is.na(x))
  if (na_count > 0) {
    result <- paste0(result, "\nMissing values: ", na_count, " (",
                     round(100 * na_count / total_elements, 1), "%)")
  }

  # Data preview (for 2D matrices)
  if (n_dims == 2) {
    if (dims[1] <= 20 && dims[2] <= 10) {
      result <- paste0(result, "\n\nFull data:")
      captured <- utils::capture.output(print(x))
      result <- paste0(result, "\n", paste(captured, collapse = "\n"))
    } else {
      n_rows <- min(5, dims[1])
      n_cols <- min(10, dims[2])
      result <- paste0(result, "\n\nPreview (", n_rows, " x ", n_cols, "):")
      preview <- x[1:n_rows, 1:n_cols, drop = FALSE]
      captured <- utils::capture.output(print(preview))
      result <- paste0(result, "\n", paste(captured, collapse = "\n"))
    }
  } else {
    # For higher-dimensional arrays, use str
    str_out <- utils::capture.output(utils::str(x, vec.len = 5))
    n_str <- min(15, length(str_out))
    result <- paste0(result, "\n\nStructure:\n", paste(str_out[1:n_str], collapse = "\n"))
  }

  result
}

# ---- S3 Objects ----

#' @noRd
inspect_s3 <- function(x) {
  obj_class <- class(x)
  result <- paste0("S3 object [", paste(obj_class, collapse = " < "), "]")

  # Available methods
  all_methods <- character(0)
  for (cls in obj_class) {
    cls_methods <- tryCatch(
      {
        found <- utils::methods(class = cls)
        as.character(found)
      },
      error = function(e) character(0)
    )
    all_methods <- unique(c(all_methods, cls_methods))
  }

  if (length(all_methods) > 0) {
    n_show <- min(15, length(all_methods))
    result <- paste0(result, "\nMethods (", length(all_methods), " total): ",
                     paste(all_methods[1:n_show], collapse = ", "))
    if (length(all_methods) > n_show) {
      result <- paste0(result, " ... and ", length(all_methods) - n_show, " more")
    }
  }

  # For model objects, try summary
  if (any(obj_class %in% c("lm", "glm", "nls", "loess", "aov"))) {
    result <- paste0(result, "\n")
    result <- paste0(result, inspect_model(x))
  } else {
    # Generic component listing
    if (is.list(x)) {
      nms <- names(x)
      if (!is.null(nms) && length(nms) > 0) {
        n_show <- min(15, length(nms))
        result <- paste0(result, "\n\nComponents (", length(nms), "):")
        for (i in seq_len(n_show)) {
          el <- x[[i]]
          el_class <- paste(class(el), collapse = "/")
          el_detail <- if (is.atomic(el) && length(el) == 1) {
            paste0(el_class, ": ", deparse(el, width.cutoff = 50L)[1])
          } else if (is.atomic(el)) {
            paste0(el_class, "[", length(el), "]")
          } else if (is.data.frame(el)) {
            paste0("data.frame [", nrow(el), " x ", ncol(el), "]")
          } else if (is.list(el)) {
            paste0("list[", length(el), "]")
          } else {
            el_class
          }
          result <- paste0(result, "\n  $", nms[i], " : ", el_detail)
        }
        if (length(nms) > n_show) {
          result <- paste0(result, "\n  ... and ", length(nms) - n_show, " more")
        }
      }
    }

    # str() fallback
    str_out <- utils::capture.output(utils::str(x, max.level = 2, vec.len = 3))
    n_str <- min(20, length(str_out))
    result <- paste0(result, "\n\nStructure:\n", paste(str_out[1:n_str], collapse = "\n"))
    if (length(str_out) > n_str) {
      result <- paste0(result, "\n... (", length(str_out) - n_str, " more lines)")
    }
  }

  result
}

# ---- Statistical Models ----

#' @noRd
inspect_model <- function(x) {
  obj_class <- class(x)
  result <- ""

  tryCatch(
    {
      # Formula
      if (!is.null(x$call)) {
        result <- paste0(result, "Call: ", deparse(x$call, width.cutoff = 80L)[1])
      }

      # Coefficients
      coefs <- stats::coef(x)
      if (!is.null(coefs)) {
        n_coefs <- length(coefs)
        result <- paste0(result, "\nCoefficients: ", n_coefs)
        n_show <- min(10, n_coefs)
        coef_text <- paste0(names(coefs)[1:n_show], " = ", round(coefs[1:n_show], 4))
        result <- paste0(result, "\n  ", paste(coef_text, collapse = "\n  "))
        if (n_coefs > n_show) {
          result <- paste0(result, "\n  ... and ", n_coefs - n_show, " more")
        }
      }

      # Model-specific summary
      model_summary <- summary(x)
      if (inherits(x, "lm")) {
        if (!is.null(model_summary$r.squared)) {
          result <- paste0(result, "\nR-squared: ", round(model_summary$r.squared, 4))
          result <- paste0(result, "\nAdj. R-squared: ", round(model_summary$adj.r.squared, 4))
        }
        if (!is.null(model_summary$sigma)) {
          result <- paste0(result, "\nResidual SE: ", round(model_summary$sigma, 4))
        }
        if (!is.null(model_summary$fstatistic)) {
          f_stat <- model_summary$fstatistic
          result <- paste0(result, "\nF-statistic: ", round(f_stat[1], 4),
                           " on ", f_stat[2], " and ", f_stat[3], " DF")
        }
      }

      if (inherits(x, "glm")) {
        result <- paste0(result, "\nFamily: ", x$family$family)
        result <- paste0(result, "\nLink: ", x$family$link)
        if (!is.null(model_summary$aic)) {
          result <- paste0(result, "\nAIC: ", round(model_summary$aic, 2))
        }
        result <- paste0(result, "\nNull deviance: ", round(x$null.deviance, 2),
                         " on ", x$df.null, " DF")
        result <- paste0(result, "\nResidual deviance: ", round(x$deviance, 2),
                         " on ", x$df.residual, " DF")
      }

      # Residuals summary
      resids <- stats::residuals(x)
      if (!is.null(resids) && length(resids) > 0) {
        result <- paste0(result, "\nResiduals: n=", length(resids),
                         " | range=[", round(min(resids), 4), ", ", round(max(resids), 4), "]",
                         " | median=", round(stats::median(resids), 4))
      }

      # Number of observations
      if (!is.null(x$model)) {
        result <- paste0(result, "\nObservations: ", nrow(x$model))
      }
    },
    error = function(e) {
      result <<- paste0(result, "\nError extracting model details: ", e$message)
    }
  )

  result
}

# ---- R6 Objects ----

#' @noRd
inspect_r6 <- function(x) {
  obj_class <- class(x)
  result <- paste0("R6 object [", paste(obj_class, collapse = " < "), "]")

  # Get the class generator for method/field info
  generator <- tryCatch(
    {
      gen_name <- obj_class[1]
      if (exists(gen_name, envir = .GlobalEnv)) {
        get(gen_name, envir = .GlobalEnv)
      } else {
        # Try to find in loaded namespaces
        for (ns in loadedNamespaces()) {
          if (exists(gen_name, envir = asNamespace(ns))) {
            get(gen_name, envir = asNamespace(ns))
            break
          }
        }
      }
    },
    error = function(e) NULL
  )

  # Public methods and fields from the live object
  pub_env <- x
  pub_names <- ls(pub_env, all.names = FALSE)

  pub_methods <- character(0)
  pub_fields <- character(0)
  for (nm in pub_names) {
    val <- tryCatch(get(nm, envir = pub_env), error = function(e) NULL)
    if (is.function(val)) {
      n_args <- length(formals(val))
      pub_methods <- c(pub_methods, paste0(nm, "(", n_args, " args)"))
    } else {
      val_class <- paste(class(val), collapse = "/")
      pub_fields <- c(pub_fields, paste0(nm, " : ", val_class))
    }
  }

  if (length(pub_methods) > 0) {
    result <- paste0(result, "\n\nPublic methods (", length(pub_methods), "):")
    result <- paste0(result, "\n  ", paste(pub_methods, collapse = "\n  "))
  }

  if (length(pub_fields) > 0) {
    n_show <- min(15, length(pub_fields))
    result <- paste0(result, "\n\nPublic fields (", length(pub_fields), "):")
    result <- paste0(result, "\n  ", paste(pub_fields[1:n_show], collapse = "\n  "))
    if (length(pub_fields) > n_show) {
      result <- paste0(result, "\n  ... and ", length(pub_fields) - n_show, " more")
    }
  }

  # Private environment
  priv_env <- x$.__enclos_env__$private
  if (!is.null(priv_env)) {
    priv_names <- ls(priv_env, all.names = TRUE)
    if (length(priv_names) > 0) {
      priv_methods <- 0
      priv_fields <- 0
      for (nm in priv_names) {
        val <- tryCatch(get(nm, envir = priv_env), error = function(e) NULL)
        if (is.function(val)) priv_methods <- priv_methods + 1
        else priv_fields <- priv_fields + 1
      }
      result <- paste0(result, "\n\nPrivate: ", priv_methods, " methods, ", priv_fields, " fields")
    }
  }

  result
}

# ---- S4 Objects ----

#' @noRd
inspect_s4 <- function(x) {
  obj_class <- class(x)
  result <- paste0("S4 object [", paste(obj_class, collapse = " < "), "]")

  # Slot information
  slot_names <- methods::slotNames(x)
  if (length(slot_names) > 0) {
    result <- paste0(result, "\n\nSlots (", length(slot_names), "):")
    for (sn in slot_names) {
      slot_val <- tryCatch(methods::slot(x, sn), error = function(e) NULL)
      if (!is.null(slot_val)) {
        slot_class <- paste(class(slot_val), collapse = "/")
        slot_detail <- if (is.atomic(slot_val) && length(slot_val) == 1) {
          paste0(slot_class, ": ", deparse(slot_val, width.cutoff = 50L)[1])
        } else if (is.atomic(slot_val)) {
          paste0(slot_class, "[", length(slot_val), "]")
        } else if (is.data.frame(slot_val)) {
          paste0("data.frame [", nrow(slot_val), " x ", ncol(slot_val), "]")
        } else {
          slot_class
        }
        result <- paste0(result, "\n  @", sn, " : ", slot_detail)
      } else {
        result <- paste0(result, "\n  @", sn, " : (inaccessible)")
      }
    }
  }

  # Class hierarchy
  super_classes <- tryCatch(
    {
      class_def <- methods::getClass(obj_class[1])
      if (!is.null(class_def@contains) && length(class_def@contains) > 0) {
        names(class_def@contains)
      } else {
        NULL
      }
    },
    error = function(e) NULL
  )

  if (!is.null(super_classes) && length(super_classes) > 0) {
    result <- paste0(result, "\n\nInherits from: ", paste(super_classes, collapse = " < "))
  }

  # Check for virtual class
  is_virtual <- tryCatch(
    methods::isVirtualClass(obj_class[1]),
    error = function(e) FALSE
  )
  if (is_virtual) {
    result <- paste0(result, "\nVirtual class: yes")
  }

  # Available methods
  s4_methods <- tryCatch(
    {
      found <- utils::methods(class = obj_class[1])
      as.character(found)
    },
    error = function(e) character(0)
  )

  if (length(s4_methods) > 0) {
    n_show <- min(10, length(s4_methods))
    result <- paste0(result, "\n\nMethods (", length(s4_methods), "): ",
                     paste(s4_methods[1:n_show], collapse = ", "))
    if (length(s4_methods) > n_show) {
      result <- paste0(result, " ... and ", length(s4_methods) - n_show, " more")
    }
  }

  result
}

# ---- Formulas ----

#' @noRd
inspect_formula <- function(x) {
  result <- paste0("Formula: ", deparse(x, width.cutoff = 80L)[1])

  # Extract variable names
  all_vars <- all.vars(x)
  result <- paste0(result, "\nVariables: ", paste(all_vars, collapse = ", "))

  # Response vs predictors
  if (length(x) == 3) {
    response <- deparse(x[[2]], width.cutoff = 80L)[1]
    predictors <- deparse(x[[3]], width.cutoff = 80L)[1]
    result <- paste0(result, "\nResponse: ", response)
    result <- paste0(result, "\nPredictors: ", predictors)
  } else if (length(x) == 2) {
    result <- paste0(result, "\nOne-sided formula (no response)")
    predictors <- deparse(x[[2]], width.cutoff = 80L)[1]
    result <- paste0(result, "\nTerms: ", predictors)
  }

  # Environment
  formula_env <- environment(x)
  if (!is.null(formula_env)) {
    env_name <- if (identical(formula_env, .GlobalEnv)) {
      "Global Environment"
    } else if (isNamespace(formula_env)) {
      paste0("Namespace: ", environmentName(formula_env))
    } else {
      format(formula_env)
    }
    result <- paste0(result, "\nEnvironment: ", env_name)
  }

  result
}

# ---- Date/Time ----

#' @noRd
inspect_datetime <- function(x) {
  obj_class <- class(x)
  len <- length(x)

  result <- paste0(paste(obj_class, collapse = "/"), " of length ", len)

  if (len == 0) {
    return(paste0(result, "\nEmpty"))
  }

  # Timezone for POSIXct/POSIXlt
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    tz <- attr(x, "tzone")
    if (is.null(tz) || !nzchar(tz[1])) tz <- Sys.timezone()
    result <- paste0(result, "\nTimezone: ", tz[1])
  }

  # Range
  non_na <- x[!is.na(x)]
  if (length(non_na) > 0) {
    result <- paste0(result, "\nRange: [", format(min(non_na)), " ... ", format(max(non_na)), "]")

    if (length(non_na) > 1 && inherits(x, c("POSIXct", "POSIXlt"))) {
      span <- difftime(max(non_na), min(non_na), units = "auto")
      result <- paste0(result, "\nSpan: ", format(span))
    }
  }

  # Sample values
  if (len <= 10) {
    result <- paste0(result, "\nValues: ", paste(format(x), collapse = ", "))
  } else {
    result <- paste0(result, "\nFirst 5: ", paste(format(utils::head(x, 5)), collapse = ", "))
    result <- paste0(result, "\nLast 5: ", paste(format(utils::tail(x, 5)), collapse = ", "))
  }

  # Missing values
  na_count <- sum(is.na(x))
  if (na_count > 0) {
    result <- paste0(result, "\nMissing values: ", na_count, " (", round(100 * na_count / len, 1), "%)")
  }

  result
}

# ---- Environments ----

#' @noRd
inspect_environment <- function(x) {
  env_name <- environmentName(x)
  if (!nzchar(env_name)) env_name <- format(x)

  result <- paste0("Environment: ", env_name)

  # Parent chain
  parents <- character(0)
  current <- parent.env(x)
  depth <- 0
  while (!identical(current, emptyenv()) && depth < 10) {
    pname <- environmentName(current)
    if (!nzchar(pname)) pname <- format(current)
    parents <- c(parents, pname)
    current <- parent.env(current)
    depth <- depth + 1
  }

  if (length(parents) > 0) {
    result <- paste0(result, "\nParent chain: ", paste(parents, collapse = " -> "))
  }

  # Bindings
  bindings <- ls(x, all.names = TRUE)
  result <- paste0(result, "\nBindings: ", length(bindings))

  if (length(bindings) > 0) {
    n_show <- min(20, length(bindings))
    result <- paste0(result, "\n\nContents:")
    for (i in seq_len(n_show)) {
      nm <- bindings[i]
      val <- tryCatch(get(nm, envir = x), error = function(e) NULL)
      if (!is.null(val)) {
        val_class <- paste(class(val), collapse = "/")
        val_detail <- if (is.function(val)) {
          paste0("function(", length(formals(val)), " args)")
        } else if (is.atomic(val) && length(val) == 1) {
          paste0(val_class, ": ", deparse(val, width.cutoff = 50L)[1])
        } else if (is.atomic(val)) {
          paste0(val_class, "[", length(val), "]")
        } else {
          val_class
        }
        result <- paste0(result, "\n  ", nm, " : ", val_detail)
      }
    }
    if (length(bindings) > n_show) {
      result <- paste0(result, "\n  ... and ", length(bindings) - n_show, " more")
    }
  }

  # Locked status
  if (environmentIsLocked(x)) {
    result <- paste0(result, "\nLocked: yes")
  }

  result
}

# ---- Default Fallback ----

#' @noRd
inspect_default <- function(x) {
  obj_class <- paste(class(x), collapse = ", ")
  obj_type <- typeof(x)

  result <- paste0("Object [", obj_class, "] (type: ", obj_type, ")")

  # Try str()
  str_out <- utils::capture.output(utils::str(x, max.level = 2, vec.len = 5))
  if (length(str_out) > 0) {
    n_str <- min(20, length(str_out))
    result <- paste0(result, "\n\nStructure:\n", paste(str_out[1:n_str], collapse = "\n"))
    if (length(str_out) > n_str) {
      result <- paste0(result, "\n... (", length(str_out) - n_str, " more lines)")
    }
  }

  # Try print()
  print_out <- utils::capture.output(print(x))
  if (length(print_out) > 0) {
    n_print <- min(15, length(print_out))
    result <- paste0(result, "\n\nPrint output:\n", paste(print_out[1:n_print], collapse = "\n"))
    if (length(print_out) > n_print) {
      result <- paste0(result, "\n... (", length(print_out) - n_print, " more lines)")
    }
  }

  result
}
