# Object Class Inspection Functions
# Inspect S3, statistical model, R6, and S4 object families.
# Loaded with inspect-object.R so dispatch can delegate to class-specific inspectors.

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

#' Find an R6 class generator by class name
#'
#' @param gen_name R6 class generator name.
#' @return R6 class generator or NULL.
#' @noRd
find_r6_generator <- function(gen_name) {
  if (exists(gen_name, envir = .GlobalEnv, inherits = FALSE)) {
    return(get(gen_name, envir = .GlobalEnv))
  }

  for (ns in loadedNamespaces()) {
    ns_env <- asNamespace(ns)
    if (exists(gen_name, envir = ns_env, inherits = FALSE)) {
      return(get(gen_name, envir = ns_env))
    }
  }

  NULL
}

#' @noRd
inspect_r6 <- function(x) {
  obj_class <- class(x)
  result <- paste0("R6 object [", paste(obj_class, collapse = " < "), "]")

  # Get the class generator for method/field info
  generator <- tryCatch(
    find_r6_generator(obj_class[1]),
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
