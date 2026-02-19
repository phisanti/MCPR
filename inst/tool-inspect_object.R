# Inspect Object Tool
# Deep analysis of R objects in the current workspace.
# Auto-detects object type and provides structured inspection for AI agents.

#* @mcp_tool
#' Inspect an R object in the current workspace
#'
#' @description Deep analysis of a specific R object. Auto-detects the object type (data frame, vector, list, factor, matrix, function, S3/S4/R6, formula, date/time, environment) and returns a detailed structural and statistical summary. Use this for understanding data structures, examining function definitions, analyzing models, or inspecting any named R object. For session state, errors, terminal output, packages, or help docs, use the view tool instead.
#' @param object_name character The name of the R object to inspect. Must be an existing object in the current R environment (e.g., "mtcars", "my_model", "my_function").
#' @keywords mcpr_tool
#' @return Detailed formatted analysis of the specified R object
inspect_object <- function(object_name) {
  if (!is.character(object_name) || length(object_name) != 1) {
    stop("Object name must be a single character string")
  }

  if (nchar(trimws(object_name)) == 0) {
    stop("Object name cannot be empty")
  }

  object_name <- trimws(object_name)

  # Resolve the object: check GlobalEnv first, then search path
  obj <- NULL
  found_env <- NULL

  if (exists(object_name, envir = .GlobalEnv)) {
    obj <- get(object_name, envir = .GlobalEnv)
    found_env <- "Global Environment"
  } else {
    for (env_name in search()) {
      env <- as.environment(env_name)
      if (exists(object_name, envir = env, inherits = FALSE)) {
        obj <- get(object_name, envir = env)
        found_env <- env_name
        break
      }
    }
  }

  if (is.null(obj)) {
    # Object not found — provide helpful suggestions
    all_objects <- ls(envir = .GlobalEnv)
    suggestions <- all_objects[agrep(object_name, all_objects, max.distance = 0.3)]

    msg <- paste0("Object '", object_name, "' not found in current environment or search path")
    if (length(suggestions) > 0) {
      msg <- paste0(msg, "\nDid you mean: ", paste(suggestions, collapse = ", "), "?")
    }
    if (length(all_objects) > 0) {
      n_show <- min(10, length(all_objects))
      msg <- paste0(msg, "\nAvailable objects: ", paste(all_objects[1:n_show], collapse = ", "))
      if (length(all_objects) > n_show) {
        msg <- paste0(msg, " ... and ", length(all_objects) - n_show, " more")
      }
    }
    stop(msg)
  }

  # Run inspection
  tryCatch(
    {
      result <- MCPR:::inspect_dispatch(obj, object_name)

      # Add source environment note if not GlobalEnv
      if (!is.null(found_env) && found_env != "Global Environment") {
        result <- paste0(result, "\n\n(found in: ", found_env, ")")
      }

      result
    },
    error = function(e) {
      paste0("Error inspecting '", object_name, "': ", e$message,
             "\nClass: ", paste(class(obj), collapse = ", "),
             "\nType: ", typeof(obj))
    }
  )
}

#' @export
inspect_object <- inspect_object
