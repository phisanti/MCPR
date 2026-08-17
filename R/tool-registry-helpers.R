# Tool Registry Helper Functions
# Helper functions for tool discovery and roxygen2 parsing in the ToolRegistry system.
# Provides roxygen2 block processing, function extraction, and tool metadata conversion.

#' Create Tool from Roxygen Block
#'
#' @include tool-definition.R
#' @include type-conversion-utilities.R
#' @include utils.R
#' @title Create Tool from Roxygen Block
#' @description Constructs ToolDef object from parsed roxygen block and associated function.
#' Extracts function metadata, validates function existence, and converts roxygen2
#' documentation into structured tool specification. Handles error cases and provides
#' comprehensive logging for tool creation workflow.
#'
#' @param block Roxygen2 block object containing function documentation
#' @param env Environment where the function is defined
#' @param file_path Path of file being parsed (for logging purposes)
#' @return ToolDef object or NULL on failure
#' @noRd
create_tool_from_block <- function(block, env, file_path) {
  # Extract function name from the block object
  func_name <- block$object$alias

  if (is.null(func_name) || !exists(func_name, envir = env)) {
    cli::cli_warn("Function {.fn {func_name %||% 'unknown'}} not found in {.file {basename(file_path)}}")
    return(NULL)
  }

  func <- get(func_name, envir = env)
  if (!is.function(func)) {
    cli::cli_warn("{.fn {func_name}} is not a function")
    return(NULL)
  }

  # Extract description
  description <- extract_description(block)

  # Roxygen auto-discovery infers types from documentation and requiredness
  # from the callable interface. This mirrors Python SDK-style signature
  # inference while leaving explicit tool() schemas authoritative.
  param_tags <- Filter(function(tag) inherits(tag, "roxy_tag_param"), block$tags)
  mcpr_args <- convert_to_schema(
    param_tags,
    function_name = func_name,
    file_path = file_path
  )
  mcpr_args <- apply_formal_requiredness(mcpr_args, formals(func))
  check_schema_covers_formals(mcpr_args, formals(func), func_name, file_path)

  # Check for companion annotations variable (.{func_name}_annotations)
  annotations_var <- paste0(".", func_name, "_annotations")
  annotations <- if (exists(annotations_var, envir = env)) {
    get(annotations_var, envir = env)
  } else {
    list()
  }

  # Create the tool using new ToolDef system
  tryCatch(
    {
      tool(
        fun = func,
        name = func_name,
        description = description,
        arguments = mcpr_args,
        annotations = annotations
      )
    },
    error = function(e) {
      cli::cli_warn("Failed to create tool for {.fn {func_name}}: {conditionMessage(e)}")
      NULL
    }
  )
}

#' Apply Requiredness from Function Formals
#'
#' @title Apply Requiredness from Function Formals
#' @description Updates roxygen-derived mcpr_type argument specs so parameters
#' with default values are not emitted as JSON Schema required properties.
#' This helper is intentionally limited to the auto-discovery path used by
#' `ToolRegistry`. Explicit schemas constructed with `tool()` remain the source
#' of truth for requiredness, matching the schema-first pattern used by the
#' official TypeScript SDK. For roxygen-discovered wrappers, requiredness is
#' inferred from R formals, matching the signature-first pattern used by the
#' official Python SDK.
#'
#' @param arguments Named list of mcpr_type argument definitions
#' @param formals Function formals pairlist
#' @return Updated named list of mcpr_type argument definitions
#' @noRd
apply_formal_requiredness <- function(arguments, formals) {
  if (!is.list(arguments) || length(arguments) == 0 || is.null(formals)) {
    return(arguments)
  }

  for (arg_name in intersect(names(arguments), names(formals))) {
    # In R, any formal with a default value can be omitted by the caller.
    # JSON Schema expresses that by excluding the parameter from `required`.
    if (!rlang::is_missing(formals[[arg_name]])) {
      arguments[[arg_name]]$required <- FALSE
    }
  }

  arguments
}

#' Check Roxygen Schema Covers Every Formal
#'
#' @title Check Roxygen Schema Covers Every Formal
#' @description Aborts when the roxygen-derived argument schema does not line
#' up with the function's formals. `tool()` rejects such a mismatch anyway, but
#' it does so from inside `create_tool_from_block()`'s error handler, which
#' downgrades the failure to a warning and drops the tool from the registry.
#'
#' The common cause is a `@param` roxygen2 refused to parse. roxygen2 requires
#' braces and quotes inside a tag to be balanced, so an unterminated
#' `object{...}` refinement makes it discard the whole tag — leaving the
#' parameter undocumented and, without this check, the tool silently missing.
#'
#' @param arguments Named list of mcpr_type argument definitions
#' @param formals Function formals pairlist
#' @param function_name Name of the function being registered
#' @param file_path Path of file being parsed (for error reporting)
#' @return None (throws on mismatch)
#' @noRd
check_schema_covers_formals <- function(arguments, formals, function_name, file_path) {
  # `...` names no single value, so it has no schema to be missing. Whether a
  # tool may take it at all is `tool()`'s call, not this check's.
  formal_names <- setdiff(names(formals), "...")
  documented <- setdiff(names(arguments), "...")

  undocumented <- setdiff(formal_names, documented)
  unmatched <- setdiff(documented, formal_names)

  if (length(undocumented) == 0 && length(unmatched) == 0) {
    return(invisible(NULL))
  }

  cli::cli_abort(c(
    "Roxygen tool schema does not match the formals of {.fn {function_name}} \\
    (file = {file_path}).",
    "x" = if (length(undocumented) > 0) "Undocumented parameter{?s}: {.val {undocumented}}",
    "x" = if (length(unmatched) > 0) "Documented but not a parameter: {.val {unmatched}}",
    "i" = "Every parameter needs a {.code @param <name> <type> <description>} tag.",
    "i" = "roxygen2 silently discards a {.code @param} whose braces or quotes are unbalanced."
  ), .subclass = "mcpr_unsupported_type_error")
}

#' Extract Description from Roxygen Block
#'
#' @title Extract Description from Roxygen Block
#' @description Extracts function description from roxygen2 documentation tags with fallback strategy.
#' Prioritizes @description tag content, falls back to @intro tag, and provides default
#' message for missing documentation. Ensures consistent description extraction for
#' tool specification creation through tag hierarchy processing.
#'
#' @param block Roxygen2 block object containing documentation tags
#' @return Character string with extracted description
#' @noRd
extract_description <- function(block) {
  # Look for @description tag first
  desc_tag <- Find(function(tag) inherits(tag, "roxy_tag_description"), block$tags)
  if (!is.null(desc_tag)) {
    return(paste(desc_tag$val, collapse = " "))
  }

  # Fall back to title/introduction
  intro_tag <- Find(function(tag) inherits(tag, "roxy_tag_intro"), block$tags)
  if (!is.null(intro_tag)) {
    return(paste(intro_tag$val, collapse = " "))
  }

  # Default
  return("No description available")
}

#' Convert Roxygen Parameters to MCPR Types
#'
#' @title Convert Roxygen Parameters to MCPR Types
#' @description Converts roxygen2 @param tags into MCPR type definitions through heuristic analysis.
#' Analyzes parameter descriptions for type hints and maps to appropriate MCPR type
#' specifications. Provides automatic type inference for tool parameter validation
#' and MCP protocol compatibility through description keyword matching.
#'
#' @param param_tags List of roxy_tag_param objects from roxygen2 parsing
#' @return Named list of MCPR type objects for tool arguments
#' @noRd
convert_to_schema <- function(param_tags, function_name = NULL, file_path = NULL) {
  mcpr_args <- list()

  for (param_tag in param_tags) {
    # Parse the val field which contains "param_name type description"
    val_str <- paste(param_tag$val, collapse = " ")
    val_parts <- trimws(strsplit(val_str, "\\s+", perl = TRUE)[[1]])

    if (length(val_parts) < 2) {
      next
    }

    # roxygen2 lets one tag document several parameters as `@param x,y ...`.
    # They share a declaration, so build the spec once and bind it to each.
    param_label <- val_parts[1]
    param_names <- trimws(strsplit(param_label, ",", fixed = TRUE)[[1]])
    param_names <- param_names[nzchar(param_names)]

    if (length(param_names) == 0) {
      next
    }

    type_and_desc <- paste(val_parts[-1], collapse = " ")

    declaration <- split_definition_declaration(
      type_and_desc,
      ctx = definition_type_context(param_label, function_name, file_path)
    )

    if (is.null(declaration)) {
      abort_unsupported_mcpr_definition_type(
        val_parts[2],
        parameter_name = param_label,
        function_name = function_name,
        file_path = file_path
      )
    }

    # Create proper mcpr_type objects directly
    spec <- map_type_schema(
      declaration$type,
      description = declaration$description,
      input_type = "definition",
      parameter_name = param_label,
      function_name = function_name,
      file_path = file_path
    )

    for (param_name in param_names) {
      mcpr_args[[param_name]] <- spec
    }
  }

  mcpr_args
}
