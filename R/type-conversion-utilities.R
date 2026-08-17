# Type Conversion Utilities
# Core serialization functions and type specifications for MCP protocol compatibility.
# Also owns the roxygen type-declaration grammar (`enum(...)`, `object{...}`) that
# turns a @param tag into an mcpr_type; keep the whole parser in this file.

#' Serialize R Object to JSON for MCP
#'
#' @include type-conversion-from-json.R
#' @include type-conversion-to-json.R
#' @include utils.R
#' @title Serialize R Object to JSON for MCP
#' @description Converts R objects to JSON string format for MCP protocol transmission.
#' Handles type preservation, object size management, and custom serialization through
#' comprehensive conversion pipeline. Enables seamless JSON-RPC communication with
#' maintained R object semantics and MCP protocol compatibility.
#'
#' @param x R object to serialize
#' @param pretty Whether to pretty-print the JSON
#' @param auto_unbox Whether to automatically unbox single-element vectors
#' @param size_limit Maximum object size in bytes before large object handling (default: 1MB)
#' @param custom_serializers List of custom serializers for specific classes
#' @return JSON string representation of the R object
#' @examples
#' mcpr_serialize(list(result = 42, message = "success"))
#' @noRd
mcpr_serialize <- function(x, pretty = FALSE, auto_unbox = TRUE, size_limit = 1e6, custom_serializers = get_mcpr_serializers()) {
  # Convert to MCP-compatible format
  mcp_obj <- to_mcpr_json(x, auto_unbox = auto_unbox, size_limit = size_limit, custom_serializers = custom_serializers)

  # Serialize to JSON
  jsonlite::toJSON(
    mcp_obj,
    pretty = pretty,
    auto_unbox = FALSE, # We handle unboxing in to_mcpr_json
    null = "null",
    na = "null"
  )
}

#' Deserialize JSON to R Object from MCP
#'
#' @title Deserialize JSON to R Object from MCP
#' @description Converts JSON string back to R object with type reconstruction for MCP protocol.
#' Reverses serialization process to restore original R object semantics from JSON-RPC
#' transmission. Handles type markers and maintains object integrity through automatic
#' deserialization pipeline.
#'
#' @param json JSON string to deserialize
#' @return Reconstructed R object with preserved types
#' @examples
#' mcpr_deserialize('{"result": 42, "message": "success"}')
#' @noRd
mcpr_deserialize <- function(json) {
  from_mcpr_json(json)
}

#' Check Object Serialization Compatibility
#'
#' @title Check Object Serialization Compatibility
#' @description Tests whether R object can be safely serialized to JSON format for MCP protocol.
#' Performs validation check through actual serialization attempt with error handling.
#' Enables pre-serialization validation for robust MCP communication workflows.
#'
#' @param x R object to check for serialization compatibility
#' @return TRUE if object can be serialized, FALSE otherwise
#' @noRd
can_serialize <- function(x) {
  tryCatch(
    {
      mcpr_serialize(x)
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}


#' Stream Large Data Frames
#'
#' @title Stream Large Data Frames
#' @description Creates streaming converter for large data frames through chunked processing.
#' Handles memory-efficient transmission of large datasets by breaking into manageable
#' chunks with callback-based processing. Enables scalable data transfer for MCP
#' protocol without memory overflow.
#'
#' @param df Data frame to stream in chunks
#' @param chunk_size Number of rows per processing chunk
#' @param callback Function to call with each processed chunk
#' @return None (processes chunks through callback)
#' @noRd
stream_dataframe <- function(df, chunk_size = 1000, callback) {
  n_rows <- nrow(df)
  n_chunks <- ceiling(n_rows / chunk_size)

  for (i in seq_len(n_chunks)) {
    start_row <- (i - 1) * chunk_size + 1
    end_row <- min(i * chunk_size, n_rows)

    chunk <- df[start_row:end_row, , drop = FALSE]
    chunk_json <- to_mcpr_json(chunk, size_limit = Inf)

    callback(list(
      chunk = i,
      total_chunks = n_chunks,
      start_row = start_row,
      end_row = end_row,
      data = chunk_json
    ))
  }
}


# Type definitions for MCPR - now using simpler R structures instead of S7

mcpr_supported_definition_types <- function(include_aliases = TRUE) {
  canonical <- c(
    "string", "number", "integer", "boolean", "enum",
    "object", "array", "json_object", "json_array"
  )

  if (!include_aliases) {
    return(canonical)
  }

  c(canonical, "character", "numeric", "int", "logical", "bool", "list", "named_list")
}

# `enum` and `object` are the only tokens that take a refinement suffix, and
# each takes exactly one delimiter: parens list the permitted values, braces
# list the named fields.
definition_refinement_delimiters <- c(enum = "(", object = "{")

closing_delimiters <- c("(" = ")", "{" = "}")

# The leading type name of a declaration. Shared by the two scanners below so
# they cannot drift apart on what counts as a token.
definition_token_pattern <- "^[A-Za-z_][A-Za-z0-9_]*"

#' @noRd
definition_type_context <- function(parameter_name = NULL,
                                    function_name = NULL,
                                    file_path = NULL) {
  list(
    parameter_name = parameter_name,
    function_name = function_name,
    file_path = file_path
  )
}

#' @noRd
definition_type_details <- function(ctx) {
  details <- c(
    if (!is.null(ctx$parameter_name)) paste("parameter =", ctx$parameter_name),
    if (!is.null(ctx$function_name)) paste("function =", ctx$function_name),
    if (!is.null(ctx$file_path)) paste("file =", ctx$file_path)
  )

  if (length(details) > 0) {
    paste0(" (", paste(details, collapse = ", "), ")")
  } else {
    ""
  }
}

#' @noRd
abort_unsupported_mcpr_definition_type <- function(type_str,
                                                   parameter_name = NULL,
                                                   function_name = NULL,
                                                   file_path = NULL) {
  supported <- mcpr_supported_definition_types(include_aliases = FALSE)
  # A bare `enum` is itself an error, so advertise the form that actually
  # works rather than sending the reader into a second, different abort.
  supported[supported == "enum"] <- "enum(a|b)"
  details_text <- definition_type_details(
    definition_type_context(parameter_name, function_name, file_path)
  )

  cli::cli_abort(c(
    "Unsupported MCPR type declaration {.val {type_str}}{details_text}.",
    "i" = "Use one of the supported MCPR types in the first token of the declaration.",
    "i" = "Supported types: {.val {supported}}",
    "i" = "For arbitrary named-list / JSON payloads, use {.val json_object}. For arbitrary arrays, use {.val json_array}."
  ), .subclass = "mcpr_unsupported_type_error")
}

#' @noRd
# Raised when the type name is recognised but its refinement is malformed.
# Shares the `mcpr_unsupported_type_error` class so `ToolRegistry` re-raises it
# instead of warning and dropping the tool: a typo in a schema declaration must
# never cost a tool silently.
#' @noRd
# Both declaration scanners report an unterminated refinement identically.
abort_unterminated_definition_delimiter <- function(declaration, delimiter, ctx) {
  abort_malformed_mcpr_definition_type(
    declaration,
    paste0("Unterminated `", delimiter, "` in the type declaration."),
    ctx
  )
}

#' @noRd
abort_malformed_mcpr_definition_type <- function(declaration, reason, ctx) {
  details_text <- definition_type_details(ctx)
  syntax <- "enum(value1|value2) or object{field: type, optional?: type}"

  # `reason`, `declaration` and `syntax` all routinely contain braces and
  # backticks. They are interpolated as values, never spliced into the format
  # string, so cli never tries to evaluate a fragment of a schema declaration.
  cli::cli_abort(c(
    "Malformed MCPR type declaration {.val {declaration}}{details_text}.",
    "x" = "{reason}",
    "i" = "Refine a type as {.code {syntax}}."
  ), .subclass = "mcpr_unsupported_type_error")
}

#' Type Specifications for MCP Protocol
#'
#' @title Type Specifications for MCP Protocol
#' @description Specifies object types for MCP tool calling and structured data extraction.
#' Based on JSON Schema standards for API compatibility with comprehensive R type mapping.
#' Enables precise type definitions for tool parameters and return values through
#' standardized type specification system.
#'
#' * `type_boolean()`, `type_integer()`, `type_number()`, and `type_string()`
#'   each represent scalars. These are equivalent to length-1 logical,
#'   integer, double, and character vectors (respectively).
#'
#' * `type_enum()` is equivalent to a length-1 factor; it is a string that can
#'   only take the specified values.
#'
#' * `type_array()` is equivalent to a vector in R. You can use it to represent
#'   an atomic vector: e.g. `type_array(type_boolean())` is equivalent
#'   to a logical vector and `type_array(type_string())` is equivalent
#'   to a character vector). You can also use it to represent a list of more
#'   complicated types where every element is the same type (R has no base
#'   equivalent to this), e.g. `type_array(type_array(type_string()))`
#'   represents a list of character vectors.
#'
#' * `type_object()` is equivalent to a named list in R, but where every element
#'   must have the specified type. For example,
#'   `type_object(a = type_string(), b = type_array(type_integer()))` is
#'   equivalent to a list with an element called `a` that is a string and
#'   an element called `b` that is an integer vector.
#'
#' * `type_json_object()` is equivalent to an arbitrary JSON object / named list.
#'   Use it when the tool should receive flexible key-value data as a named list
#'   instead of a fixed schema declared with `type_object()`.
#'
#' * `type_json_array()` is equivalent to an arbitrary JSON array / list. Use it
#'   when the tool should receive flexible list payloads instead of a homogenous
#'   typed vector declared with `type_array()`.
#'
#' @param description,.description The purpose of the component. This is
#'   used by the LLM to determine what values to pass to the tool or what
#'   values to extract in the structured data, so the more detail that you can
#'   provide here, the better.
#' @param required,.required Is the component or argument required?
#'
#'   In type descriptions for structured data, if `required = FALSE` and the
#'   component does not exist in the data, the LLM may hallucinate a value. Only
#'   applies when the element is nested inside of a `type_object()`.
#'
#'   In tool definitions, `required = TRUE` signals that the LLM should always
#'   provide a value. Arguments with `required = FALSE` should have a default
#'   value in the tool function's definition. If the LLM does not provide a
#'   value, the default value will be used.
#' @examples
#' # An integer vector
#' type_array(type_integer())
#'
#' # The closest equivalent to a data frame is an array of objects
#' type_array(type_object(
#'   x = type_boolean(),
#'   y = type_string(),
#'   z = type_number()
#' ))
#'
#' # There's no specific type for dates, but you use a string with the
#' # requested format in the description (it is not guaranteed that you will
#' # get this format back, but you should most of the time)
#' type_string("The creation date, in YYYY-MM-DD format.")
#' type_string("The update date, in dd/mm/yyyy format.")
#' @export
type_boolean <- function(description = NULL, required = TRUE, error = NULL) {
  structure(list(type = "boolean", description = description, required = required, error = error), class = "mcpr_type")
}
#' @rdname type_boolean
#' @export
type_integer <- function(description = NULL, required = TRUE, error = NULL) {
  structure(list(type = "integer", description = description, required = required, error = error), class = "mcpr_type")
}
#' @rdname type_boolean
#' @export
type_number <- function(description = NULL, required = TRUE, error = NULL) {
  structure(list(type = "number", description = description, required = required, error = error), class = "mcpr_type")
}
#' @rdname type_boolean
#' @export
type_string <- function(description = NULL, required = TRUE, error = NULL) {
  structure(list(type = "string", description = description, required = required, error = error), class = "mcpr_type")
}

#' @param values Character vector of permitted values.
#' @rdname type_boolean
#' @export
type_enum <- function(values, description = NULL, required = TRUE, error = NULL) {
  structure(list(type = "enum", values = values, description = description, required = required, error = error), class = "mcpr_type")
}

#' @param items The type of the array items. Can be created by any of the
#'   `type_` function.
#' @rdname type_boolean
#' @export
type_array <- function(items, description = NULL, required = TRUE, error = NULL) {
  structure(list(type = "array", items = items, description = description, required = required, error = error), class = "mcpr_type")
}

#' @param ... Name-type pairs defineing the components that the object must
#'   possess.
#' @param .additional_properties Can the object have arbitrary additional
#'   properties that are not explicitly listed? Only supported by Claude.
#' @rdname type_boolean
#' @export
type_object <- function(
  .description = NULL,
  ...,
  .required = TRUE,
  .additional_properties = FALSE,
  .error = NULL
) {
  structure(list(
    type = "object",
    properties = list(...),
    description = .description,
    required = .required,
    additional_properties = .additional_properties,
    error = .error
  ), class = "mcpr_type")
}

#' @rdname type_boolean
#' @export
type_json_object <- function(description = NULL, required = TRUE, error = NULL) {
  structure(
    list(type = "json_object", description = description, required = required, error = error),
    class = "mcpr_type"
  )
}

#' @rdname type_boolean
#' @export
type_json_array <- function(description = NULL, required = TRUE, error = NULL) {
  structure(
    list(type = "json_array", description = description, required = required, error = error),
    class = "mcpr_type"
  )
}


#' Convert Type Definition to MCPR Type
#'
#' @title Convert Type Definition to MCPR Type
#' @description Converts type information to MCPR type objects supporting multiple input formats.
#' Handles string-based type definitions from roxygen documentation and JSON Schema objects
#' from MCP servers. Provides unified type conversion interface for tool parameter
#' specification and validation across different type sources.
#'
#' @param type_str Type string (e.g., "character", "numeric") or JSON schema object
#' @param description Parameter description (used for string input type)
#' @param input_type Either "definition" for string-based input or "json" for JSON schema objects
#' @return MCPR type object with appropriate specification
#' @noRd
map_type_schema <- function(type_str,
                            description = NULL,
                            input_type = "definition",
                            parameter_name = NULL,
                            function_name = NULL,
                            file_path = NULL) {
  if (input_type == "json") {
    return(map_json_type_schema(type_str))
  }

  map_definition_type_schema(
    type_str,
    description = description,
    parameter_name = parameter_name,
    function_name = function_name,
    file_path = file_path
  )
}

#' @noRd
map_json_type_schema <- function(schema) {
  type_context <- json_type_context(schema)

  if (!is.null(type_context$extension_type)) {
    extension <- map_json_extension_type(type_context)
    if (!is.null(extension)) {
      return(extension)
    }
  }

  if (!is.null(schema$enum)) {
    return(type_enum(
      schema$enum,
      description = type_context$description,
      required = type_context$required,
      error = type_context$error
    ))
  }

  dispatch_json_type_schema(schema, type_context)
}

#' @noRd
json_type_context <- function(schema) {
  list(
    extension_type = schema[["x-mcpr-type"]] %||% NULL,
    description = schema$description %||% NULL,
    required = TRUE,
    error = schema[["x-mcpr-error"]] %||% NULL
  )
}

#' @noRd
map_json_extension_type <- function(type_context) {
  switch(type_context$extension_type,
    "json_object" = type_json_object(
      description = type_context$description,
      required = type_context$required,
      error = type_context$error
    ),
    "json_array" = type_json_array(
      description = type_context$description,
      required = type_context$required,
      error = type_context$error
    ),
    NULL
  )
}

#' @noRd
dispatch_json_type_schema <- function(schema, type_context) {
  switch(schema$type %||% "string",
    "string" = type_string(
      description = type_context$description,
      required = type_context$required,
      error = type_context$error
    ),
    "number" = type_number(
      description = type_context$description,
      required = type_context$required,
      error = type_context$error
    ),
    "integer" = type_integer(
      description = type_context$description,
      required = type_context$required,
      error = type_context$error
    ),
    "boolean" = type_boolean(
      description = type_context$description,
      required = type_context$required,
      error = type_context$error
    ),
    "array" = map_json_array_schema(schema, type_context),
    "object" = map_json_object_schema(schema, type_context),
    type_string(
      description = type_context$description,
      required = type_context$required,
      error = type_context$error
    )
  )
}

#' @noRd
map_json_array_schema <- function(schema, type_context) {
  items_type <- if (!is.null(schema$items)) {
    map_type_schema(schema$items, input_type = "json")
  } else {
    type_string()
  }

  type_array(
    items_type,
    description = type_context$description,
    required = type_context$required,
    error = type_context$error
  )
}

#' @noRd
map_json_object_schema <- function(schema, type_context) {
  additional <- isTRUE(schema$additionalProperties)
  props <- map_json_object_properties(schema$properties)

  do.call(
    type_object,
    c(
      list(
        .description = type_context$description,
        .required = type_context$required,
        .additional_properties = additional,
        .error = type_context$error
      ),
      props
    )
  )
}

#' @noRd
map_json_object_properties <- function(properties) {
  if (is.null(properties)) {
    return(list())
  }

  props <- list()
  for (prop_name in names(properties)) {
    props[[prop_name]] <- map_type_schema(properties[[prop_name]], input_type = "json")
  }

  props
}

#' @noRd
map_definition_type_schema <- function(type_str,
                                       description = NULL,
                                       parameter_name = NULL,
                                       function_name = NULL,
                                       file_path = NULL) {
  build_definition_type(
    type_str,
    # Top-level parameters have always carried a description, empty or not.
    description = description %||% "",
    ctx = definition_type_context(parameter_name, function_name, file_path)
  )
}

#' @noRd
# Builds an mcpr_type from one type declaration. A declaration is a type name
# optionally carrying a refinement suffix that describes what the type permits:
# `enum(auto|exact)` lists values, `object{terms: array, mode?: enum(a|b)}`
# lists named fields. Fields marked with a trailing `?` are optional.
build_definition_type <- function(declaration, description, ctx, required = TRUE) {
  parsed <- split_definition_refinement(declaration, ctx)
  lower_type <- tolower(parsed$token)

  if (!lower_type %in% mcpr_supported_definition_types(include_aliases = TRUE)) {
    abort_unsupported_mcpr_definition_type(
      parsed$token,
      parameter_name = ctx$parameter_name,
      function_name = ctx$function_name,
      file_path = ctx$file_path
    )
  }

  spec <- if (is.null(parsed$refinement)) {
    if (identical(lower_type, "enum")) {
      abort_malformed_mcpr_definition_type(
        declaration,
        "`enum` must list its permitted values, as in `enum(auto|exact|regex)`.",
        ctx
      )
    }
    dispatch_definition_type_schema(lower_type, description)
  } else {
    build_refined_definition_type(lower_type, parsed, description, ctx)
  }

  spec$required <- required
  spec
}

#' @noRd
build_refined_definition_type <- function(lower_type, parsed, description, ctx) {
  expected <- unname(definition_refinement_delimiters[lower_type])

  if (is.na(expected)) {
    abort_malformed_mcpr_definition_type(
      parsed$declaration,
      paste0("`", lower_type, "` does not take a refinement."),
      ctx
    )
  }
  if (!identical(expected, parsed$delimiter)) {
    abort_malformed_mcpr_definition_type(
      parsed$declaration,
      paste0("`", lower_type, "` refinements are delimited by `", expected, "`."),
      ctx
    )
  }

  switch(lower_type,
    "enum" = build_enum_definition_type(parsed, description, ctx),
    "object" = build_object_definition_type(parsed, description, ctx)
  )
}

#' @noRd
build_enum_definition_type <- function(parsed, description, ctx) {
  values <- trimws(strsplit(parsed$refinement, "|", fixed = TRUE)[[1]])

  # strsplit() drops a trailing empty field, so `enum(a|)` would otherwise read
  # as a well-formed one-value enum. Test the raw text for the trailing bar.
  if (length(values) == 0 || any(!nzchar(values)) ||
    grepl("\\|\\s*$", parsed$refinement)) {
    abort_malformed_mcpr_definition_type(
      parsed$declaration,
      "`enum` values must be a non-empty `|`-separated list.",
      ctx
    )
  }

  type_enum(values, description = description)
}

#' @noRd
build_object_definition_type <- function(parsed, description, ctx) {
  fields <- trimws(split_outside_delimiters(parsed$refinement, ","))
  fields <- fields[nzchar(fields)]

  if (length(fields) == 0) {
    abort_malformed_mcpr_definition_type(
      parsed$declaration,
      "`object{}` declares no fields. Use a bare `object` for an open-ended payload.",
      ctx
    )
  }

  properties <- list()
  for (field in fields) {
    spec <- parse_object_field(field, parsed$declaration, ctx)

    if (spec$name %in% names(properties)) {
      abort_malformed_mcpr_definition_type(
        parsed$declaration,
        paste0("field `", spec$name, "` is declared more than once."),
        ctx
      )
    }

    field_ctx <- ctx
    field_ctx$parameter_name <- paste(c(ctx$parameter_name, spec$name), collapse = ".")

    properties[[spec$name]] <- build_definition_type(
      spec$declaration,
      # Prose for the fields belongs in the parameter's own description; a
      # NULL here keeps the emitted field schemas free of empty descriptions.
      description = NULL,
      ctx = field_ctx,
      required = spec$required
    )
  }

  do.call(
    type_object,
    c(
      list(.description = description, .additional_properties = FALSE),
      properties
    )
  )
}

#' @noRd
parse_object_field <- function(field, declaration, ctx) {
  parts <- regmatches(
    field,
    regexec("^([A-Za-z_][A-Za-z0-9_]*)(\\?)?\\s*:\\s*(\\S.*)$", field)
  )[[1]]

  if (length(parts) == 0) {
    abort_malformed_mcpr_definition_type(
      declaration,
      paste0("field `", field, "` is not of the form `name: type` or `name?: type`."),
      ctx
    )
  }

  list(
    name = parts[2],
    required = !nzchar(parts[3]),
    declaration = trimws(parts[4])
  )
}

#' Split a Roxygen Parameter Tail into Type Declaration and Description
#'
#' @title Split a Roxygen Parameter Tail into Type Declaration and Description
#' @description Separates the leading type declaration from the prose that
#' follows it. The declaration is a type name plus an optional refinement
#' suffix (`enum(...)`, `object{...}`) which may itself contain whitespace, so
#' the split is a delimiter-balanced scan rather than a token split.
#'
#' Lives beside `split_definition_refinement()` rather than in the registry
#' helpers: the whole declaration grammar belongs in one file.
#'
#' @param type_and_desc Whitespace-normalised text following the parameter name
#' @param ctx Definition context used for error reporting
#' @return List with `type` and `description`, or NULL when no type declaration
#'   is present. An unbalanced refinement aborts rather than returning NULL.
#' @noRd
split_definition_declaration <- function(type_and_desc, ctx) {
  token_match <- regexpr(definition_token_pattern, type_and_desc)

  if (token_match == -1) {
    return(NULL)
  }

  token_end <- attr(token_match, "match.length")
  end <- token_end
  delimiter <- substr(type_and_desc, end + 1L, end + 1L)

  if (nzchar(delimiter) && delimiter %in% names(closing_delimiters)) {
    close <- find_matching_delimiter(type_and_desc, end + 1L)
    if (is.na(close)) {
      abort_unterminated_definition_delimiter(type_and_desc, delimiter, ctx)
    }
    end <- close
  }

  # `object {a: string}` — one stray space — would otherwise degrade silently
  # to a bare open-ended object with the field list buried in the description.
  # Refuse it instead of emitting a schema the author plainly did not mean.
  if (end == token_end) {
    token <- substr(type_and_desc, 1L, token_end)
    expected <- unname(definition_refinement_delimiters[tolower(token)])
    tail <- substring(type_and_desc, end + 1L)

    if (!is.na(expected) && grepl(paste0("^\\s+\\", expected), tail)) {
      abort_malformed_mcpr_definition_type(
        type_and_desc,
        paste0(
          "A `", expected, "` refinement must follow `", token,
          "` immediately, with no space before it."
        ),
        ctx
      )
    }
  }

  # A description must follow the declaration. Parameters documented with a
  # bare type and no prose have always been rejected as incomplete.
  if (!grepl("^\\s", substring(type_and_desc, end + 1L))) {
    return(NULL)
  }

  list(
    type = substring(type_and_desc, 1L, end),
    description = trimws(substring(type_and_desc, end + 1L))
  )
}

#' @noRd
# Splits a type declaration into its type name and refinement. The refinement
# must open immediately after the name, so that `object {a: string}` reads as
# an unrefined `object` followed by a description rather than being ambiguous.
split_definition_refinement <- function(declaration, ctx) {
  token_match <- regexpr(definition_token_pattern, declaration)

  if (token_match == -1) {
    abort_malformed_mcpr_definition_type(declaration, "Expected a type name.", ctx)
  }

  token <- regmatches(declaration, token_match)
  rest <- substring(declaration, attr(token_match, "match.length") + 1L)

  if (!nzchar(rest)) {
    return(list(declaration = declaration, token = token, delimiter = NULL, refinement = NULL))
  }

  delimiter <- substr(rest, 1L, 1L)
  if (!delimiter %in% names(closing_delimiters)) {
    abort_malformed_mcpr_definition_type(
      declaration,
      paste0("Unexpected text after the type name: `", rest, "`."),
      ctx
    )
  }

  close <- find_matching_delimiter(rest, 1L)
  if (is.na(close)) {
    abort_unterminated_definition_delimiter(declaration, delimiter, ctx)
  }
  if (close < nchar(rest)) {
    abort_malformed_mcpr_definition_type(
      declaration,
      paste0("Unexpected text after `", closing_delimiters[[delimiter]], "`."),
      ctx
    )
  }

  list(
    declaration = declaration,
    token = token,
    delimiter = delimiter,
    refinement = substring(rest, 2L, close - 1L)
  )
}

#' @noRd
# Index of the delimiter closing the one at `open`, or NA if unterminated.
find_matching_delimiter <- function(text, open) {
  chars <- strsplit(text, "")[[1]]

  # Both call sites guard on a non-empty opener, but an out-of-range `open`
  # would make the loop below count down instead of up. Refuse it outright.
  if (open > length(chars)) {
    return(NA_integer_)
  }

  opener <- chars[open]
  closer <- closing_delimiters[[opener]]
  depth <- 0L

  for (i in seq(open, length(chars))) {
    if (chars[i] == opener) {
      depth <- depth + 1L
    } else if (chars[i] == closer) {
      depth <- depth - 1L
      if (depth == 0L) {
        return(i)
      }
    }
  }

  NA_integer_
}

#' @noRd
# Splits on `sep` only where no `(` or `{` group is open, so nested refinements
# survive intact.
split_outside_delimiters <- function(text, sep) {
  chars <- strsplit(text, "")[[1]]
  depth <- 0L
  parts <- character()
  start <- 1L

  for (i in seq_along(chars)) {
    if (chars[i] %in% names(closing_delimiters)) {
      depth <- depth + 1L
    } else if (chars[i] %in% closing_delimiters) {
      depth <- depth - 1L
    } else if (chars[i] == sep && depth == 0L) {
      parts <- c(parts, substring(text, start, i - 1L))
      start <- i + 1L
    }
  }

  c(parts, substring(text, start))
}

#' @noRd
dispatch_definition_type_schema <- function(lower_type, description) {
  switch(lower_type,
    # `list` and `named_list` are R-side spellings of an arbitrary named list;
    # all three share the json_object representation and its runtime coercion.
    "json_object" = ,
    "named_list" = ,
    "list" = type_json_object(description = description),
    "json_array" = type_json_array(description = description),
    "character" = ,
    "string" = type_string(description = description),
    "numeric" = ,
    "number" = type_number(description = description),
    "integer" = ,
    "int" = type_integer(description = description),
    "logical" = ,
    "boolean" = ,
    "bool" = type_boolean(description = description),
    "object" = type_object(.description = description, .additional_properties = TRUE),
    "array" = type_array(description = description, items = type_string()),
    # Unreachable: every caller checks membership in
    # mcpr_supported_definition_types() first, and `enum` is diverted to
    # build_enum_definition_type() before it gets here. Falling back to
    # `type_string()` would silently mis-type a token this switch forgot.
    cli::cli_abort(
      "No schema mapping for definition type {.val {lower_type}}.",
      .internal = TRUE
    )
  )
}
