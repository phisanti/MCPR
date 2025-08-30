# Type Conversion Utilities
# Core serialization functions and type specifications for MCP protocol compatibility.
# Provides high-level interface for JSON conversion and type system definitions.

#' Serialize R Object to JSON for MCP
#'
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
#' * `type_from_schema()` allows you to specify the full schema that you want to
#'   get back from the LLM as a JSON schema. This is useful if you have a
#'   pre-defined schema that you want to use directly without manually creating
#'   the type using the `type_*()` functions. You can point to a file with the
#'   `path` argument or provide a JSON string with `text`. The schema must be a
#'   valid JSON schema object.
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
#' # requested format in the description (it's not gauranteed that you'll
#' # get this format back, but you should most of the time)
#' type_string("The creation date, in YYYY-MM-DD format.")
#' type_string("The update date, in dd/mm/yyyy format.")
#' @noRd
type_boolean <- function(description = NULL, required = TRUE) {
  structure(list(type = "boolean", description = description, required = required), class = "mcpr_type")
}
#' @rdname type_boolean
#' @noRd
type_integer <- function(description = NULL, required = TRUE) {
  structure(list(type = "integer", description = description, required = required), class = "mcpr_type")
}
#' @rdname type_boolean
#' @noRd
type_number <- function(description = NULL, required = TRUE) {
  structure(list(type = "number", description = description, required = required), class = "mcpr_type")
}
#' @rdname type_boolean
#' @noRd
type_string <- function(description = NULL, required = TRUE) {
  structure(list(type = "string", description = description, required = required), class = "mcpr_type")
}

#' @param values Character vector of permitted values.
#' @rdname type_boolean
#' @noRd
type_enum <- function(values, description = NULL, required = TRUE) {
  structure(list(type = "enum", values = values, description = description, required = required), class = "mcpr_type")
}

#' @param items The type of the array items. Can be created by any of the
#'   `type_` function.
#' @rdname type_boolean
#' @noRd
type_array <- function(items, description = NULL, required = TRUE) {
  structure(list(type = "array", items = items, description = description, required = required), class = "mcpr_type")
}

#' @param ... Name-type pairs defineing the components that the object must
#'   possess.
#' @param .additional_properties Can the object have arbitrary additional
#'   properties that are not explicitly listed? Only supported by Claude.
#' @rdname type_boolean
#' @noRd
type_object <- function(
  .description = NULL,
  ...,
  .required = TRUE,
  .additional_properties = FALSE
) {
  structure(list(
    type = "object",
    properties = list(...),
    description = .description,
    required = .required,
    additional_properties = .additional_properties
  ), class = "mcpr_type")
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
map_type_schema <- function(type_str, description = NULL, input_type = "definition") {
  if (input_type == "json") {
    # Handle JSON schema object input
    schema <- type_str # Rename for clarity
    description <- schema$description %||% NULL
    required <- TRUE # Default to required for MCP compatibility

    # Handle enum types
    if (!is.null(schema$enum)) {
      return(type_enum(schema$enum, description = description, required = required))
    }

    switch(schema$type %||% "string",
      "string" = type_string(description = description, required = required),
      "number" = type_number(description = description, required = required),
      "integer" = type_integer(description = description, required = required),
      "boolean" = type_boolean(description = description, required = required),
      "array" = {
        items_type <- if (!is.null(schema$items)) {
          map_type_schema(schema$items, input_type = "json")
        } else {
          type_string()
        }
        type_array(items_type, description = description, required = required)
      },
      "object" = {
        if (!is.null(schema$properties)) {
          props <- list()
          for (prop_name in names(schema$properties)) {
            props[[prop_name]] <- map_type_schema(schema$properties[[prop_name]], input_type = "json")
          }
          do.call(type_object, c(list(.description = description, .required = required), props))
        } else {
          type_object(.description = description, .required = required)
        }
      },
      # Default fallback
      type_string(description = description, required = required)
    )
  } else {
    # Handle string-based type definition input (original logic)
    description <- description %||% ""

    switch(tolower(type_str),
      "character" = ,
      "string" = type_string(description = description),
      "numeric" = ,
      "number" = {
        # If description mentions "vector" or "array", create an array type
        if (grepl("vector|array", description, ignore.case = TRUE)) {
          type_array(description = description, items = type_number())
        } else {
          type_number(description = description)
        }
      },
      "integer" = ,
      "int" = type_integer(description = description),
      "logical" = ,
      "boolean" = ,
      "bool" = type_boolean(description = description),
      "list" = ,
      "array" = type_array(description = description, items = type_string()),
      type_string(description = description)
    )
  }
}
