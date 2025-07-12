#' Serialize R object to JSON string for MCP
#'
#' @param x An R object to serialize
#' @param pretty Whether to pretty-print the JSON
#' @param auto_unbox Whether to automatically unbox single-element vectors
#' @return A JSON string representation of the R object
#' @export
#' @examples
#' mcp_serialize(list(result = 42, message = "success"))
mcp_serialize <- function(x, pretty = FALSE, auto_unbox = TRUE) {
  # Convert to MCP-compatible format
  mcp_obj <- to_mcp_json(x, auto_unbox = auto_unbox, custom_serializers = get_mcp_serializers())
  
  # Serialize to JSON
  jsonlite::toJSON(
    mcp_obj,
    pretty = pretty,
    auto_unbox = FALSE,  # We handle unboxing in to_mcp_json
    null = "null",
    na = "null"
  )
}

#' Deserialize JSON string to R object from MCP
#'
#' @param json A JSON string to deserialize
#' @return An R object
#' @export
#' @examples
#' mcp_deserialize('{"result": 42, "message": "success"}')
mcp_deserialize <- function(json) {
  from_mcp_json(json)
}

#' Check if an R object can be safely serialized to JSON
#'
#' @param x An R object to check
#' @return TRUE if the object can be serialized, FALSE otherwise
#' @export
can_serialize <- function(x) {
  tryCatch({
    mcp_serialize(x)
    TRUE
  }, error = function(e) {
    FALSE
  })
}

#' Validate R object against JSON schema
#'
#' @param value R object to validate
#' @param schema JSON schema definition
#' @return TRUE if valid, error otherwise
#' @export
validate_against_schema <- function(value, schema) {
  if (is.null(schema$type)) return(TRUE)
  
  valid <- switch(schema$type,
    "array" = is.vector(value) || is.list(value),
    "object" = is.list(value) && !is.null(names(value)),
    "string" = is.character(value),
    "number" = is.numeric(value),
    "integer" = is.integer(value) || (is.numeric(value) && all(value == as.integer(value), na.rm = TRUE)),
    "boolean" = is.logical(value),
    "null" = is.null(value),
    TRUE
  )
  
  if (!valid) {
    stop(sprintf("Value does not match schema type '%s'", schema$type))
  }
  
  # Additional validation for arrays
  if (schema$type == "array" && !is.null(schema$items)) {
    lapply(value, function(item) {
      validate_against_schema(item, schema$items)
    })
  }
  
  # Additional validation for objects
  if (schema$type == "object" && !is.null(schema$properties)) {
    for (prop in names(schema$properties)) {
      if (prop %in% names(value)) {
        validate_against_schema(value[[prop]], schema$properties[[prop]])
      } else if (!is.null(schema$required) && prop %in% schema$required) {
        stop(sprintf("Required property '%s' is missing", prop))
      }
    }
  }
  
  # Enum validation
  if (!is.null(schema$enum)) {
    if (!value %in% schema$enum) {
      stop(sprintf("Value '%s' is not in allowed enum values", value))
    }
  }
  
  TRUE
}

#' Create a streaming converter for large data frames
#'
#' @param df Data frame to stream
#' @param chunk_size Number of rows per chunk
#' @param callback Function to call with each chunk
#' @export
stream_dataframe <- function(df, chunk_size = 1000, callback) {
  n_rows <- nrow(df)
  n_chunks <- ceiling(n_rows / chunk_size)
  
  for (i in seq_len(n_chunks)) {
    start_row <- (i - 1) * chunk_size + 1
    end_row <- min(i * chunk_size, n_rows)
    
    chunk <- df[start_row:end_row, , drop = FALSE]
    chunk_json <- to_mcp_json(chunk, size_limit = Inf)
    
    callback(list(
      chunk = i,
      total_chunks = n_chunks,
      start_row = start_row,
      end_row = end_row,
      data = chunk_json
    ))
  }
}