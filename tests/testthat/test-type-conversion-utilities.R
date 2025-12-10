# Test for Basic Serialization/Deserialization
test_that("mcpr_serialize creates valid JSON strings", {
  # Test basic serialization
  test_obj <- list(result = 42, message = "success")
  json_str <- MCPR:::mcpr_serialize(test_obj)

  expect_true(is.character(json_str))
  expect_length(json_str, 1)

  # Should be valid JSON
  expect_no_error(jsonlite::fromJSON(json_str))
})

test_that("mcpr_deserialize reconstructs objects correctly", {
  # Test basic deserialization
  json_str <- '{"result": 42, "message": "success"}'
  result <- MCPR:::mcpr_deserialize(json_str)

  expect_equal(result$result, 42)
  expect_equal(result$message, "success")
})

test_that("can_serialize identifies serializable objects", {
  # Test objects that can be serialized
  expect_true(MCPR:::can_serialize(list(a = 1, b = "test")))
  expect_true(MCPR:::can_serialize(c(1, 2, 3)))
  expect_true(MCPR:::can_serialize("simple string"))
  expect_true(MCPR:::can_serialize(data.frame(x = 1:3, y = letters[1:3])))

  # Test NULL
  expect_true(MCPR:::can_serialize(NULL))
})

test_that("mcpr_serialize and mcpr_deserialize round-trip correctly", {
  # Test round-trip conversion
  original <- list(
    numbers = c(1, 2, 3),
    text = "hello",
    flag = TRUE,
    date = as.Date("2024-01-15")
  )

  # Serialize then deserialize
  json_str <- MCPR:::mcpr_serialize(original)
  reconstructed <- MCPR:::mcpr_deserialize(json_str)

  # Check basic equality (dates will be different class but same value)
  expect_equal(unlist(reconstructed$numbers), original$numbers)
  expect_equal(reconstructed$text, original$text)
  expect_equal(reconstructed$flag, original$flag)
  expect_equal(as.character(reconstructed$date), as.character(original$date))
})

# Test for Advanced Serialization Features
test_that("mcpr_serialize handles pretty printing and auto_unbox parameter", {
  test_obj <- list(value = 42, message = "success")

  # Test pretty printing
  json_pretty <- MCPR:::mcpr_serialize(test_obj, pretty = TRUE)
  expect_true(grepl("\\n", json_pretty))

  # Test with auto_unbox = TRUE (default)
  single_value <- list(value = 42)
  json_unbox <- MCPR:::mcpr_serialize(single_value, auto_unbox = TRUE)
  parsed_unbox <- jsonlite::fromJSON(json_unbox)
  expect_equal(parsed_unbox$value, 42) # Should be scalar

  # Test with auto_unbox = FALSE
  json_no_unbox <- MCPR:::mcpr_serialize(single_value, auto_unbox = FALSE)
  parsed_no_unbox <- jsonlite::fromJSON(json_no_unbox)
  expect_length(parsed_no_unbox$value, 1) # Should be array with one element
})

test_that("utilities handle edge cases gracefully", {
  # Test empty objects
  expect_no_error(MCPR:::mcpr_serialize(list()))
  expect_no_error(MCPR:::mcpr_serialize(character(0)))
  expect_no_error(MCPR:::mcpr_serialize(numeric(0)))

  # Test deserializing empty JSON
  expect_no_error(MCPR:::mcpr_deserialize("{}"))
  expect_no_error(MCPR:::mcpr_deserialize("[]"))
})

# Test for Data Frame Streaming
test_that("Data frame streaming works correctly", {
  df <- data.frame(
    x = 1:100,
    y = rnorm(100)
  )

  chunks <- list()
  stream_dataframe(df, chunk_size = 30, callback = function(chunk) {
    chunks <<- append(chunks, list(chunk))
  })

  expect_equal(length(chunks), 4) # 100 rows / 30 per chunk = 4 chunks
  expect_equal(chunks[[1]]$chunk, 1)
  expect_equal(chunks[[1]]$total_chunks, 4)
  expect_equal(chunks[[1]]$start_row, 1)
  expect_equal(chunks[[1]]$end_row, 30)
  expect_equal(length(chunks[[1]]$data[[1]]), 30)

  # Last chunk should have only 10 rows
  expect_equal(chunks[[4]]$start_row, 91)
  expect_equal(chunks[[4]]$end_row, 100)
  expect_equal(length(chunks[[4]]$data[[1]]), 10)
})

# Test for Schema Validation
# Schema validation function was removed in refactor - skipping these tests
test_that("Schema validation works correctly", {
  skip("validate_against_schema function not implemented in current version")
  # Valid cases
  # expect_true(validate_against_schema(42, list(type = "number")))
  # expect_true(validate_against_schema("hello", list(type = "string")))
  # expect_true(validate_against_schema(c(1, 2, 3), list(type = "array")))
  # expect_true(validate_against_schema(list(a = 1), list(type = "object")))

  # Invalid cases
  # expect_error(validate_against_schema("hello", list(type = "number")))
  # expect_error(validate_against_schema(42, list(type = "string")))

  # Array with items validation
  # schema <- list(
  #   type = "array",
  #   items = list(type = "number")
  # )
  # expect_true(validate_against_schema(c(1, 2, 3), schema))
  # expect_error(validate_against_schema(c("a", "b"), schema))

  # Object with properties
  # schema <- list(
  #   type = "object",
  #   properties = list(
  #     name = list(type = "string"),
  #     age = list(type = "number")
  #   ),
  #   required = c("name")
  # )
  # expect_true(validate_against_schema(list(name = "John", age = 30), schema))
  # expect_error(validate_against_schema(list(age = 30), schema)) # Missing required

  # Enum validation
  # schema <- list(type = "string", enum = c("red", "green", "blue"))
  # expect_true(validate_against_schema("red", schema))
  # expect_error(validate_against_schema("yellow", schema))
})

test_that("mcpr_deserialize handles malformed JSON gracefully", {
  # Test invalid JSON strings
  expect_error(MCPR:::mcpr_deserialize('{"invalid": json}'))
  expect_error(MCPR:::mcpr_deserialize('{"unclosed": "quote}'))
  expect_error(MCPR:::mcpr_deserialize("[1, 2, 3,]")) # Trailing comma
  expect_error(MCPR:::mcpr_deserialize("")) # Empty string
  expect_error(MCPR:::mcpr_deserialize("{invalid json")) # Malformed syntax
  expect_error(MCPR:::mcpr_deserialize('{"key": undefined}')) # Undefined value
})

test_that("stream_dataframe handles edge cases in chunking", {
  # Test with data frame smaller than chunk size
  small_df <- data.frame(x = 1:5, y = letters[1:5])
  chunks <- list()
  stream_dataframe(small_df, chunk_size = 10, callback = function(chunk) {
    chunks <<- append(chunks, list(chunk))
  })

  expect_equal(length(chunks), 1)
  expect_equal(chunks[[1]]$total_chunks, 1)
  expect_equal(chunks[[1]]$start_row, 1)
  expect_equal(chunks[[1]]$end_row, 5)

  # Test with empty data frame
  empty_df <- data.frame()
  empty_chunks <- list()
  expect_no_error(stream_dataframe(empty_df, chunk_size = 10, callback = function(chunk) {
    empty_chunks <<- append(empty_chunks, list(chunk))
  }))

  # Test with single row
  single_row_df <- data.frame(x = 1, y = "a")
  single_chunks <- list()
  stream_dataframe(single_row_df, chunk_size = 10, callback = function(chunk) {
    single_chunks <<- append(single_chunks, list(chunk))
  })
  expect_equal(length(single_chunks), 1)
  expect_equal(single_chunks[[1]]$start_row, 1)
  expect_equal(single_chunks[[1]]$end_row, 1)
})

# Complex Scenarios
test_that("deeply nested objects serialize and deserialize correctly", {
  # Create a deeply nested structure with mixed data types
  complex_obj <- list(
    level1 = list(
      level2 = list(
        level3 = list(
          numbers = c(1, 2, 3, NA, Inf, -Inf),
          strings = c("hello", "world", "", NA_character_),
          logicals = c(TRUE, FALSE, NA),
          dates = as.Date(c("2024-01-01", "2024-12-31")),
          factors = factor(c("A", "B", "C")),
          level4 = list(
            matrix_data = matrix(1:6, nrow = 2),
            data_frame = data.frame(
              id = 1:3,
              value = c(1.1, 2.2, 3.3),
              category = c("X", "Y", "Z")
            )
          )
        )
      ),
      metadata = list(
        created = Sys.time(),
        version = "1.0.0",
        tags = c("test", "nested", "complex")
      )
    ),
    summary = list(
      total_items = 100,
      processed = 85,
      success_rate = 0.85
    )
  )

  # Test serialization
  json_str <- MCPR:::mcpr_serialize(complex_obj)
  expect_true(is.character(json_str))
  expect_true(nchar(json_str) > 100) # Should be substantial

  # Test deserialization
  reconstructed <- MCPR:::mcpr_deserialize(json_str)

  # Verify key nested elements
  expect_equal(reconstructed$level1$level2$level3$numbers[1:3], c(1, 2, 3))
  expect_equal(reconstructed$level1$level2$level3$strings[1:2], list("hello", "world"))
  expect_equal(reconstructed$summary$total_items, 100)
  expect_equal(reconstructed$summary$success_rate, 0.85)
})

test_that("schema validation handles complex nested schemas with conditional logic", {
  skip("validate_against_schema function not implemented in current version")
  # Define a complex schema with nested objects, arrays, and conditional requirements
  api_response_schema <- list(
    type = "object",
    properties = list(
      status = list(type = "string", enum = c("success", "error", "pending")),
      data = list(
        type = "object",
        properties = list(
          users = list(
            type = "array",
            items = list(
              type = "object",
              properties = list(
                id = list(type = "integer"),
                name = list(type = "string"),
                email = list(type = "string"),
                profile = list(
                  type = "object",
                  properties = list(
                    age = list(type = "integer"),
                    preferences = list(
                      type = "array",
                      items = list(type = "string")
                    )
                  ),
                  required = c("age")
                )
              ),
              required = c("id", "name", "email")
            )
          ),
          pagination = list(
            type = "object",
            properties = list(
              page = list(type = "integer"),
              total_pages = list(type = "integer"),
              per_page = list(type = "integer")
            ),
            required = c("page", "total_pages")
          )
        ),
        required = c("users")
      ),
      error = list(
        type = "object",
        properties = list(
          code = list(type = "integer"),
          message = list(type = "string")
        )
      )
    ),
    required = c("status")
  )

  # Test valid complex object
  valid_response <- list(
    status = "success",
    data = list(
      users = list(
        list(
          id = 1L,
          name = "John Doe",
          email = "john@example.com",
          profile = list(
            age = 30L,
            preferences = c("coding", "reading")
          )
        ),
        list(
          id = 2L,
          name = "Jane Smith",
          email = "jane@example.com",
          profile = list(age = 25L)
        )
      ),
      pagination = list(
        page = 1L,
        total_pages = 5L,
        per_page = 10L
      )
    )
  )

  expect_true(validate_against_schema(valid_response, api_response_schema))

  # Test invalid cases
  # Missing required field in nested object
  invalid_user <- valid_response
  invalid_user$data$users[[1]]$profile$age <- NULL
  expect_error(validate_against_schema(invalid_user, api_response_schema))

  # Invalid enum value
  invalid_status <- valid_response
  invalid_status$status <- "unknown"
  expect_error(validate_against_schema(invalid_status, api_response_schema))

  # Wrong type in nested array
  invalid_preferences <- valid_response
  invalid_preferences$data$users[[1]]$profile$preferences <- c(1, 2, 3)
  expect_error(validate_against_schema(invalid_preferences, api_response_schema))
})

test_that("stream_dataframe processes data in chunks", {
  # Create test data
  test_df <- data.frame(
    id = 1:100,
    value = runif(100),
    category = sample(letters[1:5], 100, replace = TRUE)
  )

  # Collect chunks
  chunks <- list()
  chunk_callback <- function(chunk_info) {
    chunks <<- append(chunks, list(chunk_info))
  }

  # Stream with chunk size 25
  stream_dataframe(test_df, chunk_size = 25, chunk_callback)

  expect_equal(length(chunks), 4)

  # Check first chunk
  expect_equal(chunks[[1]]$chunk, 1)
  expect_equal(chunks[[1]]$total_chunks, 4)
  expect_equal(chunks[[1]]$start_row, 1)
  expect_equal(chunks[[1]]$end_row, 25)

  # Check last chunk
  expect_equal(chunks[[4]]$chunk, 4)
  expect_equal(chunks[[4]]$start_row, 76)
  expect_equal(chunks[[4]]$end_row, 100)
})

test_that("stream_dataframe handles edge cases", {
  # Test with small dataframe (less than chunk size)
  small_df <- data.frame(x = 1:5, y = letters[1:5])
  chunks <- list()

  stream_dataframe(small_df, chunk_size = 10, function(chunk) {
    chunks <<- append(chunks, list(chunk))
  })

  expect_equal(length(chunks), 1)
  expect_equal(chunks[[1]]$start_row, 1)
  expect_equal(chunks[[1]]$end_row, 5)

  # Test with single row
  single_df <- data.frame(x = 1, y = "a")
  single_chunks <- list()

  stream_dataframe(single_df, chunk_size = 100, function(chunk) {
    single_chunks <<- append(single_chunks, list(chunk))
  })

  expect_equal(length(single_chunks), 1)
  expect_equal(single_chunks[[1]]$total_chunks, 1)
})

test_that("type constructor functions work correctly", {
  # Test type_boolean
  bool_type <- MCPR:::type_boolean("Boolean flag", required = FALSE)
  expect_equal(bool_type$type, "boolean")
  expect_equal(bool_type$description, "Boolean flag")
  expect_equal(bool_type$required, FALSE)

  # Test type_integer
  int_type <- MCPR:::type_integer("Integer value")
  expect_equal(int_type$type, "integer")
  expect_equal(int_type$description, "Integer value")
  expect_equal(int_type$required, TRUE)

  # Test type_number
  num_type <- type_number()
  expect_equal(num_type$type, "number")
  expect_equal(num_type$required, TRUE)
  expect_true(is.null(num_type$description))
})

test_that("mcpr_serialize handles different parameters", {
  test_obj <- list(a = 1, b = "test", c = TRUE)

  # Test pretty formatting
  json_pretty <- MCPR:::mcpr_serialize(test_obj, pretty = TRUE)
  json_compact <- MCPR:::mcpr_serialize(test_obj, pretty = FALSE)

  expect_type(json_pretty, "character")
  expect_type(json_compact, "character")
  expect_true(nchar(json_pretty) >= nchar(json_compact))

  # Test auto_unbox parameter
  single_val <- list(value = 42)
  json_unbox <- MCPR:::mcpr_serialize(single_val, auto_unbox = TRUE)
  json_no_unbox <- MCPR:::mcpr_serialize(single_val, auto_unbox = FALSE)

  expect_type(json_unbox, "character")
  expect_type(json_no_unbox, "character")
})

test_that("mcpr_serialize handles size limits", {
  # Create a large object
  large_obj <- list(data = matrix(runif(1000), nrow = 100))

  # Test with very small size limit
  json_limited <- MCPR:::mcpr_serialize(large_obj, size_limit = 100)
  expect_type(json_limited, "character")

  # Should still produce valid JSON
  expect_no_error(jsonlite::fromJSON(json_limited))
})
