# Test for Basic Serialization/Deserialization
test_that("mcp_serialize creates valid JSON strings", {
  # Test basic serialization
  test_obj <- list(result = 42, message = "success")
  json_str <- mcp_serialize(test_obj)
  
  expect_true(is.character(json_str))
  expect_length(json_str, 1)
  
  # Should be valid JSON
  expect_no_error(jsonlite::fromJSON(json_str))
})

test_that("mcp_deserialize reconstructs objects correctly", {
  # Test basic deserialization
  json_str <- '{"result": 42, "message": "success"}'
  result <- mcp_deserialize(json_str)
  
  expect_equal(result$result, 42)
  expect_equal(result$message, "success")
})

test_that("can_serialize identifies serializable objects", {
  # Test objects that can be serialized
  expect_true(can_serialize(list(a = 1, b = "test")))
  expect_true(can_serialize(c(1, 2, 3)))
  expect_true(can_serialize("simple string"))
  expect_true(can_serialize(data.frame(x = 1:3, y = letters[1:3])))
  
  # Test NULL
  expect_true(can_serialize(NULL))
})

test_that("mcp_serialize and mcp_deserialize round-trip correctly", {
  # Test round-trip conversion
  original <- list(
    numbers = c(1, 2, 3),
    text = "hello",
    flag = TRUE,
    date = as.Date("2024-01-15")
  )
  
  # Serialize then deserialize
  json_str <- mcp_serialize(original)
  reconstructed <- mcp_deserialize(json_str)
  
  # Check basic equality (dates will be different class but same value)
  expect_equal(unlist(reconstructed$numbers), original$numbers)
  expect_equal(reconstructed$text, original$text)
  expect_equal(reconstructed$flag, original$flag)
  expect_equal(as.character(reconstructed$date), as.character(original$date))
})

# Test for Advanced Serialization Features
test_that("mcp_serialize handles pretty printing and auto_unbox parameter", {
  test_obj <- list(value = 42, message = "success")
  
  # Test pretty printing
  json_pretty <- mcp_serialize(test_obj, pretty = TRUE)
  expect_true(grepl("\\n", json_pretty))
  
  # Test with auto_unbox = TRUE (default)
  single_value <- list(value = 42)
  json_unbox <- mcp_serialize(single_value, auto_unbox = TRUE)
  parsed_unbox <- jsonlite::fromJSON(json_unbox)
  expect_equal(parsed_unbox$value, 42)  # Should be scalar
  
  # Test with auto_unbox = FALSE
  json_no_unbox <- mcp_serialize(single_value, auto_unbox = FALSE)
  parsed_no_unbox <- jsonlite::fromJSON(json_no_unbox)
  expect_length(parsed_no_unbox$value, 1)  # Should be array with one element
})

test_that("utilities handle edge cases gracefully", {
  # Test empty objects
  expect_no_error(mcp_serialize(list()))
  expect_no_error(mcp_serialize(character(0)))
  expect_no_error(mcp_serialize(numeric(0)))
  
  # Test deserializing empty JSON
  expect_no_error(mcp_deserialize("{}"))
  expect_no_error(mcp_deserialize("[]"))
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
  
  expect_equal(length(chunks), 4)  # 100 rows / 30 per chunk = 4 chunks
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


test_that("mcp_deserialize handles malformed JSON gracefully", {
  # Test invalid JSON strings
  expect_error(mcp_deserialize('{"invalid": json}'))
  expect_error(mcp_deserialize('{"unclosed": "quote}'))
  expect_error(mcp_deserialize('[1, 2, 3,]'))  # Trailing comma
  expect_error(mcp_deserialize(''))  # Empty string
  expect_error(mcp_deserialize('{invalid json'))  # Malformed syntax
  expect_error(mcp_deserialize('{"key": undefined}'))  # Undefined value
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
  json_str <- mcp_serialize(complex_obj)
  expect_true(is.character(json_str))
  expect_true(nchar(json_str) > 100)  # Should be substantial
  
  # Test deserialization
  reconstructed <- mcp_deserialize(json_str)
  
  # Verify key nested elements
  expect_equal(reconstructed$level1$level2$level3$numbers[1:3], c(1, 2, 3))
  expect_equal(reconstructed$level1$level2$level3$strings[1:2], list("hello", "world"))
  expect_equal(reconstructed$summary$total_items, 100)
  expect_equal(reconstructed$summary$success_rate, 0.85)
})

