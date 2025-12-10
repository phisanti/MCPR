# Test for basic JSON handling
test_that("from_mcpr_json handles basic types correctly", {
  # Test NULL
  expect_null(MCPR:::from_mcpr_json(NULL))

  # Test basic JSON string
  json_str <- '{"a": 1, "b": ["hello", "world"]}'
  result <- MCPR:::from_mcpr_json(json_str)
  expect_equal(result$a, 1)
  expect_equal(result$b, list("hello", "world")) # JSON arrays become lists

  # Test already parsed JSON
  parsed_json <- list(x = 42, y = "test")
  result2 <- MCPR:::from_mcpr_json(parsed_json)
  expect_equal(result2, parsed_json)

  # Test simple values and objects
  expect_equal(MCPR:::from_mcpr_json("42"), 42)
  expect_equal(MCPR:::from_mcpr_json('"hello"'), "hello")
  expect_equal(MCPR:::from_mcpr_json("[1, 2, 3]"), list(1, 2, 3))

  # Test objects
  json_obj <- '{"a": 1, "b": ["x", "y"]}'
  result <- MCPR:::from_mcpr_json(json_obj)
  expect_equal(result$a, 1)
  expect_equal(result$b, list("x", "y"))
})

# Test for special numeric values
test_that("from_mcpr_json reconstructs special numeric values", {
  # Test round-trip with special numeric values
  original <- c(1.5, Inf, -Inf, NaN)
  json_data <- to_mcpr_json(original)
  result <- MCPR:::from_mcpr_json(json_data)

  expect_equal(result[1], 1.5) # regular number
  expect_true(is.infinite(result[2]) && result[2] > 0) # Inf
  expect_true(is.infinite(result[3]) && result[3] < 0) # -Inf
  expect_true(is.nan(result[4])) # NaN
})

# Test for nested structures
test_that("from_mcpr_json handles nested structures", {
  # Test nested list with mixed types
  nested_json <- list(
    simple = 42,
    nested = list(
      inner = "value",
      numbers = c(1, 2, 3)
    )
  )

  result <- MCPR:::from_mcpr_json(nested_json)
  expect_equal(result$simple, 42)
  expect_equal(result$nested$inner, "value")
  expect_equal(result$nested$numbers, c(1, 2, 3))
})

# Test for large object markers
test_that("Large object markers are created for reconstruction", {
  large_vec <- 1:100000

  json_str <- mcpr_serialize(large_vec, auto_unbox = FALSE)
  json_obj <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

  # Manually create large object for testing reconstruction
  large_obj_json <- list(
    `_mcp_type` = "large_object",
    class = "integer",
    size = 400000,
    summary = c("Min: 1", "Max: 100000")
  )

  reconstructed <- MCPR:::from_mcpr_json(large_obj_json)
  expect_true(inherits(reconstructed, "mcp_large_object_marker"))
})

# Invalid JSON and empty structures
test_that("from_mcpr_json handles invalid JSON and empty structures gracefully", {
  # Test invalid JSON string
  expect_error(MCPR:::from_mcpr_json('{"invalid": json}'))

  # Test empty structures
  expect_equal(MCPR:::from_mcpr_json("{}"), structure(list(), names = character(0)))
  expect_equal(MCPR:::from_mcpr_json("[]"), list())
  expect_equal(MCPR:::from_mcpr_json('""'), "")

  # Test empty MCP type objects
  empty_matrix <- list(`_mcp_type` = "matrix", data = numeric(0), dim = c(0, 0))
  result <- MCPR:::from_mcpr_json(empty_matrix)
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(0, 0))
})


# Malformed MCP type markers
test_that("from_mcpr_json handles malformed MCP type markers", {
  # Test unknown MCP type
  unknown_type <- list(`_mcp_type` = "unknown_type", data = "test")
  result <- MCPR:::from_mcpr_json(unknown_type)
  expect_equal(result, unknown_type) # Should return as-is

  # Test MCP type with missing required fields
  incomplete_matrix <- list(`_mcp_type` = "matrix", data = c(1, 2, 3)) # missing dim
  expect_error(MCPR:::from_mcpr_json(incomplete_matrix))

  # Test MCP type with NULL values
  null_factor <- list(`_mcp_type` = "factor", values = NULL, levels = c("a", "b"))
  result <- MCPR:::from_mcpr_json(null_factor)
  expect_true(is.factor(result))
  expect_equal(levels(result), c("a", "b"))
})

# S3 objects with nested MCP types
test_that("from_mcpr_json reconstructs complex S3 objects with nested MCP types", {
  # Create complex S3 object with nested special types
  s3_object <- list(
    `_mcp_type` = "S3",
    `_mcp_class` = c("custom_class", "list"),
    matrix_data = list(
      `_mcp_type` = "matrix",
      data = c(1, 2, 3, 4),
      dim = c(2, 2)
    ),
    special_nums = list(
      `_mcp_type` = "numeric_vector_special",
      values = list(1.5, "Inf", "-Inf", "NaN")
    ),
    date_info = list(
      `_mcp_type` = "Date",
      values = "2023-01-01"
    )
  )

  result <- MCPR:::from_mcpr_json(s3_object)
  expect_true(inherits(result, "custom_class"))
  expect_true(is.matrix(result$matrix_data))
  expect_equal(dim(result$matrix_data), c(2, 2))
  expect_true(is.infinite(result$special_nums[2]))
  expect_true(inherits(result$date_info, "Date"))
})

# Data frame with mixed special column types
test_that("from_mcpr_json reconstructs data frames with complex column types", {
  # Create data frame with multiple special column types
  df_json <- list(
    `_mcp_type` = "data.frame",
    `_mcp_nrow` = 3,
    id = c(1, 2, 3),
    category = list(
      `_mcp_type` = "factor",
      values = c("A", "B", "A"),
      levels = c("A", "B", "C")
    ),
    measurement = list(
      `_mcp_type` = "numeric_vector_special",
      values = list(1.5, "Inf", 2.3)
    ),
    timestamp = list(
      `_mcp_type` = "POSIXct",
      values = c("2023-01-01T12:00:00", "2023-01-02T12:00:00", "2023-01-03T12:00:00"),
      timezone = "UTC"
    )
  )

  result <- MCPR:::from_mcpr_json(df_json)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true(is.factor(result$category))
  expect_equal(levels(result$category), c("A", "B", "C"))
  expect_true(is.infinite(result$measurement[2]))
  expect_true(inherits(result$timestamp, "POSIXct"))
})

# Multi-dimensional arrays with regular values and attributes
test_that("from_mcpr_json reconstructs complex multi-dimensional arrays", {
  # Create 3D array with regular values and dimnames
  array_json <- list(
    `_mcp_type` = "array",
    data = c(1, 2, 3, 4, 5, 6, 7, 8),
    dim = c(2, 2, 2),
    dimnames = list(
      c("row1", "row2"),
      c("col1", "col2"),
      c("slice1", "slice2")
    )
  )

  result <- MCPR:::from_mcpr_json(array_json)
  expect_true(is.array(result))
  expect_equal(dim(result), c(2, 2, 2))
  expect_equal(dimnames(result)[[1]], c("row1", "row2"))
  expect_equal(dimnames(result)[[2]], c("col1", "col2"))
  expect_equal(dimnames(result)[[3]], c("slice1", "slice2"))

  # Check that values are preserved correctly
  expect_equal(result[1, 1, 1], 1)
  expect_equal(result[2, 2, 2], 8)
  expect_equal(result[1, 2, 1], 3)
})
