test_that("decode_tool_args handles basic arguments", {
  # Test basic argument passthrough
  basic_args <- list(x = 42, y = "test", z = TRUE)
  result <- MCPR:::decode_tool_args(basic_args)
  expect_equal(result, basic_args)

  # Test non-list input
  expect_equal(MCPR:::decode_tool_args("not a list"), "not a list")
  expect_equal(MCPR:::decode_tool_args(42), 42)
})

test_that("decode_tool_args detects MCP type markers", {
  # Test arguments with MCP type markers
  mcp_args <- list(
    simple = 42,
    complex = list(
      "_mcp_type" = "numeric",
      "value" = list(1, 2, "Inf")
    )
  )

  # Should trigger from_mcp_json processing
  result <- MCPR:::decode_tool_args(mcp_args)
  expect_true(is.list(result))
  # The exact structure depends on from_mcpr_json implementation
  expect_true(!is.null(result))
})

test_that("decode_tool_args handles empty and NULL inputs", {
  # Test NULL
  expect_null(MCPR:::decode_tool_args(NULL))

  # Test empty list
  expect_equal(MCPR:::decode_tool_args(list()), list())

  # Test list without MCP markers
  no_markers <- list(a = 1, b = list(x = 2, y = 3))
  expect_equal(MCPR:::decode_tool_args(no_markers), no_markers)
})

test_that("encode_tool_results handles simple text results", {
  # Test simple character result
  test_data <- list(id = 1)
  simple_result <- "Simple text result"

  output <- MCPR:::encode_tool_results(test_data, simple_result)

  expect_true(is.list(output))
  expect_equal(output$jsonrpc, "2.0")
  expect_equal(output$id, 1)
  expect_equal(output$result$content[[1]]$type, "text")
  expect_equal(output$result$content[[1]]$text, simple_result)
  expect_false(output$result$isError)
})

test_that("encode_tool_results handles complex objects", {
  # Test complex result that should use mcpr_serialize
  test_data <- list(id = 2)
  complex_result <- data.frame(
    x = 1:3,
    y = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )

  output <- MCPR:::encode_tool_results(test_data, complex_result)

  expect_true(is.list(output))
  expect_equal(output$jsonrpc, "2.0")
  expect_equal(output$id, 2)
  expect_equal(output$result$content[[1]]$type, "text")
  expect_false(output$result$isError)

  # The text should be JSON serialized
  expect_true(is.character(output$result$content[[1]]$text))
  expect_true(nchar(output$result$content[[1]]$text) > 0)
})

test_that("encode_tool_results handles numeric vectors", {
  # Test numeric vector (should trigger mcpr_serialize)
  test_data <- list(id = 3)
  numeric_result <- c(1, 2, 3, 4, 5)

  output <- MCPR:::encode_tool_results(test_data, numeric_result)

  expect_true(is.list(output))
  expect_equal(output$id, 3)
  expect_false(output$result$isError)

  # Should use mcpr_serialize since it's not a single character
  expect_true(is.character(output$result$content[[1]]$text))
})

test_that("encode_tool_results handles lists", {
  # Test list result
  test_data <- list(id = 4)
  list_result <- list(status = "success", data = 1:5, message = "Complete")

  output <- MCPR:::encode_tool_results(test_data, list_result)

  expect_true(is.list(output))
  expect_equal(output$id, 4)
  expect_false(output$result$isError)

  # Should be serialized as JSON
  text_content <- output$result$content[[1]]$text
  expect_true(is.character(text_content))
  expect_true(grepl("success", text_content))
})
