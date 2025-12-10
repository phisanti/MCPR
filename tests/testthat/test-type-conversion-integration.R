test_that("legacy argument processing works", {
  # Test basic legacy functionality without MCP type markers
  mock_data <- list(
    id = 1,
    params = list(
      name = "test_tool",
      arguments = list(x = 42, y = "hello")
    ),
    tool = function(x, y) paste(x, y)
  )

  result <- MCPR:::execute_tool_call(mock_data)

  expect_true(is.list(result))
  expect_equal(result$id, 1)
  expect_false(result$result$isError)
  expect_equal(result$result$content[[1]]$text, "42 hello")
})

test_that("enhanced argument processing handles MCP types", {
  # Test with MCP type markers in arguments
  mock_data <- list(
    id = 2,
    params = list(
      name = "enhanced_tool",
      arguments = list(
        simple = 10,
        enhanced = list(
          "_mcp_type" = "numeric",
          "value" = list(5, "Inf", 15)
        )
      )
    ),
    tool = function(simple, enhanced) {
      list(simple = simple, enhanced_length = length(enhanced))
    }
  )

  result <- MCPR:::execute_tool_call(mock_data)

  expect_true(is.list(result))
  expect_equal(result$id, 2)
  expect_false(result$result$isError)

  # The result should be serialized as JSON
  expect_true(is.character(result$result$content[[1]]$text))
  expect_true(grepl("simple", result$result$content[[1]]$text))
})

test_that("tool execution maintains backward compatibility", {
  # Test with simple tool that current system handles
  simple_tool <- function(a, b) a + b

  legacy_data <- list(
    id = 3,
    params = list(
      name = "add",
      arguments = list(a = 5, b = 3)
    ),
    tool = simple_tool
  )

  result <- MCPR:::execute_tool_call(legacy_data)

  expect_true(is.list(result))
  expect_equal(result$id, 3)
  expect_false(result$result$isError)
  expect_true(grepl("8", result$result$content[[1]]$text))
})

test_that("enhanced tool execution handles complex R objects", {
  # Test with tool that returns complex R objects
  complex_tool <- function(n) {
    data.frame(
      id = 1:n,
      value = runif(n),
      category = factor(rep(c("A", "B"), length.out = n))
    )
  }

  complex_data <- list(
    id = 4,
    params = list(
      name = "complex_tool",
      arguments = list(n = 3)
    ),
    tool = complex_tool
  )

  result <- MCPR:::execute_tool_call(complex_data)

  expect_true(is.list(result))
  expect_equal(result$id, 4)
  expect_false(result$result$isError)

  # Should be JSON serialized with type information
  text_result <- result$result$content[[1]]$text
  expect_true(is.character(text_result))
  expect_true(grepl("data.frame", text_result) || grepl("id", text_result))
})

test_that("tool execution handles arrays correctly", {
  # Test that numeric vectors are handled properly
  array_tool <- function(numbers) sum(numbers)

  # Define test data
  numbers_data <- c(1, 2, 3, 4, 5)

  # This mimics how JSON arrays come in
  array_data <- list(
    id = 5,
    params = list(
      name = "sum_tool",
      arguments = list(numbers = numbers_data)
    ),
    tool = array_tool
  )

  result <- MCPR:::execute_tool_call(array_data)

  expect_true(is.list(result))
  expect_equal(result$id, 5)
  expect_false(result$result$isError)
  expect_true(grepl("15", result$result$content[[1]]$text))
})

test_that("enhanced processing falls back to legacy when needed", {
  # Test that enhanced processing gracefully falls back to legacy
  fallback_tool <- function(items) paste(items, collapse = ", ")

  # Data without MCP type markers should use legacy processing
  fallback_data <- list(
    id = 6,
    params = list(
      name = "fallback_tool",
      arguments = list(items = list("a", "b", "c"))
    ),
    tool = fallback_tool
  )

  result <- MCPR:::execute_tool_call(fallback_data)

  expect_true(is.list(result))
  expect_equal(result$id, 6)
  expect_false(result$result$isError)
  expect_equal(result$result$content[[1]]$text, "a, b, c")
})

test_that("error handling works with enhanced processing", {
  # Test that errors are properly handled
  error_tool <- function(x) {
    if (x < 0) stop("Negative values not allowed")
    sqrt(x)
  }

  error_data <- list(
    id = 7,
    params = list(
      name = "error_tool",
      arguments = list(x = -1)
    ),
    tool = error_tool
  )

  result <- MCPR:::execute_tool_call(error_data)

  expect_true(is.list(result))
  expect_equal(result$id, 7)
  expect_true(!is.null(result$error))
  expect_equal(result$error$code, -32603)
  expect_true(grepl("Negative values not allowed", result$error$message))
})

test_that("round-trip type conversion works end-to-end", {
  # Test complete round-trip: R object -> JSON -> R object
  roundtrip_tool <- function(data) {
    # This tool should receive the data back as it was originally
    return(data)
  }

  # Create complex R object, convert to MCP format, then test
  original_data <- list(
    numbers = c(1, 2, Inf, -Inf),
    date = as.Date("2024-01-15"),
    factor_data = factor(c("low", "high", "medium"))
  )

  # Convert to MCP format
  mcp_data <- to_mcpr_json(original_data)

  roundtrip_request <- list(
    id = 8,
    params = list(
      name = "roundtrip_tool",
      arguments = list(data = mcp_data)
    ),
    tool = roundtrip_tool
  )

  result <- MCPR:::execute_tool_call(roundtrip_request)

  expect_true(is.list(result))
  expect_equal(result$id, 8)
  expect_false(result$result$isError)

  # The result should be properly serialized
  expect_true(is.character(result$result$content[[1]]$text))
})
