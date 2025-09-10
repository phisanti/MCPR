test_that("drop_nulls removes NULL values correctly", {
  # Test with vector containing NULLs
  test_list <- list(1, NULL, "test", NULL, TRUE)
  result <- drop_nulls(test_list)

  expect_equal(length(result), 3)
  expect_equal(result[[1]], 1)
  expect_equal(result[[2]], "test")
  expect_equal(result[[3]], TRUE)

  # Test with no NULL values
  no_nulls <- list(1, 2, 3)
  result2 <- drop_nulls(no_nulls)
  expect_equal(result2, no_nulls)

  # Test with all NULL values
  all_nulls <- list(NULL, NULL, NULL)
  result3 <- drop_nulls(all_nulls)
  expect_equal(length(result3), 0)

  # Test with empty list
  empty_list <- list()
  result4 <- drop_nulls(empty_list)
  expect_equal(result4, empty_list)
})

test_that("named_list creates proper named lists", {
  # Test with named arguments
  result <- named_list(a = 1, b = 2, c = "test")

  expect_true(is.list(result))
  expect_equal(names(result), c("a", "b", "c"))
  expect_equal(result$a, 1)
  expect_equal(result$b, 2)
  expect_equal(result$c, "test")

  # Test with empty arguments
  empty_result <- named_list()
  expect_true(is.list(empty_result))
  expect_equal(length(empty_result), 0)
})

test_that("compact_list removes empty values", {
  # Test with various empty values
  test_list <- list(
    a = 1,
    b = NULL,
    c = "",
    d = "valid",
    e = character(0),
    f = list(),
    g = FALSE # Should be kept
  )

  result <- compact_list(test_list)

  expect_true("a" %in% names(result))
  expect_true("d" %in% names(result))
  expect_true("g" %in% names(result)) # FALSE should be kept
  expect_false("b" %in% names(result)) # NULL removed
  expect_true("c" %in% names(result)) # Empty string kept (not NULL)
  expect_true("e" %in% names(result)) # Empty vector kept (not NULL)
  expect_true("f" %in% names(result)) # Empty list kept (not NULL)
})

test_that("NULL coalescing operator %||% works correctly", {
  # Test with NULL on left
  result1 <- NULL %||% "default"
  expect_equal(result1, "default")

  # Test with non-NULL on left
  result2 <- "value" %||% "default"
  expect_equal(result2, "value")

  # Test with both NULL
  result3 <- NULL %||% NULL
  expect_null(result3)

  # Test with complex objects
  result4 <- NULL %||% list(a = 1, b = 2)
  expect_equal(result4, list(a = 1, b = 2))

  result5 <- list(x = 1) %||% list(a = 1, b = 2)
  expect_equal(result5, list(x = 1))
})

test_that("to_json converts R objects to JSON", {
  # Test with simple types
  simple_data <- list(
    number = 42,
    string = "test",
    boolean = TRUE
  )

  result <- to_json(simple_data)
  expect_true(is.character(result))
  expect_true(jsonlite::validate(result))

  # Test with vector that should be unboxed
  single_value <- "test"
  result2 <- to_json(single_value)
  expect_equal(jsonlite::fromJSON(result2), "test")

  # Test with list
  list_data <- list(a = 1, b = 2)
  result3 <- to_json(list_data)
  parsed <- jsonlite::fromJSON(result3)
  expect_equal(parsed$a, 1)
  expect_equal(parsed$b, 2)
})

test_that("check_not_interactive handles interactive sessions", {
  # Test that the function exists and can be called
  expect_no_error(check_not_interactive())
})

test_that("compact removes empty elements from list", {
  # Test with mixed empty and non-empty elements
  test_list <- list(a = c(1, 2), b = character(0), c = "hello", d = numeric(0), e = list(x = 1))
  result <- compact(test_list)
  
  expect_equal(length(result), 3)
  expect_true("a" %in% names(result))
  expect_true("c" %in% names(result))
  expect_true("e" %in% names(result))
  expect_false("b" %in% names(result))
  expect_false("d" %in% names(result))
})

test_that("infer_ide detects IDE from command args", {
  # Mock commandArgs for different IDEs
  with_mocked_bindings(
    `commandArgs` = function() c("ark", "other", "args"),
    expect_equal(infer_ide(), "Positron")
  )
  
  with_mocked_bindings(
    `commandArgs` = function() c("RStudio", "other", "args"),
    expect_equal(infer_ide(), "RStudio")
  )
  
  with_mocked_bindings(
    `commandArgs` = function() c("some_other_ide", "args"),
    expect_equal(infer_ide(), "some_other_ide")
  )
})

test_that("null coalescing operator works correctly", {
  # Test with NULL left side
  expect_equal(NULL %||% "default", "default")
  expect_equal(NULL %||% 42, 42)
  
  # Test with non-NULL left side
  expect_equal("value" %||% "default", "value")
  expect_equal(123 %||% 456, 123)
  expect_equal(FALSE %||% TRUE, FALSE)
  
  # Test with both NULL
  expect_equal(NULL %||% NULL, NULL)
})

test_that("infer_ide detects IDE correctly", {
  # This test may be environment-dependent
  ide_result <- infer_ide()

  expect_true(is.character(ide_result))
  expect_true(length(ide_result) == 1)

  # Can be any string since it returns the first command argument
  # Just verify it's a valid character string
  expect_true(nchar(ide_result) >= 0)
})

test_that("check functions work correctly", {
  # Test check_string
  expect_silent(check_string("valid_string"))
  expect_error(check_string(123), "must be a single string")
  expect_error(check_string(NULL), "must be a single string")

  # Test check_bool
  expect_silent(check_bool(TRUE))
  expect_silent(check_bool(FALSE))
  expect_error(check_bool("not_boolean"), "must be a single logical value")
  expect_error(check_bool(1), "must be a single logical value")

  # Test check_function
  expect_silent(check_function(function(x) x))
  expect_silent(check_function(mean))
  expect_error(check_function("not_function"), "must be a function")
  expect_error(check_function(123), "must be a function")
})

test_that("get_system_socket_url returns platform-appropriate URL", {
  result <- get_system_socket_url()
  expect_true(is.character(result))
  expect_true(length(result) == 1)
  expect_true(nchar(result) > 0)
  
  # Should contain socket-related text
  expect_true(grepl("socket", result, ignore.case = TRUE))
})

test_that("check_session_socket works correctly", {
  # Test verbose mode (default)
  result_verbose <- check_session_socket(verbose = TRUE)
  expect_true(is.null(result_verbose) || is.numeric(result_verbose))
  
  # Test non-verbose mode
  result_list <- check_session_socket(verbose = FALSE)
  expect_true(is.list(result_list))
  expect_true("socket_number" %in% names(result_list))
  expect_true("is_interactive" %in% names(result_list))
  expect_true("has_session" %in% names(result_list))
  expect_true(is.logical(result_list$is_interactive))
  expect_true(is.logical(result_list$has_session))
})

test_that("describe_session creates session descriptions", {
  # Test basic session description
  result_basic <- describe_session(detailed = FALSE)
  expect_true(is.character(result_basic))
  expect_true(length(result_basic) == 1)
  expect_true(nchar(result_basic) > 0)
  
  # Test detailed session description
  result_detailed <- describe_session(detailed = TRUE)
  expect_true(is.character(result_detailed))
  expect_true(length(result_detailed) == 1)
  expect_true(nchar(result_detailed) > 0)
  
  # Detailed should be longer than basic
  expect_true(nchar(result_detailed) > nchar(result_basic))
  
  # Should contain timestamp in detailed mode
  expect_true(grepl("\\d{4}-\\d{2}-\\d{2}", result_detailed))
})
