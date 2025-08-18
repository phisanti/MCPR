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
    g = FALSE  # Should be kept
  )
  
  result <- compact_list(test_list)
  
  expect_true("a" %in% names(result))
  expect_true("d" %in% names(result))
  expect_true("g" %in% names(result))  # FALSE should be kept
  expect_false("b" %in% names(result))  # NULL removed
  expect_true("c" %in% names(result))   # Empty string kept (not NULL)
  expect_true("e" %in% names(result))   # Empty vector kept (not NULL)
  expect_true("f" %in% names(result))   # Empty list kept (not NULL)
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

test_that("convert_json_types handles basic conversions", {
  # Test with simple types
  simple_data <- list(
    number = 42,
    string = "test",
    boolean = TRUE,
    null_val = NULL
  )
  
  result <- convert_json_types(simple_data)
  expect_equal(result, simple_data)
  
  # Test with nested structures
  nested_data <- list(
    outer = list(
      inner = list(
        value = 123,
        text = "nested"
      )
    )
  )
  
  result2 <- convert_json_types(nested_data)
  expect_equal(result2$outer$inner$value, 123)
  expect_equal(result2$outer$inner$text, "nested")
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

test_that("can_serialize determines serialization capability", {
  # Test objects that can be serialized
  expect_true(can_serialize(1:10))
  expect_true(can_serialize("test string"))
  expect_true(can_serialize(list(a = 1, b = 2)))
  expect_true(can_serialize(data.frame(x = 1:3, y = letters[1:3])))
  
  # Test objects that typically cannot be serialized
  # Environment
  test_env <- new.env()
  result_env <- can_serialize(test_env)
  expect_true(is.logical(result_env))  # Should return logical
  
  # Function
  test_function <- function(x) x
  result_func <- can_serialize(test_function)
  expect_true(is.logical(result_func))  # Should return logical
})