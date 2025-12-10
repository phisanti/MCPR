# Unit tests for view utility functions
# Tests shared utility functions in view-utils.R

test_that("capture_print captures object output", {
  result <- MCPR:::capture_print(1:5, max_print = 10)

  expect_type(result, "character")
  expect_true(length(result) > 0)
  expect_true(any(grepl("\\[1\\] 1 2 3 4 5", result)))
})

test_that("format_file_size formats sizes correctly", {
  expect_equal(MCPR:::format_file_size(100), " (100 bytes)")
  expect_equal(MCPR:::format_file_size(1500), " (1.5 KB)")
  expect_equal(MCPR:::format_file_size(1500000), " (1.4 MB)")
  expect_equal(MCPR:::format_file_size(1500000000), " (1.4 GB)")
  expect_equal(MCPR:::format_file_size(NA), "")
})

test_that("format_count formats counts correctly", {
  expect_equal(MCPR:::format_count(0, "item"), "No items")
  expect_equal(MCPR:::format_count(1, "item"), "1 item")
  expect_equal(MCPR:::format_count(5, "item"), "5 items")
  expect_equal(MCPR:::format_count(20, "item", 10), "20 items (showing first 10)")
})

test_that("build_result builds formatted results", {
  result <- MCPR:::build_result("Title", "Line 1", "Line 2")
  expected <- "Title\nLine 1\nLine 2"
  expect_equal(result, expected)

  # Test with no content
  result2 <- MCPR:::build_result("Title Only")
  expect_equal(result2, "Title Only")
})

test_that("safe_eval handles errors gracefully", {
  # Test successful evaluation
  result1 <- MCPR:::safe_eval(2 + 2, "fallback", TRUE)
  expect_equal(result1, 4)

  # Test error handling with error message
  result2 <- MCPR:::safe_eval(stop("test error"), "fallback", TRUE)
  expect_equal(result2, "fallback: test error")

  # Test error handling without error message
  result3 <- MCPR:::safe_eval(stop("test error"), "fallback", FALSE)
  expect_equal(result3, "fallback")
})

test_that("truncate_text truncates properly", {
  short_text <- "short"
  expect_equal(MCPR:::truncate_text(short_text, 10), "short")

  long_text <- "this is a very long text that should be truncated"
  result <- MCPR:::truncate_text(long_text, 20)
  expect_equal(nchar(result), 20)
  expect_true(grepl("\\.\\.\\.$", result))
})

test_that("is_likely_text_file identifies file types correctly", {
  # Test common text extensions (these return TRUE based on extension alone)
  expect_true(MCPR:::is_likely_text_file("test.txt", "txt"))
  expect_true(MCPR:::is_likely_text_file("test.R", "R"))
  expect_true(MCPR:::is_likely_text_file("test.md", "md"))
  expect_true(MCPR:::is_likely_text_file("test.json", "json"))
  expect_true(MCPR:::is_likely_text_file("test.py", "py"))
  expect_true(MCPR:::is_likely_text_file("test.css", "css"))

  # Function should always return a logical value
  expect_type(MCPR:::is_likely_text_file("test.txt", "txt"), "logical")
})

test_that("capture_print handles different object types", {
  # Test with data frame
  df_result <- MCPR:::capture_print(data.frame(x = 1:3, y = letters[1:3]))
  expect_type(df_result, "character")
  expect_true(any(grepl("x", df_result)))

  # Test with list
  list_result <- MCPR:::capture_print(list(a = 1, b = "hello"))
  expect_type(list_result, "character")

  # Test with function
  func_result <- MCPR:::capture_print(function() "test")
  expect_type(func_result, "character")
})

test_that("capture_print respects max_print parameter", {
  long_vector <- 1:1000
  result_short <- MCPR:::capture_print(long_vector, max_print = 10)
  result_long <- MCPR:::capture_print(long_vector, max_print = 200)

  expect_type(result_short, "character")
  expect_type(result_long, "character")
  # Short version should be truncated
  expect_true(length(result_short) <= length(result_long))
})

test_that("format_error_info cleans error messages", {
  test_error <- structure(list(message = "Error in test: something failed"), class = "simpleError")
  result <- MCPR:::format_error_info(test_error)

  expect_type(result, "character")
  expect_false(grepl("^Error in", result))
  expect_true(grepl("something failed", result))
})

test_that("get_file_content_preview works with text files", {
  # Create a temporary text file
  temp_file <- tempfile(fileext = ".txt")
  writeLines(c("line 1", "line 2", "line 3", "line 4", "line 5"), temp_file)

  result <- MCPR:::get_file_content_preview(temp_file, max_lines = 3)

  expect_type(result, "character")
  expect_equal(length(result), 4) # 3 lines + continuation message
  expect_equal(result[1], "line 1")
  expect_true(grepl("more", result[4]))

  unlink(temp_file)
})

test_that("get_file_content_preview handles empty files", {
  temp_file <- tempfile(fileext = ".txt")
  file.create(temp_file)

  result <- MCPR:::get_file_content_preview(temp_file, max_lines = 5)

  expect_true(is.null(result) || length(result) == 0)

  unlink(temp_file)
})

test_that("is_likely_text_file handles unknown extensions", {
  # For files with no clear extension, function tries content analysis
  # Create a temporary file to test with
  temp_file <- tempfile()
  writeLines("test content", temp_file)

  result <- MCPR:::is_likely_text_file(temp_file, "")
  expect_type(result, "logical")

  unlink(temp_file)
})

test_that("build_result handles NULL inputs", {
  result <- MCPR:::build_result("Title", NULL, "Valid line", NULL)
  expect_equal(result, "Title\nValid line")
})

test_that("utility functions handle edge cases", {
  # Test with empty/null inputs where appropriate
  expect_equal(MCPR:::format_file_size(0), " (0 bytes)")
  expect_equal(MCPR:::format_count(0, "test"), "No tests")
  expect_equal(MCPR:::build_result("Test"), "Test")
  expect_equal(MCPR:::truncate_text("", 10), "")

  # Test error handling
  expect_no_error(MCPR:::safe_eval(NULL, "fallback"))
  expect_no_error(MCPR:::format_error_info(list()))
})
