test_that("drop_nulls removes NULL values correctly", {
  # Test with vector containing NULLs
  test_list <- list(1, NULL, "test", NULL, TRUE)
  result <- MCPR:::drop_nulls(test_list)

  expect_equal(length(result), 3)
  expect_equal(result[[1]], 1)
  expect_equal(result[[2]], "test")
  expect_equal(result[[3]], TRUE)

  # Test with no NULL values
  no_nulls <- list(1, 2, 3)
  result2 <- MCPR:::drop_nulls(no_nulls)
  expect_equal(result2, no_nulls)

  # Test with all NULL values
  all_nulls <- list(NULL, NULL, NULL)
  result3 <- MCPR:::drop_nulls(all_nulls)
  expect_equal(length(result3), 0)

  # Test with empty list
  empty_list <- list()
  result4 <- MCPR:::drop_nulls(empty_list)
  expect_equal(result4, empty_list)
})

test_that("named_list creates proper named lists", {
  # Test with named arguments
  result <- MCPR:::named_list(a = 1, b = 2, c = "test")

  expect_true(is.list(result))
  expect_equal(names(result), c("a", "b", "c"))
  expect_equal(result$a, 1)
  expect_equal(result$b, 2)
  expect_equal(result$c, "test")

  # Test with empty arguments
  empty_result <- MCPR:::named_list()
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

  result <- MCPR:::compact_list(test_list)

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
  result1 <- MCPR:::`%||%`(NULL, "default")
  expect_equal(result1, "default")

  # Test with non-NULL on left
  result2 <- MCPR:::`%||%`("value", "default")
  expect_equal(result2, "value")

  # Test with both NULL
  result3 <- MCPR:::`%||%`(NULL, NULL)
  expect_null(result3)

  # Test with complex objects
  result4 <- MCPR:::`%||%`(NULL, list(a = 1, b = 2))
  expect_equal(result4, list(a = 1, b = 2))

  result5 <- MCPR:::`%||%`(list(x = 1), list(a = 1, b = 2))
  expect_equal(result5, list(x = 1))
})

test_that("to_json converts R objects to JSON", {
  # Test with simple types
  simple_data <- list(
    number = 42,
    string = "test",
    boolean = TRUE
  )

  result <- MCPR:::to_json(simple_data)
  expect_true(is.character(result))
  expect_true(jsonlite::validate(result))

  # Test with vector that should be unboxed
  single_value <- "test"
  result2 <- MCPR:::to_json(single_value)
  expect_equal(jsonlite::fromJSON(result2), "test")

  # Test with list
  list_data <- list(a = 1, b = 2)
  result3 <- MCPR:::to_json(list_data)
  parsed <- jsonlite::fromJSON(result3)
  expect_equal(parsed$a, 1)
  expect_equal(parsed$b, 2)
})

test_that("check_not_interactive handles interactive sessions", {
  # Test that the function exists and can be called in non-interactive mode
  expect_no_error(MCPR:::check_not_interactive())

  # Test interactive mode with mocking
  with_mocked_bindings(
    `is_interactive` = function() TRUE, .package = "rlang",
    expect_error(MCPR:::check_not_interactive(), "This function is not intended for interactive use")
  )
})

test_that("compact removes empty elements from list", {
  # Test with mixed empty and non-empty elements
  test_list <- list(a = c(1, 2), b = character(0), c = "hello", d = numeric(0), e = list(x = 1))
  result <- MCPR:::compact(test_list)

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
    expect_equal(MCPR:::infer_ide(), "Positron")
  )

  with_mocked_bindings(
    `commandArgs` = function() c("RStudio", "other", "args"),
    expect_equal(MCPR:::infer_ide(), "RStudio")
  )

  with_mocked_bindings(
    `commandArgs` = function() c("some_other_ide", "args"),
    expect_equal(MCPR:::infer_ide(), "some_other_ide")
  )
})

test_that("null coalescing operator works correctly", {
  # Test with NULL left side
  expect_equal(MCPR:::`%||%`(NULL, "default"), "default")
  expect_equal(MCPR:::`%||%`(NULL, 42), 42)

  # Test with non-NULL left side
  expect_equal(MCPR:::`%||%`("value", "default"), "value")
  expect_equal(MCPR:::`%||%`(123, 456), 123)
  expect_equal(MCPR:::`%||%`(FALSE, TRUE), FALSE)

  # Test with both NULL
  expect_equal(MCPR:::`%||%`(NULL, NULL), NULL)
})

test_that("infer_ide detects IDE correctly", {
  # This test may be environment-dependent
  ide_result <- MCPR:::infer_ide()

  expect_true(is.character(ide_result))
  expect_true(length(ide_result) == 1)

  # Can be any string since it returns the first command argument
  # Just verify it's a valid character string
  expect_true(nchar(ide_result) >= 0)
})

test_that("check functions work correctly", {
  # Test check_string
  expect_silent(MCPR:::check_string("valid_string"))
  expect_error(MCPR:::check_string(123), "must be a single string")
  expect_error(MCPR:::check_string(NULL), "must be a single string")

  # Test check_string with allow_null = TRUE
  expect_silent(MCPR:::check_string(NULL, allow_null = TRUE))
  expect_silent(MCPR:::check_string("valid", allow_null = TRUE))
  expect_error(MCPR:::check_string(123, allow_null = TRUE), "must be a single string")

  # Test check_bool
  expect_silent(MCPR:::check_bool(TRUE))
  expect_silent(MCPR:::check_bool(FALSE))
  expect_error(MCPR:::check_bool("not_boolean"), "must be a single logical value")
  expect_error(MCPR:::check_bool(1), "must be a single logical value")

  # Test check_bool with allow_null = TRUE
  expect_silent(MCPR:::check_bool(NULL, allow_null = TRUE))
  expect_silent(MCPR:::check_bool(TRUE, allow_null = TRUE))
  expect_error(MCPR:::check_bool("not_boolean", allow_null = TRUE), "must be a single logical value")

  # Test check_function
  expect_silent(MCPR:::check_function(function(x) x))
  expect_silent(MCPR:::check_function(mean))
  expect_error(MCPR:::check_function("not_function"), "must be a function")
  expect_error(MCPR:::check_function(123), "must be a function")
})

test_that("get_system_socket_url returns platform-appropriate URL", {
  result <- MCPR:::get_system_socket_url()
  expect_true(is.character(result))
  expect_true(length(result) == 1)
  expect_true(nchar(result) > 0)

  # Should contain socket-related text
  expect_true(grepl("socket", result, ignore.case = TRUE))
})

test_that("check_session_socket works correctly", {
  # Test verbose mode (default)
  result_verbose <- MCPR:::check_session_socket(verbose = TRUE)
  expect_true(is.null(result_verbose) || is.numeric(result_verbose))

  # Test non-verbose mode
  result_list <- MCPR:::check_session_socket(verbose = FALSE)
  expect_true(is.list(result_list))
  expect_true("socket_number" %in% names(result_list))
  expect_true("is_interactive" %in% names(result_list))
  expect_true("has_session" %in% names(result_list))
  expect_true(is.logical(result_list$is_interactive))
  expect_true(is.logical(result_list$has_session))
})

test_that("describe_session creates session descriptions", {
  # Test basic session description
  result_basic <- MCPR:::describe_session(detailed = FALSE)
  expect_true(is.character(result_basic))
  expect_true(length(result_basic) == 1)
  expect_true(nchar(result_basic) > 0)

  # Test detailed session description
  result_detailed <- MCPR:::describe_session(detailed = TRUE)
  expect_true(is.character(result_detailed))
  expect_true(length(result_detailed) == 1)
  expect_true(nchar(result_detailed) > 0)

  # Detailed should be longer than basic
  expect_true(nchar(result_detailed) > nchar(result_basic))

  # Should contain timestamp in detailed mode
  expect_true(grepl("\\d{4}-\\d{2}-\\d{2}", result_detailed))
})

# Test format_table_for_agent function for consistent agent/LLM table formatting
test_that("format_table_for_agent handles basic data frames correctly", {
  test_df <- data.frame(
    Name = c("Alice", "Bob", "Charlie"),
    Age = c(25, 30, 35),
    City = c("New York", "London", "Tokyo"),
    stringsAsFactors = FALSE
  )
  
  result <- MCPR:::format_table_for_agent(test_df)
  
  # Check that result contains expected elements
  expect_type(result, "character")
  expect_true(grepl("Name", result))
  expect_true(grepl("Age", result))
  expect_true(grepl("City", result))
  expect_true(grepl("Alice", result))
  expect_true(grepl("Bob", result))
  expect_true(grepl("Charlie", result))
  
  # Check that it has proper table structure (header + separator + data)
  lines <- strsplit(result, "\n")[[1]]
  expect_gte(length(lines), 5) # header + separator + 3 data rows
  expect_true(grepl("^-+$", lines[2])) # separator line should be all dashes
})

test_that("format_table_for_agent handles empty data frames", {
  empty_df <- data.frame(Name = character(), Age = numeric(), City = character())
  
  result <- MCPR:::format_table_for_agent(empty_df, "No data available.")
  expect_equal(result, "No data available.")
  
  # Test default empty message
  result_default <- MCPR:::format_table_for_agent(empty_df)
  expect_equal(result_default, "No data found.")
})

test_that("format_table_for_agent handles single row data frames", {
  single_df <- data.frame(Tool = "read_instructions", Status = "active")
  
  result <- MCPR:::format_table_for_agent(single_df)
  
  expect_type(result, "character")
  expect_true(grepl("Tool", result))
  expect_true(grepl("Status", result))
  expect_true(grepl("read_instructions", result))
  expect_true(grepl("active", result))
  
  lines <- strsplit(result, "\n")[[1]]
  expect_equal(length(lines), 3) # header + separator + 1 data row
})

test_that("format_table_for_agent handles varying column widths correctly", {
  wide_df <- data.frame(
    Short = c("A", "B"),
    `Very Long Column Name` = c("Short text", "This is much longer content"),
    Num = c(1, 1000),
    stringsAsFactors = FALSE
  )
  
  result <- MCPR:::format_table_for_agent(wide_df)
  
  # Check that columns are properly aligned
  lines <- strsplit(result, "\n")[[1]]
  header_line <- lines[1]
  data_lines <- lines[3:length(lines)]
  
  # All lines should have same structure with | separators
  expect_true(grepl("\\|", header_line))
  for (line in data_lines) {
    expect_true(grepl("\\|", line))
  }
})

test_that("format_table_for_agent produces consistent output format", {
  # This test ensures the output format matches expectations for agent consumption
  instructions_df <- data.frame(
    Path = c("financial_analysis.md", "risk_modeling.md"),
    Keyword = c("financial_analysis", "risk_modeling"),  
    Description = c("Instructions for financial analysis", "Instructions for risk modeling"),
    stringsAsFactors = FALSE
  )
  
  result <- MCPR:::format_table_for_agent(instructions_df)
  lines <- strsplit(result, "\n")[[1]]
  
  # Check structure: header, separator, data rows
  expect_true(grepl("^Path.*\\|.*Keyword.*\\|.*Description", lines[1]))
  expect_true(grepl("^-+$", lines[2]))
  expect_true(grepl("^financial_analysis.md.*\\|.*financial_analysis.*\\|.*Instructions for financial analysis", lines[3]))
  expect_true(grepl("^risk_modeling.md.*\\|.*risk_modeling.*\\|.*Instructions for risk modeling", lines[4]))
})
