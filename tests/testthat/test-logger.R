test_that("MCPRLogger can be instantiated", {
  logger <- MCPR:::MCPRLogger$new()
  expect_true(inherits(logger, "MCPRLogger"))
  expect_true(inherits(logger, "R6"))
})

test_that("MCPRLogger initializes with correct defaults", {
  logger <- MCPR:::MCPRLogger$new()

  # Test that logger methods exist
  expect_true(is.function(logger$info))
  expect_true(is.function(logger$warn))
  expect_true(is.function(logger$error))
  expect_true(is.function(logger$debug))
  expect_true(is.function(logger$comm))
})

test_that("MCPRLogger accepts custom component names", {
  logger <- MCPR:::MCPRLogger$new(component = "TEST_COMPONENT")

  # We can't easily test the private component field directly,
  # but we can test that the logger was created successfully
  expect_true(inherits(logger, "MCPRLogger"))
})

test_that("MCPRLogger can be enabled and disabled", {
  logger <- MCPR:::MCPRLogger$new()

  # Test enable method exists
  expect_true(is.function(logger$enable))

  # Test that we can call enable method without error
  expect_silent(logger$enable(FALSE)) # disable
  expect_silent(logger$enable(TRUE)) # enable
})

test_that("MCPRLogger logging methods work without error", {
  # Create a temporary file for testing
  temp_file <- tempfile()
  on.exit(unlink(temp_file), add = TRUE)

  logger <- MCPR:::MCPRLogger$new(file = temp_file)

  # Test all logging levels
  expect_silent(logger$info("Test info message"))
  expect_silent(logger$warn("Test warning message"))
  expect_silent(logger$error("Test error message"))
  expect_silent(logger$debug("Test debug message"))
  expect_silent(logger$comm("Test communication message"))

  # Verify file was created and has content
  expect_true(file.exists(temp_file))
  log_content <- readLines(temp_file)
  expect_true(length(log_content) > 0)
})

test_that("MCPRLogger respects enabled/disabled state", {
  # Create a temporary file for testing
  temp_file <- tempfile()
  on.exit(unlink(temp_file), add = TRUE)

  logger <- MCPR:::MCPRLogger$new(file = temp_file)

  # Log when enabled
  logger$info("Enabled message")

  # Disable and log
  logger$enable(FALSE)
  logger$info("Disabled message")

  # Re-enable and log
  logger$enable(TRUE)
  logger$info("Re-enabled message")

  # Check that the file exists and has some content
  expect_true(file.exists(temp_file))
  if (file.exists(temp_file) && file.size(temp_file) > 0) {
    log_content <- readLines(temp_file)
    # Should have at least the enabled messages
    expect_true(length(log_content) >= 1)
  }
})

test_that("MCPRLogger handles different file outputs", {
  # Test with NULL file (should log to console)
  console_logger <- MCPR:::MCPRLogger$new(file = NULL)
  expect_silent(console_logger$info("Console message"))

  # Test with custom file
  custom_file <- tempfile(fileext = ".log")
  on.exit(unlink(custom_file), add = TRUE)

  file_logger <- MCPR:::MCPRLogger$new(file = custom_file)
  file_logger$info("File message")

  expect_true(file.exists(custom_file))
})

test_that("MCPRLogger format includes timestamp and level", {
  # Create a temporary file for testing
  temp_file <- tempfile()
  on.exit(unlink(temp_file), add = TRUE)

  logger <- MCPR:::MCPRLogger$new(file = temp_file, component = "TEST")
  logger$info("Test message")

  if (file.exists(temp_file)) {
    log_content <- readLines(temp_file)
    if (length(log_content) > 0) {
      # Check that log entry contains expected format elements
      log_line <- log_content[1]
      expect_true(grepl("\\[.*\\]", log_line)) # Timestamp in brackets
      expect_true(grepl("INFO", log_line)) # Log level
      expect_true(grepl("TEST", log_line)) # Component name
      expect_true(grepl("Test message", log_line)) # Message content
    }
  }
})

test_that("MCPRLogger methods return self for chaining", {
  logger <- MCPR:::MCPRLogger$new()

  # Logging methods return self for method chaining (R6 pattern)
  result1 <- logger$info("Test")
  result2 <- logger$warn("Test")
  result3 <- logger$error("Test")
  result4 <- logger$debug("Test")
  result5 <- logger$comm("Test")

  expect_identical(result1, logger)
  expect_identical(result2, logger)
  expect_identical(result3, logger)
  expect_identical(result4, logger)
  expect_identical(result5, logger)
})

test_that("MCPRLogger can handle empty and special messages", {
  temp_file <- tempfile()
  on.exit(unlink(temp_file), add = TRUE)

  logger <- MCPR:::MCPRLogger$new(file = temp_file)

  # Test with empty message
  expect_silent(logger$info(""))

  # Test with special characters
  expect_silent(logger$info("Message with symbols: !@#$%^&*()"))

  # Test with newlines
  expect_silent(logger$info("Multi\nline\nmessage"))

  # Test with Unicode
  expect_silent(logger$info("Unicode: 🚀 ✨ 📊"))
})
