# Unit tests for view session state functions
# Tests individual helper functions in view-session-state.R

test_that("view_session returns formatted session info", {
  result <- MCPR:::view_session(20)

  expect_type(result, "character")
  expect_true(grepl("R Session Information", result))
  expect_true(grepl("R version:", result))
  expect_true(grepl("Global Environment:", result))
  expect_true(grepl("Working directory:", result))
})

test_that("view_workspace returns directory structure", {
  result <- MCPR:::view_workspace(15)

  expect_type(result, "character")
  expect_true(grepl("Workspace Directory:", result))
  expect_true(grepl(getwd(), result))
})

test_that("view_last_error returns error info", {
  result <- MCPR:::view_last_error(10)

  expect_type(result, "character")
  expect_true(grepl("Last Error Information", result))
  # Should handle case when no error exists or when error exists
  expect_true(grepl("No recent errors|Error message:", result))
})

test_that("view_warnings returns warning info", {
  result <- MCPR:::view_warnings(10)

  expect_type(result, "character")
  expect_true(grepl("Recent Warnings Summary", result))
  # Should handle case when no warnings exist or when warnings exist
  expect_true(grepl("No recent warnings|Total warnings:", result))
})

test_that("view_terminal returns terminal info", {
  result <- MCPR:::view_terminal(10)

  expect_type(result, "character")
  expect_true(grepl("Terminal Output Summary", result))
  # Should handle various scenarios: recent commands, no history, or error
  expect_true(grepl("Recent commands|No command history|Error capturing terminal", result))
})

test_that("get_command_history helper function works", {
  # Test the multi-strategy history retrieval
  result <- MCPR:::get_command_history(5)

  # Result should be NULL or character vector
  expect_true(is.null(result) || is.character(result))

  # If result exists, should be non-empty
  if (!is.null(result)) {
    expect_true(length(result) > 0)
  }
})

test_that("parse_radian_history filters correctly", {
  # Create sample radian history format
  sample_history <- c(
    "# time: 2025-08-20 10:00:00 UTC",
    "# mode: r",
    "+library(MCPR)",
    "# time: 2025-08-20 10:01:00 UTC",
    "# mode: shell",
    "+ls -la",
    "# time: 2025-08-20 10:02:00 UTC",
    "# mode: r",
    "+view(\"session\")",
    "# time: 2025-08-20 09:00:00 UTC", # Old command
    "# mode: r",
    "+old_command()"
  )

  # Set session start to filter out old commands
  session_start <- as.POSIXct("2025-08-20 09:30:00", tz = "UTC")

  result <- MCPR:::parse_radian_history(sample_history, session_start, 10)

  expect_type(result, "character")
  expect_true("library(MCPR)" %in% result)
  expect_true("view(\"session\")" %in% result)
  expect_false("ls -la" %in% result) # Shell command should be filtered out
  expect_false("old_command()" %in% result) # Old command should be filtered out
})

test_that("get_session_start_time returns reasonable time", {
  session_start <- MCPR:::get_session_start_time()

  expect_s3_class(session_start, "POSIXct")
  # Should be sometime in the past but not too far
  expect_true(session_start <= Sys.time())
  expect_true(session_start > Sys.time() - as.difftime(24, units = "hours"))
})

test_that("session state functions handle edge cases gracefully", {
  # These should not error even if environment is minimal
  expect_no_error(MCPR:::view_session(5))
  expect_no_error(MCPR:::view_workspace(5))
  expect_no_error(MCPR:::view_last_error(5))
  expect_no_error(MCPR:::view_warnings(5))
  expect_no_error(MCPR:::view_terminal(5))
})
