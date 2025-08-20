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
  # May not have history in test environment, but should handle gracefully
  expect_true(grepl("Recent commands|No command history|Error capturing terminal", result))
})

test_that("session state functions handle edge cases gracefully", {
  # These should not error even if environment is minimal
  expect_no_error(MCPR:::view_session(5))
  expect_no_error(MCPR:::view_workspace(5)) 
  expect_no_error(MCPR:::view_last_error(5))
  expect_no_error(MCPR:::view_warnings(5))
  expect_no_error(MCPR:::view_terminal(5))
})