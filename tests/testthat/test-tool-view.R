# Unit tests for the view tool
# Tests the main view function dispatcher and argument validation

# Load the view tool
source(file.path(system.file(package = "MCPR"), "tool-view.R"))

test_that("view accepts valid what arguments", {
  valid_options <- c(
    "session", "terminal", "last_error", "installed_packages",
    "workspace", "search_path", "warnings"
  )

  for (option in valid_options) {
    result <- view(option, max_lines = 10)
    expect_type(result, "character")
    expect_true(grepl("View completed:", result))
  }
})

test_that("view rejects invalid arguments", {
  # Invalid what argument
  expect_error(view("invalid_option"), "should be one of")
  expect_error(view(c("session", "terminal")), "single character string")
  expect_error(view(123), "single character string")
  expect_error(view(""), "cannot be empty")
  expect_error(view(NULL), "single character string")

  # Invalid max_lines argument
  expect_error(view("session", max_lines = "ten"), "positive integer")
  expect_error(view("session", max_lines = -5), "positive integer")
  expect_error(view("session", max_lines = c(10, 20)), "positive integer")
  expect_error(view("session", max_lines = NULL), "positive integer")
})

test_that("view handles max_lines edge cases", {
  # Very small max_lines
  result1 <- view("session", max_lines = 1)
  expect_type(result1, "character")
  expect_true(grepl("View completed:", result1))

  # Large max_lines
  result2 <- view("session", max_lines = 1000)
  expect_type(result2, "character")
  expect_true(grepl("View completed:", result2))
})

test_that("view tool works in realistic workflow", {
  # Create some test objects
  test_var <- 1:10
  test_df <- data.frame(a = 1:3, b = letters[1:3])

  # Test session view shows environment info
  session_result <- view("session", max_lines = 100)
  expect_true(grepl("Global Environment", session_result))
  expect_true(grepl("objects", session_result))

  # Test that different views complete successfully
  workspace_result <- view("workspace", max_lines = 20)
  packages_result <- view("installed_packages", max_lines = 15)

  expect_true(grepl("View completed: session", session_result))
  expect_true(grepl("View completed: workspace", workspace_result))
  expect_true(grepl("View completed: installed_packages", packages_result))

  # Clean up test objects
  rm(test_var, test_df)
})
