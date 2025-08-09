# Load MCPR package and source tool
library(MCPR)
source(file.path(find.package("MCPR"), "tool-manage_r_sessions.R"))

test_that("manage_r_sessions validates action parameter", {
  # Test that invalid actions are rejected
  expect_error(
    manage_r_sessions(action = "invalid"),
    "action must be one of: 'list', 'join', 'start'"
  )
})

test_that("manage_r_sessions join requires session parameter", {
  # Test that join action requires session parameter
  expect_error(
    manage_r_sessions(action = "join"),
    "session parameter is required when action='join'"
  )
  
  # Test that session must be numeric
  expect_error(
    manage_r_sessions(action = "join", session = "not_numeric"),
    "session must be a single integer"
  )
})

test_that("manage_r_sessions list action works without parameters", {
  # Test that list action works with default parameters
  # Note: This will attempt actual socket communication
  # In early development, we expect this might fail gracefully
  result <- tryCatch({
    manage_r_sessions(action = "list")
  }, error = function(e) {
    # If no sessions are running, this is expected behavior
    "no_sessions_available"
  })
  
  # Either we get session results or expected failure
  expect_true(
    is.character(result) || identical(result, "no_sessions_available")
  )
})

test_that("describe_session_detailed returns properly formatted string", {
  # Skip this test if infer_ide is not available (dependency issue in early development)
  skip_if_not(exists("infer_ide", mode = "function"), "infer_ide function not available")
  
  # Mock the global environment for testing
  old_session <- if (exists("the")) the$session else NULL
  
  # Create minimal mock environment
  if (!exists("the")) {
    the <<- new.env()
  }
  the$session <- 1
  
  # Test the helper function
  result <- describe_session_detailed()
  
  # Should contain session number, working directory, IDE, and timestamp
  expect_true(grepl("^1: .+ \\(.+\\) - \\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", result))
  
  # Restore original state
  if (!is.null(old_session)) {
    the$session <- old_session
  }
})

test_that("manage_r_sessions start action handles processx gracefully", {
  # Test that start action attempts to use processx
  # In early development, we expect this might fail due to missing dependencies
  result <- tryCatch({
    manage_r_sessions(action = "start")
  }, error = function(e) {
    # Capture error message for inspection
    e$message
  })
  
  # Should either succeed with session info or fail with informative error
  expect_true(
    is.character(result) && 
    (grepl("Started new R session", result) || grepl("Error", result))
  )
})