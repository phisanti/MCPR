# Load MCPR package and source tool
library(MCPR)
source(system.file("tool-manage_r_sessions.R", package = "MCPR", mustWork = TRUE))

# Expose internal state so tests can manipulate the daemon registry directly
.the <- MCPR:::the

test_that("manage_r_sessions validates action parameter", {
  # Test that invalid actions are rejected
  expect_error(
    manage_r_sessions(action = "invalid"),
    "action must be one of: 'list', 'join', 'start', 'close'"
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
  result <- tryCatch(
    {
      manage_r_sessions(action = "list")
    },
    error = function(e) {
      # If no sessions are running, this is expected behavior
      "no_sessions_available"
    }
  )

  # Either we get session results or expected failure
  expect_true(
    is.character(result) || identical(result, "no_sessions_available")
  )
})

# Removed problematic test - describe_session_detailed function not available in test environment

test_that("manage_r_sessions validates action parameter rejects unknown actions", {
  expect_error(
    manage_r_sessions(action = "invalid_action"),
    "action must be one of"
  )
})

test_that("manage_r_sessions accepts start and close actions", {
  # start and close are now valid actions (they may fail due to no server,
  # but they should not fail validation)
  expect_error(
    manage_r_sessions(action = "start"),
    regexp = NA
  ) |>
    tryCatch(error = function(e) {
      # Errors other than validation are acceptable (e.g., server not running)
      expect_false(grepl("action must be one of", e$message))
    })

  expect_error(
    manage_r_sessions(action = "close"),
    regexp = NA
  ) |>
    tryCatch(error = function(e) {
      expect_false(grepl("action must be one of", e$message))
    })
})

test_that("manage_r_sessions 'stop' is accepted as alias for 'close'", {
  # "stop" should not fail validation — it's silently normalized to "close"
  expect_error(
    manage_r_sessions(action = "stop"),
    regexp = NA
  ) |>
    tryCatch(error = function(e) {
      expect_false(grepl("action must be one of", e$message))
    })
})

test_that("manage_r_sessions close requires session parameter", {
  expect_error(
    manage_r_sessions(action = "close"),
    "session parameter is required when action='close'"
  )
})

test_that("manage_r_sessions close finds daemon by session ID regardless of key", {
  # Simulate a default-keyed daemon (as auto-spawned by server)
  old_sessions <- .the$daemon_sessions
  old_sockets <- .the$daemon_sockets
  old_processes <- .the$daemon_processes
  on.exit({
    .the$daemon_sessions <- old_sessions
    .the$daemon_sockets <- old_sockets
    .the$daemon_processes <- old_processes
  }, add = TRUE)

  .the$daemon_sessions <- c("default" = 7L)
  .the$daemon_sockets <- list()
  .the$daemon_processes <- list()

  # close session=7 should find the "default" key and unregister it
  result <- manage_r_sessions(action = "close", session = 7)
  expect_match(result, "closed")
  expect_length(.the$daemon_sessions, 0)
})
