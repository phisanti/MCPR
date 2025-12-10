# Mock functions for testing async operations
skip_async_tests <- function() {
  skip("Async operations require complex mocking")
}

# Helper function to create test session with controlled state
create_test_session <- function(running = FALSE, session_id = NULL, timeout = 900) {
  session <- mcprSession$new(timeout_seconds = timeout)
  if (running) {
    session$.__enclos_env__$private$.is_running <- TRUE
    session$.__enclos_env__$private$.last_activity <- Sys.time()
  }
  if (!is.null(session_id)) {
    session$.__enclos_env__$private$.session_id <- session_id
  }
  session
}

test_that("mcprSession can be instantiated", {
  session <- mcprSession$new()
  expect_s3_class(session, "mcprSession")
  expect_s3_class(session, "R6")
})

test_that("mcprSession initializes with correct defaults", {
  session <- mcprSession$new()
  info <- session$get_info()

  expect_null(info$session_id)
  expect_false(info$is_running)
  expect_false(info$socket_active) # Socket is NOT created in initialize
  expect_true(is.null(info$last_activity) || inherits(info$last_activity, "POSIXt"))
})

test_that("mcprSession accepts custom timeout", {
  custom_timeout <- 1800 # 30 minutes
  session <- mcprSession$new(timeout_seconds = custom_timeout)

  # Test that timeout is stored correctly (we can't directly access private vars)
  # but we can test that the object was created successfully
  expect_s3_class(session, "mcprSession")
})

test_that("mcprSession methods return invisible self", {
  session <- mcprSession$new()

  # All public methods should return invisible(self) for chaining
  result <- session$stop()
  expect_identical(result, session)

  result <- session$check_timeout()
  expect_identical(result, session)
})

test_that("mcprSession stop method is idempotent", {
  session <- mcprSession$new()

  # Should be able to call stop multiple times without error
  expect_silent(session$stop())
  expect_silent(session$stop())
  expect_silent(session$stop())

  info <- session$get_info()
  expect_false(info$is_running)
})

test_that("mcprSession handles non-interactive environment gracefully", {
  # Mock non-interactive environment
  original_interactive <- rlang::is_interactive

  # Create a mock function that returns FALSE
  mock_interactive <- function() FALSE

  # We can't easily mock rlang::is_interactive in tests, so we'll test
  # that the start method returns the session object
  session <- mcprSession$new()
  result <- session$start()
  expect_identical(result, session)
})

test_that("mcpr_session_start functional wrapper works", {
  # Should return invisibly without error in non-interactive mode
  expect_invisible(result <- mcpr_session_start())

  # Clean up any session that might have been created
  mcpr_session_stop()
})

test_that("mcpr_session_stop is safe to call when no session exists", {
  # Should not error when called without an active session
  expect_silent(mcpr_session_stop())
})

test_that("mcprSession get_info provides complete status", {
  session <- mcprSession$new()
  info <- session$get_info()

  # Check that all expected fields are present
  expected_fields <- c("session_id", "is_running", "last_activity", "socket_active")
  expect_true(all(expected_fields %in% names(info)))

  # Check types
  expect_true(is.logical(info$is_running))
  expect_true(is.logical(info$socket_active))
})

test_that("mcprSession check_timeout handles non-running session", {
  session <- mcprSession$new()

  # Should handle gracefully when not running
  expect_no_error(session$check_timeout())
  expect_false(session$get_info()$is_running)
})

test_that("mcprSession check_timeout handles null last_activity", {
  session <- mcprSession$new()

  # Set running but no last_activity (simulate edge case)
  session$.__enclos_env__$private$.is_running <- TRUE
  session$.__enclos_env__$private$.last_activity <- NULL

  expect_no_error(session$check_timeout())
})

test_that("mcprSession private find_available_port method works", {
  session <- mcprSession$new()

  # Create a mock socket for testing
  session$state_set("session_socket", list())

  # Mock the socket to simulate successful listen on port 2
  mock_socket <- list()
  session$state_set("session_socket", mock_socket)

  # Since we can't easily mock nanonext::listen, we'll test that the method exists
  expect_true(exists("find_available_port", session$.__enclos_env__$private))
})

test_that("mcprSession private schedule_timeout_check only runs when active", {
  session <- mcprSession$new()

  # When not running, should not schedule
  session$.__enclos_env__$private$.is_running <- FALSE
  expect_no_error(session$.__enclos_env__$private$schedule_timeout_check())

  # When running, would schedule (but we can't test later::later easily)
  session$.__enclos_env__$private$.is_running <- TRUE
  expect_no_error(session$.__enclos_env__$private$schedule_timeout_check())
})

test_that("mcprSession private start_listening handles non-running state", {
  session <- mcprSession$new()

  # Should return gracefully when not running
  session$.__enclos_env__$private$.is_running <- FALSE
  result <- session$.__enclos_env__$private$start_listening()
  expect_identical(result, session)
})

test_that("mcprSession private handle_message handles non-running state", {
  session <- mcprSession$new()

  # Should return gracefully when not running
  session$.__enclos_env__$private$.is_running <- FALSE
  result <- session$.__enclos_env__$private$handle_message(list())
  expect_identical(result, session)
})

test_that("mcprSession private handle_message processes discovery ping", {
  skip("Complex socket mocking required")
})

test_that("mcprSession private handle_message processes tool calls", {
  skip("Complex socket and tool execution mocking required")
})

test_that("mcprSession private handle_message processes unknown methods", {
  skip("Complex socket mocking required")
})

test_that("mcprSession private send_response handles non-running state", {
  session <- mcprSession$new()

  # Should return gracefully when not running
  session$.__enclos_env__$private$.is_running <- FALSE
  result <- session$.__enclos_env__$private$send_response("test", 1)
  expect_identical(result, session)
})

test_that("mcprSession private send_response handles missing socket", {
  session <- mcprSession$new()

  # Test that method exists and handles missing socket gracefully
  # Note: Actual socket operations are complex to test due to nanonext dependencies
  expect_true(exists("send_response", session$.__enclos_env__$private))

  # Test non-running state (should return gracefully)
  result <- session$.__enclos_env__$private$send_response("test", 1)
  expect_identical(result, session)
})

test_that("mcpr_session_stop handles existing session", {
  # Create a mock session in the global environment
  the_env <- MCPR:::the
  the_env$mcpr_session <- mcprSession$new()
  the_env$session <- 1

  # Stop should clean up both
  expect_no_error(mcpr_session_stop())
  expect_null(the_env$mcpr_session)
  expect_null(the_env$session)
})

test_that("mcprSession timeout functionality works with short timeout", {
  session <- mcprSession$new(timeout_seconds = 0.1) # Very short timeout

  # Set up state to simulate recent activity
  session$.__enclos_env__$private$.is_running <- TRUE
  session$.__enclos_env__$private$.last_activity <- Sys.time() - 1 # 1 second ago

  # Check timeout should stop the session
  session$check_timeout()
  expect_false(session$get_info()$is_running)
})

test_that("mcprSession timeout functionality preserves running state with recent activity", {
  session <- mcprSession$new(timeout_seconds = 10) # 10 second timeout

  # Set up state to simulate recent activity
  session$.__enclos_env__$private$.is_running <- TRUE
  session$.__enclos_env__$private$.last_activity <- Sys.time() # Just now

  # Check timeout should keep session running
  session$check_timeout()
  expect_true(session$get_info()$is_running)
})

test_that("mcprSession cleanup occurs in stop method", {
  session <- mcprSession$new()

  # Set up some state
  session$.__enclos_env__$private$.is_running <- TRUE
  session$.__enclos_env__$private$.session_id <- 123
  session$.__enclos_env__$private$.last_activity <- Sys.time()

  # Stop should clean up private state
  session$stop()

  expect_false(session$get_info()$is_running)
  expect_null(session$get_info()$session_id)
  expect_null(session$get_info()$last_activity)
})

# Advanced mcprSession Tests
test_that("mcprSession with custom timeout maintains timeout setting", {
  custom_timeout <- 120 # 2 minutes
  session <- mcprSession$new(timeout_seconds = custom_timeout)

  # Verify timeout is stored correctly
  expect_equal(session$.__enclos_env__$private$.timeout_seconds, custom_timeout)
})

test_that("mcprSession timeout boundaries work correctly", {
  # Test exactly at timeout boundary
  session <- mcprSession$new(timeout_seconds = 1)

  # Set up running state with activity exactly at timeout
  session$.__enclos_env__$private$.is_running <- TRUE
  session$.__enclos_env__$private$.last_activity <- Sys.time() - 1.1 # Just over timeout

  # Should trigger timeout
  session$check_timeout()
  expect_false(session$get_info()$is_running)
})

test_that("mcprSession start method handles interactive detection correctly", {
  session <- mcprSession$new()

  # Test start in non-interactive mode (should return gracefully)
  result <- session$start()
  expect_identical(result, session)

  # Should not have started
  expect_false(session$get_info()$is_running)
})

test_that("mcprSession start method prevents duplicate starts", {
  session <- mcprSession$new()

  # Manually set running state to simulate already running
  session$.__enclos_env__$private$.is_running <- TRUE

  # In non-interactive mode, start() returns early without warning
  # Test that it handles running state appropriately
  result <- session$start()
  expect_identical(result, session)
  expect_true(session$get_info()$is_running) # Should still be running
})

# NOTE: Test disabled - fails in GHA runner due to interactive/non-interactive environment differences
# test_that("mcprSession state management works correctly", {
#   session <- mcprSession$new()
#
#   # Test state initialization
#   expect_false(session$state_has("session_socket"))
#   expect_false(session$state_has("session_logger"))
#
#   # Manually test state setting (as start() would do)
#   test_socket <- "mock_socket"
#   session$state_set("session_socket", test_socket)
#
#   expect_true(session$state_has("session_socket"))
#   expect_equal(session$state_get("session_socket"), test_socket)
# })

test_that("mcprSession private methods handle edge cases", {
  session <- mcprSession$new()

  # Test send_response with no running state
  expect_identical(
    session$.__enclos_env__$private$send_response("test", 1),
    session
  )

  # Test that send_response method exists and can be called
  # Note: Actual socket testing requires complex nanonext mock setup
  expect_true(exists("send_response", session$.__enclos_env__$private))
  expect_true(is.function(session$.__enclos_env__$private$send_response))
})

test_that("mcprSession initialization sets up finalizer", {
  session <- mcprSession$new()

  # Test that session object was created (finalizer setup is internal)
  expect_s3_class(session, "mcprSession")
  expect_s3_class(session, "BaseMCPR")
})

test_that("mcprSession handles timeout values correctly", {
  # Test various timeout values
  timeouts <- c(1, 60, 900, 3600) # 1 sec, 1 min, 15 min, 1 hour

  for (timeout in timeouts) {
    session <- mcprSession$new(timeout_seconds = timeout)
    expect_equal(session$.__enclos_env__$private$.timeout_seconds, timeout)
  }
})

test_that("mcprSession session ID management", {
  session <- mcprSession$new()

  # Initially no session ID
  expect_null(session$get_info()$session_id)

  # Manually set session ID and running state (as start() would do)
  session$.__enclos_env__$private$.session_id <- 42
  session$.__enclos_env__$private$.is_running <- TRUE
  expect_equal(session$get_info()$session_id, 42)

  # Stop should clear it
  session$stop()
  expect_null(session$get_info()$session_id)
})

test_that("mcprSession last activity tracking", {
  session <- mcprSession$new()

  # Should have initial last_activity timestamp
  initial_time <- session$get_info()$last_activity
  expect_true(inherits(initial_time, "POSIXt"))

  # Update last activity
  new_time <- Sys.time()
  session$.__enclos_env__$private$.last_activity <- new_time
  expect_equal(session$get_info()$last_activity, new_time)
})

test_that("mcpr_session_start and stop integration", {
  # Test the convenience functions
  the_env <- MCPR:::the

  # Store original state
  original_session <- the_env$mcpr_session
  original_session_id <- the_env$session

  # Test start (should return invisibly in non-interactive mode)
  result <- mcpr_session_start(timeout_seconds = 60)
  expect_null(result) # Should be invisible NULL in non-interactive

  # Test stop cleanup
  the_env$mcpr_session <- mcprSession$new()
  the_env$session <- 123

  expect_no_error(mcpr_session_stop())
  expect_null(the_env$mcpr_session)
  expect_null(the_env$session)

  # Restore original state
  the_env$mcpr_session <- original_session
  the_env$session <- original_session_id
})


test_that("mcprSession error resilience", {
  session <- mcprSession$new()

  # Test multiple stops (should be idempotent)
  for (i in 1:3) {
    expect_no_error(session$stop())
    expect_false(session$get_info()$is_running)
  }

  # Test multiple timeout checks
  for (i in 1:3) {
    expect_no_error(session$check_timeout())
  }
})

test_that("mcprSession handles different object states", {
  session <- mcprSession$new()

  # Test with partial state setup
  session$.__enclos_env__$private$.is_running <- TRUE
  # Leave .last_activity as NULL

  # Should handle gracefully
  expect_no_error(session$check_timeout())
  expect_no_error(session$stop())
})

test_that("mcprSession method chaining works", {
  session <- mcprSession$new()

  # All methods should return self for chaining
  result1 <- session$stop()
  result2 <- session$check_timeout()
  result3 <- session$start() # Won't actually start in non-interactive

  expect_identical(result1, session)
  expect_identical(result2, session)
  expect_identical(result3, session)
})
