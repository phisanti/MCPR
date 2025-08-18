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
  expect_false(info$socket_active)  # Socket is NOT created in initialize
  expect_true(is.null(info$last_activity) || inherits(info$last_activity, "POSIXt"))
})

test_that("mcprSession accepts custom timeout", {
  custom_timeout <- 1800  # 30 minutes
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

test_that("mcp_session functional wrapper works", {
  # Should return invisibly without error in non-interactive mode
  expect_invisible(result <- mcp_session())
  
  # Clean up any session that might have been created
  mcp_session_stop()
})

test_that("mcp_session_stop is safe to call when no session exists", {
  # Should not error when called without an active session
  expect_silent(mcp_session_stop())
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