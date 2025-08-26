# Formal Tests for BaseMCPR Class
# Tests for the BaseMCPR foundation class providing shared functionality
# across MCPR components including state management, logging, and cleanup

test_that("BaseMCPR initialization requires component name", {
  base <- BaseMCPR$new()

  # Should fail without component name
  expect_error(
    base$initialize_base(),
    "component_name must be a single character string"
  )

  # Should fail with invalid component name
  expect_error(
    base$initialize_base(c("TEST1", "TEST2")),
    "component_name must be a single character string"
  )

  expect_error(
    base$initialize_base(123),
    "component_name must be a single character string"
  )
})

test_that("BaseMCPR initialization works correctly", {
  base <- BaseMCPR$new()
  base$initialize_base("TEST")

  expect_true(base$is_initialized())
  expect_equal(base$get_component_name(), "TEST")
  expect_s3_class(base$get_logger(), "MCPRLogger")
})

test_that("BaseMCPR prevents access before initialization", {
  base <- BaseMCPR$new()

  expect_error(
    base$state_get("test"),
    "BaseMCPR not initialized"
  )

  expect_error(
    base$state_set("test", "value"),
    "BaseMCPR not initialized"
  )

  expect_error(
    base$log_info("test"),
    "BaseMCPR not initialized"
  )
})

test_that("state management functions work correctly", {
  base <- BaseMCPR$new()
  base$initialize_base("TEST")

  # Test state_get with default
  expect_equal(base$state_get("nonexistent", "default"), "default")
  expect_null(base$state_get("nonexistent"))

  # Test state_set and state_get
  base$state_set("test_key", "test_value")
  expect_equal(base$state_get("test_key"), "test_value")

  # Test state_has
  expect_true(base$state_has("test_key"))
  expect_false(base$state_has("missing_key"))

  # Test state ownership tracking
  owned_keys <- base$state_keys_owned()
  expect_true("test_key" %in% owned_keys)
  expect_equal(length(owned_keys), 1)

  # Test state_clear
  base$state_clear("test_key")
  expect_false(base$state_has("test_key"))
  expect_equal(length(base$state_keys_owned()), 0)
})

test_that("state ownership is tracked separately by instances", {
  base1 <- BaseMCPR$new()
  base1$initialize_base("TEST1")

  base2 <- BaseMCPR$new()
  base2$initialize_base("TEST2")

  # Set different keys in each instance
  base1$state_set("key1", "value1")
  base2$state_set("key2", "value2")

  # Check ownership tracking
  keys1 <- base1$state_keys_owned()
  keys2 <- base2$state_keys_owned()

  expect_true("key1" %in% keys1)
  expect_false("key2" %in% keys1)
  expect_true("key2" %in% keys2)
  expect_false("key1" %in% keys2)

  # Both should see all global state (shared environment)
  expect_equal(base1$state_get("key1"), "value1")
  expect_equal(base1$state_get("key2"), "value2")
  expect_equal(base2$state_get("key1"), "value1")
  expect_equal(base2$state_get("key2"), "value2")

  # Cleanup
  base1$cleanup_all()
  base2$cleanup_all()
})

test_that("logging methods work correctly", {
  base <- BaseMCPR$new()
  base$initialize_base("TEST")

  # All logging methods should return self for chaining
  expect_identical(base$log_info("test"), base)
  expect_identical(base$log_warn("test"), base)
  expect_identical(base$log_error("test"), base)
  expect_identical(base$log_debug("test"), base)
  expect_identical(base$log_comm("test"), base)

  # Logger should be properly configured
  logger <- base$get_logger()
  expect_s3_class(logger, "MCPRLogger")
})

test_that("resource cleanup registration works", {
  base <- BaseMCPR$new()
  base$initialize_base("TEST")

  # Test resource cleanup
  test_resource <- list(active = TRUE)

  base$register_cleanup(
    function() {
      test_resource$active <<- FALSE
    },
    "test_resource"
  )

  expect_true(test_resource$active)

  # Execute cleanup
  base$cleanup_all()
  expect_false(test_resource$active)
})

test_that("resource cleanup validation works", {
  base <- BaseMCPR$new()
  base$initialize_base("TEST")

  # Should reject non-function cleanup
  expect_error(
    base$register_cleanup("not_a_function", "test"),
    "cleanup_fn must be a function"
  )
})

test_that("resource cleanup is executed in LIFO order", {
  base <- BaseMCPR$new()
  base$initialize_base("TEST")

  execution_order <- character(0)

  # Register cleanup functions
  base$register_cleanup(
    function() {
      execution_order <<- c(execution_order, "first")
    },
    "first"
  )

  base$register_cleanup(
    function() {
      execution_order <<- c(execution_order, "second")
    },
    "second"
  )

  base$register_cleanup(
    function() {
      execution_order <<- c(execution_order, "third")
    },
    "third"
  )

  # Execute cleanup
  base$cleanup_all()

  # Should execute in reverse order (LIFO)
  expect_equal(execution_order, c("third", "second", "first"))
})

test_that("socket utilities work correctly", {
  base <- BaseMCPR$new()
  base$initialize_base("TEST")

  # Set socket URL
  base$state_set("socket_url", "ipc:///tmp/test_")

  # Test socket URL generation
  url1 <- base$socket_url(1)
  url2 <- base$socket_url(42)

  expect_equal(url1, "ipc:///tmp/test_1")
  expect_equal(url2, "ipc:///tmp/test_42")

  # Test with default socket URL when not set
  base$state_clear("socket_url")
  url_default <- base$socket_url(1)
  expect_equal(url_default, "ipc:///tmp/mcpr_1")
})

test_that("socket creation works with cleanup", {
  skip_if_not_installed("nanonext")

  base <- BaseMCPR$new()
  base$initialize_base("TEST")

  # Create socket
  socket <- base$create_socket("poly", "test_socket")

  # Should be a nanonext socket
  expect_true(inherits(socket, "nanoSocket"))

  # Cleanup should work without errors
  expect_no_error(base$cleanup_all())
})

test_that("inheritance pattern works correctly", {
  # Create mock class that inherits from BaseMCPR
  MockComponent <- R6::R6Class("MockComponent",
    inherit = BaseMCPR,
    public = list(
      initialize = function() {
        self$initialize_base("MOCK")
        private$.mock_data <- "initialized"
      },
      get_mock_data = function() {
        private$.mock_data
      },
      do_something_with_state = function() {
        self$state_set("mock_state", "mock_value")
        self$state_get("mock_state")
      }
    ),
    private = list(
      .mock_data = NULL
    )
  )

  # Test inheritance
  mock <- MockComponent$new()

  expect_equal(mock$get_component_name(), "MOCK")
  expect_true(mock$is_initialized())
  expect_equal(mock$get_mock_data(), "initialized")

  # Test inherited state functionality
  result <- mock$do_something_with_state()
  expect_equal(result, "mock_value")
  expect_true("mock_state" %in% mock$state_keys_owned())

  # Cleanup
  mock$cleanup_all()
})

test_that("cleanup_all is safe to call multiple times", {
  base <- BaseMCPR$new()
  base$initialize_base("TEST")

  # Set some state and register cleanup
  base$state_set("test_key", "test_value")
  test_resource <- list(active = TRUE)
  base$register_cleanup(
    function() {
      test_resource$active <<- FALSE
    },
    "test_resource"
  )

  # First cleanup
  base$cleanup_all()
  expect_false(test_resource$active)
  expect_equal(length(base$state_keys_owned()), 0)

  # Second cleanup should not error
  expect_no_error(base$cleanup_all())
})

test_that("cleanup handles errors gracefully", {
  base <- BaseMCPR$new()
  base$initialize_base("TEST")

  # Register cleanup that will error
  base$register_cleanup(
    function() {
      stop("cleanup error")
    },
    "error_cleanup"
  )

  # Register normal cleanup
  test_resource <- list(active = TRUE)
  base$register_cleanup(
    function() {
      test_resource$active <<- FALSE
    },
    "normal_cleanup"
  )

  # Cleanup should not stop on errors
  expect_no_error(base$cleanup_all())

  # Normal cleanup should still execute
  expect_false(test_resource$active)
})

test_that("state_clear handles missing keys gracefully", {
  base <- BaseMCPR$new()
  base$initialize_base("TEST")

  # Clearing non-existent key should not error
  expect_no_error(base$state_clear("nonexistent_key"))

  # Should return self for chaining
  expect_identical(base$state_clear("nonexistent_key"), base)
})
