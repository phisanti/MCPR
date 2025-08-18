test_that("mcprClient finalize implements graceful shutdown", {
  # Create a mock process object that becomes dead after signal
  alive_state <- TRUE
  signal_called <- FALSE
  kill_called <- FALSE
  
  mock_process <- list(
    is_alive = function() alive_state,
    signal = function(sig) {
      signal_called <<- TRUE
      expect_equal(sig, tools::SIGTERM)
      # Simulate process responding to SIGTERM
      alive_state <<- FALSE
      TRUE
    },
    kill = function() {
      kill_called <<- TRUE
      TRUE
    }
  )
  
  # Create client instance and inject mock process
  client <- mcprClient$new()
  client$.__enclos_env__$private$.server_processes <- list(test_server = mock_process)
  
  # Call finalize
  client$.__enclos_env__$private$finalize()
  
  # Verify graceful shutdown was attempted
  expect_true(signal_called, "SIGTERM should be sent for graceful shutdown")
  expect_false(kill_called, "Kill should not be called if process responds to SIGTERM")
})

test_that("mcprClient finalize implements timeout and force kill", {
  # Create a mock process that doesn't respond to SIGTERM
  alive_state <- TRUE
  signal_called <- FALSE
  kill_called <- FALSE
  
  mock_process <- list(
    is_alive = function() alive_state,
    signal = function(sig) {
      signal_called <<- TRUE
      expect_equal(sig, tools::SIGTERM)
      # Process doesn't respond to SIGTERM (stays alive)
      TRUE
    },
    kill = function() {
      kill_called <<- TRUE
      alive_state <<- FALSE
      TRUE
    }
  )
  
  # Create client instance and inject mock process
  client <- mcprClient$new()
  client$.__enclos_env__$private$.server_processes <- list(test_server = mock_process)
  
  # Override the timeout to be very short for testing
  finalize_fn <- client$.__enclos_env__$private$finalize
  environment(finalize_fn)$timeout_ms <- 100  # 100ms timeout
  
  # Call finalize
  finalize_fn()
  
  # Verify both graceful shutdown and force kill were attempted
  expect_true(signal_called, "SIGTERM should be sent for graceful shutdown")
  expect_true(kill_called, "Kill should be called after timeout")
})

test_that("mcprClient finalize handles dead processes", {
  # Create a mock process that's already dead
  mock_process <- list(
    is_alive = function() FALSE,
    signal = function(sig) TRUE,
    kill = function() TRUE,
    signal_called = FALSE,
    kill_called = FALSE
  )
  
  # Override methods to track calls
  mock_process$signal <- function(sig) {
    mock_process$signal_called <<- TRUE
    TRUE
  }
  
  mock_process$kill <- function() {
    mock_process$kill_called <<- TRUE
    TRUE
  }
  
  # Create client instance and inject mock process
  client <- mcprClient$new()
  client$.__enclos_env__$private$.server_processes <- list(test_server = mock_process)
  
  # Call finalize
  client$.__enclos_env__$private$finalize()
  
  # Verify no shutdown attempts on dead process
  expect_false(mock_process$signal_called, "No signal should be sent to dead process")
  expect_false(mock_process$kill_called, "No kill should be called on dead process")
})

test_that("mcprClient finalize handles multiple processes", {
  # Create responsive process
  responsive_alive <- TRUE
  responsive_signal_called <- FALSE
  responsive_kill_called <- FALSE
  
  responsive_process <- list(
    is_alive = function() responsive_alive,
    signal = function(sig) {
      responsive_signal_called <<- TRUE
      responsive_alive <<- FALSE  # Dies gracefully
      TRUE
    },
    kill = function() {
      responsive_kill_called <<- TRUE
      TRUE
    }
  )
  
  # Create dead process
  dead_signal_called <- FALSE
  dead_kill_called <- FALSE
  
  dead_process <- list(
    is_alive = function() FALSE,
    signal = function(sig) { dead_signal_called <<- TRUE; TRUE },
    kill = function() { dead_kill_called <<- TRUE; TRUE }
  )
  
  # Create client instance and inject mock processes
  client <- mcprClient$new()
  client$.__enclos_env__$private$.server_processes <- list(
    responsive = responsive_process,
    dead = dead_process
  )
  
  # Call finalize
  client$.__enclos_env__$private$finalize()
  
  # Verify correct behavior for each process type
  expect_true(responsive_signal_called, "Responsive process should receive SIGTERM")
  expect_false(responsive_kill_called, "Responsive process should not be killed")
  
  expect_false(dead_signal_called, "Dead process should not receive signal")
  expect_false(dead_kill_called, "Dead process should not be killed")
})

# Additional edge case tests for mcprClient

test_that("mcprClient handles malformed JSON config", {
  # Create malformed JSON config
  malformed_config <- tempfile(fileext = ".json")
  writeLines('{"mcpServers": {invalid json}', malformed_config)

  expect_error(
    mcprClient$new(config = malformed_config),
    "Configuration processing failed"
  )

  # Cleanup
  unlink(malformed_config)
})

test_that("mcprClient handles config without mcpServers key", {
  # Create config without mcpServers
  invalid_config <- tempfile(fileext = ".json")
  writeLines('{"otherKey": "value"}', invalid_config)

  expect_error(
    mcprClient$new(config = invalid_config),
    "must have a top-level.*mcpServers"
  )

  # Cleanup
  unlink(invalid_config)
})

test_that("mcprClient handles tool_ref function creation", {
  client <- mcprClient$new()

  # Test tool_ref creation (accessing private method for testing)
  tool_func <- client$.__enclos_env__$private$tool_ref(
    server = "test_server",
    tool = "test_tool",
    arguments = c("arg1", "arg2")
  )

  expect_type(tool_func, "closure")
  expect_named(formals(tool_func), c("arg1", "arg2"))
})

test_that("mcprClient jsonrpc_id increments correctly", {
  client <- mcprClient$new()

  # Manually add a server for testing
  client$.__enclos_env__$private$.servers[["test"]] <- list(
    name = "test",
    process = NULL,
    tools = list(),
    id = 1
  )

  # Test ID incrementation
  id1 <- client$.__enclos_env__$private$jsonrpc_id("test")
  id2 <- client$.__enclos_env__$private$jsonrpc_id("test")

  expect_equal(id1, 1)
  expect_equal(id2, 2)
})