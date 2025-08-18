test_that("MessageHandler can be instantiated", {
  messenger <- MessageHandler$new()
  expect_true(inherits(messenger, "MessageHandler"))
  expect_true(inherits(messenger, "R6"))
})

test_that("MessageHandler accepts custom parameters", {
  # Test with custom logger
  log_messages <- character()
  custom_logger <- function(msg) {
    log_messages <<- c(log_messages, msg)
  }
  
  messenger <- MessageHandler$new(
    logger = custom_logger,
    timeout_seconds = 30,
    retry_delay = 0.1
  )
  
  expect_true(inherits(messenger, "MessageHandler"))
})

test_that("MessageHandler creates valid JSON-RPC responses", {
  messenger <- MessageHandler$new()
  
  # Test success response
  success_response <- messenger$create_response(id = 1, result = list(status = "ok"))
  
  expect_true(is.list(success_response))
  expect_equal(success_response$jsonrpc, "2.0")
  expect_equal(success_response$id, 1)
  expect_true("result" %in% names(success_response))
  expect_false("error" %in% names(success_response))
  
  # Test error response
  error_response <- messenger$create_response(
    id = 2, 
    error = list(code = -32601, message = "Method not found")
  )
  
  expect_equal(error_response$id, 2)
  expect_true("error" %in% names(error_response))
  expect_false("result" %in% names(error_response))
})

test_that("MessageHandler creates valid tool requests", {
  messenger <- MessageHandler$new()
  
  # Test simple tool request
  simple_request <- messenger$create_tool_request(
    id = 1, 
    tool = "test_tool",
    arguments = list()
  )
  
  expect_true(is.list(simple_request))
  expect_equal(simple_request$jsonrpc, "2.0")
  expect_equal(simple_request$id, 1)
  expect_equal(simple_request$method, "tools/call")
  expect_equal(simple_request$params$name, "test_tool")
  
  # Test tool request with arguments
  complex_request <- messenger$create_tool_request(
    id = 2,
    tool = "complex_tool",
    arguments = list(x = 42, y = "test")
  )
  
  expect_equal(complex_request$params$arguments$x, 42)
  expect_equal(complex_request$params$arguments$y, "test")
})

test_that("MessageHandler creates capabilities", {
  messenger <- MessageHandler$new()
  
  capabilities <- messenger$create_capabilities()
  expect_true(is.list(capabilities))
  expect_true("protocolVersion" %in% names(capabilities))
  expect_true("capabilities" %in% names(capabilities))
  expect_true("serverInfo" %in% names(capabilities))
})

test_that("MessageHandler creates initialization requests", {
  messenger <- MessageHandler$new()
  
  # Test default initialization request
  init_request <- messenger$create_initialize_request()
  expect_true(is.list(init_request))
  expect_equal(init_request$method, "initialize")
  
  # Test custom initialization request
  custom_init <- messenger$create_initialize_request("Test Client", "1.0.0")
  expect_true(is.list(custom_init))
  expect_equal(custom_init$method, "initialize")
})

test_that("MessageHandler outputs JSON", {
  messenger <- MessageHandler$new()
  
  test_obj <- list(message = "test", code = 200)
  
  # Should not error when outputting JSON
  expect_silent(capture.output(messenger$output_json(test_obj)))
})

test_that("MessageHandler parses messages", {
  messenger <- MessageHandler$new()
  
  # Test valid JSON message
  valid_json <- '{"jsonrpc":"2.0","id":1,"method":"test"}'
  parsed <- messenger$parse_message(valid_json)
  
  expect_true(is.list(parsed))
  expect_equal(parsed$jsonrpc, "2.0")
  expect_equal(parsed$id, 1)
  expect_equal(parsed$method, "test")
  
  # Test empty input
  empty_result <- messenger$parse_message(character(0))
  expect_null(empty_result)
  
  # Test invalid JSON (should return NULL and not error)
  invalid_result <- messenger$parse_message('{"invalid": json}')
  expect_null(invalid_result)
})

test_that("MessageHandler integrates with logging", {
  log_entries <- character()
  test_logger <- function(message) {
    log_entries <<- c(log_entries, message)
  }
  
  messenger <- MessageHandler$new(logger = test_logger)
  
  # Create a message (should trigger logging in some operations)
  request <- messenger$create_tool_request(1, "test_tool", list())
  
  # Just verify logger was set up correctly
  expect_true(inherits(messenger, "MessageHandler"))
})

test_that("MessageHandler handles edge cases", {
  messenger <- MessageHandler$new()
  
  # Test empty arguments
  empty_request <- messenger$create_tool_request(1, "tool", arguments = list())
  expect_true(is.list(empty_request))
  
  # Test special characters in messages
  special_request <- messenger$create_tool_request(
    1, 
    "tool_with_special_chars", 
    arguments = list(text = "Hello 🌍! Test: @#$%")
  )
  
  expect_equal(special_request$params$arguments$text, "Hello 🌍! Test: @#$%")
})