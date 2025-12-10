test_that("create_tool_request creates valid JSON-RPC requests", {
  # Test with no arguments
  request1 <- MCPR:::create_tool_request(id = 1, tool = "test_tool")

  expect_true(is.list(request1))
  expect_equal(request1$jsonrpc, "2.0")
  expect_equal(request1$id, 1)
  expect_equal(request1$method, "tools/call")
  expect_equal(request1$params$name, "test_tool")
  expect_false("arguments" %in% names(request1$params))

  # Test with arguments
  args <- list(x = 42, y = "test")
  request2 <- MCPR:::create_tool_request(id = 2, tool = "complex_tool", arguments = args)

  expect_equal(request2$id, 2)
  expect_equal(request2$params$name, "complex_tool")
  expect_true("arguments" %in% names(request2$params))
  expect_equal(request2$params$arguments, args)
})

test_that("jsonrpc_response creates valid success responses", {
  # Test successful response
  result_data <- list(output = "success", value = 42)
  response <- MCPR:::jsonrpc_response(id = 123, result = result_data)

  expect_true(is.list(response))
  expect_equal(response$jsonrpc, "2.0")
  expect_equal(response$id, 123)
  expect_true("result" %in% names(response))
  expect_false("error" %in% names(response))
  expect_equal(response$result, result_data)
})

test_that("jsonrpc_response creates valid error responses", {
  # Test error response
  error_info <- list(code = -32601, message = "Method not found")
  response <- MCPR:::jsonrpc_response(id = 456, error = error_info)

  expect_true(is.list(response))
  expect_equal(response$jsonrpc, "2.0")
  expect_equal(response$id, 456)
  expect_true("error" %in% names(response))
  expect_false("result" %in% names(response))
  expect_equal(response$error, error_info)
})

test_that("create_capabilities returns version-specific MCP capabilities", {
  # Test with specific version (using internal function)
  caps_old <- MCPR:::create_capabilities("2024-11-05")
  expect_true(is.list(caps_old))
  expect_equal(caps_old$protocolVersion, "2024-11-05")
  expect_true("capabilities" %in% names(caps_old))
  expect_true("serverInfo" %in% names(caps_old))
  expect_true("instructions" %in% names(caps_old))

  # Test with latest version
  caps_new <- MCPR:::create_capabilities("2025-11-25")
  expect_equal(caps_new$protocolVersion, "2025-11-25")

  # Test default server info
  expect_equal(caps_old$serverInfo$name, "R MCPR server")
  expect_equal(caps_old$serverInfo$version, "1.0.0")

  # Test custom server info
  caps_custom <- MCPR:::create_capabilities("2024-11-05", server_name = "Custom Server", server_version = "2.0.0")
  expect_equal(caps_custom$serverInfo$name, "Custom Server")
  expect_equal(caps_custom$serverInfo$version, "2.0.0")

  # Test that unsupported version throws error
  expect_error(
    MCPR:::create_capabilities("2099-99-99"),
    "Unsupported protocol version"
  )
})

test_that("negotiate_protocol_version handles exact matches", {
  # Test exact version match (using internal function)
  result <- MCPR:::negotiate_protocol_version("2024-11-05")
  expect_equal(result, "2024-11-05")

  result <- MCPR:::negotiate_protocol_version("2025-11-25")
  expect_equal(result, "2025-11-25")
})

test_that("negotiate_protocol_version handles NULL client version", {
  # Should default to oldest version for backward compatibility
  expect_warning(
    result <- MCPR:::negotiate_protocol_version(NULL),
    "did not specify protocolVersion"
  )
  expect_equal(result, "2024-11-05")
})

test_that("negotiate_protocol_version handles newer client versions", {
  # Client requests future version - should downgrade to max supported
  result <- MCPR:::negotiate_protocol_version("2099-12-31")
  expect_equal(result, "2025-11-25")  # Latest supported version
})

test_that("negotiate_protocol_version handles older client versions", {
  # Client requests version older than our minimum - should use minimum
  expect_warning(
    result <- MCPR:::negotiate_protocol_version("2020-01-01"),
    "minimum supported version"
  )
  expect_equal(result, "2024-11-05")  # Oldest supported version
})

test_that("negotiate_protocol_version handles intermediate versions", {
  # Client requests version between our supported versions
  result <- MCPR:::negotiate_protocol_version("2025-10-01")  # Between 2025-06-18 and 2025-11-25
  expect_equal(result, "2025-06-18")  # Should use highest version <= client version
})

test_that("create_client_capabilities returns valid client capabilities", {
  caps <- MCPR:::create_client_capabilities("2025-11-25")
  expect_true(is.list(caps))
  expect_true("tools" %in% names(caps))
  expect_equal(caps$tools$listChanged, FALSE)
})

test_that("jsonrpc_response validates mutual exclusivity", {
  # Should warn if both result and error provided
  expect_warning(
    MCPR:::jsonrpc_response(id = 1, result = "success", error = list(code = -1)),
    "Either.*result.*or.*error.*must be provided"
  )

  # Should warn if neither provided (both NULL)
  expect_warning(
    MCPR:::jsonrpc_response(id = 1, result = NULL, error = NULL),
    "Either.*result.*or.*error.*must be provided"
  )
})

test_that("convert_json_types handles complex structures", {
  # Test with nested structure
  complex_args <- list(
    simple = "text",
    nested = list(inner = 123),
    array = c(1, 2, 3)
  )

  result <- MCPR:::convert_json_types(complex_args)
  expect_equal(result$simple, "text")
  expect_equal(result$nested$inner, 123)
  expect_equal(result$array, c(1, 2, 3))
})

test_that("create_initialize_request creates proper initialization", {
  # Test with default parameters
  init_req1 <- MCPR:::create_initialize_request()

  expect_equal(init_req1$jsonrpc, "2.0")
  expect_equal(init_req1$id, 1)
  expect_equal(init_req1$method, "initialize")
  expect_equal(init_req1$params$protocolVersion, max(MCPR:::SUPPORTED_VERSIONS))  # Should use latest version
  expect_equal(init_req1$params$clientInfo$name, "MCP Test Client")
  expect_equal(init_req1$params$clientInfo$version, "0.1.0")

  # Test with custom parameters
  init_req2 <- MCPR:::create_initialize_request("Custom Client", "2.0.0")
  expect_equal(init_req2$params$clientInfo$name, "Custom Client")
  expect_equal(init_req2$params$clientInfo$version, "2.0.0")
})

test_that("create_tools_list_request creates tool discovery request", {
  # Test with default ID
  tools_req1 <- MCPR:::create_tools_list_request()

  expect_equal(tools_req1$jsonrpc, "2.0")
  expect_equal(tools_req1$id, 2)
  expect_equal(tools_req1$method, "tools/list")
  expect_true(is.null(tools_req1$params))

  # Test with custom ID
  tools_req2 <- MCPR:::create_tools_list_request(id = 99)
  expect_equal(tools_req2$id, 99)
})

test_that("cat_json outputs JSON to stdout", {
  # Test that cat_json doesn't error
  test_obj <- list(message = "test", code = 200)
  expect_no_error(MCPR:::cat_json(test_obj))
})

test_that("jsonrpc_response handles NULL id", {
  # Test with NULL id (notification responses)
  response <- MCPR:::jsonrpc_response(id = NULL, result = "OK")

  expect_equal(response$jsonrpc, "2.0")
  expect_null(response$id)
  expect_equal(response$result, "OK")
})

test_that("to_json produces valid JSON strings", {
  # Test simple object
  simple_obj <- list(name = "test", value = 42, active = TRUE)
  json_str <- MCPR:::to_json(simple_obj)

  expect_true(is.character(json_str))
  expect_true(length(json_str) == 1)

  # Should be valid JSON (can be parsed back)
  parsed <- jsonlite::fromJSON(json_str)
  expect_equal(parsed$name, "test")
  expect_equal(parsed$value, 42)
  expect_equal(parsed$active, TRUE)
})

test_that("to_json handles complex R objects", {
  # Test with nested structures
  complex_obj <- list(
    metadata = list(
      version = "1.0",
      author = "test"
    ),
    data = list(
      numbers = c(1, 2, 3),
      labels = c("a", "b", "c")
    ),
    settings = list(
      enabled = TRUE,
      count = 10
    )
  )

  json_str <- MCPR:::to_json(complex_obj)
  expect_true(is.character(json_str))

  # Verify it can be parsed back
  parsed <- jsonlite::fromJSON(json_str)
  expect_equal(parsed$metadata$version, "1.0")
  expect_equal(length(parsed$data$numbers), 3)
})

test_that("to_json handles special values", {
  # Test with NULL, NA, etc.
  special_obj <- list(
    null_val = NULL,
    na_val = NA,
    inf_val = Inf,
    string_val = "test"
  )

  json_str <- MCPR:::to_json(special_obj)
  expect_true(is.character(json_str))
  expect_true(nchar(json_str) > 0)
})

test_that("cat_json outputs JSON to stdout", {
  # Since cat_json writes to stdout, we can't easily test the output
  # But we can test that it doesn't error
  test_obj <- list(message = "test", code = 200)

  expect_silent(capture.output(MCPR:::cat_json(test_obj)))
})

test_that("JSON-RPC error codes follow standard", {
  # Test standard JSON-RPC error codes
  parse_error <- MCPR:::jsonrpc_response(
    id = 1,
    error = list(code = -32700, message = "Parse error")
  )

  invalid_request <- MCPR:::jsonrpc_response(
    id = 2,
    error = list(code = -32600, message = "Invalid Request")
  )

  method_not_found <- MCPR:::jsonrpc_response(
    id = 3,
    error = list(code = -32601, message = "Method not found")
  )

  expect_equal(parse_error$error$code, -32700)
  expect_equal(invalid_request$error$code, -32600)
  expect_equal(method_not_found$error$code, -32601)
})

test_that("protocol functions handle edge cases", {
  # Test empty tool name
  empty_request <- MCPR:::create_tool_request(id = 1, tool = "")
  expect_equal(empty_request$params$name, "")

  # Test with complex arguments
  complex_args <- list(
    nested = list(
      deep = list(
        value = 42
      )
    ),
    array = c(1, 2, 3, 4, 5)
  )

  complex_request <- MCPR:::create_tool_request(
    id = 999,
    tool = "complex_tool",
    arguments = complex_args
  )

  expect_equal(complex_request$params$arguments$nested$deep$value, 42)
  expect_equal(length(complex_request$params$arguments$array), 5)
})
