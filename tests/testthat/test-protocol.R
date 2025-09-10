test_that("capabilities returns proper MCP server capabilities", {
  caps <- capabilities()

  expect_true(is.list(caps))
  expect_true("protocolVersion" %in% names(caps))
  expect_true("capabilities" %in% names(caps))
  expect_true("serverInfo" %in% names(caps))
  expect_true("instructions" %in% names(caps))

  # Check protocol version
  expect_equal(caps$protocolVersion, "2024-11-05")

  # Check server info structure
  expect_true(is.list(caps$serverInfo))
  expect_true("name" %in% names(caps$serverInfo))
  expect_true("version" %in% names(caps$serverInfo))

  # Check capabilities structure
  expect_true(is.list(caps$capabilities))
  expect_true("tools" %in% names(caps$capabilities))
  expect_true("prompts" %in% names(caps$capabilities))
  expect_true("resources" %in% names(caps$capabilities))
})

test_that("create_tool_request creates valid JSON-RPC requests", {
  # Test with no arguments
  request1 <- create_tool_request(id = 1, tool = "test_tool")

  expect_true(is.list(request1))
  expect_equal(request1$jsonrpc, "2.0")
  expect_equal(request1$id, 1)
  expect_equal(request1$method, "tools/call")
  expect_equal(request1$params$name, "test_tool")
  expect_false("arguments" %in% names(request1$params))

  # Test with arguments
  args <- list(x = 42, y = "test")
  request2 <- create_tool_request(id = 2, tool = "complex_tool", arguments = args)

  expect_equal(request2$id, 2)
  expect_equal(request2$params$name, "complex_tool")
  expect_true("arguments" %in% names(request2$params))
  expect_equal(request2$params$arguments, args)
})

test_that("jsonrpc_response creates valid success responses", {
  # Test successful response
  result_data <- list(output = "success", value = 42)
  response <- jsonrpc_response(id = 123, result = result_data)

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
  response <- jsonrpc_response(id = 456, error = error_info)

  expect_true(is.list(response))
  expect_equal(response$jsonrpc, "2.0")
  expect_equal(response$id, 456)
  expect_true("error" %in% names(response))
  expect_false("result" %in% names(response))
  expect_equal(response$error, error_info)
})

test_that("create_capabilities returns MCP capabilities", {
  caps <- create_capabilities()
  
  expect_true(is.list(caps))
  expect_equal(caps$protocolVersion, "2024-11-05")
  expect_true("capabilities" %in% names(caps))
  expect_true("serverInfo" %in% names(caps))
  expect_true("instructions" %in% names(caps))
  
  # Should be similar to capabilities() function
  expect_equal(caps$serverInfo$name, "R mcptools server")
  expect_equal(caps$serverInfo$version, "0.0.1")
})

test_that("jsonrpc_response validates mutual exclusivity", {
  # Should warn if both result and error provided
  expect_warning(
    jsonrpc_response(id = 1, result = "success", error = list(code = -1)),
    "Either.*result.*or.*error.*must be provided"
  )
  
  # Should warn if neither provided (both NULL)
  expect_warning(
    jsonrpc_response(id = 1, result = NULL, error = NULL),
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
  
  result <- convert_json_types(complex_args)
  expect_equal(result$simple, "text")
  expect_equal(result$nested$inner, 123)
  expect_equal(result$array, c(1, 2, 3))
})

test_that("create_initialize_request creates proper initialization", {
  # Test with default parameters
  init_req1 <- create_initialize_request()
  
  expect_equal(init_req1$jsonrpc, "2.0")
  expect_equal(init_req1$id, 1)
  expect_equal(init_req1$method, "initialize")
  expect_equal(init_req1$params$protocolVersion, "2024-11-05")
  expect_equal(init_req1$params$clientInfo$name, "MCP Test Client")
  expect_equal(init_req1$params$clientInfo$version, "0.1.0")
  
  # Test with custom parameters
  init_req2 <- create_initialize_request("Custom Client", "2.0.0")
  expect_equal(init_req2$params$clientInfo$name, "Custom Client")
  expect_equal(init_req2$params$clientInfo$version, "2.0.0")
})

test_that("create_tools_list_request creates tool discovery request", {
  # Test with default ID
  tools_req1 <- create_tools_list_request()
  
  expect_equal(tools_req1$jsonrpc, "2.0")
  expect_equal(tools_req1$id, 2)
  expect_equal(tools_req1$method, "tools/list")
  expect_true(is.null(tools_req1$params))
  
  # Test with custom ID
  tools_req2 <- create_tools_list_request(id = 99)
  expect_equal(tools_req2$id, 99)
})

test_that("cat_json outputs JSON to stdout", {
  # Test that cat_json doesn't error
  test_obj <- list(message = "test", code = 200)
  expect_no_error(cat_json(test_obj))
})

test_that("jsonrpc_response handles NULL id", {
  # Test with NULL id (notification responses)
  response <- jsonrpc_response(id = NULL, result = "OK")

  expect_equal(response$jsonrpc, "2.0")
  expect_null(response$id)
  expect_equal(response$result, "OK")
})

test_that("to_json produces valid JSON strings", {
  # Test simple object
  simple_obj <- list(name = "test", value = 42, active = TRUE)
  json_str <- to_json(simple_obj)

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

  json_str <- to_json(complex_obj)
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

  json_str <- to_json(special_obj)
  expect_true(is.character(json_str))
  expect_true(nchar(json_str) > 0)
})

test_that("cat_json outputs JSON to stdout", {
  # Since cat_json writes to stdout, we can't easily test the output
  # But we can test that it doesn't error
  test_obj <- list(message = "test", code = 200)

  expect_silent(capture.output(cat_json(test_obj)))
})

test_that("JSON-RPC error codes follow standard", {
  # Test standard JSON-RPC error codes
  parse_error <- jsonrpc_response(
    id = 1,
    error = list(code = -32700, message = "Parse error")
  )

  invalid_request <- jsonrpc_response(
    id = 2,
    error = list(code = -32600, message = "Invalid Request")
  )

  method_not_found <- jsonrpc_response(
    id = 3,
    error = list(code = -32601, message = "Method not found")
  )

  expect_equal(parse_error$error$code, -32700)
  expect_equal(invalid_request$error$code, -32600)
  expect_equal(method_not_found$error$code, -32601)
})

test_that("protocol functions handle edge cases", {
  # Test empty tool name
  empty_request <- create_tool_request(id = 1, tool = "")
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

  complex_request <- create_tool_request(
    id = 999,
    tool = "complex_tool",
    arguments = complex_args
  )

  expect_equal(complex_request$params$arguments$nested$deep$value, 42)
  expect_equal(length(complex_request$params$arguments$array), 5)
})
