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
  environment(finalize_fn)$timeout_ms <- 100 # 100ms timeout

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
      responsive_alive <<- FALSE # Dies gracefully
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
    signal = function(sig) {
      dead_signal_called <<- TRUE
      TRUE
    },
    kill = function() {
      dead_kill_called <<- TRUE
      TRUE
    }
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

test_that("mcprClient initialize handles config file", {
  # Test with no config file
  client1 <- mcprClient$new()
  expect_s3_class(client1, "mcprClient")

  # Test with custom config path
  temp_config <- tempfile(fileext = ".json")
  writeLines('{"mcpServers": {}}', temp_config)

  client2 <- mcprClient$new(config = temp_config)
  expect_s3_class(client2, "mcprClient")

  unlink(temp_config)
})

test_that("mcprClient connect_servers handles empty config", {
  client <- mcprClient$new()

  # Should handle empty config gracefully
  expect_no_error(client$connect_servers())
})

test_that("mcprClient handles server operations", {
  client <- mcprClient$new()

  # Test getting servers (should be empty initially)
  expect_no_error(client$get_servers)
})

test_that("mcprClient initialization sets up private fields", {
  client <- mcprClient$new()

  # Check that private fields are initialized
  expect_true(exists(".servers", client$.__enclos_env__$private))
  expect_true(exists(".server_processes", client$.__enclos_env__$private))
  expect_true(exists(".config_path", client$.__enclos_env__$private))
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

test_that("mcprClient get_mcpr_tools returns NULL when no servers", {
  client <- mcprClient$new()

  # Should return NULL when no servers configured (unlist of empty list)
  tools <- client$get_mcpr_tools()
  expect_null(tools)
})

test_that("mcprClient get_server_status returns empty list when no servers", {
  client <- mcprClient$new()

  status <- client$get_server_status()
  expect_type(status, "list")
  expect_length(status, 0)
})

test_that("mcprClient get_server_status returns correct format with mock server", {
  client <- mcprClient$new()

  # Add mock server
  mock_process <- list(
    is_alive = function() TRUE
  )

  client$.__enclos_env__$private$.servers[["test_server"]] <- list(
    name = "test_server",
    process = mock_process,
    tools = list(tools = list(tool1 = list(), tool2 = list())),
    id = 5
  )

  status <- client$get_server_status()

  expect_type(status, "list")
  expect_length(status, 1)
  expect_equal(status[[1]]$name, "test_server")
  expect_true(status[[1]]$connected)
  expect_equal(status[[1]]$tools_count, 2)
  expect_equal(status[[1]]$last_id, 5)
})

test_that("mcprClient as_mcpr_types handles empty tool schema", {
  client <- mcprClient$new()

  # Test with NULL inputSchema
  tool_null <- list(inputSchema = NULL)
  result <- client$as_mcpr_types(tool_null)
  expect_type(result, "list")
  expect_length(result, 0)

  # Test with NULL properties
  tool_no_props <- list(inputSchema = list(properties = NULL))
  result2 <- client$as_mcpr_types(tool_no_props)
  expect_type(result2, "list")
  expect_length(result2, 0)
})

test_that("mcprClient as_mcpr_types converts tool schema properties", {
  client <- mcprClient$new()

  tool_with_props <- list(
    inputSchema = list(
      properties = list(
        param1 = list(type = "string", description = "A string parameter"),
        param2 = list(type = "number", description = "A number parameter")
      )
    )
  )

  result <- client$as_mcpr_types(tool_with_props)
  expect_type(result, "list")
  expect_length(result, 2)
  expect_true("param1" %in% names(result))
  expect_true("param2" %in% names(result))
})

test_that("mcprClient private methods create correct JSON-RPC messages", {
  client <- mcprClient$new()

  # Test mcp_request_initialize
  init_msg <- client$.__enclos_env__$private$mcp_request_initialize()
  expect_equal(init_msg$jsonrpc, "2.0")
  expect_equal(init_msg$id, 1)
  expect_equal(init_msg$method, "initialize")
  expect_equal(init_msg$params$protocolVersion, max(MCPR:::SUPPORTED_VERSIONS))  # Should use latest version
  expect_equal(init_msg$params$clientInfo$name, "MCPR Client")
  # Test that capabilities are extracted from helper
  expect_true("capabilities" %in% names(init_msg$params))
  expect_true("tools" %in% names(init_msg$params$capabilities))

  # Test mcp_request_tools_list
  tools_msg <- client$.__enclos_env__$private$mcp_request_tools_list()
  expect_equal(tools_msg$jsonrpc, "2.0")
  expect_equal(tools_msg$id, 2)
  expect_equal(tools_msg$method, "tools/list")

  # Test mcp_request_tool_call with arguments
  call_msg <- client$.__enclos_env__$private$mcp_request_tool_call(
    id = 3,
    tool = "test_tool",
    arguments = list(param1 = "value1", param2 = 42)
  )
  expect_equal(call_msg$jsonrpc, "2.0")
  expect_equal(call_msg$id, 3)
  expect_equal(call_msg$method, "tools/call")
  expect_equal(call_msg$params$name, "test_tool")
  expect_equal(call_msg$params$arguments$param1, "value1")
  expect_equal(call_msg$params$arguments$param2, 42)

  # Test mcp_request_tool_call without arguments
  call_msg_no_args <- client$.__enclos_env__$private$mcp_request_tool_call(
    id = 4,
    tool = "simple_tool",
    arguments = list()
  )
  expect_equal(call_msg_no_args$params$name, "simple_tool")
  expect_false("arguments" %in% names(call_msg_no_args$params))
})

test_that("mcprClient default_mcp_client_config returns correct path", {
  client <- mcprClient$new()

  config_path <- client$.__enclos_env__$private$default_mcp_client_config()
  expect_type(config_path, "character")
  expect_true(grepl("config/mcptools/config.*json$", config_path))
})

test_that("mcprClient read_mcp_config handles valid JSON", {
  client <- mcprClient$new()

  # Create valid config file
  valid_config <- tempfile(fileext = ".json")
  config_content <- '{
    "mcpServers": {
      "server1": {
        "command": "node",
        "args": ["server.js"]
      }
    }
  }'
  writeLines(config_content, valid_config)

  result <- client$.__enclos_env__$private$read_mcp_config(valid_config)
  expect_type(result, "list")
  expect_true("server1" %in% names(result))
  expect_equal(result$server1$command, "node")

  unlink(valid_config)
})

test_that("mcprClient read_mcp_config handles empty file", {
  client <- mcprClient$new()

  # Create empty config file (invalid JSON)
  empty_config <- tempfile(fileext = ".json")
  writeLines("", empty_config)

  # Empty file should trigger JSON parsing error
  expect_error(
    client$.__enclos_env__$private$read_mcp_config(empty_config),
    "Configuration processing failed"
  )

  unlink(empty_config)
})

test_that("mcprClient error_no_mcp_config throws correct error", {
  client <- mcprClient$new()

  expect_error(
    client$.__enclos_env__$private$error_no_mcp_config(),
    "The mcptools MCP client configuration file does not exist"
  )
})

test_that("mcpr_tools function creates client and gets tools", {
  # Test the exported function
  tools <- mcpr_tools(config = tempfile()) # Non-existent config
  expect_null(tools) # Should be NULL with non-existent config (same as get_mcpr_tools)
})

test_that("mcprClient server_as_mcpr_tools converts server tools correctly", {
  client <- mcprClient$new()

  # Create mock server with tools
  mock_server <- list(
    name = "test_server",
    tools = list(
      tools = list(
        list(
          name = "test_tool",
          description = "A test tool",
          inputSchema = list(
            properties = list(
              param1 = list(type = "string")
            )
          )
        )
      )
    )
  )

  result <- client$.__enclos_env__$private$server_as_mcpr_tools(mock_server)
  expect_type(result, "list")
  expect_length(result, 1)
  expect_s3_class(result[[1]], "ToolDef")
})

# Test process communication edge cases
test_that("mcprClient send_and_receive handles timeout scenarios", {
  client <- mcprClient$new()

  # Mock process that never returns output
  mock_process <- list(
    write_input = function(input) TRUE,
    read_output_lines = function() character(0) # Always returns empty
  )

  # Mock the message
  test_message <- list(jsonrpc = "2.0", id = 1, method = "test")

  result <- client$.__enclos_env__$private$send_and_receive(mock_process, test_message)
  expect_null(result)
})


test_that("mcprClient finalize handles signal errors gracefully", {
  # Test finalize with process that throws error on signal
  signal_error_process <- list(
    is_alive = function() TRUE,
    signal = function(sig) {
      stop("Signal failed")
    },
    kill = function() TRUE
  )

  client <- mcprClient$new()
  client$.__enclos_env__$private$.server_processes <- list(error_process = signal_error_process)

  # Should handle signal errors gracefully
  expect_no_error(client$.__enclos_env__$private$finalize())
})


test_that("mcprClient tool_ref handles empty arguments correctly", {
  client <- mcprClient$new()

  # Test with no arguments
  tool_func <- client$.__enclos_env__$private$tool_ref(
    server = "test_server",
    tool = "no_args_tool",
    arguments = character(0)
  )

  expect_type(tool_func, "closure")
  expect_length(formals(tool_func), 0)
})

test_that("mcprClient tool_ref handles special character arguments", {
  client <- mcprClient$new()

  # Test with arguments containing special characters
  tool_func <- client$.__enclos_env__$private$tool_ref(
    server = "test_server",
    tool = "special_tool",
    arguments = c("arg-with-dash", "arg_with_underscore", "arg.with.dots")
  )

  expect_type(tool_func, "closure")
  expect_length(formals(tool_func), 3)
})

test_that("mcprClient as_mcpr_types handles complex nested schemas", {
  client <- mcprClient$new()

  # Test with nested object schema
  complex_tool <- list(
    inputSchema = list(
      properties = list(
        simple_param = list(type = "string", description = "Simple parameter"),
        complex_param = list(
          type = "object",
          properties = list(
            nested_string = list(type = "string"),
            nested_number = list(type = "number")
          )
        ),
        array_param = list(
          type = "array",
          items = list(type = "string")
        )
      )
    )
  )

  result <- client$as_mcpr_types(complex_tool)
  expect_type(result, "list")
  expect_length(result, 3)
  expect_true("simple_param" %in% names(result))
  expect_true("complex_param" %in% names(result))
  expect_true("array_param" %in% names(result))
})

test_that("mcprClient jsonrpc_id handles missing server gracefully", {
  client <- mcprClient$new()

  # Test with non-existent server - actually it returns NULL without error
  result <- client$.__enclos_env__$private$jsonrpc_id("nonexistent_server")
  expect_null(result)
})

test_that("mcprClient get_server_status handles server with NULL tools", {
  client <- mcprClient$new()

  mock_process <- list(
    is_alive = function() TRUE
  )

  # Add server with NULL tools
  client$.__enclos_env__$private$.servers[["null_tools_server"]] <- list(
    name = "null_tools_server",
    process = mock_process,
    tools = NULL,
    id = 1
  )

  status <- client$get_server_status()
  expect_type(status, "list")
  expect_length(status, 1)
  expect_equal(status[[1]]$tools_count, 0)
})

test_that("mcprClient get_server_status handles server with empty tools list", {
  client <- mcprClient$new()

  mock_process <- list(
    is_alive = function() FALSE # Test with disconnected server
  )

  # Add server with empty tools
  client$.__enclos_env__$private$.servers[["empty_tools_server"]] <- list(
    name = "empty_tools_server",
    process = mock_process,
    tools = list(tools = list()),
    id = 10
  )

  status <- client$get_server_status()
  expect_type(status, "list")
  expect_length(status, 1)
  expect_false(status[[1]]$connected)
  expect_equal(status[[1]]$tools_count, 0)
})

test_that("mcprClient server_as_mcpr_tools handles server with no tools", {
  client <- mcprClient$new()

  # Test with server that has no tools
  empty_server <- list(
    name = "empty_server",
    tools = list(tools = list())
  )

  result <- client$.__enclos_env__$private$server_as_mcpr_tools(empty_server)
  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("mcprClient server_as_mcpr_tools handles tools without input schema", {
  client <- mcprClient$new()

  # Test with tools that have no input schema
  server_no_schema <- list(
    name = "no_schema_server",
    tools = list(
      tools = list(
        list(
          name = "simple_tool",
          description = "A simple tool with no parameters"
          # No inputSchema
        )
      )
    )
  )

  result <- client$.__enclos_env__$private$server_as_mcpr_tools(server_no_schema)
  expect_type(result, "list")
  expect_length(result, 1)
  expect_s3_class(result[[1]], "ToolDef")
})
