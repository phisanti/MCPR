# Additional edge case tests for mcpClient

test_that("mcpClient handles malformed JSON config", {
  # Create malformed JSON config
  malformed_config <- tempfile(fileext = ".json")
  writeLines('{"mcpServers": {invalid json}', malformed_config)
  
  expect_error(
    mcpClient$new(config = malformed_config),
    "Configuration processing failed"
  )
  
  # Cleanup
  unlink(malformed_config)
})

test_that("mcpClient handles config without mcpServers key", {
  # Create config without mcpServers
  invalid_config <- tempfile(fileext = ".json")
  writeLines('{"otherKey": "value"}', invalid_config)
  
  expect_error(
    mcpClient$new(config = invalid_config),
    "must have a top-level.*mcpServers"
  )
  
  # Cleanup
  unlink(invalid_config)
})

test_that("mcpClient handles tool_ref function creation", {
  client <- mcpClient$new()
  
  # Test tool_ref creation (accessing private method for testing)
  tool_func <- client$.__enclos_env__$private$tool_ref(
    server = "test_server",
    tool = "test_tool", 
    arguments = c("arg1", "arg2")
  )
  
  expect_type(tool_func, "closure")
  expect_named(formals(tool_func), c("arg1", "arg2"))
})

test_that("mcpClient jsonrpc_id increments correctly", {
  client <- mcpClient$new()
  
  # Manually add a server for testing
  client$.__enclos_env__$private$.servers[["test"]] <- list(
    name = "test",
    process = NULL,
    tools = list(),
    id = 1
  )
  
  # Test ID incrementation
  id1 <- client$.__enclos_env__$private$next_id("test")
  id2 <- client$.__enclos_env__$private$next_id("test")
  
  expect_equal(id1, 1)
  expect_equal(id2, 2)
})

