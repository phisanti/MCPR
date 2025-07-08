test_that("mcpServer initializes correctly", {
  server <- mcpServer$new()
  expect_true(inherits(server, "mcpServer"),
    info = "mcpServer$new() should return a mcpServer object"
  )
  expect_false(server$is_running(), "Server should not be running after initialization")
})

test_that("mcpServer accepts different tool configurations on initialization", {
  # Test 1: Initialization with NULL tools (default)
  expect_no_error(mcpServer$new(tools = NULL))

  # Test 2: Initialization with an empty list of tools
  expect_no_error(mcpServer$new(tools = list()))

  # Test 3: Initialization with a path to a valid tool file
  tool_file <- tempfile(fileext = ".R")
  # The file should return a list of tools. An empty list is sufficient for this test.
  writeLines("list()", tool_file)
  on.exit(unlink(tool_file), add = TRUE)
  expect_no_error(mcpServer$new(tools = tool_file))
})

test_that("mcpServer$stop sets the running flag to FALSE", {
  server <- mcpServer$new()

  # To properly test the stop() method, we first need to simulate a "running" state.
  # We do this by directly manipulating the private .running field for this test.
  server$.__enclos_env__$private$.running <- TRUE
  expect_true(server$is_running(), "Server should be in a running state for the test")

  server$stop()
  expect_false(server$is_running(), "stop() should set the server's running state to FALSE")
})

test_that("mcpServer accepts ToolRegistry", {
  # Create a minimal ToolRegistry instance
  registry <- ToolRegistry$new()
  
  # Test that server accepts registry parameter
  expect_no_error(mcpServer$new(registry = registry))
  
  server <- mcpServer$new(registry = registry)
  expect_true(inherits(server, "mcpServer"))
})

test_that("mcpServer rejects invalid registry parameter", {
  # Test that server rejects non-ToolRegistry objects
  expect_error(mcpServer$new(registry = "not_a_registry"),
               "registry must be a ToolRegistry instance")
  
  expect_error(mcpServer$new(registry = list()),
               "registry must be a ToolRegistry instance")
})

test_that("ToolRegistry takes precedence over tools parameter", {
  # Create a temporary tool file
  tool_file <- tempfile(fileext = ".R")
  writeLines("list()", tool_file)
  on.exit(unlink(tool_file), add = TRUE)
  
  # Create a registry
  registry <- ToolRegistry$new()
  
  # When both are provided, registry should take precedence
  expect_no_error(mcpServer$new(tools = tool_file, registry = registry))
})

test_that("mcp_server convenience function creates and returns a server instance", {
  # The mcp_server() function is a wrapper that calls the blocking `start()` method.
  # To test the initialization part of the function without blocking the test suite,
  # we temporarily override the start method with a mock version that returns immediately.

  original_start <- mcpServer$public_methods$start
  mcpServer$public_methods$start <- function() {
    # This is a mock start that does not block and simulates a running server
    private$.running <- TRUE
    invisible(self)
  }
  on.exit({
    # Ensure the original method is restored even if the test fails
    mcpServer$public_methods$start <- original_start
  })

  # Test with default tools
  server_instance_default <- mcp_server()
  expect_s3_class(server_instance_default, "mcpServer")
  expect_true(server_instance_default$is_running(), "Server started via convenience function should be running")

  # Test with a list of tools
  server_instance_list <- mcp_server(tools = list())
  expect_s3_class(server_instance_list, "mcpServer")
})
