test_that("set_server_tools accepts valid ToolRegistry", {
  # Test with valid ToolRegistry
  registry <- ToolRegistry$new()
  expect_silent(MCPR:::set_server_tools(registry = registry))

  # Verify tools were set
  tools <- MCPR:::get_mcptools_tools()
  expect_true(is.list(tools))
})

test_that("set_server_tools validates ToolRegistry parameter", {
  # Test with invalid registry parameter
  expect_error(
    MCPR:::set_server_tools(registry = "not_a_registry"),
    "registry must be a ToolRegistry instance"
  )

  expect_error(
    MCPR:::set_server_tools(registry = list()),
    "registry must be a ToolRegistry instance"
  )

  expect_error(
    MCPR:::set_server_tools(registry = 123),
    "registry must be a ToolRegistry instance"
  )
})

test_that("set_server_tools handles NULL registry", {
  # Test with NULL registry (should set empty tools list)
  expect_silent(MCPR:::set_server_tools(registry = NULL))

  # Verify empty tools list was set
  tools <- MCPR:::get_mcptools_tools()
  expect_true(is.list(tools))
  expect_equal(length(tools), 0)
})

test_that("get_mcptools_tools returns current server tools", {
  # Set up a registry with some tools
  temp_dir <- tempfile("tools_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create a simple tool file
  tool_file <- file.path(temp_dir, "tool-test.R")
  writeLines(c(
    "#' Test Tool",
    "#' @description A simple test tool",
    "#' @param x numeric Input value",
    "#' @keywords mcpr_tool",
    "test_tool <- function(x) x * 2"
  ), tool_file)

  registry <- ToolRegistry$new(tools_dir = temp_dir)
  registry$search_tools()
  MCPR:::set_server_tools(registry = registry)

  tools <- MCPR:::get_mcptools_tools()
  expect_true(is.list(tools))
  expect_true("test_tool" %in% names(tools))
})

test_that("get_mcptools_tools_as_json returns JSON-compatible format", {
  # Set up a registry
  registry <- ToolRegistry$new()
  MCPR:::set_server_tools(registry = registry)

  server <- mcprServer$new(registry = registry)
  json_tools <- server$get_tools("json")
  expect_true(is.list(json_tools))

  # Each tool should be JSON-compatible
  if (length(json_tools) > 0) {
    tool <- json_tools[[1]]
    expect_true(is.list(tool))
    # JSON tools should have MCP-compatible structure
    expect_true(any(c("name", "description") %in% names(tool)))
  }
})

test_that("server tools integration with global state", {
  # Test that tools are stored in global state
  registry <- ToolRegistry$new()
  MCPR:::set_server_tools(registry = registry)

  # Check that global state was updated
  expect_true(exists("server_tools", envir = MCPR:::the))
  expect_true(is.list(MCPR:::the$server_tools))
})

test_that("set_server_tools preserves existing registry tools", {
  # Create a registry with tools
  temp_dir <- tempfile("tools_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create multiple tool files
  tool_file1 <- file.path(temp_dir, "tool-first.R")
  writeLines(c(
    "#' First Tool",
    "#' @description First test tool",
    "#' @param x numeric Input",
    "#' @keywords mcpr_tool",
    "first_tool <- function(x) x + 1"
  ), tool_file1)

  tool_file2 <- file.path(temp_dir, "tool-second.R")
  writeLines(c(
    "#' Second Tool",
    "#' @description Second test tool",
    "#' @param y numeric Input",
    "#' @keywords mcpr_tool",
    "second_tool <- function(y) y * 2"
  ), tool_file2)

  registry <- ToolRegistry$new(tools_dir = temp_dir)
  registry$search_tools()
  MCPR:::set_server_tools(registry = registry)

  tools <- MCPR:::get_mcptools_tools()
  expect_true("first_tool" %in% names(tools))
  expect_true("second_tool" %in% names(tools))
  expect_equal(length(tools), 2)
})

test_that("get_mcptools_tools handles empty tools list", {
  # Set empty tools
  MCPR:::set_server_tools(registry = NULL)

  tools <- MCPR:::get_mcptools_tools()
  expect_true(is.list(tools))
  expect_equal(length(tools), 0)
  expect_equal(names(tools), character(0))
})

test_that("tool functions can be executed from server tools", {
  # Create a registry with an executable tool
  temp_dir <- tempfile("tools_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  tool_file <- file.path(temp_dir, "tool-executable.R")
  writeLines(c(
    "#' Executable Tool",
    "#' @description Tool that can be executed",
    "#' @param value numeric Input value",
    "#' @keywords mcpr_tool",
    "executable_tool <- function(value) value^2"
  ), tool_file)

  registry <- ToolRegistry$new(tools_dir = temp_dir)
  registry$search_tools()
  MCPR:::set_server_tools(registry = registry)

  tools <- MCPR:::get_mcptools_tools()
  tool_obj <- tools[["executable_tool"]]

  expect_true(inherits(tool_obj, "ToolDef"))
  expect_true(is.function(tool_obj$fun))

  # Test that the tool function works
  result <- tool_obj$fun(4)
  expect_equal(result, 16)
})

test_that("tool_as_json converts ToolDef to MCP format", {
  # Create a simple ToolDef
  test_tool <- tool(
    function(x) x * 2,
    name = "double_value",
    description = "Doubles the input value",
    arguments = list(x = "number")
  )

  json_result <- MCPR:::tool_as_json(test_tool)

  expect_type(json_result, "list")
  expect_equal(json_result$name, "double_value")
  expect_equal(json_result$description, "Doubles the input value")
  expect_true("inputSchema" %in% names(json_result))
  expect_true(is.list(json_result$inputSchema))
})

test_that("tool_as_json handles complex argument types", {
  complex_tool <- tool(
    function(data, threshold = 0.5) data > threshold,
    name = "threshold_filter",
    description = "Filters data by threshold",
    arguments = list(
      data = "array",
      threshold = "number"
    )
  )

  json_result <- MCPR:::tool_as_json(complex_tool)

  expect_type(json_result, "list")
  expect_true("inputSchema" %in% names(json_result))
  schema <- json_result$inputSchema
  expect_true("properties" %in% names(schema))
})

test_that("set_server_tools handles concurrent access", {
  # Test multiple calls to set_server_tools
  registry1 <- ToolRegistry$new()
  registry2 <- ToolRegistry$new()

  expect_no_error(MCPR:::set_server_tools(registry1))
  expect_no_error(MCPR:::set_server_tools(registry2))
  expect_no_error(MCPR:::set_server_tools(NULL))
})

test_that("get_mcptools_tools preserves tool names correctly", {
  # Create tools with specific names
  temp_dir <- tempfile("tools_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  tool_file <- file.path(temp_dir, "tool-named.R")
  writeLines(c(
    "#' Named Tool",
    "#' @description Tool with specific name",
    "#' @param input any Input value",
    "#' @keywords mcpr_tool",
    "specific_name_tool <- function(input) input"
  ), tool_file)

  registry <- ToolRegistry$new(tools_dir = temp_dir)
  registry$search_tools()
  MCPR:::set_server_tools(registry)

  tools <- MCPR:::get_mcptools_tools()
  tool_names <- names(tools)

  expect_true("specific_name_tool" %in% tool_names)
  expect_true(all(nchar(tool_names) > 0))
  expect_equal(length(tool_names), length(tools))
})

test_that("server tools error handling", {
  # Test error conditions
  expect_error(MCPR:::set_server_tools(42), "registry must be a ToolRegistry instance")
  expect_error(MCPR:::set_server_tools("invalid"), "registry must be a ToolRegistry instance")

  # Test with corrupted global state
  the_env <- MCPR:::the
  old_server_tools <- the_env$server_tools
  the_env$server_tools <- "invalid"

  # Should still work - functions should handle invalid state
  expect_no_error(MCPR:::set_server_tools(NULL))

  # Restore
  the_env$server_tools <- old_server_tools
})
