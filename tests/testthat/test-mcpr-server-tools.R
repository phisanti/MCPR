test_that("set_server_tools accepts valid ToolRegistry", {
  # Test with valid ToolRegistry
  registry <- ToolRegistry$new()
  expect_silent(set_server_tools(registry = registry))

  # Verify tools were set
  tools <- get_mcptools_tools()
  expect_true(is.list(tools))
})

test_that("set_server_tools validates ToolRegistry parameter", {
  # Test with invalid registry parameter
  expect_error(
    set_server_tools(registry = "not_a_registry"),
    "registry must be a ToolRegistry instance"
  )

  expect_error(
    set_server_tools(registry = list()),
    "registry must be a ToolRegistry instance"
  )

  expect_error(
    set_server_tools(registry = 123),
    "registry must be a ToolRegistry instance"
  )
})

test_that("set_server_tools handles NULL registry", {
  # Test with NULL registry (should set empty tools list)
  expect_silent(set_server_tools(registry = NULL))

  # Verify empty tools list was set
  tools <- get_mcptools_tools()
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
  set_server_tools(registry = registry)

  tools <- get_mcptools_tools()
  expect_true(is.list(tools))
  expect_true("test_tool" %in% names(tools))
})

test_that("get_mcptools_tools_as_json returns JSON-compatible format", {
  # Set up a registry
  registry <- ToolRegistry$new()
  set_server_tools(registry = registry)

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
  set_server_tools(registry = registry)

  # Check that global state was updated
  expect_true(exists("server_tools", envir = the))
  expect_true(is.list(the$server_tools))
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
  set_server_tools(registry = registry)

  tools <- get_mcptools_tools()
  expect_true("first_tool" %in% names(tools))
  expect_true("second_tool" %in% names(tools))
  expect_equal(length(tools), 2)
})

test_that("get_mcptools_tools handles empty tools list", {
  # Set empty tools
  set_server_tools(registry = NULL)

  tools <- get_mcptools_tools()
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
  set_server_tools(registry = registry)

  tools <- get_mcptools_tools()
  tool_obj <- tools[["executable_tool"]]

  expect_true(inherits(tool_obj, "ToolDef"))
  expect_true(is.function(tool_obj$fun))

  # Test that the tool function works
  result <- tool_obj$fun(4)
  expect_equal(result, 16)
})
