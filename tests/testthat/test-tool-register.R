# This file tests the ToolRegister R6 class.
# It ensures that the auto-discovery mechanism correctly parses tool files,
# creates valid ellmer::tool objects, and handles various edge cases.

# Load the class definition for testing
source("../../R/tool-register.R")

# Helper to create a temporary tool file
create_tool_file <- function(path, ..., content = NULL) {
  if (is.null(content)) {
    content <- paste(
      c(
        "#' @mcpr_tool",
        ...
      ),
      collapse = "\n"
    )
  }
  writeLines(content, file.path(path, basename(tempfile(fileext = ".R"))))
}

test_that("ToolRegister initializes correctly", {
  register <- ToolRegister$new(
    tools_dir = "my_tools",
    pattern = "\\.r$",
    recursive = TRUE,
    verbose = FALSE
  )
  
  expect_s3_class(register, "ToolRegister")
  expect_equal(register$.__enclos_env__$private$.tools_dir, "my_tools")
  expect_equal(register$.__enclos_env__$private$.pattern, "\\.r$")
  expect_true(register$.__enclos_env__$private$.recursive)
  expect_false(register$.__enclos_env__$private$.verbose)
})

test_that("discover_tools finds and parses a valid tool", {
  local_temp_dir <- tempfile()
  dir.create(local_temp_dir)
  
  create_tool_file(
    local_temp_dir,
    "#' @description A simple test tool.",
    "#' @param x string The first parameter.",
    "#' @param y numeric The second parameter.",
    "test_tool <- function(x, y) { paste(x, y) }"
  )
  
  register <- ToolRegister$new(tools_dir = local_temp_dir, verbose = FALSE)
  tools <- register$discover_tools()
  
  expect_length(tools, 1)
  expect_true(register$has_tool("test_tool"))
  
  tool <- register$get_tool("test_tool")
  expect_s4_class(tool, "ellmer::ToolDef")
  expect_equal(tool@name, "test_tool")
  expect_equal(tool@description, "A simple test tool.")
  
  # Verify parameters (which become arguments in the tool definition)
  expect_length(tool@arguments, 2)
  expect_equal(names(tool@arguments), c("x", "y"))
  
  # Check compatibility with existing mechanism (get_mcptools_tools_as_json)
  # This ensures the output can be consumed by the server
  tool_json <- tool_as_json(tool)
  expect_equal(tool_json$name, "test_tool")
  expect_equal(tool_json$inputSchema$type, "object")
  expect_true(all(c("x", "y") %in% names(tool_json$inputSchema$properties)))
  expect_equal(tool_json$inputSchema$properties$x$type, "string")
  expect_equal(tool_json$inputSchema$properties$y$type, "number")
  
  unlink(local_temp_dir, recursive = TRUE)
})

test_that("discover_tools handles directories that do not exist or are empty", {
  non_existent_dir <- file.path(tempdir(), "non_existent_dir")
  
  # Test non-existent directory
  register <- ToolRegister$new(tools_dir = non_existent_dir, verbose = FALSE)
  expect_length(register$discover_tools(), 0)
  
  # Test empty directory
  empty_dir <- tempfile()
  dir.create(empty_dir)
  register <- ToolRegister$new(tools_dir = empty_dir, verbose = FALSE)
  expect_length(register$discover_tools(), 0)
  
  unlink(empty_dir, recursive = TRUE)
})

test_that("discover_tools ignores files without @mcpr_tool marker", {
  local_temp_dir <- tempfile()
  dir.create(local_temp_dir)
  
  # File with no marker
  create_tool_file(
    local_temp_dir,
    content = "not_a_tool <- function() { 'hello' }"
  )
  
  register <- ToolRegister$new(tools_dir = local_temp_dir, verbose = FALSE)
  tools <- register$discover_tools()
  
  expect_length(tools, 0)
  
  unlink(local_temp_dir, recursive = TRUE)
})

test_that("discover_tools handles multi-line descriptions and validation", {
  local_temp_dir <- tempfile()
  dir.create(local_temp_dir)
  
  create_tool_file(
    local_temp_dir,
    "#' @description This is a long description that",
    "#' spans multiple lines.",
    "#' @param data array The data to process.",
    "multiline_tool <- function(data) { length(data) }"
  )
  
  # Add a file with a reserved name
  create_tool_file(
    local_temp_dir,
    "#' @description This tool uses a reserved name.",
    "list_r_sessions <- function() { 'reserved' }"
  )
  
  register <- ToolRegister$new(tools_dir = local_temp_dir, verbose = FALSE)
  
  # Check for warnings on validation failure
  expect_warning(
    tools <- register$discover_tools(),
    "Tools use reserved names: .*"
  )
  
  expect_length(tools, 2) # Both are discovered, but a warning is thrown
  
  tool <- register$get_tool("multiline_tool")
  expect_equal(tool@description, "This is a long description that spans multiple lines.")
  
  unlink(local_temp_dir, recursive = TRUE)
})

test_that("caching mechanism works correctly", {
  local_temp_dir <- tempfile()
  dir.create(local_temp_dir)
  
  tool_filepath <- create_tool_file(
    local_temp_dir,
    "#' @description Initial version of the tool.",
    "cache_test_tool <- function() { 1 }"
  )
  
  register <- ToolRegister$new(tools_dir = local_temp_dir, verbose = FALSE)
  
  # First discovery
  tools1 <- register$discover_tools()
  expect_length(tools1, 1)
  expect_equal(tools1[[1]]@description, "Initial version of the tool.")
  
  # Overwrite the file with new content
  writeLines(
    "#' @mcpr_tool\n#' @description Updated version.\ncache_test_tool <- function() { 2 }",
    tool_filepath
  )
  
  # Discover again without force_refresh, should get cached version
  tools2 <- register$discover_tools(force_refresh = FALSE)
  expect_equal(tools2[[1]]@description, "Initial version of the tool.")
  
  # Discover with force_refresh, should get updated version
  tools3 <- register$discover_tools(force_refresh = TRUE)
  expect_equal(tools3[[1]]@description, "Updated version.")
  
  unlink(local_temp_dir, recursive = TRUE)
})

test_that("get_tool_summary provides correct output", {
  local_temp_dir <- tempfile()
  dir.create(local_temp_dir)
  
  create_tool_file(
    local_temp_dir,
    "#' @description A tool for testing summary.",
    "#' @param arg1 string A string argument.",
    "summary_tool <- function(arg1) { arg1 }"
  )
  
  register <- ToolRegister$new(tools_dir = local_temp_dir, verbose = FALSE)
  register$discover_tools()
  
  summary_df <- register$get_tool_summary()
  
  expect_s3_class(summary_df, "data.frame")
  expect_equal(nrow(summary_df), 1)
  expect_equal(names(summary_df), c("name", "description", "parameters"))
  expect_equal(summary_df$name, "summary_tool")
  expect_equal(summary_df$parameters, "arg1")
  
  unlink(local_temp_dir, recursive = TRUE)
})

test_that("ToolRegister handles file with syntax error gracefully", {
  local_temp_dir <- tempfile()
  dir.create(local_temp_dir)
  
  create_tool_file(
    local_temp_dir,
    content = "#' @mcpr_tool\n#' @description A broken tool.\nbroken_tool <- function( { 'bad' }"
  )
  
  register <- ToolRegister$new(tools_dir = local_temp_dir, verbose = FALSE)
  
  # Sourcing should fail, and the tool should not be registered
  expect_warning(
    tools <- register$discover_tools(),
    "Failed to load tools from"
  )
  
  expect_length(tools, 0)
  
  unlink(local_temp_dir, recursive = TRUE)
})
