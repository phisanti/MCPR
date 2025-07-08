tool_code_mean <- "
#' @title Calculate Mean
#' @description Calculates the mean of a numeric vector.
#' @param x numeric A numeric vector.
#' @keywords mcpr_tool
calculate_mean <- function(x) {
  mean(x, na.rm = TRUE)
}
"

tool_code_sum <- "
#' @title Calculate Sum
#' @description Calculates the sum of a numeric vector.
#' @param x numeric A numeric vector.
#' @keywords mcpr_tool
calculate_sum <- function(x) {
  sum(x, na.rm = TRUE)
}
"

# Traditional tool definitions for comparison
calculate_mean_tool_traditional <- function() {
  ellmer::tool(
    .fun = function(x) mean(x, na.rm = TRUE),
    .name = "calculate_mean",
    .description = "Calculates the mean of a numeric vector.",
    x = ellmer::type_number(description = "A numeric vector.")
  )
}

calculate_sum_tool_traditional <- function() {
  ellmer::tool(
    .fun = function(x) sum(x, na.rm = TRUE),
    .name = "calculate_sum",
    .description = "Calculates the sum of a numeric vector.",
    x = ellmer::type_number(description = "A numeric vector.")
  )
}


test_that("ToolRegistry discovers and registers tools correctly", {
  # 1. Setup: Create a temporary directory for tools
  temp_dir <- tempfile("tools_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Write tool code to files in the temp directory
  writeLines(tool_code_mean, file.path(temp_dir, "mean_tool.R"))
  writeLines(tool_code_sum, file.path(temp_dir, "sum_tool.R"))

  # 2. Execution: Use ToolRegistry to discover tools
  registry <- ToolRegistry$new(tools_dir = temp_dir, verbose = FALSE)
  discovered_tools <- registry$search_tools()

  # 3. Assertions
  expect_equal(length(discovered_tools), 2, info = "Should discover exactly two tools.")

  # Get the discovered tools by name
  mean_tool_new <- registry$get_tool("calculate_mean")
  sum_tool_new <- registry$get_tool("calculate_sum")

  expect_false(is.null(mean_tool_new), "The 'calculate_mean' tool should be found.")
  expect_false(is.null(sum_tool_new), "The 'calculate_sum' tool should be found.")

  # Compare with traditional definitions
  traditional_mean <- calculate_mean_tool_traditional()
  traditional_sum <- calculate_sum_tool_traditional()

  # Compare mean tool
  expect_equal(mean_tool_new@name, traditional_mean@name)
  # The description from roxygen adds a period, so we test for the base string
  expect_true(grepl(traditional_mean@description, mean_tool_new@description))
  expect_equal(length(mean_tool_new@arguments), length(traditional_mean@arguments))
  expect_equal(names(mean_tool_new@arguments), names(traditional_mean@arguments))

  # Compare sum tool
  expect_equal(sum_tool_new@name, traditional_sum@name)
  expect_true(grepl(traditional_sum@description, sum_tool_new@description))
  expect_equal(length(sum_tool_new@arguments), length(traditional_sum@arguments))
  expect_equal(names(sum_tool_new@arguments), names(traditional_sum@arguments))
})

test_that("ToolRegistry handles no tools found gracefully", {
  # 1. Setup: Create an empty temporary directory
  temp_dir <- tempfile("empty_tools_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # 2. Execution
  registry <- ToolRegistry$new(tools_dir = temp_dir, verbose = FALSE)
  discovered_tools <- registry$search_tools()

  # 3. Assertions
  expect_equal(length(discovered_tools), 0, info = "Should return an empty list when no tools are found.")
  summary_df <- registry$get_tool_summary()
  expect_equal(nrow(summary_df), 0)
})

test_that("has_tool and get_tool methods work correctly", {
  # 1. Setup
  temp_dir <- tempfile("tools_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  writeLines(tool_code_mean, file.path(temp_dir, "mean_tool.R"))

  # 2. Execution
  registry <- ToolRegistry$new(tools_dir = temp_dir, verbose = FALSE)
  registry$search_tools()

  # 3. Assertions
  expect_true(registry$has_tool("calculate_mean"))
  expect_false(registry$has_tool("nonexistent_tool"))

  retrieved_tool <- registry$get_tool("calculate_mean")
  expect_false(is.null(retrieved_tool))
  expect_true(inherits(retrieved_tool, "ellmer::ToolDef"))
  expect_equal(retrieved_tool@name, "calculate_mean")

  null_tool <- registry$get_tool("nonexistent_tool")
  expect_null(null_tool)
})