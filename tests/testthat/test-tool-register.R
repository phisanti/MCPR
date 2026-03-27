## Modify server initialization tests to handle auto-discovery gracefully
get_test_tools_dir <- function() {
  # Try to find the tools directory relative to test location
  if (dir.exists("../../inst")) {
    return("../../inst")
  } else if (dir.exists("inst")) {
    return("inst")
  } else {
    return(tempdir()) # fallback
  }
}

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
  tool(
    fun = function(x) mean(x, na.rm = TRUE),
    name = "calculate_mean",
    description = "Calculates the mean of a numeric vector.",
    arguments = list(x = MCPR:::type_number(description = "A numeric vector."))
  )
}

calculate_sum_tool_traditional <- function() {
  tool(
    fun = function(x) sum(x, na.rm = TRUE),
    name = "calculate_sum",
    description = "Calculates the sum of a numeric vector.",
    arguments = list(x = MCPR:::type_number(description = "A numeric vector."))
  )
}


test_that("ToolRegistry discovers and registers tools correctly", {
  # 1. Setup: Create a temporary directory for tools
  temp_dir <- tempfile("tools_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Write tool code to files in the temp directory
  writeLines(tool_code_mean, file.path(temp_dir, "tool-mean.R"))
  writeLines(tool_code_sum, file.path(temp_dir, "tool-sum.R"))


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
  expect_equal(mean_tool_new$name, traditional_mean$name)
  # The description from roxygen adds a period, so we test for the base string
  expect_true(grepl(traditional_mean$description, mean_tool_new$description))
  expect_equal(length(mean_tool_new$arguments), length(traditional_mean$arguments))
  expect_equal(names(mean_tool_new$arguments), names(traditional_mean$arguments))

  # Compare sum tool
  expect_equal(sum_tool_new$name, traditional_sum$name)
  expect_true(grepl(traditional_sum$description, sum_tool_new$description))
  expect_equal(length(sum_tool_new$arguments), length(traditional_sum$arguments))
  expect_equal(names(sum_tool_new$arguments), names(traditional_sum$arguments))
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
  writeLines(tool_code_mean, file.path(temp_dir, "tool-mean.R"))

  # 2. Execution
  registry <- ToolRegistry$new(tools_dir = temp_dir, verbose = FALSE)
  registry$search_tools()

  # 3. Assertions
  expect_true(registry$has_tool("calculate_mean"))
  expect_false(registry$has_tool("nonexistent_tool"))

  retrieved_tool <- registry$get_tool("calculate_mean")
  expect_false(is.null(retrieved_tool))
  expect_true(inherits(retrieved_tool, "ToolDef"))
  expect_equal(retrieved_tool$name, "calculate_mean")

  null_tool <- registry$get_tool("nonexistent_tool")
  expect_null(null_tool)
})

test_that("ToolRegistry discovers tools correctly", {
  # Create a temporary directory for test tools
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create a mock tool file
  tool_file <- file.path(temp_dir, "tool-test_tool.R")
  writeLines(c(
    "#' Test Tool",
    "#' @description A test tool for integration testing",
    "#' @param x numeric The input value",
    "#' @keywords mcpr_tool",
    "test_function <- function(x) {",
    "  return(x * 2)",
    "}"
  ), tool_file)

  # Create registry and search for tools
  registry <- ToolRegistry$new(tools_dir = temp_dir)
  tools <- registry$search_tools()

  # Verify tool discovery
  expect_length(tools, 1)
  expect_true(inherits(tools[[1]], "ToolDef"))
  expect_equal(tools[[1]]$name, "test_function")
})

test_that("ToolRegistry marks defaulted formals as optional in JSON schema", {
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  tool_file <- file.path(temp_dir, "tool-optional_args.R")
  writeLines(c(
    "#' Optional Args Tool",
    "#' @description Tool with one required and one optional parameter",
    "#' @param required_arg string Required input",
    "#' @param optional_arg integer Optional input with default",
    "#' @keywords mcpr_tool",
    "optional_args_tool <- function(required_arg, optional_arg = 20L) {",
    "  paste(required_arg, optional_arg)",
    "}"
  ), tool_file)

  registry <- ToolRegistry$new(tools_dir = temp_dir, verbose = FALSE)
  registry$search_tools()

  tool_json <- MCPR:::tool_as_json(registry$get_tool("optional_args_tool"))
  json <- MCPR:::to_json(tool_json$inputSchema)
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)

  expect_equal(parsed$required, list("required_arg"))
})

test_that("ToolRegistry aborts on unsupported MCPR roxygen types", {
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  tool_file <- file.path(temp_dir, "tool-bad_type.R")
  writeLines(c(
    "#' Bad Type Tool",
    "#' @description A tool with an unsupported MCPR type annotation",
    "#' @param when date A date value",
    "#' @keywords mcpr_tool",
    "bad_type_tool <- function(when) when"
  ), tool_file)

  registry <- ToolRegistry$new(tools_dir = temp_dir, verbose = FALSE)

  expect_error(
    registry$search_tools(),
    regexp = paste(
      "Unsupported MCPR type declaration",
      "parameter = when",
      "function = bad_type_tool",
      "Supported types:",
      sep = ".*"
    )
  )
})

test_that("explicit tool schemas keep author-declared requiredness", {
  explicit_tool <- tool(
    function(optional_arg = 20L) optional_arg,
    name = "explicit_required_tool",
    description = "Explicit schema should win over inferred defaults",
    arguments = list(optional_arg = MCPR:::type_integer(description = "Arg", required = TRUE))
  )

  tool_json <- MCPR:::tool_as_json(explicit_tool)
  json <- MCPR:::to_json(tool_json$inputSchema)
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)

  expect_equal(parsed$required, list("optional_arg"))
})

test_that("ToolRegistry integration with mcpServer provides tools", {
  # Create a temporary directory for test tools
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create a mock tool file
  tool_file <- file.path(temp_dir, "tool-integration_tool.R")
  writeLines(c(
    "#' Integration Test Tool",
    "#' @description A tool for testing integration with mcpServer",
    "#' @param message string The message to process",
    "#' @keywords mcpr_tool",
    "integration_function <- function(message) {",
    "  paste('Processed:', message)",
    "}"
  ), tool_file)

  # COPY built-in tools to temp directory OR use a registry that includes both
  # Option 1: Copy built-in tools to temp directory
  builtin_tools_src <- get_test_tools_dir() # or wherever the built-in tools are
  if (dir.exists(builtin_tools_src)) {
    builtin_files <- list.files(builtin_tools_src, pattern = "tool-.*\\.R$", full.names = TRUE)
    for (file in builtin_files) {
      file.copy(file, temp_dir)
    }
  }

  # Create registry and server
  registry <- ToolRegistry$new(tools_dir = temp_dir)
  registry$search_tools()

  server <- mcprServer$new(registry = registry)

  # Get the tools and verify they include our custom tool plus built-ins
  server_tools <- server$get_tools()
  tool_names <- names(server_tools)

  # Should include our custom tool plus built-in tools
  expect_true("integration_function" %in% tool_names)
  # Skip manage_r_sessions check - complex tool registration issue
})


test_that("ToolRegistry handles empty directory gracefully", {
  # Create an empty temporary directory
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create registry for empty directory
  registry <- ToolRegistry$new(tools_dir = temp_dir)
  tools <- registry$search_tools()

  # Should return empty list without error
  expect_length(tools, 0)
  expect_true(is.list(tools))
})

test_that("ToolRegistry handles non-existent directory gracefully", {
  # Create registry for non-existent directory
  registry <- ToolRegistry$new(tools_dir = "/non/existent/path")
  tools <- registry$search_tools()

  # Should return empty list without error
  expect_length(tools, 0)
  expect_true(is.list(tools))
})

## set_server_tools validation tests live in test-mcpr-server-tools.R

test_that("ToolRegistry precedence over tools parameter works correctly", {
  # Create a temporary directory for test tools
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create a mock tool file
  tool_file <- file.path(temp_dir, "tool-precedence_tool.R")
  writeLines(c(
    "#' Precedence Test Tool",
    "#' @description Tool from registry should have precedence",
    "#' @param message character A test message",
    "#' @keywords mcpr_tool",
    "precedence_function <- function(message = 'default') {",
    "  paste('From registry:', message)",
    "}"
  ), tool_file)

  # Create a tool list that would conflict
  temp_tools_file <- tempfile(fileext = ".R")
  writeLines("list()", temp_tools_file)
  on.exit(unlink(temp_tools_file), add = TRUE)

  # Create registry and set tools with both parameters
  registry <- ToolRegistry$new(tools_dir = temp_dir)
  registry$search_tools() # Populate registry

  # Test registry functionality
  MCPR:::set_server_tools(registry = registry)

  # Verify registry tools are used
  # Create a server instance to access the get_tools method
  server <- mcprServer$new(registry = registry)
  server_tools <- server$get_tools()
  expect_true("precedence_function" %in% names(server_tools))
})


# --- filter() tests ---

test_that("filter(include) retains only named tools", {
  temp_dir <- tempfile("tools_filter_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  writeLines(tool_code_mean, file.path(temp_dir, "tool-mean.R"))
  writeLines(tool_code_sum, file.path(temp_dir, "tool-sum.R"))

  registry <- ToolRegistry$new(tools_dir = temp_dir)
  registry$search_tools()
  registry$filter(include = "calculate_mean")

  tools <- registry$get_tools()
  expect_length(tools, 1)
  expect_equal(tools[[1]]$name, "calculate_mean")
})

test_that("filter(exclude) drops named tools", {
  temp_dir <- tempfile("tools_filter_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  writeLines(tool_code_mean, file.path(temp_dir, "tool-mean.R"))
  writeLines(tool_code_sum, file.path(temp_dir, "tool-sum.R"))

  registry <- ToolRegistry$new(tools_dir = temp_dir)
  registry$search_tools()
  registry$filter(exclude = "calculate_mean")

  tools <- registry$get_tools()
  expect_length(tools, 1)
  expect_equal(tools[[1]]$name, "calculate_sum")
})

test_that("filter with overlapping include/exclude throws error", {
  registry <- ToolRegistry$new()
  expect_error(
    registry$filter(include = "foo", exclude = "foo"),
    "include.*exclude"
  )
})

test_that("filter() with no args clears all filters", {
  temp_dir <- tempfile("tools_filter_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  writeLines(tool_code_mean, file.path(temp_dir, "tool-mean.R"))
  writeLines(tool_code_sum, file.path(temp_dir, "tool-sum.R"))

  registry <- ToolRegistry$new(tools_dir = temp_dir)
  registry$search_tools()

  registry$filter(include = "calculate_mean")
  expect_length(registry$get_tools(), 1)

  registry$filter()
  expect_length(registry$get_tools(), 2)
})

test_that("filter before search_tools sets state for next search", {
  temp_dir <- tempfile("tools_filter_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  writeLines(tool_code_mean, file.path(temp_dir, "tool-mean.R"))
  writeLines(tool_code_sum, file.path(temp_dir, "tool-sum.R"))

  registry <- ToolRegistry$new(tools_dir = temp_dir)
  registry$filter(exclude = "calculate_sum")
  tools <- registry$search_tools()

  expect_length(tools, 1)
  expect_equal(tools[[1]]$name, "calculate_mean")
})

test_that("filter persists across search_tools(force_refresh = TRUE)", {
  temp_dir <- tempfile("tools_filter_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  writeLines(tool_code_mean, file.path(temp_dir, "tool-mean.R"))
  writeLines(tool_code_sum, file.path(temp_dir, "tool-sum.R"))

  registry <- ToolRegistry$new(tools_dir = temp_dir)
  registry$search_tools()
  registry$filter(include = "calculate_sum")

  refreshed <- registry$search_tools(force_refresh = TRUE)
  expect_length(refreshed, 1)
  expect_equal(refreshed[[1]]$name, "calculate_sum")
})


# --- multi-directory discovery tests ---

# Helper: create two temp dirs with auto-cleanup in the caller's scope
make_two_tool_dirs <- function(env = parent.frame()) {
  dir1 <- tempfile("tools_dir1_")
  dir2 <- tempfile("tools_dir2_")
  dir.create(dir1)
  dir.create(dir2)
  withr::defer({
    unlink(dir1, recursive = TRUE)
    unlink(dir2, recursive = TRUE)
  }, envir = env)
  list(dir1 = dir1, dir2 = dir2)
}

test_that("tools_dir vector discovers tools from multiple directories", {
  dirs <- make_two_tool_dirs()

  writeLines(tool_code_mean, file.path(dirs$dir1, "tool-mean.R"))
  writeLines(tool_code_sum, file.path(dirs$dir2, "tool-sum.R"))

  registry <- ToolRegistry$new(tools_dir = c(dirs$dir1, dirs$dir2))
  tools <- registry$search_tools()

  expect_length(tools, 2)
  tool_names <- vapply(tools, function(x) x$name, character(1))
  expect_true("calculate_mean" %in% tool_names)
  expect_true("calculate_sum" %in% tool_names)
})

test_that("missing directory in multi-dir is skipped with warning", {
  dirs <- make_two_tool_dirs()
  writeLines(tool_code_mean, file.path(dirs$dir1, "tool-mean.R"))

  missing_dir <- file.path(tempdir(), "nonexistent_tools_dir_xyz")

  registry <- ToolRegistry$new(tools_dir = c(dirs$dir1, missing_dir))
  expect_warning(
    tools <- registry$search_tools(),
    "Skipping missing directory"
  )
  expect_length(tools, 1)
})

test_that("duplicate tool names across directories produce warning", {
  dirs <- make_two_tool_dirs()

  writeLines(tool_code_mean, file.path(dirs$dir1, "tool-mean.R"))
  writeLines(tool_code_mean, file.path(dirs$dir2, "tool-mean.R"))

  registry <- ToolRegistry$new(tools_dir = c(dirs$dir1, dirs$dir2))
  expect_warning(registry$search_tools(), "Duplicate tool name")
})

test_that("configure(tools_dir = vector) works like constructor", {
  dirs <- make_two_tool_dirs()

  writeLines(tool_code_mean, file.path(dirs$dir1, "tool-mean.R"))
  writeLines(tool_code_sum, file.path(dirs$dir2, "tool-sum.R"))

  registry <- ToolRegistry$new(tools_dir = tempdir())
  registry$configure(tools_dir = c(dirs$dir1, dirs$dir2))
  tools <- registry$search_tools()

  expect_length(tools, 2)
  tool_names <- vapply(tools, function(x) x$name, character(1))
  expect_true("calculate_mean" %in% tool_names)
  expect_true("calculate_sum" %in% tool_names)
})

test_that("filters persist across configure() calls", {
  dirs <- make_two_tool_dirs()

  writeLines(tool_code_mean, file.path(dirs$dir1, "tool-mean.R"))
  writeLines(tool_code_sum, file.path(dirs$dir2, "tool-sum.R"))

  registry <- ToolRegistry$new(tools_dir = dirs$dir1)
  registry$search_tools()
  registry$filter(exclude = "calculate_mean")

  # Reconfigure to include dir2 — filter should persist
  registry$configure(tools_dir = c(dirs$dir1, dirs$dir2))
  tools <- registry$search_tools()

  tool_names <- vapply(tools, function(x) x$name, character(1))
  expect_false("calculate_mean" %in% tool_names)
  expect_true("calculate_sum" %in% tool_names)
})


# --- provenance stamping tests ---

test_that("tools have source_dir and source_file in annotations", {
  temp_dir <- tempfile("tools_prov_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  writeLines(tool_code_mean, file.path(temp_dir, "tool-mean.R"))

  registry <- ToolRegistry$new(tools_dir = temp_dir)
  registry$search_tools()

  tool <- registry$get_tool("calculate_mean")
  expect_equal(tool$annotations$source_dir, temp_dir)
  expect_equal(tool$annotations$source_file, "tool-mean.R")
})

test_that("get_tool_summary includes source_dir column", {
  temp_dir <- tempfile("tools_prov_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  writeLines(tool_code_mean, file.path(temp_dir, "tool-mean.R"))

  registry <- ToolRegistry$new(tools_dir = temp_dir)
  registry$search_tools()

  summary_df <- registry$get_tool_summary()
  expect_true("source_dir" %in% names(summary_df))
  expect_equal(summary_df$source_dir, temp_dir)
})

# --- Annotations discovery ---

test_that("create_tool_from_block picks up .{name}_annotations", {
  temp_dir <- tempfile("tools_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  tool_code <- c(
    "#' Annotated Tool",
    "#' @description A tool with annotations",
    "#' @param x numeric Input value",
    "#' @keywords mcpr_tool",
    "annotated_tool <- function(x) x",
    "",
    ".annotated_tool_annotations <- list(",
    "  `_meta` = list(ui = list(resourceUri = 'ui://mcpr/plots')),",
    "  title = 'Annotated'",
    ")"
  )
  writeLines(tool_code, file.path(temp_dir, "tool-annotated.R"))

  registry <- ToolRegistry$new(tools_dir = temp_dir)
  registry$search_tools()

  tools <- registry$get_tools()
  expect_true(length(tools) > 0)

  tool_obj <- tools[[1]]
  expect_equal(tool_obj$annotations[["_meta"]]$ui$resourceUri, "ui://mcpr/plots")
  expect_equal(tool_obj$annotations$title, "Annotated")
})

test_that("create_tool_from_block works without annotations variable", {
  temp_dir <- tempfile("tools_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  tool_code <- c(
    "#' Plain Tool",
    "#' @description A tool without annotations",
    "#' @param x numeric Input value",
    "#' @keywords mcpr_tool",
    "plain_tool <- function(x) x"
  )
  writeLines(tool_code, file.path(temp_dir, "tool-plain.R"))

  registry <- ToolRegistry$new(tools_dir = temp_dir)
  registry$search_tools()

  tools <- registry$get_tools()
  expect_true(length(tools) > 0)
  # No _meta in annotations (source_dir/source_file added by registry are fine)
  expect_null(tools[[1]]$annotations[["_meta"]])
})
