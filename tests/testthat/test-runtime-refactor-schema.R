# Runtime Refactor Schema Contract
# Verifies public server signatures and MCP tool schemas for private-session defaults.
# These tests define the breaking API contract before implementation changes.

runtime_refactor_builtin_server <- function() {
  mcprServer$new(.tools_dir = system.file(package = "MCPR", mustWork = TRUE))
}

runtime_refactor_tool_json <- function(server, tool_name) {
  tools <- server$get_tools("json")
  matches <- vapply(tools, function(tool) identical(tool$name, tool_name), logical(1))
  expect_true(any(matches), info = paste("Missing tool", tool_name))
  tools[[which(matches)[[1]]]]
}

runtime_refactor_schema_names <- function(tool) {
  properties <- tool$inputSchema$properties
  if (is.null(properties)) {
    properties <- list()
  }
  names(properties)
}

test_that("server constructors do not expose session_discovery", {
  expect_false("session_discovery" %in% names(formals(MCPR::mcpr_server)))
  expect_false("session_discovery" %in% names(formals(MCPR:::mcprServer$public_methods$initialize)))
})

test_that("ordinary built-in tools do not expose session input schemas", {
  server <- runtime_refactor_builtin_server()

  execute_tool <- runtime_refactor_tool_json(server, "execute_r_code")
  expect_true(all(c("code", "timeout") %in% runtime_refactor_schema_names(execute_tool)))
  expect_false("session" %in% runtime_refactor_schema_names(execute_tool))

  view_tool <- runtime_refactor_tool_json(server, "view")
  expect_true(all(c("what", "max_lines", "topic") %in% runtime_refactor_schema_names(view_tool)))
  expect_false("session" %in% runtime_refactor_schema_names(view_tool))

  inspect_tool <- runtime_refactor_tool_json(server, "inspect_object")
  expect_true("object_name" %in% runtime_refactor_schema_names(inspect_tool))
  expect_false("session" %in% runtime_refactor_schema_names(inspect_tool))

  plot_tool <- runtime_refactor_tool_json(server, "show_plot")
  expect_true(all(c("plot", "target") %in% runtime_refactor_schema_names(plot_tool)))
  expect_false("session" %in% runtime_refactor_schema_names(plot_tool))
})

test_that("manage_r_sessions remains the only built-in with session control input", {
  server <- runtime_refactor_builtin_server()
  manage_tool <- runtime_refactor_tool_json(server, "manage_r_sessions")

  expect_true(all(c("action", "session") %in% runtime_refactor_schema_names(manage_tool)))
})

test_that("ordinary built-in descriptions do not instruct per-call session routing", {
  server <- runtime_refactor_builtin_server()
  ordinary_names <- c("execute_r_code", "view", "inspect_object", "show_plot")
  ordinary_tools <- lapply(ordinary_names, function(name) runtime_refactor_tool_json(server, name))
  ordinary_text <- paste(vapply(ordinary_tools, `[[`, character(1), "description"), collapse = "\n")

  expect_false(grepl("session=N", ordinary_text, fixed = TRUE))
  expect_false(grepl("pass session", ordinary_text, ignore.case = TRUE))
  expect_false(grepl("carry that session", ordinary_text, ignore.case = TRUE))
})
