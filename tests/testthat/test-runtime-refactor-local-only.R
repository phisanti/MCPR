# Runtime Refactor Local-Only Contract
# Exercises a downstream-style registry without manage_r_sessions.
# The server must behave as one private local R runtime with no hidden routing.

runtime_refactor_make_local_registry <- function() {
  temp_dir <- tempfile("mcpr-local-tools-")
  dir.create(temp_dir, recursive = TRUE)

  tool_file <- file.path(temp_dir, "tool-marker.R")
  writeLines(c(
    "#' Set Marker",
    "#' @description Store a marker in the private R workspace.",
    "#' @param value string Marker value",
    "#' @keywords mcpr_tool",
    "set_marker <- function(value) {",
    "  assign('.mcpr_runtime_refactor_marker', value, envir = .GlobalEnv)",
    "  get('.mcpr_runtime_refactor_marker', envir = .GlobalEnv)",
    "}",
    "",
    "#' Get Marker",
    "#' @description Read a marker from the private R workspace.",
    "#' @keywords mcpr_tool",
    "get_marker <- function() {",
    "  if (!exists('.mcpr_runtime_refactor_marker', envir = .GlobalEnv)) {",
    "    return('<missing>')",
    "  }",
    "  get('.mcpr_runtime_refactor_marker', envir = .GlobalEnv)",
    "}"
  ), tool_file)

  registry <- ToolRegistry$new(tools_dir = temp_dir, verbose = FALSE)
  registry$search_tools()
  attr(registry, "mcpr_test_tools_dir") <- temp_dir
  registry
}

runtime_refactor_capture_response <- function(server, request) {
  captured <- NULL
  local_mocked_bindings(
    cat_json = function(x) {
      captured <<- x
      invisible(NULL)
    },
    .package = "MCPR"
  )
  server$.__enclos_env__$private$handle_message_from_client(request)
  captured
}

runtime_refactor_tool_call <- function(id, name, arguments = list()) {
  jsonlite::toJSON(list(
    jsonrpc = "2.0",
    id = id,
    method = "tools/call",
    params = list(name = name, arguments = arguments)
  ), auto_unbox = TRUE)
}

runtime_refactor_response_text <- function(response) {
  paste(vapply(response$result$content, `[[`, character(1), "text"), collapse = "\n")
}

test_that("local-only registry exposes no hidden session-management tools", {
  registry <- runtime_refactor_make_local_registry()
  on.exit(unlink(attr(registry, "mcpr_test_tools_dir"), recursive = TRUE), add = TRUE)

  server <- mcprServer$new(registry = registry)
  tools <- server$get_tools("json")
  names <- vapply(tools, `[[`, character(1), "name")

  expect_setequal(names, c("set_marker", "get_marker"))
  expect_false("manage_r_sessions" %in% names)
  expect_false(server$session_management_enabled())
  expect_equal(server$active_session_label(), "private (local)")

  priv <- server$.__enclos_env__$private
  expect_length(priv$.daemon_listeners, 0L)
  expect_length(priv$.user_listeners, 0L)
  expect_length(priv$.pending_requests, 0L)

  active <- server$active_session_binding()
  expect_equal(active$type, "local")
  expect_null(active$session_id)
  expect_null(active$process)
})

test_that("local-only server start path does not initialize session transport", {
  registry <- runtime_refactor_make_local_registry()
  on.exit(unlink(attr(registry, "mcpr_test_tools_dir"), recursive = TRUE), add = TRUE)

  server <- mcprServer$new(registry = registry)
  priv <- server$.__enclos_env__$private
  priv$.cv <- nanonext::cv()

  reader <- priv$setup_session_transport()

  expect_null(reader)
  expect_null(priv$.session_reader)
  expect_null(server$state_get("server_socket"))
})

test_that("local-only registry remains isolated after built-in server construction", {
  registry <- runtime_refactor_make_local_registry()
  on.exit(unlink(attr(registry, "mcpr_test_tools_dir"), recursive = TRUE), add = TRUE)

  local_server <- mcprServer$new(registry = registry)
  builtin_server <- mcprServer$new(.tools_dir = system.file(package = "MCPR", mustWork = TRUE))
  skip_if(!builtin_server$session_management_enabled(), "manage_r_sessions not discoverable")

  tools <- local_server$get_tools("json")
  names <- vapply(tools, `[[`, character(1), "name")
  expect_setequal(names, c("set_marker", "get_marker"))
  expect_false(local_server$session_management_enabled())

  response <- runtime_refactor_capture_response(
    local_server,
    runtime_refactor_tool_call(3L, "manage_r_sessions", list(action = "list"))
  )
  expect_equal(response$error$code, -32601)
  expect_equal(response$error$message, "Method not found")
})

test_that("local-only ordinary tools persist private workspace state without session", {
  registry <- runtime_refactor_make_local_registry()
  on.exit(unlink(attr(registry, "mcpr_test_tools_dir"), recursive = TRUE), add = TRUE)
  on.exit({
    if (exists(".mcpr_runtime_refactor_marker", envir = .GlobalEnv)) {
      rm(".mcpr_runtime_refactor_marker", envir = .GlobalEnv)
    }
  }, add = TRUE)

  server <- mcprServer$new(registry = registry)

  set_response <- runtime_refactor_capture_response(
    server,
    runtime_refactor_tool_call(1L, "set_marker", list(value = "local-only"))
  )
  expect_null(set_response$error)
  expect_equal(runtime_refactor_response_text(set_response), "local-only")
  expect_false(grepl("Active session:", runtime_refactor_response_text(set_response), fixed = TRUE))

  get_response <- runtime_refactor_capture_response(
    server,
    runtime_refactor_tool_call(2L, "get_marker")
  )
  expect_null(get_response$error)
  expect_equal(runtime_refactor_response_text(get_response), "local-only")
  expect_false(grepl("Active session:", runtime_refactor_response_text(get_response), fixed = TRUE))

  priv <- server$.__enclos_env__$private
  expect_length(priv$.daemon_listeners, 0L)
  expect_length(priv$.user_listeners, 0L)
  expect_length(priv$.pending_requests, 0L)
})
