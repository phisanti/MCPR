# Runtime Refactor Built-In Private Workflow
# Exercises MCPR built-in tools through the JSON-RPC server path.
# Ordinary built-ins must share private workspace state without session routing.

runtime_refactor_builtin_call <- function(id, name, arguments = list()) {
  jsonlite::toJSON(list(
    jsonrpc = "2.0",
    id = id,
    method = "tools/call",
    params = list(name = name, arguments = arguments)
  ), auto_unbox = TRUE)
}

runtime_refactor_capture_builtin_response <- function(server, request) {
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

runtime_refactor_builtin_text <- function(response) {
  paste(vapply(response$result$content, `[[`, character(1), "text"), collapse = "\n")
}

test_that("built-in tools share private state without session arguments", {
  server <- mcprServer$new(.tools_dir = system.file(package = "MCPR", mustWork = TRUE))
  skip_if(!all(c("execute_r_code", "inspect_object", "view") %in% names(MCPR:::get_mcptools_tools())),
          "required built-in tools not discoverable")
  on.exit({
    if (exists("mcpr_phase6_private_x", envir = .GlobalEnv)) {
      rm("mcpr_phase6_private_x", envir = .GlobalEnv)
    }
  }, add = TRUE)

  execute_response <- runtime_refactor_capture_builtin_response(
    server,
    runtime_refactor_builtin_call(
      601L,
      "execute_r_code",
      list(code = "mcpr_phase6_private_x <- 'private'; mcpr_phase6_private_x")
    )
  )
  expect_null(execute_response$error)
  execute_text <- runtime_refactor_builtin_text(execute_response)
  expect_match(execute_text, "Code executed successfully", fixed = TRUE)
  expect_match(execute_text, "private", fixed = TRUE)
  expect_false(grepl("session=N", execute_text, fixed = TRUE))

  inspect_response <- runtime_refactor_capture_builtin_response(
    server,
    runtime_refactor_builtin_call(
      602L,
      "inspect_object",
      list(object_name = "mcpr_phase6_private_x")
    )
  )
  expect_null(inspect_response$error)
  inspect_text <- runtime_refactor_builtin_text(inspect_response)
  expect_match(inspect_text, "Object: mcpr_phase6_private_x", fixed = TRUE)
  expect_match(inspect_text, "private", fixed = TRUE)
  expect_false(grepl("session=N", inspect_text, fixed = TRUE))

  view_response <- runtime_refactor_capture_builtin_response(
    server,
    runtime_refactor_builtin_call(
      603L,
      "view",
      list(what = "session")
    )
  )
  expect_null(view_response$error)
  view_text <- runtime_refactor_builtin_text(view_response)
  expect_match(view_text, "View completed: session", fixed = TRUE)
  expect_match(view_text, "mcpr_phase6_private_x", fixed = TRUE)
  expect_false(grepl("session=N", view_text, fixed = TRUE))

  priv <- server$.__enclos_env__$private
  expect_length(priv$.daemon_listeners, 0L)
  expect_length(priv$.pending_requests, 0L)
})
