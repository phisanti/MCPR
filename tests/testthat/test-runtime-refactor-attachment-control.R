# Runtime Refactor Attachment Control
# Verifies Phase 4 server-owned active session control behavior.
# Uses mocked manager callbacks to avoid brittle process-spawn sleeps.

test_that("server-owned manage_r_sessions list reports private/local state", {
  server <- mcprServer$new(.tools_dir = system.file(package = "MCPR", mustWork = TRUE))
  skip_if(!server$session_management_enabled(), "manage_r_sessions not discoverable")

  captured <- NULL
  local_mocked_bindings(
    cat_json = function(x) { captured <<- x },
    .package = "MCPR"
  )

  request <- jsonlite::toJSON(list(
    jsonrpc = "2.0",
    id = 401L,
    method = "tools/call",
    params = list(
      name = "manage_r_sessions",
      arguments = list(action = "list")
    )
  ), auto_unbox = TRUE)

  server$.__enclos_env__$private$handle_message_from_client(request)

  text <- captured$result$content[[1]]$text
  expect_match(text, "Private session: ok", fixed = TRUE)
  expect_match(text, "Active session: private (local)", fixed = TRUE)
})

test_that("control actions still work after an attached binding is marked dead", {
  manager <- MCPR:::mcprSessionManager$new(
    enabled = TRUE,
    callbacks = list(
      start_secondary = function(working_dir = getwd()) list(session_id = 47L, key = "daemon-47")
    )
  )

  manager$handle_control("start")
  manager$mark_dead("daemon-47")
  result <- manager$handle_control("detach")

  expect_equal(result, "Detached. Active session: private (local)")
  expect_equal(manager$active_label(), "private (local)")
})

test_that("pending timeout resets an active attached binding to private/local", {
  server <- mcprServer$new(.tools_dir = system.file(package = "MCPR", mustWork = TRUE))
  manager <- server$session_manager()
  manager_priv <- manager$.__enclos_env__$private
  manager_priv$.callbacks <- list(
    start_secondary = function(working_dir = getwd()) list(session_id = 51L, key = "daemon-51")
  )

  manager$handle_control("start")
  priv <- server$.__enclos_env__$private
  priv$.pending_requests[["daemon-51"]] <- list(
    client_request_id = 510L,
    session_key = "daemon-51",
    sent_at = Sys.time() - 10,
    timeout_secs = 1L
  )

  captured <- NULL
  local_mocked_bindings(
    cat_json = function(x) { captured <<- x },
    .package = "MCPR"
  )

  priv$sweep_pending_requests()

  expect_equal(captured$error$code, -32603L)
  expect_equal(manager$active_label(), "private (local)")
  expect_null(priv$.pending_requests[["daemon-51"]])
  expect_true("510" %in% priv$.timed_out_ids)
})
