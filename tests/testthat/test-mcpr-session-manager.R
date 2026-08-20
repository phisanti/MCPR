# MCPR Session Manager
# Verifies the server-owned session runtime boundary introduced by the refactor.
# Focuses on local/private state and capability detection before attach forwarding exists.

test_that("mcprSessionManager constructs with private local binding", {
  manager <- MCPR:::mcprSessionManager$new()

  expect_s3_class(manager, "mcprSessionManager")
  expect_false(manager$is_enabled())
  expect_equal(manager$active_label(), "private (local)")

  binding <- manager$active_binding()
  expect_true(is.environment(binding))
  expect_equal(binding$type, "local")
  expect_null(binding$session_id)
})

test_that("mcprServer detects session capability from active registry tools", {
  local_registry <- ToolRegistry$new()
  local_server <- mcprServer$new(registry = local_registry)
  expect_false(local_server$session_management_enabled())

  builtin_server <- mcprServer$new(.tools_dir = system.file(package = "MCPR", mustWork = TRUE))
  expect_true(builtin_server$session_management_enabled())
})

test_that("mcprServer instances own separate active binding state", {
  registry_a <- ToolRegistry$new()
  registry_b <- ToolRegistry$new()
  server_a <- mcprServer$new(registry = registry_a)
  server_b <- mcprServer$new(registry = registry_b)

  binding_a <- server_a$active_session_binding()
  binding_b <- server_b$active_session_binding()

  expect_false(identical(binding_a, binding_b))
  expect_equal(server_a$active_session_label(), "private (local)")
  expect_equal(server_b$active_session_label(), "private (local)")

  binding_a$label <- "server-a-local"
  expect_equal(server_a$active_session_label(), "server-a-local")
  expect_equal(server_b$active_session_label(), "private (local)")
})

test_that("manage_r_sessions list reports private and active local state through JSON-RPC", {
  server <- mcprServer$new(.tools_dir = system.file(package = "MCPR", mustWork = TRUE))
  skip_if(!server$session_management_enabled(), "manage_r_sessions not discoverable")

  captured <- NULL
  local_mocked_bindings(
    cat_json = function(x) { captured <<- x },
    .package = "MCPR"
  )

  request <- jsonlite::toJSON(list(
    jsonrpc = "2.0",
    id = 101L,
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

test_that("start attaches a secondary session immediately", {
  manager <- MCPR:::mcprSessionManager$new(
    enabled = TRUE,
    local_executor = function(data) "local",
    callbacks = list(
      start_secondary = function(working_dir = getwd()) list(session_id = 42L, key = "daemon-42"),
      forward_secondary = function(data, binding) {
        paste0("forwarded-", binding$session_id)
      }
    )
  )

  result <- manager$handle_control("start")

  expect_match(result, "Secondary session 42 started and attached", fixed = TRUE)
  expect_equal(manager$active_binding()$type, "secondary")
  expect_equal(manager$active_binding()$session_id, 42L)
  expect_equal(manager$active_label(), "42 (attached secondary)")
  expect_equal(manager$execute(list(id = 1L)), "forwarded-42")
})

test_that("detach returns to private/local without closing attached session", {
  closed <- FALSE
  manager <- MCPR:::mcprSessionManager$new(
    enabled = TRUE,
    callbacks = list(
      start_secondary = function(working_dir = getwd()) list(session_id = 43L, key = "daemon-43"),
      close_secondary = function(binding) { closed <<- TRUE }
    )
  )

  manager$handle_control("start")
  result <- manager$handle_control("detach")

  expect_equal(result, "Detached. Active session: private (local)")
  expect_equal(manager$active_binding()$type, "local")
  expect_false(closed)
})

test_that("private and secondary binding labels remain separate across attach and detach", {
  manager <- MCPR:::mcprSessionManager$new(
    enabled = TRUE,
    callbacks = list(
      start_secondary = function(working_dir = getwd()) list(session_id = 44L, key = "daemon-44")
    )
  )

  private_binding <- manager$active_binding()
  manager$handle_control("start")
  secondary_binding <- manager$active_binding()
  manager$handle_control("detach")

  expect_false(identical(private_binding, secondary_binding))
  expect_equal(private_binding$label, "private (local)")
  expect_equal(secondary_binding$label, "44 (attached secondary)")
  expect_equal(manager$active_binding()$label, "private (local)")
})

test_that("close rejects human sessions and closes only owned secondary sessions", {
  closed <- integer(0)
  manager <- MCPR:::mcprSessionManager$new(
    enabled = TRUE,
    callbacks = list(
      discover_human = function() integer(0),
      join_human = function(session_id) list(session_id = session_id),
      start_secondary = function(working_dir = getwd()) list(session_id = 45L, key = "daemon-45"),
      close_secondary = function(binding) { closed <<- c(closed, binding$session_id) }
    )
  )

  manager$handle_control("join", session = 7L)
  expect_error(
    manager$handle_control("close", session = 7L),
    "human-owned"
  )
  expect_equal(manager$active_label(), "private (local)")
  expect_equal(closed, integer(0))

  manager$handle_control("start")
  result <- manager$handle_control("close", session = 45L)

  expect_match(result, "Secondary session 45 closed", fixed = TRUE)
  expect_equal(closed, 45L)
  expect_equal(manager$active_label(), "private (local)")
})

test_that("start does not displace an existing attachment", {
  # Regression: the active binding is server-wide, so a second caller's start
  # used to reroute a caller that had explicitly joined another session, with
  # nothing in either response saying so.
  started <- 60L
  manager <- MCPR:::mcprSessionManager$new(
    enabled = TRUE,
    callbacks = list(
      discover_human = function() integer(0),
      join_human = function(session_id) list(session_id = session_id),
      start_secondary = function(working_dir = getwd()) {
        started <<- started + 1L
        list(session_id = started, key = MCPR:::secondary_session_key(started))
      },
      close_secondary = function(binding) invisible(NULL)
    )
  )

  first <- manager$handle_control("start")
  expect_match(first, "Secondary session 61 started and attached", fixed = TRUE)
  expect_equal(manager$active_binding()$session_id, 61L)

  second <- manager$handle_control("start")
  expect_match(second, "Secondary session 62 started but not attached", fixed = TRUE)
  expect_match(second, "Active session is unchanged: 61 (attached secondary)", fixed = TRUE)
  expect_equal(manager$active_binding()$session_id, 61L)

  # The new session is real and reachable through an explicit join.
  joined <- manager$handle_control("join", session = 62L)
  expect_match(joined, "This replaced previously attached session 61", fixed = TRUE)
  expect_equal(manager$active_binding()$session_id, 62L)
})

test_that("join attaches an owned secondary instead of re-joining it as human", {
  # Regression: start's own instruction text points at join, and join used to
  # resolve every id through join_human. That dialled a second socket to this
  # server's own worker, labelled it human, and left close() unable to reset
  # the active binding, so execution kept forwarding to a killed process.
  joined_as_human <- integer(0)
  manager <- MCPR:::mcprSessionManager$new(
    enabled = TRUE,
    callbacks = list(
      discover_human = function() integer(0),
      join_human = function(session_id) {
        joined_as_human <<- c(joined_as_human, session_id)
        list(session_id = session_id, socket = "human-socket")
      },
      start_secondary = local({
        n <- 70L
        function(working_dir = getwd()) {
          n <<- n + 1L
          list(session_id = n, key = MCPR:::secondary_session_key(n))
        }
      }),
      close_secondary = function(binding) invisible(NULL),
      forward_human = function(data, binding) "human",
      forward_secondary = function(data, binding) "secondary"
    )
  )

  manager$handle_control("start")
  manager$handle_control("start")
  manager$handle_control("join", session = 72L)

  expect_equal(joined_as_human, integer(0))
  expect_equal(manager$active_binding()$type, "secondary")
  expect_equal(manager$active_label(), "72 (attached secondary)")
  expect_equal(manager$execute(list()), "secondary")

  listing <- manager$handle_control("list")
  expect_equal(lengths(regmatches(listing, gregexpr("- 72:", listing, fixed = TRUE))), 1L)

  manager$handle_control("close", session = 72L)
  expect_equal(manager$active_label(), "private (local)")
})

test_that("starting a secondary drops a stale discovered human binding", {
  manager <- MCPR:::mcprSessionManager$new(
    enabled = TRUE,
    callbacks = list(
      discover_human = function() 80L,
      join_human = function(session_id) list(session_id = session_id, socket = "s"),
      start_secondary = function(working_dir = getwd()) {
        list(session_id = 80L, key = MCPR:::secondary_session_key(80L))
      },
      close_secondary = function(binding) invisible(NULL)
    )
  )

  manager$handle_control("list")
  manager$handle_control("start")
  listing <- manager$handle_control("list")

  expect_equal(lengths(regmatches(listing, gregexpr("- 80:", listing, fixed = TRUE))), 1L)
  expect_match(listing, "- 80: secondary active", fixed = TRUE)
})

test_that("joining from the private session reports no displacement", {
  manager <- MCPR:::mcprSessionManager$new(
    enabled = TRUE,
    callbacks = list(
      discover_human = function() 8L,
      join_human = function(session_id) list(session_id = session_id)
    )
  )

  result <- manager$handle_control("join", session = 8L)
  expect_match(result, "Attached to session 8.", fixed = TRUE)
  expect_false(grepl("replaced", result, fixed = TRUE))
})

test_that("discovery never shadows an owned secondary session", {
  # Regression: socket discovery cannot tell an MCPR-owned worker from a human
  # REPL, so it used to register a second, human binding for the same id. That
  # binding shadowed the secondary one and close() refused forever, leaking the
  # R process.
  closed <- integer(0)
  manager <- MCPR:::mcprSessionManager$new(
    enabled = TRUE,
    callbacks = list(
      discover_human = function() 45L,
      join_human = function(session_id) list(session_id = session_id),
      start_secondary = function(working_dir = getwd()) list(session_id = 45L, key = "daemon-45"),
      close_secondary = function(binding) { closed <<- c(closed, binding$session_id) }
    )
  )

  manager$handle_control("start")
  listing <- manager$handle_control("list")

  expect_equal(lengths(regmatches(listing, gregexpr("- 45:", listing, fixed = TRUE))), 1L)
  expect_match(listing, "- 45: secondary active", fixed = TRUE)

  result <- manager$handle_control("close", session = 45L)
  expect_match(result, "Secondary session 45 closed", fixed = TRUE)
  expect_equal(closed, 45L)
})

test_that("an owned secondary binding wins over a stale human binding", {
  closed <- integer(0)
  manager <- MCPR:::mcprSessionManager$new(
    enabled = TRUE,
    callbacks = list(
      discover_human = function() 46L,
      join_human = function(session_id) list(session_id = session_id),
      start_secondary = function(working_dir = getwd()) list(session_id = 46L, key = "daemon-46"),
      close_secondary = function(binding) { closed <<- c(closed, binding$session_id) }
    )
  )

  # Human binding recorded first (discovery ran before the worker was started),
  # then the same id is started as an owned secondary.
  manager$handle_control("list")
  manager$handle_control("start")

  result <- manager$handle_control("close", session = 46L)
  expect_match(result, "Secondary session 46 closed", fixed = TRUE)
  expect_equal(closed, 46L)
})

test_that("dead active attached binding resets to private/local", {
  manager <- MCPR:::mcprSessionManager$new(
    enabled = TRUE,
    callbacks = list(
      start_secondary = function(working_dir = getwd()) list(session_id = 46L, key = "daemon-46")
    )
  )

  manager$handle_control("start")
  reset <- manager$mark_dead("daemon-46")
  listing <- manager$handle_control("list")

  expect_true(reset)
  expect_equal(manager$active_label(), "private (local)")
  expect_match(listing, "Active session: private (local)", fixed = TRUE)
  expect_match(listing, "Previous active session 46 is no longer responding.", fixed = TRUE)
})

test_that("timed-out active secondary is closed and recycled", {
  started <- 46L
  closed <- integer(0)
  manager <- MCPR:::mcprSessionManager$new(
    enabled = TRUE,
    callbacks = list(
      start_secondary = function(working_dir = getwd()) {
        started <<- started + 1L
        list(session_id = started, key = MCPR:::secondary_session_key(started))
      },
      close_secondary = function(binding) {
        closed <<- c(closed, binding$session_id)
      }
    )
  )

  manager$handle_control("start")
  recovery <- manager$recover_timeout("daemon-47")
  listing <- manager$handle_control("list")

  expect_equal(recovery$action, "recycled")
  expect_equal(recovery$old_session_id, 47L)
  expect_equal(recovery$new_session_id, 48L)
  expect_equal(closed, 47L)
  expect_equal(manager$active_binding()$type, "secondary")
  expect_equal(manager$active_binding()$session_id, 48L)
  expect_match(listing, "Active session: 48 (attached secondary)", fixed = TRUE)
  expect_match(listing, "Previous active session 47 timed out and was recycled as session 48.", fixed = TRUE)
})

test_that("start_secondary callback receives current working directory", {
  original_wd <- getwd()
  temp_wd <- tempfile("mcpr-secondary-wd-")
  dir.create(temp_wd)
  on.exit(setwd(original_wd), add = TRUE)
  on.exit(unlink(temp_wd, recursive = TRUE), add = TRUE)

  received_wd <- NULL
  manager <- MCPR:::mcprSessionManager$new(
    enabled = TRUE,
    callbacks = list(
      start_secondary = function(working_dir = getwd()) {
        received_wd <<- working_dir
        list(session_id = 48L, key = "daemon-48")
      }
    )
  )

  setwd(temp_wd)
  manager$handle_control("start")

  expect_equal(normalizePath(received_wd), normalizePath(temp_wd))
})

test_that("ordinary local execution does not create pending remote requests", {
  executed <- FALSE
  manager <- MCPR:::mcprSessionManager$new(
    enabled = TRUE,
    local_executor = function(data) {
      executed <<- TRUE
      "local"
    }
  )

  result <- manager$execute(list(id = 49L))

  expect_true(executed)
  expect_equal(result, "local")
  expect_equal(manager$active_label(), "private (local)")
})

test_that("attached execution uses pending registration seam", {
  pending_keys <- character(0)
  manager <- MCPR:::mcprSessionManager$new(
    enabled = TRUE,
    callbacks = list(
      start_secondary = function(working_dir = getwd()) list(session_id = 50L, key = "daemon-50"),
      forward_secondary = function(data, binding) {
        pending_keys <<- c(pending_keys, binding$key)
        "forwarded"
      }
    )
  )

  manager$handle_control("start")
  result <- manager$execute(list(id = 50L))

  expect_equal(result, "forwarded")
  expect_equal(pending_keys, "daemon-50")
})
