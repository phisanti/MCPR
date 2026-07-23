# Per-Session Pending-Request Queue Regression Tests
# Guards the active/waiting bookkeeping in mcprServer's forwarded-request queue, which
# replaced a single-slot .pending_requests[[session_key]] record that silently dropped
# the older of two concurrent forwarded requests to the same session.
# Drives the private methods directly on a freshly constructed, never-started mcprServer.

# Expose internal 'the' environment so tests can read/write the daemon socket registry.
the <- MCPR:::the

.pending_tools_dir <- system.file(package = "MCPR", mustWork = TRUE)

# Build a bare JSON-RPC tools/call request envelope. enqueue_pending_request only reads
# data$id and data$params$arguments$timeout, so this is intentionally minimal.
make_request <- function(id, timeout = NULL) {
  args <- list(code = "1+1")
  if (!is.null(timeout)) {
    args$timeout <- timeout
  }
  list(
    jsonrpc = "2.0",
    id = id,
    method = "tools/call",
    params = list(name = "execute_r_code", arguments = args)
  )
}

# --- enqueue_pending_request ------------------------------------------------

test_that("enqueue_pending_request activates the first request for a session", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private

  became_active <- priv$enqueue_pending_request(make_request(id = 1L), "daemon-1")

  expect_true(became_active)
  state <- priv$.pending_requests[["daemon-1"]]
  expect_false(is.null(state$active))
  expect_equal(state$active$client_request_id, 1L)
  expect_false(is.null(state$active$sent_at))
  expect_length(state$waiting, 0L)
})

test_that("enqueue_pending_request queues a second concurrent request without a deadline", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private

  first  <- priv$enqueue_pending_request(make_request(id = 1L), "daemon-1")
  second <- priv$enqueue_pending_request(make_request(id = 2L), "daemon-1")

  expect_true(first)
  expect_false(second)

  state <- priv$.pending_requests[["daemon-1"]]
  expect_equal(state$active$client_request_id, 1L)
  expect_length(state$waiting, 1L)
  expect_equal(state$waiting[[1L]]$client_request_id, 2L)
  # Queued records are armed only when promoted - see dispatch_next(); until then
  # they must carry no sent_at, otherwise sweep_pending_requests could time them out
  # while they are not even the request being executed.
  expect_null(state$waiting[[1L]]$sent_at)
})

test_that("two overlapping requests to the same session are BOTH tracked (regression: old single-slot bug lost one)", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private

  priv$enqueue_pending_request(make_request(id = "req-A"), "daemon-9")
  priv$enqueue_pending_request(make_request(id = "req-B"), "daemon-9")

  state <- priv$.pending_requests[["daemon-9"]]
  ids <- c(state$active$client_request_id, vapply(state$waiting, `[[`, character(1), "client_request_id"))

  # The pre-fix implementation kept a single record per session_key, so the second
  # enqueue would silently overwrite (or be lost alongside) the first. Both ids must
  # be present and distinct here.
  expect_length(ids, 2L)
  expect_setequal(ids, c("req-A", "req-B"))
  expect_equal(state$active$client_request_id, "req-A")
  expect_equal(state$waiting[[1L]]$client_request_id, "req-B")
})

test_that("enqueue_pending_request respects a per-call timeout override, else falls back to server default", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir, execution_timeout_secs = 300L)
  priv <- server$.__enclos_env__$private

  priv$enqueue_pending_request(make_request(id = 1L, timeout = 15L), "daemon-1")
  priv$enqueue_pending_request(make_request(id = 2L), "daemon-1")

  state <- priv$.pending_requests[["daemon-1"]]
  expect_equal(state$active$timeout_secs, 15L)
  # Waiting record's timeout is captured at enqueue time even though it isn't armed yet.
  expect_equal(state$waiting[[1L]]$timeout_secs, 300L)
})

test_that("forwarded requests use unique internal ids even when a client id is reused", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private

  priv$enqueue_pending_request(make_request(id = "reused"), "daemon-1")
  priv$enqueue_pending_request(make_request(id = "reused"), "daemon-1")

  state <- priv$.pending_requests[["daemon-1"]]
  expect_equal(state$active$client_request_id, "reused")
  expect_equal(state$waiting[[1L]]$client_request_id, "reused")
  expect_false(identical(state$active$wire_request_id, state$waiting[[1L]]$wire_request_id))
  expect_equal(state$active$data$id, state$active$wire_request_id)
  expect_equal(state$waiting[[1L]]$data$id, state$waiting[[1L]]$wire_request_id)
})

test_that("enqueue_pending_request rejects invalid timeouts without creating state", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private
  captured <- NULL
  local_mocked_bindings(
    cat_json = function(x) { captured <<- x },
    .package = "MCPR"
  )

  accepted <- priv$enqueue_pending_request(make_request(id = 1L, timeout = 0L), "daemon-1")

  expect_true(is.na(accepted))
  expect_equal(captured$error$code, -32602L)
  expect_null(priv$.pending_requests[["daemon-1"]])
})

test_that("enqueue_pending_request applies bounded backpressure", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private
  priv$.max_waiting_per_session <- 1L
  captured <- NULL
  local_mocked_bindings(
    cat_json = function(x) { captured <<- x },
    .package = "MCPR"
  )

  expect_true(priv$enqueue_pending_request(make_request(id = 1L), "daemon-1"))
  expect_false(priv$enqueue_pending_request(make_request(id = 2L), "daemon-1"))
  rejected <- priv$enqueue_pending_request(make_request(id = 3L), "daemon-1")

  expect_true(is.na(rejected))
  expect_equal(captured$id, 3L)
  expect_equal(captured$error$code, -32000L)
  expect_length(priv$.pending_requests[["daemon-1"]]$waiting, 1L)
})

test_that("unknown forwarded tools fail before pending state is created", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private
  priv$.user_listeners[["52"]] <- TRUE
  captured <- NULL
  local_mocked_bindings(
    cat_json = function(x) { captured <<- x },
    .package = "MCPR"
  )
  request <- make_request(id = 9L)
  request$params$name <- "not_a_registered_tool"

  priv$forward_request_to_user(request, 52L, sock = "unused")

  expect_equal(captured$id, 9L)
  expect_equal(captured$error$code, -32601L)
  expect_null(priv$.pending_requests[["52"]])
})

test_that("an immediate send failure resolves active state", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private
  captured <- NULL
  local_mocked_bindings(
    cat_json = function(x) { captured <<- x },
    .package = "MCPR"
  )
  prepared <- priv$prepare_forward_request(make_request(id = 10L))
  priv$enqueue_pending_request(prepared, "daemon-1")

  sent <- priv$send_active_request("daemon-1", sock = "not-a-socket")

  expect_false(sent)
  expect_equal(captured$id, 10L)
  expect_match(captured$error$message, "Could not send request", fixed = TRUE)
  expect_null(priv$.pending_requests[["daemon-1"]])
})

test_that("session responses translate internal ids back to client ids", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private
  prepared <- priv$prepare_forward_request(make_request(id = "client-id"))
  priv$enqueue_pending_request(prepared, "daemon-1")
  wire_id <- priv$.pending_requests[["daemon-1"]]$active$wire_request_id
  emitted <- NULL
  local_mocked_bindings(
    write_stdout = function(x) { emitted <<- x },
    .package = "nanonext"
  )

  priv$handle_message_from_session(
    to_json(list(jsonrpc = "2.0", id = wire_id, result = list(value = 1L))),
    session_key = "daemon-1"
  )

  response <- jsonlite::parse_json(emitted)
  expect_equal(response$id, "client-id")
  expect_null(priv$.pending_requests[["daemon-1"]])
})

test_that("malformed session response ids are dropped without disturbing active state", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private
  prepared <- priv$prepare_forward_request(make_request(id = 13L))
  priv$enqueue_pending_request(prepared, "daemon-1")
  emitted <- NULL
  local_mocked_bindings(
    write_stdout = function(x) { emitted <<- x },
    .package = "nanonext"
  )

  priv$handle_message_from_session(
    to_json(list(jsonrpc = "2.0", id = list("not", "scalar"), result = "wrong")),
    session_key = "daemon-1"
  )

  expect_null(emitted)
  expect_false(is.null(priv$.pending_requests[["daemon-1"]]$active))
})

test_that("session responses with unowned ids are dropped without clearing active state", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private
  prepared <- priv$prepare_forward_request(make_request(id = 12L))
  priv$enqueue_pending_request(prepared, "daemon-1")
  emitted <- NULL
  local_mocked_bindings(
    write_stdout = function(x) { emitted <<- x },
    .package = "nanonext"
  )

  priv$handle_message_from_session(
    to_json(list(jsonrpc = "2.0", id = "foreign-wire-id", result = "wrong")),
    session_key = "daemon-1"
  )

  expect_null(emitted)
  expect_false(is.null(priv$.pending_requests[["daemon-1"]]$active))
})

# --- daemon_pipe_dropped -----------------------------------------------------
# Covers the SIGKILLed-worker detection path: nanonext's recv_aio never resolves
# on a peer that dies without replying, so this is the only signal that catches it.

test_that("daemon_pipe_dropped is FALSE when there is no pending state for the session", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private

  expect_false(priv$daemon_pipe_dropped("daemon-none"))
})

test_that("daemon_pipe_dropped is FALSE when the session only has a waiting record (no active request)", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private
  priv$.pending_requests[["daemon-1"]] <- list(
    active = NULL,
    waiting = list(list(client_request_id = "waiting-id"))
  )

  expect_false(priv$daemon_pipe_dropped("daemon-1"))
})

test_that("daemon_pipe_dropped is TRUE when an active request exists but the daemon socket is already gone", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private
  priv$.pending_requests[["daemon-1"]] <- list(
    active = list(client_request_id = "active-id"),
    waiting = list()
  )
  old_sockets <- the$daemon_sockets
  on.exit(the$daemon_sockets <- old_sockets, add = TRUE)
  the$daemon_sockets <- list()

  expect_true(priv$daemon_pipe_dropped("daemon-1"))
})

test_that("daemon_pipe_dropped is TRUE when the connected socket reports zero pipes (worker died without replying)", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private
  priv$.pending_requests[["daemon-1"]] <- list(
    active = list(client_request_id = "active-id"),
    waiting = list()
  )
  old_sockets <- the$daemon_sockets
  on.exit(the$daemon_sockets <- old_sockets, add = TRUE)
  the$daemon_sockets <- list("daemon-1" = "fake-socket")
  local_mocked_bindings(
    stat = function(object, which) 0L,
    .package = "nanonext"
  )

  expect_true(priv$daemon_pipe_dropped("daemon-1"))
})

test_that("daemon_pipe_dropped is FALSE when the connected socket still has open pipes", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private
  priv$.pending_requests[["daemon-1"]] <- list(
    active = list(client_request_id = "active-id"),
    waiting = list()
  )
  old_sockets <- the$daemon_sockets
  on.exit(the$daemon_sockets <- old_sockets, add = TRUE)
  the$daemon_sockets <- list("daemon-1" = "fake-socket")
  local_mocked_bindings(
    stat = function(object, which) 1L,
    .package = "nanonext"
  )

  expect_false(priv$daemon_pipe_dropped("daemon-1"))
})

test_that("daemon_pipe_dropped is FALSE when nanonext::stat errors (treated as unknown, not dropped)", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private
  priv$.pending_requests[["daemon-1"]] <- list(
    active = list(client_request_id = "active-id"),
    waiting = list()
  )
  old_sockets <- the$daemon_sockets
  on.exit(the$daemon_sockets <- old_sockets, add = TRUE)
  the$daemon_sockets <- list("daemon-1" = "fake-socket")
  local_mocked_bindings(
    stat = function(object, which) stop("boom"),
    .package = "nanonext"
  )

  expect_false(priv$daemon_pipe_dropped("daemon-1"))
})

# --- sweep_pending_requests --------------------------------------------------

test_that("sweep_pending_requests times out a stale active record but leaves a queued record alone", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private

  priv$.pending_requests[["daemon-1"]] <- list(
    active = list(
      client_request_id = "active-id",
      session_key = "daemon-1",
      data = list(id = "active-id"),
      sent_at = Sys.time() - 120,   # far in the past
      timeout_secs = 30L
    ),
    waiting = list(list(
      client_request_id = "waiting-id",
      session_key = "daemon-1",
      data = list(id = "waiting-id"),
      sent_at = NULL,               # queued records carry no deadline
      timeout_secs = 30L
    ))
  )

  captured <- list()
  local_mocked_bindings(
    cat_json = function(x) { captured[[length(captured) + 1]] <<- x },
    .package = "MCPR"
  )

  # recover_timeout is exercised through the real mcprSessionManager here: with no
  # registered binding for "daemon-1" it deterministically returns action = "marked_dead",
  # which is the same real behavior test-mcpr-server.R relies on for its sweep tests. That
  # means the queued record also gets a terminal response in this same sweep (there is no
  # surviving worker to hold it for) - but critically via the DEAD-SESSION path, never via
  # the TIMEOUT path, since it was never armed with its own deadline.
  priv$sweep_pending_requests()

  expect_length(captured, 2L)
  by_id <- stats::setNames(captured, vapply(captured, function(x) x$id, character(1)))

  # The active wire id got a genuine timeout error and was recorded so a late
  # response is dropped.
  expect_equal(by_id[["active-id"]]$error$code, -32603L)
  expect_match(by_id[["active-id"]]$error$message, "timed out after 30s", fixed = TRUE)
  expect_true("active-id" %in% priv$.terminal_wire_ids)

  # The queued id was never armed with a deadline, so it must NOT go through the timeout
  # message/bookkeeping - it is failed via the dead-session path instead (fail_pending_dead),
  # and must not pollute terminal wire ids (nothing will emit a late response for it).
  expect_match(by_id[["waiting-id"]]$error$message, "no longer responding", fixed = TRUE)
  expect_no_match(by_id[["waiting-id"]]$error$message, "timed out")
  expect_false("waiting-id" %in% priv$.terminal_wire_ids)
})

test_that("sweep_pending_requests is a no-op when only a queued record exists (no active deadline)", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private

  priv$.pending_requests[["daemon-1"]] <- list(
    active = NULL,
    waiting = list(list(
      client_request_id = "waiting-only",
      session_key = "daemon-1",
      data = list(id = "waiting-only"),
      sent_at = NULL,
      timeout_secs = 1L  # would fire immediately if (incorrectly) treated as armed
    ))
  )

  captured <- NULL
  local_mocked_bindings(
    cat_json = function(x) { captured <<- x },
    .package = "MCPR"
  )

  priv$sweep_pending_requests()

  expect_null(captured)
  # The record must still be sitting untouched in the queue.
  expect_equal(priv$.pending_requests[["daemon-1"]]$waiting[[1L]]$client_request_id, "waiting-only")
})

test_that("sweep_pending_requests migrates the waiting queue and dispatches on a recycled recovery", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private

  priv$.pending_requests[["daemon-1"]] <- list(
    active = list(
      client_request_id = "active-id",
      session_key = "daemon-1",
      data = list(id = "active-id"),
      sent_at = Sys.time() - 120,
      timeout_secs = 30L
    ),
    waiting = list(list(
      client_request_id = "waiting-id",
      session_key = "daemon-1",
      data = list(id = "waiting-id"),
      sent_at = NULL,
      timeout_secs = 30L
    ))
  )

  # Stub the session manager to report a "recycled" recovery onto a fresh key, and stub
  # dispatch_next / session_socket_for so the migration can be observed without any real
  # sockets or worker processes.
  session_manager <- priv$.session_manager
  unlockBinding("recover_timeout", session_manager)
  session_manager$recover_timeout <- function(key_or_session) {
    list(action = "recycled", old_session_id = 1L, new_session_id = 2L, key = "daemon-2")
  }
  unlockBinding("active_binding", session_manager)
  session_manager$active_binding <- function() list(key = "daemon-2")
  on.exit({
    unlockBinding("recover_timeout", session_manager)
    unlockBinding("active_binding", session_manager)
  }, add = TRUE)

  dispatched <- NULL
  unlockBinding("dispatch_next", priv)
  original_dispatch_next <- priv$dispatch_next
  on.exit({
    unlockBinding("dispatch_next", priv)
    priv$dispatch_next <- original_dispatch_next
  }, add = TRUE)
  priv$dispatch_next <- function(session_key, sock) {
    dispatched <<- session_key
  }

  local_mocked_bindings(
    cat_json = function(x) invisible(NULL),
    .package = "MCPR"
  )

  priv$sweep_pending_requests()

  # The queue must have moved from the old key to the new key, and dispatch_next
  # must have been invoked for the NEW key so the promoted request reaches the
  # fresh worker rather than being silently stranded on the dead key.
  expect_null(priv$.pending_requests[["daemon-1"]])
  expect_false(is.null(priv$.pending_requests[["daemon-2"]]))
  expect_equal(priv$.pending_requests[["daemon-2"]]$waiting[[1L]]$client_request_id, "waiting-id")
  expect_equal(dispatched, "daemon-2")
})

# --- handle_session_listener_resolved (dead path) ---------------------------

test_that("handle_session_listener_resolved dead path fails BOTH the active and a queued record", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private

  priv$.pending_requests[["daemon-7"]] <- list(
    active = list(
      client_request_id = "active-id",
      session_key = "daemon-7",
      data = list(id = "active-id"),
      sent_at = Sys.time(),
      timeout_secs = 30L
    ),
    waiting = list(list(
      client_request_id = "waiting-id",
      session_key = "daemon-7",
      data = list(id = "waiting-id"),
      sent_at = NULL,
      timeout_secs = 30L
    ))
  )

  failed_ids <- character(0)
  unlockBinding("fail_pending_dead", priv)
  original_fail <- priv$fail_pending_dead
  on.exit({
    unlockBinding("fail_pending_dead", priv)
    priv$fail_pending_dead <- original_fail
  }, add = TRUE)
  priv$fail_pending_dead <- function(record, session_key) {
    failed_ids <<- c(failed_ids, as.character(record$client_request_id))
  }
  retired_key <- NULL
  unlockBinding("retire_session_transport", priv)
  original_retire <- priv$retire_session_transport
  on.exit({
    unlockBinding("retire_session_transport", priv)
    priv$retire_session_transport <- original_retire
  }, add = TRUE)
  priv$retire_session_transport <- function(session_key) {
    retired_key <<- session_key
  }

  # Non-character data (e.g. a nanonext error object/integer) signals a dead peer.
  priv$handle_session_listener_resolved(1L, "daemon-7", "daemon")

  # Both the active record and the single queued record must be failed exactly once each.
  expect_length(failed_ids, 2L)
  expect_setequal(failed_ids, c("active-id", "waiting-id"))
  expect_equal(retired_key, "daemon-7")

  # The session entry must be fully cleared afterward - nothing left to leak or re-sweep.
  expect_null(priv$.pending_requests[["daemon-7"]])
})

test_that("handle_session_listener_resolved dead path emits two cat_json error responses via the real fail_pending_dead", {
  server <- mcprServer$new(.tools_dir = .pending_tools_dir)
  priv <- server$.__enclos_env__$private

  priv$.pending_requests[["daemon-8"]] <- list(
    active = list(
      client_request_id = 101L,
      session_key = "daemon-8",
      data = list(id = 101L),
      sent_at = Sys.time(),
      timeout_secs = 30L
    ),
    waiting = list(list(
      client_request_id = 102L,
      session_key = "daemon-8",
      data = list(id = 102L),
      sent_at = NULL,
      timeout_secs = 30L
    ))
  )

  captured <- list()
  local_mocked_bindings(
    cat_json = function(x) { captured[[length(captured) + 1]] <<- x },
    .package = "MCPR"
  )

  priv$handle_session_listener_resolved(1L, "daemon-8", "daemon")

  expect_length(captured, 2L)
  emitted_ids <- vapply(captured, function(x) as.character(x$id), character(1))
  expect_setequal(emitted_ids, c("101", "102"))
  for (resp in captured) {
    expect_equal(resp$error$code, -32603L)
    expect_match(resp$error$message, "no longer responding", fixed = TRUE)
  }
  expect_null(priv$.pending_requests[["daemon-8"]])
})
