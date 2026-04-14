# Tests for daemon session utilities
# Tests registry functions and daemon lifecycle management.
# Verifies spawn, register, unregister, and lookup operations.

library(MCPR)

# Expose internal 'the' environment so tests can read/write state directly
the <- MCPR:::the

# --- Registry Tests (no socket/process needed) ---

test_that("register_daemon adds entry to the$daemon_sessions", {
  # Save and restore state
  old_sessions <- the$daemon_sessions
  old_sockets <- the$daemon_sockets
  old_processes <- the$daemon_processes
  on.exit({
    the$daemon_sessions <- old_sessions
    the$daemon_sockets <- old_sockets
    the$daemon_processes <- old_processes
  }, add = TRUE)

  the$daemon_sessions <- integer(0)
  the$daemon_sockets <- list()
  the$daemon_processes <- list()

  MCPR:::register_daemon("agent-a", 5L)
  expect_equal(the$daemon_sessions, c("agent-a" = 5L))
})

test_that("get_daemon_session returns session ID for registered client", {
  old_sessions <- the$daemon_sessions
  on.exit(the$daemon_sessions <- old_sessions, add = TRUE)

  the$daemon_sessions <- c("agent-a" = 5L, "agent-b" = 6L)

  expect_equal(MCPR:::get_daemon_session("agent-a"), 5L)
  expect_equal(MCPR:::get_daemon_session("agent-b"), 6L)
  expect_null(MCPR:::get_daemon_session("agent-c"))
})

test_that("list_daemon_sessions returns the registry", {
  old_sessions <- the$daemon_sessions
  on.exit(the$daemon_sessions <- old_sessions, add = TRUE)

  the$daemon_sessions <- c("agent-a" = 5L, "agent-b" = 6L)
  result <- MCPR:::list_daemon_sessions()
  expect_equal(result, c("agent-a" = 5L, "agent-b" = 6L))
})

test_that("list_daemon_sessions returns empty when no daemons", {
  old_sessions <- the$daemon_sessions
  on.exit(the$daemon_sessions <- old_sessions, add = TRUE)

  the$daemon_sessions <- integer(0)
  result <- MCPR:::list_daemon_sessions()
  expect_length(result, 0)
  expect_type(result, "integer")
})

test_that("unregister_daemon removes from all registries", {
  old_sessions <- the$daemon_sessions
  old_sockets <- the$daemon_sockets
  old_processes <- the$daemon_processes
  on.exit({
    the$daemon_sessions <- old_sessions
    the$daemon_sockets <- old_sockets
    the$daemon_processes <- old_processes
  }, add = TRUE)

  the$daemon_sessions <- c("agent-a" = 5L, "agent-b" = 6L)
  the$daemon_sockets <- list("agent-a" = NULL, "agent-b" = NULL)
  the$daemon_processes <- list("agent-a" = NULL, "agent-b" = NULL)

  MCPR:::unregister_daemon("agent-a")

  expect_equal(the$daemon_sessions, c("agent-b" = 6L))
  expect_null(the$daemon_sockets[["agent-a"]])
  expect_null(the$daemon_processes[["agent-a"]])
  # agent-b should still be there

  expect_equal(the$daemon_sockets[["agent-b"]], NULL)
  expect_true("agent-b" %in% names(the$daemon_sockets))
})

test_that("unregister_daemon is idempotent on unknown client_id", {
  old_sessions <- the$daemon_sessions
  old_sockets <- the$daemon_sockets
  old_processes <- the$daemon_processes
  on.exit({
    the$daemon_sessions <- old_sessions
    the$daemon_sockets <- old_sockets
    the$daemon_processes <- old_processes
  }, add = TRUE)

  the$daemon_sessions <- integer(0)
  the$daemon_sockets <- list()
  the$daemon_processes <- list()

  # Should not error
  expect_no_error(MCPR:::unregister_daemon("nonexistent"))
})

test_that("daemon_process_label returns correct format", {
  expect_equal(MCPR:::daemon_process_label(5L), "MCPR-5")
  expect_equal(MCPR:::daemon_process_label(42L), "MCPR-42")
})

test_that("register_daemon then unregister_daemon round-trips cleanly", {
  old_sessions <- the$daemon_sessions
  old_sockets <- the$daemon_sockets
  old_processes <- the$daemon_processes
  on.exit({
    the$daemon_sessions <- old_sessions
    the$daemon_sockets <- old_sockets
    the$daemon_processes <- old_processes
  }, add = TRUE)

  the$daemon_sessions <- integer(0)
  the$daemon_sockets <- list()
  the$daemon_processes <- list()

  MCPR:::register_daemon("test-client", 10L)
  expect_equal(MCPR:::get_daemon_session("test-client"), 10L)

  MCPR:::unregister_daemon("test-client")
  expect_null(MCPR:::get_daemon_session("test-client"))
  expect_length(the$daemon_sessions, 0)
})

# --- find_daemon_port tests ---

test_that("find_daemon_port returns a valid integer port", {
  port <- MCPR:::find_daemon_port()
  expect_type(port, "integer")
  expect_true(port >= 1L && port <= 1023L)
})

test_that("find_daemon_port returns different ports on successive calls when port is occupied", {
  # Occupy a port with a listener, then verify find_daemon_port skips it
  sock <- nanonext::socket("poly")
  on.exit(nanonext::reap(sock), add = TRUE)

  socket_base <- MCPR:::get_system_socket_url()
  port1 <- MCPR:::find_daemon_port()

  # Bind to that port
  nanonext::listen(sock, url = sprintf("%s%d", socket_base, port1))

  # Next call should return a different port
  port2 <- MCPR:::find_daemon_port()
  expect_true(port2 != port1)
})

# --- spawn_daemon / await_daemon_ready integration tests ---
# These actually spawn R processes and need cleanup

test_that("spawn_daemon creates a process and await_daemon_ready connects", {
  skip_on_cran()
  skip_if_not_installed("processx")

  old_sessions <- the$daemon_sessions
  old_sockets <- the$daemon_sockets
  old_processes <- the$daemon_processes
  on.exit({
    the$daemon_sessions <- old_sessions
    the$daemon_sockets <- old_sockets
    the$daemon_processes <- old_processes
  }, add = TRUE)

  the$daemon_sessions <- integer(0)
  the$daemon_sockets <- list()
  the$daemon_processes <- list()

  # Find a free port
  port <- MCPR:::find_daemon_port()
  client_id <- "test-spawn"

  # Spawn daemon
  proc <- MCPR:::spawn_daemon(client_id, port, working_dir = getwd())
  on.exit({
    if (proc$is_alive()) proc$kill()
  }, add = TRUE)

  expect_true(inherits(proc, "process"))
  expect_true(proc$is_alive())
  expect_true(client_id %in% names(the$daemon_processes))

  # Wait for daemon - returns connected socket or NULL
  sock <- MCPR:::await_daemon_ready(port, timeout_ms = 15000)
  on.exit(if (!is.null(sock)) nanonext::reap(sock), add = TRUE)

  expect_false(is.null(sock))
  expect_true(nanonext::stat(sock, "pipes") > 0L)

  # Clean up
  proc$kill()
})

test_that("await_daemon_ready returns NULL on timeout for non-existent session", {
  skip_on_cran()

  sock <- MCPR:::await_daemon_ready(999L, timeout_ms = 1000)
  expect_null(sock)
})

test_that("find_daemon_key_by_session finds key by session ID value", {
  old_sessions <- the$daemon_sessions
  on.exit(the$daemon_sessions <- old_sessions, add = TRUE)

  the$daemon_sessions <- c("default" = 5L, "daemon-6" = 6L)

  expect_equal(MCPR:::find_daemon_key_by_session(5L), "default")
  expect_equal(MCPR:::find_daemon_key_by_session(6L), "daemon-6")
  expect_null(MCPR:::find_daemon_key_by_session(99L))
})
