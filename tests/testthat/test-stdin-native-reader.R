# Native Stdin Reader Regression Tests
# Guards the lock-free stdin reader (src/mcpr-stdin.c) and parent-death watchdog
# (src/mcpr-watchdog.c) that replaced nanonext::read_stdin() to fix the macOS FILE-lock deadlock.
# Only exercises non-blocking, deterministic entry points - never feeds or blocks on real stdin.

# The compiled routines are registered via R_registerRoutines/R_useDynamicSymbols(FALSE) in
# src/init.c, so a plain .Call(..., PACKAGE = "MCPR") is the right way to reach them from R.
skip_if_stdin_native_unavailable <- function() {
  dll <- tryCatch(getLoadedDLLs()[["MCPR"]], error = function(e) NULL)
  skip_if(is.null(dll), "MCPR native DLL is not loaded")
}

test_that("mcpr_stdin_start, mcpr_stdin_poll, mcpr_stdin_stop are registered and callable", {
  skip_if_stdin_native_unavailable()

  # Starting the reader spins up a native thread reading the stdin OS handle.
  # Under `Rscript testthat.R` (and CI runners) stdin is not a live TTY, so this is safe:
  # the thread parks in raw input until mcpr_stdin_stop cancels and joins it.
  expect_true(.Call("mcpr_stdin_start", PACKAGE = "MCPR"))
  on.exit(.Call("mcpr_stdin_stop", PACKAGE = "MCPR"), add = TRUE)

  # Idempotent: calling start a second time must not error or spawn a second thread.
  expect_true(.Call("mcpr_stdin_start", PACKAGE = "MCPR"))

  # Non-blocking poll (timeout_ms = 0L) must return immediately, never hang. This is the
  # core deadlock guard at the unit level: a zero-timeout poll can never wait on the stdin
  # FILE lock because the reader never touches it (raw read(2)). The result is either NULL
  # (queue empty, no EOF yet) or FALSE (EOF sentinel) depending on whether the test runner's
  # stdin is a live pipe or already closed - both are valid non-blocking outcomes; what must
  # never happen is a hang or an unhandled error reaching this line.
  result <- .Call("mcpr_stdin_poll", 0L, PACKAGE = "MCPR")
  expect_true(is.null(result) || identical(result, FALSE))

  expect_no_error(.Call("mcpr_stdin_stop", PACKAGE = "MCPR"))

  # Stop is idempotent too.
  expect_no_error(.Call("mcpr_stdin_stop", PACKAGE = "MCPR"))
})

test_that("mcpr_stdin_poll returns NULL (not blocking) for a short bounded timeout with an empty queue", {
  skip_if_stdin_native_unavailable()

  .Call("mcpr_stdin_start", PACKAGE = "MCPR")
  on.exit(.Call("mcpr_stdin_stop", PACKAGE = "MCPR"), add = TRUE)

  # A short, bounded wait (10ms) must return within a small multiple of that bound - this
  # proves the poll loop is a real timed wait (pthread_cond_timedwait), not an indefinite
  # block. Result is NULL (timeout, queue still empty) or FALSE (EOF observed meanwhile);
  # see the non-blocking test above for why both are acceptable in this harness.
  start <- Sys.time()
  result <- .Call("mcpr_stdin_poll", 10L, PACKAGE = "MCPR")
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))

  expect_true(is.null(result) || identical(result, FALSE))
  expect_lt(elapsed, 2)
})

test_that("mcpr_watchdog_start and stop are registered, callable, and idempotent", {
  skip_if_stdin_native_unavailable()

  # Idempotent by design (see mcpr-watchdog.c): a second call while already
  # started is a documented no-op, so this is safe to call from a test process.
  # Use this process's own pid so the watchdog's liveness checks are trivially
  # satisfied throughout the test run (it never observes an orphan condition).
  expect_true(.Call("mcpr_watchdog_start", as.integer(Sys.getpid()), 250L, PACKAGE = "MCPR"))
  on.exit(.Call("mcpr_watchdog_stop", PACKAGE = "MCPR"), add = TRUE)
  expect_true(.Call("mcpr_watchdog_start", as.integer(Sys.getpid()), 250L, PACKAGE = "MCPR"))
  expect_no_error(.Call("mcpr_watchdog_stop", PACKAGE = "MCPR"))
  expect_no_error(.Call("mcpr_watchdog_stop", PACKAGE = "MCPR"))
})
