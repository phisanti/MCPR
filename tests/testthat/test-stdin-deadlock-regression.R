# Stdin Deadlock Subprocess Regression Test
# Guards the macOS stdio FILE-lock deadlock fix end-to-end: a real MCPR server subprocess
# must answer a tools/call whose R code yields the event loop (Sys.sleep) instead of wedging.
# Spawns MCPR::mcpr_server() via processx (mirrors test-mcpr-server-daemon.R's real-subprocess
# pattern) and drives a real MCP JSON-RPC handshake over stdio with a hard wall-clock timeout.

test_that("server subprocess answers a Sys.sleep-yielding tools/call within a bounded timeout (no deadlock)", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("processx")

  proc <- processx::process$new(
    file.path(R.home("bin"), "Rscript"),
    c("-e", "MCPR::mcpr_server()"),
    stdin = "|", stdout = "|", stderr = "|"
  )
  on.exit({
    if (proc$is_alive()) proc$kill()
  }, add = TRUE)

  # Line-delimited JSON-RPC 2.0 over stdio: one JSON object per line, read a line back
  # with a bounded poll loop so a real regression (the reader wedged on the stdin FILE
  # lock) FAILS the test instead of hanging the test runner.
  send_line <- function(obj) {
    proc$write_input(paste0(jsonlite::toJSON(obj, auto_unbox = TRUE), "\n"))
  }

  # Poll stdout for the next complete line up to timeout_secs, discarding blank reads.
  # Returns the parsed JSON-RPC message, or NULL if nothing arrived in time.
  read_response <- function(timeout_secs) {
    deadline <- Sys.time() + timeout_secs
    buffer <- ""
    while (Sys.time() < deadline) {
      if (!proc$is_alive()) {
        return(NULL)
      }
      proc$poll_io(200)
      chunk <- proc$read_output_lines()
      if (length(chunk) > 0) {
        for (line in chunk) {
          if (nzchar(trimws(line))) {
            return(tryCatch(jsonlite::parse_json(line), error = function(e) NULL))
          }
        }
      }
    }
    NULL
  }

  # 1. MCP initialize handshake.
  send_line(list(
    jsonrpc = "2.0", id = 1, method = "initialize",
    params = list(
      protocolVersion = "2024-11-05",
      capabilities = list(),
      clientInfo = list(name = "mcpr-deadlock-regression-test", version = "0.0.1")
    )
  ))
  init_resp <- read_response(20)
  expect_false(is.null(init_resp), label = "server did not respond to initialize within 20s")
  expect_true(is.null(init_resp$error))

  # 2. notifications/initialized (no response expected, but must not wedge the loop).
  send_line(list(jsonrpc = "2.0", method = "notifications/initialized"))

  # 3. tools/call for execute_r_code with code that yields the event loop mid-flight.
  # This is the exact shape of the deadlock: nanonext::read_stdin()'s fgetc/flockfile
  # thread would block R's main thread on the stdin FILE lock while Sys.sleep() ran,
  # so the response would never arrive. The native reader (raw read(2), no FILE lock)
  # must let this round-trip complete well inside the bound below.
  send_line(list(
    jsonrpc = "2.0", id = 2, method = "tools/call",
    params = list(
      name = "execute_r_code",
      arguments = list(code = 'Sys.sleep(1); cat("MCPRSEQ:OK\\n")')
    )
  ))

  call_resp <- read_response(20)

  expect_false(is.null(call_resp), label = "server deadlocked: no response to sleeping tools/call within 20s")
  if (!is.null(call_resp)) {
    expect_equal(call_resp$id, 2)
    expect_true(is.null(call_resp$error))
    text <- call_resp$result$content[[1]]$text
    expect_match(text, "MCPRSEQ:OK", fixed = TRUE)
  }
})
