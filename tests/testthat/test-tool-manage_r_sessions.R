# Manage R Sessions Tool Shim
# Verifies the discoverable tool body stays a server-owned control shim.
# Runtime behavior is covered through mcprServer and mcprSessionManager tests.

library(MCPR)
source(system.file("tool-manage_r_sessions.R", package = "MCPR", mustWork = TRUE))

test_that("manage_r_sessions validates action parameter", {
  expect_error(
    manage_r_sessions(action = "invalid"),
    "action must be one of: 'list', 'join', 'start', 'detach', 'close'"
  )
})

test_that("manage_r_sessions validates session requirements before shim error", {
  expect_error(
    manage_r_sessions(action = "join"),
    "session parameter is required when action='join'"
  )
  expect_error(
    manage_r_sessions(action = "join", session = "not_numeric"),
    "session must be a single integer"
  )
  expect_error(
    manage_r_sessions(action = "close"),
    "session parameter is required when action='close'"
  )
})

test_that("manage_r_sessions direct execution is a server-owned shim", {
  expect_error(
    manage_r_sessions(action = "list"),
    "handled by mcprServer"
  )
  expect_error(
    manage_r_sessions(action = "start"),
    "handled by mcprServer"
  )
  expect_error(
    manage_r_sessions(action = "detach"),
    "handled by mcprServer"
  )
  expect_error(
    manage_r_sessions(action = "stop", session = 1L),
    "handled by mcprServer"
  )
})
