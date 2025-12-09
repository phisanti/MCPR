# Tests for minimal TOML helpers used by install_mcpr

test_that("read_toml_config returns empty MCP list when file is missing", {
  tmp <- tempfile(fileext = ".toml")
  if (file.exists(tmp)) unlink(tmp)

  config <- MCPR:::read_toml_config(tmp)

  expect_true(is.list(config$mcp))
  expect_length(config$mcp, 0)
})

test_that("read_toml_config parses MCP sections with strings, arrays, and inline tables", {
  tmp <- tempfile(fileext = ".toml")
  toml_lines <- c(
    "[mcp.mcpr]",
    'command = "R"',
    'args = ["--quiet", "--slave", "-e", "MCPR::mcpr_server()"]',
    'env = { PATH = "/tmp", KEY = "VALUE" }',
    "",
    "[other]",
    'keep = "yes"'
  )

  writeLines(toml_lines, tmp)

  config <- MCPR:::read_toml_config(tmp)
  mcpr <- config$mcp$mcpr

  expect_equal(mcpr$command, "R")
  expect_equal(mcpr$args, c("--quiet", "--slave", "-e", "MCPR::mcpr_server()"))
  expect_equal(mcpr$env$PATH, "/tmp")
  expect_equal(mcpr$env$KEY, "VALUE")

  unlink(tmp)
})

test_that("write_toml_config preserves non-MCP sections", {
  tmp <- tempfile(fileext = ".toml")
  writeLines(c("[other]", 'key = "value"'), tmp)

  config <- list(
    mcp = list(
      mcpr = list(
        command = "R",
        args = c("--quiet", "-e", "MCPR::mcpr_server()")
      )
    )
  )

  MCPR:::write_toml_config(config, tmp)

  lines <- readLines(tmp)
  expect_true(any(grepl("^\\[other\\]", lines)))
  expect_true(any(grepl('^key = "value"', lines)))
  expect_true(any(grepl("^\\[mcp\\.mcpr\\]", lines)))
  expect_true(any(grepl('^command = "R"', lines)))

  unlink(tmp)
})

test_that("write_toml_config and read_toml_config round-trip MCP config", {
  tmp <- tempfile(fileext = ".toml")
  config <- list(
    mcp = list(
      mcpr = list(
        command = "R",
        args = c("--quiet", "--slave"),
        env = list(PATH = "/tmp", KEY = "VALUE")
      )
    )
  )

  MCPR:::write_toml_config(config, tmp)
  round_trip <- MCPR:::read_toml_config(tmp)
  mcpr <- round_trip$mcp$mcpr

  expect_equal(mcpr$command, "R")
  expect_equal(mcpr$args, c("--quiet", "--slave"))
  expect_equal(mcpr$env$PATH, "/tmp")
  expect_equal(mcpr$env$KEY, "VALUE")

  unlink(tmp)
})
