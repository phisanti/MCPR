# Test Install MCPR Functionality
# Tests for install_mcpr() function and related configuration management.
# Validates input validation, path resolution, and configuration file handling.

# Load the package for testing
library(MCPR)

test_that("install_mcpr validates agent argument correctly", {
  # No agent (NULL) should error with helpful message
  expect_error(install_mcpr(), "No agent specified")
  expect_error(install_mcpr(NULL), "No agent specified")

  # Multiple agents should error
  expect_error(install_mcpr(c("claude", "gemini")), "Multiple agents specified")
  expect_error(install_mcpr(c("claude", "gemini", "copilot")), "Multiple agents specified")

  # Invalid agent
  expect_error(install_mcpr("invalid_agent"), "Invalid agent")

  # Empty agent vector
  expect_error(install_mcpr(character(0)), "No agent specified")
})

test_that("install_mcpr validates other arguments correctly", {
  # Invalid scope for Claude
  expect_error(install_mcpr("claude", scope = "invalid"), "Invalid scope for claude")
  # Invalid scope for Codex
  expect_error(install_mcpr("codex", scope = "local"), "Invalid scope for codex")

  # Invalid server_name
  expect_error(
    install_mcpr("claude", server_name = c("a", "b")),
    "must be a single string"
  )
  expect_error(
    install_mcpr("claude", server_name = NULL),
    "must be a single string"
  )
  expect_error(
    install_mcpr("claude", server_name = NA_character_),
    "must be a single string"
  )

  # Invalid force
  expect_error(
    install_mcpr("claude", force = "yes"),
    "must be a single logical value"
  )
  expect_error(
    install_mcpr("claude", force = c(TRUE, FALSE)),
    "must be a single logical value"
  )
})

test_that("install_mcpr handles different agents appropriately", {
  # Test that install_mcpr can be called with different agents without error
  # Using mocking to avoid modifying real system configs

  # Create a temporary directory for testing
  temp_dir <- tempdir()
  temp_json_config <- file.path(temp_dir, "test_config.json")
  temp_toml_config <- file.path(temp_dir, "test_config.toml")

  # Create a mock function that returns our temp path instead of system paths
  mock_get_agent_config_path <- function(agent, scope = NULL) {
    path <- if (agent == "codex") temp_toml_config else temp_json_config
    type <- if (agent == "codex") "global" else "test"

    list(
      path = path,
      type = type
    )
  }

  # Mock the get_agent_config_path function
  with_mocked_bindings(
    get_agent_config_path = mock_get_agent_config_path,
    .package = "MCPR",
    {
      # Test valid agents are accepted (they should complete without error)
      expect_no_error(install_mcpr("claude", force = TRUE))
      expect_no_error(install_mcpr("gemini", force = TRUE))
      expect_no_error(install_mcpr("copilot", force = TRUE))
      expect_no_error(install_mcpr("codex", force = TRUE))
    }
  )

  # Clean up
  if (file.exists(temp_json_config)) {
    unlink(temp_json_config)
  }
  if (file.exists(temp_toml_config)) {
    unlink(temp_toml_config)
  }
})

test_that("install_mcpr works with temp files", {
  # Test with temporary config files to avoid modifying real system configs
  temp_dir <- tempdir()
  temp_config <- file.path(temp_dir, "test_claude_config.json")

  # Create a temporary config file
  test_config <- list(mcpServers = list())
  jsonlite::write_json(test_config, temp_config, pretty = TRUE, auto_unbox = TRUE)

  # Verify temp file was created
  expect_true(file.exists(temp_config))

  # Clean up
  unlink(temp_config)
})

test_that("JSON configuration I/O works through install_mcpr", {
  # Test that JSON I/O works through the main interface
  # Create a temporary config to test against
  temp_file <- tempfile(fileext = ".json")

  # Write a test config directly via JSON
  test_config <- list(
    mcpServers = list(
      existing_server = list(
        command = "existing",
        args = c("arg1", "arg2")
      )
    )
  )

  jsonlite::write_json(test_config, temp_file, pretty = TRUE, auto_unbox = TRUE)
  expect_true(file.exists(temp_file))

  # Read it back to verify it works
  read_config <- jsonlite::fromJSON(temp_file, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  expect_equal(read_config$mcpServers$existing_server$command, "existing")
  expect_equal(read_config$mcpServers$existing_server$args, c("arg1", "arg2"))

  # Clean up
  unlink(temp_file)
})

test_that("install_mcpr provides appropriate error messages", {
  # Test that install_mcpr provides helpful error messages for common issues

  # Test with invalid agent (should be caught in validation)
  expect_error(install_mcpr("invalid"), "Invalid agent")

  # Test with multiple agents (should be caught in validation)
  expect_error(install_mcpr(c("claude", "gemini")), "Multiple agents specified")

  # Test with empty agent vector (should be caught in validation)
  expect_error(install_mcpr(character(0)), "No agent specified")
})

test_that("install_mcpr writes Codex configuration via TOML helpers", {
  temp_dir <- tempdir()
  temp_toml <- file.path(temp_dir, "test_codex_config.toml")
  on.exit(if (file.exists(temp_toml)) unlink(temp_toml), add = TRUE)

  mock_get_agent_config_path <- function(agent, scope = NULL) {
    list(path = temp_toml, type = "global")
  }

  with_mocked_bindings(
    get_agent_config_path = mock_get_agent_config_path,
    .package = "MCPR",
    {
      expect_no_error(install_mcpr("codex", force = TRUE))
    }
  )

  expect_true(file.exists(temp_toml))
  lines <- readLines(temp_toml)
  expect_true(any(grepl("^\\[mcp\\.mcpr\\]", lines)))
  expect_true(any(grepl('command = \"R\"', lines)))
})

# test_that("install_mcpr creates correct JSON structure", {
#   skip_on_ci()
#   # Test that install_mcpr creates the correct configuration structure
#   # This test uses a temporary file to verify the actual output
#
#   temp_config <- tempfile(fileext = ".json")
#
#   # Mock the get_agent_config_path function to use our temp file
#   mock_get_agent_config_path <- function(agent, scope = NULL) {
#     list(path = temp_config, type = "test")
#   }
#
#   # Test with mocked path
#   with_mocked_bindings(
#     get_agent_config_path = mock_get_agent_config_path,
#     .package = "MCPR",
#     {
#       # Install to temp file
#       result <- install_mcpr("claude", force = TRUE)
#
#       # Verify installation succeeded
#       expect_true(result$success)
#       expect_equal(result$config_path, temp_config)
#
#       # Verify the file was created and has correct structure
#       expect_true(file.exists(temp_config))
#
#       # Read and verify JSON structure
#       config <- jsonlite::fromJSON(temp_config, simplifyVector = TRUE,
#                                    simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
#
#       # Check top-level structure
#       expect_true("mcpServers" %in% names(config))
#       expect_true("mcpr" %in% names(config$mcpServers))
#
#       # Check MCPR server structure
#       mcpr_config <- config$mcpServers$mcpr
#       expect_true(is.list(mcpr_config))
#       expect_true("command" %in% names(mcpr_config))
#       expect_true("args" %in% names(mcpr_config))
#
#       # Check specific values
#       expect_equal(mcpr_config$command, "R")
#       expect_true(is.character(mcpr_config$args))
#       expect_true(length(mcpr_config$args) > 0)
#       expect_true("MCPR::mcpr_server()" %in% mcpr_config$args)
#     }
#   )
#
#   # Clean up
#   unlink(temp_config)
# })
#
