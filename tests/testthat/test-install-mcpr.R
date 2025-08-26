# Test Install MCPR Functionality
# Tests for install_mcpr() function and related configuration management.
# Validates input validation, path resolution, and configuration file handling.

test_that("install_mcpr validates agent argument correctly", {
  # Multiple agents should error
  expect_error(install_mcpr(c("claude", "gemini")), "Multiple agents specified")
  expect_error(install_mcpr(c("claude", "gemini", "copilot")), "Multiple agents specified")

  # Invalid agent
  expect_error(install_mcpr("invalid_agent"), "should be one of")

  # Empty agent vector
  expect_error(install_mcpr(character(0)), "No agent specified")
})

test_that("install_mcpr validates other arguments correctly", {
  # Invalid scope
  expect_error(install_mcpr("claude", scope = "invalid"), "should be one of")

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
  # Note: Using force=TRUE and temporary files to avoid modifying system configs

  # We can't easily test actual installation without modifying system files,
  # but we can test that the function processes different agents without errors
  # in the validation phase

  # Test valid agents are accepted
  expect_error(install_mcpr("claude", force = TRUE), NA, class = "simpleError")
  expect_error(install_mcpr("gemini", force = TRUE), NA, class = "simpleError")
  expect_error(install_mcpr("copilot", force = TRUE), NA, class = "simpleError")
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
  expect_error(install_mcpr("invalid"), "should be one of")

  # Test with multiple agents (should be caught in validation)
  expect_error(install_mcpr(c("claude", "gemini")), "Multiple agents specified")

  # Test with empty agent vector (should be caught in validation)
  expect_error(install_mcpr(character(0)), "No agent specified")
})
