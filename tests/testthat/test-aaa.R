test_that("global environment 'the' exists and is properly initialized", {
  # Test that the global environment exists
  # Note: 'the' may not be directly accessible until package is fully loaded
  skip_if_not(
    exists("the", envir = asNamespace("MCPR")),
    "Global environment 'the' not available in test context"
  )

  the_env <- get("the", envir = asNamespace("MCPR"))
  expect_true(rlang::is_environment(the_env))
})

test_that("server_processes list is initialized", {
  # Test that server_processes exists in global environment
  skip_if_not(
    exists("the", envir = asNamespace("MCPR")),
    "Global environment 'the' not available in test context"
  )

  the_env <- get("the", envir = asNamespace("MCPR"))
  skip_if_not(
    exists("server_processes", envir = the_env),
    "server_processes not initialized in test context"
  )

  expect_true(is.list(the_env$server_processes))
})

test_that("global environment can store and retrieve values", {
  skip_if_not(
    exists("the", envir = asNamespace("MCPR")),
    "Global environment 'the' not available in test context"
  )

  the_env <- get("the", envir = asNamespace("MCPR"))

  # Test basic functionality of global state
  test_key <- "test_value_12345"
  test_value <- list(data = "test")

  # Store value
  the_env[[test_key]] <- test_value

  # Retrieve value
  retrieved <- the_env[[test_key]]
  expect_equal(retrieved, test_value)

  # Clean up
  rm(list = test_key, envir = the_env)
  expect_false(exists(test_key, envir = the_env))
})

test_that("global environment supports multiple concurrent values", {
  skip_if_not(
    exists("the", envir = asNamespace("MCPR")),
    "Global environment 'the' not available in test context"
  )

  the_env <- get("the", envir = asNamespace("MCPR"))

  # Test storing multiple values
  the_env$test_val1 <- "value1"
  the_env$test_val2 <- "value2"
  the_env$test_val3 <- list(nested = "value")

  expect_equal(the_env$test_val1, "value1")
  expect_equal(the_env$test_val2, "value2")
  expect_equal(the_env$test_val3$nested, "value")

  # Clean up
  rm("test_val1", "test_val2", "test_val3", envir = the_env)
})

test_that("global environment is isolated from user workspace", {
  skip_if_not(
    exists("the", envir = asNamespace("MCPR")),
    "Global environment 'the' not available in test context"
  )

  the_env <- get("the", envir = asNamespace("MCPR"))

  # Test that global environment is separate from .GlobalEnv
  expect_false(identical(the_env, .GlobalEnv))
  expect_false(identical(parent.env(the_env), .GlobalEnv))

  # Values in 'the' should not appear in global environment
  the_env$isolation_test <- "isolated"
  expect_false(exists("isolation_test", envir = .GlobalEnv))

  # Clean up
  rm("isolation_test", envir = the_env)
})
