# Unit tests for view system info functions
# Tests functions in view-system-info.R for packages and search path info

test_that("view_installed_packages returns package info", {
  result <- MCPR:::view_installed_packages(20)
  
  expect_type(result, "character")
  expect_true(grepl("Installed Packages Summary", result))
  expect_true(grepl("Total packages:", result))
  expect_true(grepl("Library paths:", result))
})

test_that("view_search_path returns search path info", {
  result <- MCPR:::view_search_path(15)
  
  expect_type(result, "character")
  expect_true(grepl("Package Search Path", result))
  expect_true(grepl("Search path entries:", result))
  expect_true(grepl(".GlobalEnv", result))
})

test_that("system info functions handle different max_lines values", {
  # Test with very small max_lines
  small_result <- MCPR:::view_installed_packages(5)
  expect_type(small_result, "character")
  expect_true(grepl("Installed Packages Summary", small_result))
  
  # Test with large max_lines
  large_result <- MCPR:::view_search_path(100)
  expect_type(large_result, "character")
  expect_true(grepl("Package Search Path", large_result))
})

test_that("system info functions handle edge cases gracefully", {
  # These should not error even with minimal inputs
  expect_no_error(MCPR:::view_installed_packages(1))
  expect_no_error(MCPR:::view_search_path(1))
})