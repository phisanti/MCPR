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

test_that("view_installed_packages shows package categories", {
  result <- MCPR:::view_installed_packages(100)

  # Should show base or priority packages
  expect_true(grepl("Base R packages|packages", result))

  # Should show total count
  expect_true(grepl("Total packages: \\d+", result))

  # Should show library paths with counts
  expect_true(grepl("Library paths: \\d+", result))
})

test_that("view_search_path shows detailed search info", {
  result <- MCPR:::view_search_path(100)

  # Should identify GlobalEnv specifically
  expect_true(grepl(".GlobalEnv.*user workspace", result))

  # Should show namespace summary
  expect_true(grepl("Namespace Summary", result))
  expect_true(grepl("Attached packages:", result))
  expect_true(grepl("Loaded only.*not attached", result))

  # Should show tips
  expect_true(grepl("Tips:", result))
})

test_that("view_search_path handles conflicts", {
  result <- MCPR:::view_search_path(50)

  # Should attempt to detect conflicts
  expect_true(grepl("conflict", result, ignore.case = TRUE))
})

test_that("system info functions handle edge cases gracefully", {
  # These should not error even with minimal inputs
  expect_no_error(MCPR:::view_installed_packages(1))
  expect_no_error(MCPR:::view_search_path(1))
})
