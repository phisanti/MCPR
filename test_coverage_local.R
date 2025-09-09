#!/usr/bin/env Rscript

# Working coverage script - bypasses covr's installation issues

cat("=== Working Coverage Solution ===\n")

# Save original testthat.R
if (file.exists("tests/testthat.R")) {
  original_testthat <- readLines("tests/testthat.R")
} else {
  original_testthat <- c("pkgload::load_all('.', helpers = FALSE, quiet = TRUE)", "", "library(testthat)", "test_dir('testthat')")
}

cat("Step 1: Manual install and test approach...\n")

# Install package to temporary location
temp_lib <- file.path(tempdir(), "coverage_lib")
dir.create(temp_lib, recursive = TRUE, showWarnings = FALSE)

tryCatch({
  # Install the package
  install.packages(".", lib = temp_lib, repos = NULL, type = "source", quiet = TRUE)
  cat("Package installed successfully\n")
  
  # Try Option 2: Use pkgload but with proper coverage tracking
  cat("Step 2: Using pkgload with covr integration...\n")
  
  # Modify testthat.R to use pkgload (exposes internal functions)
  writeLines(c(
    "pkgload::load_all('.', helpers = FALSE, quiet = TRUE)",
    "",
    "library(testthat)",
    "test_dir('testthat')"
  ), "tests/testthat.R")
  
  library(covr)
  library(testthat)
  
  # Use a different covr approach that works with pkgload
  # This uses package_coverage but with custom code that ensures source tracking
  cat("Running coverage with pkgload integration...\n")
  
  # Try the working approach: package_coverage with explicit test running
  cov_results <- tryCatch({
    covr::package_coverage(
      path = ".",
      type = "tests",
      quiet = FALSE
    )
  }, error = function(e) {
    cat("Standard package_coverage failed, trying alternative...\n")
    # Alternative: Use package_coverage with clean = FALSE
    covr::package_coverage(
      path = ".",
      type = "tests", 
      clean = FALSE,
      quiet = FALSE
    )
  })
  
  cat("Coverage calculation completed!\n")
  cat("Number of coverage entries:", length(cov_results), "\n")
  
  if (length(cov_results) > 0) {
    pct_coverage <- covr::percent_coverage(cov_results)
    cat("Coverage percentage:", round(pct_coverage, 2), "%\n")
    
    # Show some results
    cat("Sample coverage data:\n")
    cov_df <- as.data.frame(cov_results)
    if (nrow(cov_df) > 0) {
      print(head(cov_df[, c("filename", "functions", "value")], 10))
    }
    
    # Generate cobertura format
    cat("Generating cobertura XML...\n")
    covr::to_cobertura(cov_results, filename = "coverage.xml")
    
    if (file.exists("coverage.xml")) {
      cat("SUCCESS: coverage.xml generated!\n")
      file_size <- file.size("coverage.xml")
      cat("File size:", file_size, "bytes\n")
    }
    
  } else {
    cat("No coverage data generated\n")
  }
  
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  print(e)
}, finally = {
  # Clean up
  writeLines(original_testthat, "tests/testthat.R")
  cat("Restored original testthat.R\n")
})

cat("\n=== Test completed ===\n")