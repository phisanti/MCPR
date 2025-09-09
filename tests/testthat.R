pkgload::load_all(".", helpers = FALSE, quiet = TRUE)

library(testthat)
test_dir("tests/testthat")
