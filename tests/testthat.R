pkgload::load_all(".", helpers = FALSE, quiet = TRUE)

library(testthat)
library(MCPR)

test_check("MCPR")
