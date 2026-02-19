# Unit tests for object inspection functions
# Tests individual inspector functions in inspect-object.R

# ---- inspect_dispatch ----

test_that("inspect_dispatch routes data.frame correctly", {
  result <- MCPR:::inspect_dispatch(mtcars, "mtcars")
  expect_type(result, "character")
  expect_true(grepl("Object: mtcars", result))
  expect_true(grepl("Data frame", result))
})

test_that("inspect_dispatch routes numeric vector correctly", {
  x <- 1:10
  result <- MCPR:::inspect_dispatch(x, "my_vec")
  expect_type(result, "character")
  expect_true(grepl("Object: my_vec", result))
  expect_true(grepl("Vector", result))
})

test_that("inspect_dispatch routes character vector correctly", {
  x <- letters[1:5]
  result <- MCPR:::inspect_dispatch(x, "my_chars")
  expect_type(result, "character")
  expect_true(grepl("Object: my_chars", result))
  expect_true(grepl("Vector", result))
  expect_true(grepl("character", result))
})

test_that("inspect_dispatch routes factor correctly", {
  x <- factor(c("a", "b", "a"))
  result <- MCPR:::inspect_dispatch(x, "my_fac")
  expect_type(result, "character")
  expect_true(grepl("Object: my_fac", result))
  expect_true(grepl("Factor", result))
})

test_that("inspect_dispatch routes function correctly", {
  x <- function(a, b = 1) a + b
  result <- MCPR:::inspect_dispatch(x, "my_fn")
  expect_type(result, "character")
  expect_true(grepl("Object: my_fn", result))
  expect_true(grepl("Function", result))
})

test_that("inspect_dispatch routes list correctly", {
  x <- list(a = 1, b = "hello")
  result <- MCPR:::inspect_dispatch(x, "my_list")
  expect_type(result, "character")
  expect_true(grepl("Object: my_list", result))
  expect_true(grepl("List", result))
})

test_that("inspect_dispatch routes matrix correctly", {
  x <- matrix(1:6, nrow = 2)
  result <- MCPR:::inspect_dispatch(x, "my_mat")
  expect_type(result, "character")
  expect_true(grepl("Object: my_mat", result))
  expect_true(grepl("Matrix", result))
})

test_that("inspect_dispatch routes formula correctly", {
  x <- y ~ x1 + x2
  result <- MCPR:::inspect_dispatch(x, "my_form")
  expect_type(result, "character")
  expect_true(grepl("Object: my_form", result))
  expect_true(grepl("Formula", result))
})

test_that("inspect_dispatch routes Date correctly", {
  x <- as.Date("2025-01-01")
  result <- MCPR:::inspect_dispatch(x, "my_date")
  expect_type(result, "character")
  expect_true(grepl("Object: my_date", result))
  expect_true(grepl("Date", result))
})

test_that("inspect_dispatch routes POSIXct correctly", {
  x <- as.POSIXct("2025-01-01 12:00:00")
  result <- MCPR:::inspect_dispatch(x, "my_time")
  expect_type(result, "character")
  expect_true(grepl("Object: my_time", result))
  expect_true(grepl("POSIXct", result))
})

test_that("inspect_dispatch routes environment correctly", {
  x <- new.env(parent = emptyenv())
  result <- MCPR:::inspect_dispatch(x, "my_env")
  expect_type(result, "character")
  expect_true(grepl("Object: my_env", result))
  expect_true(grepl("Environment", result))
})

test_that("inspect_dispatch includes object size in header", {
  result <- MCPR:::inspect_dispatch(mtcars, "mtcars")
  expect_true(grepl("\\(.*bytes\\)|\\(.*Kb\\)|\\(.*Mb\\)", result, ignore.case = TRUE))
})

test_that("inspect_dispatch routes lm model to S3/model inspector", {
  model <- lm(mpg ~ wt, data = mtcars)
  result <- MCPR:::inspect_dispatch(model, "my_model")
  expect_type(result, "character")
  expect_true(grepl("Object: my_model", result))
  expect_true(grepl("S3 object|lm", result))
})

# ---- inspect_data_frame ----

test_that("inspect_data_frame handles large data frame (mtcars)", {
  result <- MCPR:::inspect_data_frame(mtcars)
  expect_type(result, "character")
  expect_true(grepl("Data frame: 32 rows x 11 columns", result))
  expect_true(grepl("Columns", result))
  expect_true(grepl("mpg", result))
  expect_true(grepl("First 5 rows", result))
  expect_true(grepl("Numeric summary", result))
})

test_that("inspect_data_frame shows full data for small df", {
  small_df <- data.frame(a = 1:5, b = letters[1:5])
  result <- MCPR:::inspect_data_frame(small_df)
  expect_type(result, "character")
  expect_true(grepl("Data frame: 5 rows x 2 columns", result))
  expect_true(grepl("Full data", result))
})

test_that("inspect_data_frame reports NAs", {
  df_na <- data.frame(x = c(1, NA, 3), y = c(NA, NA, "a"))
  result <- MCPR:::inspect_data_frame(df_na)
  expect_type(result, "character")
  expect_true(grepl("Missing values", result))
})

test_that("inspect_data_frame handles empty data frame", {
  empty_df <- data.frame()
  result <- MCPR:::inspect_data_frame(empty_df)
  expect_type(result, "character")
  expect_true(grepl("0 rows x 0 columns", result))
  expect_true(grepl("Empty data frame", result))
})

test_that("inspect_data_frame handles many columns", {
  wide_df <- as.data.frame(matrix(1:60, nrow = 3, ncol = 20))
  result <- MCPR:::inspect_data_frame(wide_df)
  expect_type(result, "character")
  expect_true(grepl("first 10", result))
  expect_true(grepl("more columns", result))
})

# ---- inspect_vector ----

test_that("inspect_vector handles numeric with stats", {
  x <- c(1, 2, 3, 4, 5)
  result <- MCPR:::inspect_vector(x)
  expect_type(result, "character")
  expect_true(grepl("Vector.*double.*length 5", result))
  expect_true(grepl("Range: \\[1, 5\\]", result))
  expect_true(grepl("Mean:", result))
  expect_true(grepl("Median:", result))
})

test_that("inspect_vector handles character with unique count", {
  x <- c("a", "b", "a", "c")
  result <- MCPR:::inspect_vector(x)
  expect_type(result, "character")
  expect_true(grepl("character", result))
  expect_true(grepl("Unique values: 3", result))
})

test_that("inspect_vector handles logical with TRUE/FALSE counts", {
  x <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  result <- MCPR:::inspect_vector(x)
  expect_type(result, "character")
  expect_true(grepl("TRUE: 3", result))
  expect_true(grepl("FALSE: 2", result))
})

test_that("inspect_vector handles named vector", {
  x <- c(a = 1, b = 2, c = 3)
  result <- MCPR:::inspect_vector(x)
  expect_type(result, "character")
  expect_true(grepl("Named:", result))
  expect_true(grepl("3 of 3 elements have names", result))
})

test_that("inspect_vector handles empty vector", {
  x <- numeric(0)
  result <- MCPR:::inspect_vector(x)
  expect_type(result, "character")
  expect_true(grepl("length 0", result))
  expect_true(grepl("Empty vector", result))
})

test_that("inspect_vector handles NAs", {
  x <- c(1, NA, 3, NA, 5)
  result <- MCPR:::inspect_vector(x)
  expect_type(result, "character")
  expect_true(grepl("Missing values: 2", result))
})

test_that("inspect_vector shows head/tail for long vectors", {
  x <- 1:100
  result <- MCPR:::inspect_vector(x)
  expect_type(result, "character")
  expect_true(grepl("First 5:", result))
  expect_true(grepl("Last 5:", result))
})

# ---- inspect_factor ----

test_that("inspect_factor handles unordered factor", {
  x <- factor(c("a", "b", "a", "c", "b", "a"))
  result <- MCPR:::inspect_factor(x)
  expect_type(result, "character")
  expect_true(grepl("Factor of length 6 with 3 levels", result))
  expect_true(grepl("Levels:", result))
  expect_true(grepl("Frequencies", result))
  expect_false(grepl("ordered", result))
})

test_that("inspect_factor handles ordered factor", {
  x <- ordered(c("low", "mid", "high", "low"), levels = c("low", "mid", "high"))
  result <- MCPR:::inspect_factor(x)
  expect_type(result, "character")
  expect_true(grepl("ordered", result))
})

test_that("inspect_factor handles NAs", {
  x <- factor(c("a", NA, "b", NA))
  result <- MCPR:::inspect_factor(x)
  expect_type(result, "character")
  expect_true(grepl("Missing values: 2", result))
})

test_that("inspect_factor truncates many levels", {
  x <- factor(paste0("level_", 1:20))
  result <- MCPR:::inspect_factor(x)
  expect_type(result, "character")
  expect_true(grepl("first 10", result))
  expect_true(grepl("more levels", result))
})

# ---- inspect_function ----

test_that("inspect_function handles closure with defaults", {
  fn <- function(a, b = 10, c = "hello") a + b
  result <- MCPR:::inspect_function(fn)
  expect_type(result, "character")
  expect_true(grepl("Function", result))
  expect_true(grepl("Arguments: 3", result))
  expect_true(grepl("b = 10", result))
  expect_true(grepl("Body", result))
})

test_that("inspect_function handles primitive function", {
  result <- MCPR:::inspect_function(sum)
  expect_type(result, "character")
  expect_true(grepl("Primitive function", result))
})

test_that("inspect_function handles no-arg function", {
  fn <- function() 42
  result <- MCPR:::inspect_function(fn)
  expect_type(result, "character")
  expect_true(grepl("Arguments: 0", result))
})

# ---- inspect_list ----

test_that("inspect_list handles named list", {
  x <- list(name = "Alice", age = 30, scores = 1:5)
  result <- MCPR:::inspect_list(x)
  expect_type(result, "character")
  expect_true(grepl("List of length 3", result))
  expect_true(grepl("Named: 3 of 3", result))
  expect_true(grepl("\\$name", result))
  expect_true(grepl("\\$age", result))
})

test_that("inspect_list handles nested list", {
  x <- list(a = list(b = 1, c = 2), d = "hello")
  result <- MCPR:::inspect_list(x)
  expect_type(result, "character")
  expect_true(grepl("List of length 2", result))
  expect_true(grepl("list\\[2\\]", result))
})

test_that("inspect_list handles empty list", {
  x <- list()
  result <- MCPR:::inspect_list(x)
  expect_type(result, "character")
  expect_true(grepl("List of length 0", result))
  expect_true(grepl("Empty list", result))
})

# ---- inspect_matrix_array ----

test_that("inspect_matrix_array handles numeric matrix", {
  x <- matrix(1:12, nrow = 3, ncol = 4)
  result <- MCPR:::inspect_matrix_array(x)
  expect_type(result, "character")
  expect_true(grepl("Matrix.*integer.*3 rows x 4 columns", result))
  expect_true(grepl("Range:", result))
  expect_true(grepl("Mean:", result))
})

test_that("inspect_matrix_array handles matrix with dimnames", {
  x <- matrix(1:6, nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  result <- MCPR:::inspect_matrix_array(x)
  expect_type(result, "character")
  expect_true(grepl("r1", result))
  expect_true(grepl("c1", result))
})

test_that("inspect_matrix_array handles 3D array", {
  x <- array(1:24, dim = c(2, 3, 4))
  result <- MCPR:::inspect_matrix_array(x)
  expect_type(result, "character")
  expect_true(grepl("Array.*2 x 3 x 4", result))
  expect_true(grepl("Structure", result))
})

# ---- inspect_s3 ----

test_that("inspect_s3 handles lm model with model details", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  result <- MCPR:::inspect_s3(model)
  expect_type(result, "character")
  expect_true(grepl("S3 object.*lm", result))
  expect_true(grepl("Coefficients", result))
  expect_true(grepl("R-squared", result))
  expect_true(grepl("wt", result))
})

test_that("inspect_s3 handles generic S3 object", {
  x <- structure(list(a = 1, b = "hello"), class = "my_custom_class")
  result <- MCPR:::inspect_s3(x)
  expect_type(result, "character")
  expect_true(grepl("S3 object.*my_custom_class", result))
  expect_true(grepl("Components", result))
  expect_true(grepl("\\$a", result))
  expect_true(grepl("\\$b", result))
})

# ---- inspect_model ----

test_that("inspect_model extracts lm details", {
  model <- lm(mpg ~ wt, data = mtcars)
  result <- MCPR:::inspect_model(model)
  expect_type(result, "character")
  expect_true(grepl("Call:", result))
  expect_true(grepl("Coefficients:", result))
  expect_true(grepl("R-squared:", result))
  expect_true(grepl("Adj. R-squared:", result))
  expect_true(grepl("Residuals:", result))
})

# ---- inspect_formula ----

test_that("inspect_formula handles two-sided formula", {
  x <- y ~ x1 + x2
  result <- MCPR:::inspect_formula(x)
  expect_type(result, "character")
  expect_true(grepl("Formula:", result))
  expect_true(grepl("Variables:.*y.*x1.*x2", result))
  expect_true(grepl("Response: y", result))
  expect_true(grepl("Predictors:", result))
})

test_that("inspect_formula handles one-sided formula", {
  x <- ~ x + z
  result <- MCPR:::inspect_formula(x)
  expect_type(result, "character")
  expect_true(grepl("One-sided formula", result))
  expect_true(grepl("Variables:.*x.*z", result))
  expect_true(grepl("Terms:", result))
})

# ---- inspect_datetime ----

test_that("inspect_datetime handles Date vector", {
  x <- as.Date(c("2025-01-01", "2025-06-15", "2025-12-31"))
  result <- MCPR:::inspect_datetime(x)
  expect_type(result, "character")
  expect_true(grepl("Date of length 3", result))
  expect_true(grepl("Range:", result))
  expect_true(grepl("2025-01-01", result))
  expect_true(grepl("2025-12-31", result))
})

test_that("inspect_datetime handles POSIXct vector", {
  x <- as.POSIXct(c("2025-01-01 00:00:00", "2025-01-02 12:00:00"))
  result <- MCPR:::inspect_datetime(x)
  expect_type(result, "character")
  expect_true(grepl("POSIXct", result))
  expect_true(grepl("Range:", result))
  expect_true(grepl("Timezone:", result))
})

test_that("inspect_datetime handles empty datetime", {
  x <- as.Date(character(0))
  result <- MCPR:::inspect_datetime(x)
  expect_type(result, "character")
  expect_true(grepl("length 0", result))
  expect_true(grepl("Empty", result))
})

test_that("inspect_datetime reports NAs in datetime", {
  x <- as.Date(c("2025-01-01", NA, "2025-12-31"))
  result <- MCPR:::inspect_datetime(x)
  expect_type(result, "character")
  expect_true(grepl("Missing values: 1", result))
})

# ---- inspect_environment ----

test_that("inspect_environment handles env with bindings", {
  e <- new.env(parent = emptyenv())
  e$x <- 42
  e$name <- "test"
  e$fn <- function(a) a + 1

  result <- MCPR:::inspect_environment(e)
  expect_type(result, "character")
  expect_true(grepl("Environment:", result))
  expect_true(grepl("Bindings: 3", result))
  expect_true(grepl("Contents:", result))
  expect_true(grepl("x", result))
  expect_true(grepl("name", result))
})

test_that("inspect_environment shows parent chain", {
  e <- new.env(parent = baseenv())
  result <- MCPR:::inspect_environment(e)
  expect_type(result, "character")
  expect_true(grepl("Parent chain:", result))
})

test_that("inspect_environment handles empty environment", {
  e <- new.env(parent = emptyenv())
  result <- MCPR:::inspect_environment(e)
  expect_type(result, "character")
  expect_true(grepl("Bindings: 0", result))
})

# ---- inspect_default ----

test_that("inspect_default handles raw vector", {
  x <- as.raw(c(0x01, 0x02, 0xff))
  result <- MCPR:::inspect_default(x)
  expect_type(result, "character")
  expect_true(grepl("raw", result))
  expect_true(grepl("Structure", result))
})

test_that("inspect_default handles uncommon type", {
  x <- as.raw(0x00)
  result <- MCPR:::inspect_default(x)
  expect_type(result, "character")
  expect_true(grepl("type: raw", result))
})

# ---- S4 minimal test ----

test_that("inspect_dispatch does not error on S4 object", {
  setClass("TestS4Class", representation(value = "numeric"), where = .GlobalEnv)
  obj <- new("TestS4Class", value = 42)
  expect_no_error({
    result <- MCPR:::inspect_dispatch(obj, "s4_obj")
  })
  expect_type(result, "character")
  expect_true(grepl("Object: s4_obj", result))
  removeClass("TestS4Class", where = .GlobalEnv)
})

# ---- Edge cases ----

test_that("inspect_dispatch does not error on NULL-like objects", {
  expect_no_error(MCPR:::inspect_dispatch(NULL, "null_obj"))
})

test_that("inspect_vector handles all-NA vector", {
  x <- c(NA_real_, NA_real_, NA_real_)
  result <- MCPR:::inspect_vector(x)
  expect_type(result, "character")
  expect_true(grepl("Missing values: 3", result))
})

test_that("inspect_data_frame handles single-column df", {
  df <- data.frame(x = 1:5)
  result <- MCPR:::inspect_data_frame(df)
  expect_type(result, "character")
  expect_true(grepl("5 rows x 1 columns", result))
  expect_true(grepl("Full data", result))
})
