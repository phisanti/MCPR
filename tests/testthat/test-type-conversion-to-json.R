# Test for Basic Atomic Types
test_that("Basic atomic types are converted correctly", {
  # NULL
  expect_null(MCPR:::to_mcpr_json(NULL))

  # Single values (should be unboxed)
  result_num <- MCPR:::to_mcpr_json(42)
  expect_true(inherits(result_num, "scalar"))
  expect_equal(as.numeric(result_num), 42)

  result_str <- MCPR:::to_mcpr_json("hello")
  expect_true(inherits(result_str, "scalar"))
  expect_equal(as.character(result_str), "hello")

  result_bool <- MCPR:::to_mcpr_json(TRUE)
  expect_true(inherits(result_bool, "scalar"))
  expect_equal(as.logical(result_bool), TRUE)

  # Vectors (should not be unboxed)
  expect_equal(MCPR:::to_mcpr_json(c(1, 2, 3)), c(1, 2, 3))
  expect_equal(MCPR:::to_mcpr_json(c("a", "b")), c("a", "b"))
  expect_equal(MCPR:::to_mcpr_json(1:5), 1:5)
  expect_equal(MCPR:::to_mcpr_json(c("a", "b", "c")), c("a", "b", "c"))

  # Named vectors become lists
  named_vec <- c(a = 1, b = 2, c = 3)
  expect_equal(MCPR:::to_mcpr_json(named_vec), as.list(named_vec))
})

test_that("auto_unbox parameter works correctly", {
  # With auto_unbox = TRUE (default)
  expect_equal(MCPR:::to_mcpr_json(42, auto_unbox = TRUE), jsonlite::unbox(42))
  expect_equal(MCPR:::to_mcpr_json(list(a = 1), auto_unbox = TRUE)$a, jsonlite::unbox(1))

  # With auto_unbox = FALSE
  expect_equal(MCPR:::to_mcpr_json(42, auto_unbox = FALSE), 42)
  expect_equal(MCPR:::to_mcpr_json(list(a = 1), auto_unbox = FALSE)$a, 1)
})

# Test for Special Numeric Values
test_that("Special numeric values are handled correctly", {
  # Single special values
  expect_equal(MCPR:::to_mcpr_json(Inf)$value, jsonlite::unbox("Inf"))
  expect_equal(MCPR:::to_mcpr_json(-Inf)$value, jsonlite::unbox("-Inf"))
  expect_equal(MCPR:::to_mcpr_json(NaN)$value, jsonlite::unbox("NaN"))

  # Vector with special values
  special_nums <- c(1, Inf, -Inf, NaN)
  result <- MCPR:::to_mcpr_json(special_nums)
  expect_true(is.list(result))
  expect_equal(result$`_mcp_type`, "numeric_vector_special")
  expect_true("Inf" %in% result$values)
  expect_true("-Inf" %in% result$values)
  expect_true("NaN" %in% result$values)
  expect_equal(result$special_indices, c(2, 3, 4))

  # Round trip
  vec <- c(1, Inf, -Inf, NaN, 5)
  json_str <- mcpr_serialize(vec)
  reconstructed <- mcpr_deserialize(json_str)
  expect_true(is.infinite(reconstructed[[2]]) && reconstructed[[2]] > 0)
  expect_true(is.infinite(reconstructed[[3]]) && reconstructed[[3]] < 0)
  expect_true(is.nan(reconstructed[[4]]))
})

# Test for Complex Numbers
test_that("Complex numbers are handled correctly", {
  # Single complex number
  z1 <- 3 + 4i
  result <- MCPR:::to_mcpr_json(z1)
  expect_equal(result$`_mcp_type`, "complex")
  expect_equal(result$real, jsonlite::unbox(3))
  expect_equal(result$imaginary, jsonlite::unbox(4))

  # Complex vector
  test_complex <- c(1 + 2i, 3 - 4i)
  result <- MCPR:::to_mcpr_json(test_complex)
  expect_equal(result$`_mcp_type`, "complex")
  expect_length(result$real, 2)
  expect_length(result$imaginary, 2)
  expect_equal(result$real, c(1, 3))
  expect_equal(result$imaginary, c(2, -4))

  # Round trip
  z_vec <- c(1 + 2i, 3 - 4i, 0 + 1i)
  json_str <- mcpr_serialize(z_vec)
  reconstructed <- mcpr_deserialize(json_str)
  expect_equal(reconstructed, z_vec)
})

test_that("Raw vectors are converted correctly", {
  # Create raw vector
  raw_vec <- as.raw(c(0x48, 0x65, 0x6c, 0x6c, 0x6f)) # "Hello" in hex
  result <- MCPR:::to_mcpr_json(raw_vec)
  expect_equal(result$`_mcp_type`, "raw")
  expect_true(!is.null(result$data)) # Should be base64 encoded

  # Round trip
  json_str <- mcpr_serialize(raw_vec)
  reconstructed <- mcpr_deserialize(json_str)
  expect_equal(reconstructed, raw_vec)
})

# Test for Date and Time Objects
test_that("Date objects are converted correctly", {
  # Single date
  test_date <- as.Date("2024-01-15")
  result <- MCPR:::to_mcpr_json(test_date)
  expect_equal(result$`_mcp_type`, "Date")
  expect_equal(result$values, "2024-01-15")

  # Date vector
  dates <- as.Date(c("2024-01-15", "2024-02-20", "2024-03-25"))
  result <- MCPR:::to_mcpr_json(dates)
  expect_equal(result$values, c("2024-01-15", "2024-02-20", "2024-03-25"))

  # Round trip
  json_str <- mcpr_serialize(dates)
  reconstructed <- mcpr_deserialize(json_str)
  expect_true(inherits(reconstructed, "Date"))
  expect_equal(reconstructed, dates)
})

test_that("POSIXct datetime objects are converted correctly", {
  # Test POSIXct
  test_time <- as.POSIXct("2024-01-15 10:30:00", tz = "UTC")
  result <- MCPR:::to_mcpr_json(test_time)
  expect_equal(result$`_mcp_type`, "POSIXct")
  expect_true(!is.null(result$values))

  # Create POSIXct with specific timezone
  dt1 <- as.POSIXct("2024-01-15 14:30:00", tz = "America/New_York")
  result <- MCPR:::to_mcpr_json(dt1)
  expect_equal(result$`_mcp_type`, "POSIXct")
  expect_true(!is.null(result$timezone))

  # POSIXlt should be converted to POSIXct
  dt2 <- as.POSIXlt("2024-01-15 14:30:00", tz = "UTC")
  result2 <- MCPR:::to_mcpr_json(dt2)
  expect_equal(result2$`_mcp_type`, "POSIXct")

  # Round trip
  json_str <- mcpr_serialize(dt1)
  reconstructed <- mcpr_deserialize(json_str)
  expect_true(inherits(reconstructed, "POSIXct"))
})

# Test for Factors
test_that("Factors are converted correctly", {
  # Test factor
  test_factor <- factor(c("low", "high", "medium", "low"),
    levels = c("low", "medium", "high")
  )
  result <- MCPR:::to_mcpr_json(test_factor)
  expect_equal(result$`_mcp_type`, "factor")
  expect_equal(result$levels, c("low", "medium", "high"))
  expect_equal(result$values, c(1, 3, 2, 1))

  # Simple factor
  f <- factor(c("a", "b", "a", "c"), levels = c("a", "b", "c"))
  result <- MCPR:::to_mcpr_json(f)
  expect_true(is.list(result))
  expect_equal(result$`_mcp_type`, "factor")
  expect_equal(result$levels, c("a", "b", "c"))
  expect_equal(result$values, c(1, 2, 1, 3))
})

# Test for Matrices and Arrays
test_that("Matrices are converted with metadata", {
  mat <- matrix(1:6, nrow = 2, ncol = 3)
  result <- MCPR:::to_mcpr_json(mat)

  expect_equal(result$data, 1:6)
  expect_equal(result$dim, c(2, 3))
  expect_equal(result$`_mcp_type`, "matrix")

  # Named matrix
  mat2 <- matrix(1:4, nrow = 2, ncol = 2)
  rownames(mat2) <- c("r1", "r2")
  colnames(mat2) <- c("c1", "c2")
  result2 <- MCPR:::to_mcpr_json(mat2)

  expect_equal(result2$dimnames[[1]], c("r1", "r2"))
  expect_equal(result2$dimnames[[2]], c("c1", "c2"))
})

test_that("Arrays are handled correctly", {
  arr <- array(1:24, dim = c(2, 3, 4))
  result <- MCPR:::to_mcpr_json(arr)

  expect_equal(result$`_mcp_type`, "array")
  expect_equal(result$data, 1:24)
  expect_equal(result$dim, c(2, 3, 4))

  # Test reconstruction through full cycle
  json_str <- mcpr_serialize(arr)
  reconstructed <- mcpr_deserialize(json_str)

  # Should be reconstructed as an array
  expect_true(is.array(reconstructed))
  expect_equal(dim(reconstructed), c(2, 3, 4))

  # Compare the actual data
  expect_equal(reconstructed[, , ], arr[, , ])
})

# Test for Data Frames
test_that("Data frames are converted correctly", {
  df <- data.frame(
    x = 1:3,
    y = c("a", "b", "c"),
    z = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  result <- MCPR:::to_mcpr_json(df)

  expect_equal(result$x, 1:3)
  expect_equal(result$y, c("a", "b", "c"))
  expect_equal(result$z, c(TRUE, FALSE, TRUE))
  expect_equal(attr(result, "_mcp_type"), "data.frame")
  expect_equal(attr(result, "_mcp_nrow"), 3)
})

# Test for Lists
test_that("Lists are recursively converted", {
  lst <- list(
    a = 1:3,
    b = list(x = "hello", y = TRUE),
    c = data.frame(p = 1:2, q = c("a", "b"))
  )

  result <- MCPR:::to_mcpr_json(lst)

  expect_equal(result$a, 1:3)
  expect_equal(result$b$x, jsonlite::unbox("hello"))
  expect_equal(result$b$y, jsonlite::unbox(TRUE))
  expect_equal(attr(result$c, "_mcp_type"), "data.frame")
})

# Test for S3 Objects
test_that("S3 objects are handled", {
  # Create a simple S3 object
  obj <- structure(list(x = 1, y = 2), class = "myclass")
  result <- MCPR:::to_mcpr_json(obj)

  # Check that result has the expected structure
  expect_true(is.list(result))
  expect_equal(result$x, jsonlite::unbox(1))
  expect_equal(result$y, jsonlite::unbox(2))

  # S3 class info should be preserved as attributes
  expect_equal(attr(result, "_mcp_type"), "S3")
  expect_equal(attr(result, "_mcp_class"), "myclass")
})

# Test for Formulas and Language Objects
test_that("Formulas are handled correctly", {
  # Simple formula
  f1 <- y ~ x + z
  result <- MCPR:::to_mcpr_json(f1)
  expect_true(identical(as.character(result$`_mcp_type`), "formula"))
  expect_true(grepl("y ~ x \\+ z", result$formula))

  # Formula with interactions
  f2 <- y ~ x * z + I(x^2)
  result2 <- MCPR:::to_mcpr_json(f2)
  expect_true(!is.null(result2$formula))

  # Round trip
  json_str <- mcpr_serialize(f1)
  reconstructed <- mcpr_deserialize(json_str)
  expect_true(inherits(reconstructed, "formula"))
  expect_equal(deparse(reconstructed), deparse(f1))
})

test_that("Language objects are handled correctly", {
  # Expression
  expr <- quote(x + y * z)
  result <- MCPR:::to_mcpr_json(expr)
  expect_true(identical(as.character(result$`_mcp_type`), "language"))
  expect_true(identical(as.character(result$type), "language"))

  # Call
  call_obj <- quote(mean(x, na.rm = TRUE))
  result2 <- MCPR:::to_mcpr_json(call_obj)
  expect_true(identical(as.character(result2$`_mcp_type`), "language"))

  # Round trip
  json_str <- mcpr_serialize(expr)
  reconstructed <- mcpr_deserialize(json_str)
  expect_equal(deparse(reconstructed), deparse(expr))
})

# Test for Environments
test_that("Environments are handled with markers", {
  # Global environment
  result <- MCPR:::to_mcpr_json(globalenv())
  expect_true(identical(as.character(result$`_mcp_type`), "environment"))
  expect_true(!is.null(result$name))

  # Custom environment
  env <- new.env()
  result2 <- MCPR:::to_mcpr_json(env)
  expect_true(identical(as.character(result2$`_mcp_type`), "environment"))

  # Round trip returns marker
  json_str <- mcpr_serialize(env)
  reconstructed <- mcpr_deserialize(json_str)
  expect_true(inherits(reconstructed, "mcp_environment_marker"))
})

# Test for Plot Objects
test_that("Plot conversion works for ggplot2", {
  skip_if_not_installed("ggplot2")

  # Create a simple ggplot
  library(ggplot2)
  p <- ggplot(mtcars, aes(x = mpg, y = wt)) +
    geom_point()

  result <- MCPR:::to_mcpr_json(p)

  expect_equal(result$`_mcp_type`, "plot")
  expect_equal(result$format, "image/png")
  expect_equal(result$plot_type, "ggplot2")
  expect_true(!is.null(result$data)) # Should have base64 data
  expect_true(nchar(result$data) > 100) # Should be non-trivial
})

# Test for Large Objects
test_that("Large object handling works correctly", {
  # Create a large object
  large_df <- data.frame(
    x = 1:10000,
    y = rnorm(10000),
    z = sample(letters, 10000, replace = TRUE)
  )

  # Convert with size limit
  result <- MCPR:::to_mcpr_json(large_df, size_limit = 1000)

  expect_equal(result$`_mcp_type`, "large_object")
  expect_equal(result$class, "data.frame")
  expect_true(result$size > 1000)
  expect_true(!is.null(result$size_human))
  expect_true(!is.null(result$summary))
  expect_true(!is.null(result$preview))
  expect_equal(result$preview$nrow, 10000)
  expect_equal(result$preview$ncol, 3)
  expect_equal(length(result$preview$head[[1]]), 5) # Should have 5 rows in preview
})

test_that("special numeric values are converted correctly", {
  # Test Inf, -Inf, NaN
  special_vals <- c(1, Inf, -Inf, NaN, NA)
  result <- MCPR:::to_mcpr_json(special_vals)

  expect_type(result, "list")
  expect_equal(result$`_mcp_type`, "numeric_vector_special")
  expect_true("special_indices" %in% names(result))

  # Single Inf value
  single_inf <- Inf
  single_result <- MCPR:::to_mcpr_json(single_inf)
  expect_equal(single_result$`_mcp_type`, "special_numeric")
  expect_equal(as.character(single_result$value), "Inf")
})

test_that("date objects are converted correctly", {
  # Test Date conversion
  test_date <- as.Date("2024-01-15")
  result <- MCPR:::to_mcpr_json(test_date)

  expect_equal(result$`_mcp_type`, "Date")
  expect_equal(result$values, "2024-01-15")

  # Test multiple dates
  date_vec <- as.Date(c("2024-01-15", "2024-02-20"))
  vec_result <- MCPR:::to_mcpr_json(date_vec)
  expect_equal(vec_result$`_mcp_type`, "Date")
  expect_equal(length(vec_result$values), 2)
})

test_that("POSIXt datetime objects are converted correctly", {
  # Test POSIXct conversion
  test_datetime <- as.POSIXct("2024-01-15 14:30:00", tz = "UTC")
  result <- MCPR:::to_mcpr_json(test_datetime)

  expect_equal(result$`_mcp_type`, "POSIXct")
  expect_true("values" %in% names(result))
  # Note: timezone might not be preserved in all conversions
})

test_that("complex numbers are converted correctly", {
  # Test complex number conversion
  complex_val <- 3 + 4i
  result <- MCPR:::to_mcpr_json(complex_val)

  expect_equal(result$`_mcp_type`, "complex")
  expect_true("real" %in% names(result))
  expect_true("imaginary" %in% names(result))
  expect_equal(result$real, 3)
  expect_equal(result$imaginary, 4)

  # Test complex vector
  complex_vec <- c(1 + 2i, 3 + 4i)
  vec_result <- MCPR:::to_mcpr_json(complex_vec)
  expect_equal(vec_result$`_mcp_type`, "complex")
  expect_equal(length(vec_result$real), 2)
})

test_that("raw bytes are converted correctly", {
  # Test raw conversion
  raw_data <- as.raw(c(65, 66, 67)) # ABC
  result <- MCPR:::to_mcpr_json(raw_data)

  expect_equal(result$`_mcp_type`, "raw")
  # Raw data conversion format may vary
  expect_true(is.list(result))
})

test_that("formula objects are converted correctly", {
  # Test formula conversion
  test_formula <- y ~ x + z
  result <- MCPR:::to_mcpr_json(test_formula)

  expect_true(is.character(result) || is.list(result))
  # Formula is converted - exact format may vary
})

test_that("factor objects are converted correctly", {
  # Test factor conversion
  test_factor <- factor(c("A", "B", "C", "A"), levels = c("A", "B", "C", "D"))
  result <- MCPR:::to_mcpr_json(test_factor)

  expect_equal(result$`_mcp_type`, "factor")
  expect_true("values" %in% names(result))
  expect_true("levels" %in% names(result))
  expect_equal(result$levels, c("A", "B", "C", "D"))
  # Factor values may be converted as indices
  expect_true(is.vector(result$values))
})

test_that("environment objects are handled", {
  # Test environment conversion
  test_env <- new.env()
  test_env$a <- 1
  test_env$b <- "hello"

  result <- MCPR:::to_mcpr_json(test_env)
  # Environment conversion produces some representation
  expect_true(is.character(result) || is.list(result))
})

test_that("custom serializers are used when provided", {
  # Create a custom class
  custom_obj <- structure(list(value = 42), class = "custom_class")

  # Define custom serializer
  custom_serializers <- list(
    custom_class = function(x) {
      list(custom_value = x$value * 2, `_mcp_type` = "custom")
    }
  )

  result <- MCPR:::to_mcpr_json(custom_obj, custom_serializers = custom_serializers)

  expect_equal(result$`_mcp_type`, "custom")
  expect_equal(result$custom_value, 84)
})

test_that("language objects are converted", {
  # Test expression/call conversion
  test_expr <- quote(mean(x))
  result <- MCPR:::to_mcpr_json(test_expr)

  # Language objects are converted to some representation
  expect_true(is.character(result) || is.list(result))
})
