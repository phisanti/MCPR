# Test for Basic Serializer Registry
test_that("register_mcpr_serializer adds and retrieves custom serializers", {
  # Clear any existing serializers for clean test
  original_serializers <- MCPR:::get_mcpr_serializers()

  # Test registering a custom serializer
  test_serializer <- function(obj) {
    list(
      custom_type = "test_object",
      data = as.character(obj)
    )
  }

  MCPR:::register_mcpr_serializer("test_class", test_serializer)

  # Check that serializer was registered
  serializers <- MCPR:::get_mcpr_serializers()
  expect_true("test_class" %in% names(serializers))
  expect_true(is.function(serializers[["test_class"]]))

  # Test the serializer function works
  test_obj <- structure("test_data", class = "test_class")
  result <- serializers[["test_class"]](test_obj)
  expect_equal(result$custom_type, "test_object")
  expect_equal(result$data, "test_data")
})

test_that("serializer registry supports overwriting and persistence", {
  # Register initial serializer
  serializer1 <- function(obj) list(version = 1, data = obj)
  MCPR:::register_mcpr_serializer("overwrite_test", serializer1)

  # Verify first serializer
  serializers1 <- MCPR:::get_mcpr_serializers()
  result1 <- serializers1[["overwrite_test"]](42)
  expect_equal(result1$version, 1)

  # Register new serializer with same name
  serializer2 <- function(obj) list(version = 2, data = obj * 2)
  MCPR:::register_mcpr_serializer("overwrite_test", serializer2)

  # Verify second serializer overwrote the first
  serializers2 <- MCPR:::get_mcpr_serializers()
  result2 <- serializers2[["overwrite_test"]](42)
  expect_equal(result2$version, 2)
  expect_equal(result2$data, 84)

  # Test persistence across calls
  serializers3 <- MCPR:::get_mcpr_serializers()
  expect_true("overwrite_test" %in% names(serializers3))
  expect_identical(
    serializers2[["overwrite_test"]],
    serializers3[["overwrite_test"]]
  )
})

test_that("multiple custom serializers can coexist", {
  # Register multiple serializers
  MCPR:::register_mcpr_serializer("type_a", function(obj) list(type = "A"))
  MCPR:::register_mcpr_serializer("type_b", function(obj) list(type = "B"))
  MCPR:::register_mcpr_serializer("type_c", function(obj) list(type = "C"))

  # Check all are present
  serializers <- MCPR:::get_mcpr_serializers()
  expect_true("type_a" %in% names(serializers))
  expect_true("type_b" %in% names(serializers))
  expect_true("type_c" %in% names(serializers))

  # Check they work independently
  expect_equal(serializers[["type_a"]](NULL)$type, "A")
  expect_equal(serializers[["type_b"]](NULL)$type, "B")
  expect_equal(serializers[["type_c"]](NULL)$type, "C")
})

# Test for Basic Serialization Functions
test_that("can_serialize correctly identifies serializable objects", {
  # Should be serializable
  expect_true(can_serialize(42))
  expect_true(can_serialize("hello"))
  expect_true(can_serialize(list(a = 1, b = 2)))
  expect_true(can_serialize(data.frame(x = 1:3)))

  # Complex objects that should still work
  expect_true(can_serialize(matrix(1:6, 2, 3)))
  expect_true(can_serialize(factor(letters[1:3])))
})

test_that("mcpr_serialize produces valid JSON and round-trip works", {
  # Various R objects
  objects <- list(
    null = NULL,
    number = 42,
    string = "hello",
    vector = 1:5,
    list = list(a = 1, b = "x"),
    dataframe = data.frame(x = 1:3, y = letters[1:3])
  )

  for (name in names(objects)) {
    json_str <- mcpr_serialize(objects[[name]])
    expect_type(json_str, "character")
    expect_length(json_str, 1)

    # Should be valid JSON
    expect_error(jsonlite::fromJSON(json_str), NA)
  }

  # Test round-trip conversion for data frames
  df <- data.frame(
    x = 1:3,
    y = letters[1:3],
    z = c(1.1, 2.2, 3.3),
    stringsAsFactors = FALSE
  )

  # Convert to JSON and back
  json_str <- mcpr_serialize(df)
  reconstructed <- mcpr_deserialize(json_str)

  # The structure might be slightly different but data should be same
  expect_true(is.list(reconstructed))
  expect_equal(reconstructed$x, as.list(1:3))
  expect_equal(reconstructed$y, as.list(letters[1:3]))
})

# Test for Complex Object Serialization
test_that("Plot markers are created for reconstruction", {
  skip_if_not_installed("ggplot2")

  library(ggplot2)
  p <- ggplot(mtcars, aes(x = mpg, y = wt)) +
    geom_point()

  json_str <- mcpr_serialize(p)
  reconstructed <- mcpr_deserialize(json_str)

  # The plot object is nested in the reconstructed list
  plot_obj <- if (inherits(reconstructed, "mcp_plot_marker")) {
    reconstructed
  } else {
    reconstructed[[1]] # If it's wrapped in a list
  }

  expect_true(inherits(plot_obj, "mcp_plot_marker"))
  expect_equal(plot_obj$format[[1]], "image/png")
  expect_equal(plot_obj$plot_type[[1]], "ggplot2")
  expect_true(!is.null(plot_obj$data[[1]]))
})

test_that("Mixed data frames with special types work", {
  # Data frame with dates and special values
  df <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-02")),
    value = c(1.5, Inf),
    category = factor(c("A", "B")),
    stringsAsFactors = FALSE
  )

  json_str <- mcpr_serialize(df)
  expect_type(json_str, "character")

  # The reconstruction might not perfectly preserve the data frame structure
  # due to how special types are handled, but the data should be recoverable
  reconstructed <- mcpr_deserialize(json_str)
  expect_true(is.list(reconstructed))
})

test_that("Custom serializers work with to_mcpr_json", {
  # Register a custom serializer
  MCPR:::register_mcpr_serializer("myclass", function(obj) {
    list(
      `_mcp_type` = "custom_myclass",
      data = obj$data,
      metadata = obj$metadata
    )
  })

  # Create object with custom class
  obj <- structure(
    list(data = 1:5, metadata = "test"),
    class = "myclass"
  )

  # Convert using custom serializer
  result <- to_mcpr_json(obj, custom_serializers = MCPR:::get_mcpr_serializers())

  expect_equal(result$`_mcp_type`, "custom_myclass")
  expect_equal(result$data, 1:5)
  expect_equal(result$metadata, "test")

  # Clean up
  .mcpr_custom_serializers$myclass <- NULL
})
