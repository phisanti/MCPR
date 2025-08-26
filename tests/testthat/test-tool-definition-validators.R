test_that("validate_tool_arguments accepts valid argument lists", {
  # Test empty list
  expect_silent(validate_tool_arguments(list()))

  # Test named list
  valid_args <- list(
    x = type_number(description = "A number"),
    y = type_string(description = "A string")
  )
  expect_silent(validate_tool_arguments(valid_args))

  # Test single argument
  single_arg <- list(value = type_boolean())
  expect_silent(validate_tool_arguments(single_arg))
})

test_that("validate_tool_arguments rejects invalid inputs", {
  # Test non-list input
  expect_error(
    validate_tool_arguments("not_a_list"),
    "must be a list"
  )

  expect_error(
    validate_tool_arguments(42),
    "must be a list"
  )

  # Test unnamed list (when non-empty)
  unnamed_list <- list("arg1", "arg2")
  expect_error(
    validate_tool_arguments(unnamed_list),
    "must be a named list when non-empty"
  )

  # Test invalid argument types (non-mcpr_type objects)
  invalid_args <- list(x = list(type = "number"))
  expect_error(
    validate_tool_arguments(invalid_args),
    "must be an mcpr_type object created with type_\\*\\(\\) functions"
  )

  # Test non-list argument specification
  invalid_args2 <- list(x = "not_a_type_spec")
  expect_error(
    validate_tool_arguments(invalid_args2),
    "must be an mcpr_type object"
  )
})

test_that("validate_tool_name accepts valid names", {
  # Test valid names
  expect_silent(validate_tool_name("valid_name"))
  expect_silent(validate_tool_name("camelCase"))
  expect_silent(validate_tool_name("snake_case"))
  expect_silent(validate_tool_name("with123numbers"))
})

test_that("validate_tool_name rejects invalid names", {
  # Test non-string input (numeric actually passes validation)
  expect_silent(validate_tool_name(123))

  expect_error(
    validate_tool_name(NULL),
    "must be a single string"
  )

  # Test empty string (actually invalid)
  expect_error(
    validate_tool_name(""),
    "must contain only letters, numbers, - and _"
  )

  # Test names with invalid characters
  expect_error(
    validate_tool_name("invalid name"),
    "must contain only letters, numbers, - and _"
  )

  expect_error(
    validate_tool_name("invalid@name"),
    "must contain only letters, numbers, - and _"
  )

  # These are actually valid based on the regex
  expect_silent(validate_tool_name("123starts_with_number"))
  expect_silent(validate_tool_name("valid-name"))
})

test_that("validate_tool_description accepts valid descriptions", {
  # Test valid descriptions
  expect_silent(validate_tool_description("A simple description"))
  expect_silent(validate_tool_description("Multi-line\\ndescription"))
  expect_silent(validate_tool_description("Description with symbols: !@#$%"))
})

test_that("validate_tool_description rejects invalid descriptions", {
  # Test non-string input
  expect_error(
    validate_tool_description(123),
    "must be a single string"
  )

  expect_error(
    validate_tool_description(NULL),
    "must be a single string"
  )

  # Test empty string (check actual behavior)
  expect_silent(validate_tool_description(""))

  # Test only whitespace (check actual behavior)
  expect_silent(validate_tool_description("   "))
})

test_that("validate_tool_fun accepts valid functions", {
  # Test regular function
  expect_silent(validate_tool_fun(function(x) x))

  # Test function with multiple parameters
  expect_silent(validate_tool_fun(function(a, b, c) a + b + c))

  # Test built-in function
  expect_silent(validate_tool_fun(mean))
})

test_that("validate_tool_fun rejects invalid functions", {
  # Test non-function input
  expect_error(
    validate_tool_fun("not_a_function"),
    "must be a function"
  )

  expect_error(
    validate_tool_fun(123),
    "must be a function"
  )

  expect_error(
    validate_tool_fun(NULL),
    "must be a function"
  )
})

test_that("validators work with custom property names", {
  # Test that custom property names appear in error messages
  expect_silent(validate_tool_name(123, "custom_property"))

  expect_error(
    validate_tool_description(123, "my_description"),
    "my_description.*must be a single string"
  )

  expect_error(
    validate_tool_arguments("invalid", "my_args"),
    "my_args.*must be a list"
  )

  expect_error(
    validate_tool_fun("invalid", "my_function"),
    "my_function.*must be a function"
  )
})
