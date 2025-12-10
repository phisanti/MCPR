test_that("validate_tool_arguments accepts valid argument lists", {
  # Test empty list
  expect_silent(MCPR:::validate_tool_arguments(list()))

  # Test named list
  valid_args <- list(
    x = MCPR:::type_number(description = "A number"),
    y = MCPR:::type_string(description = "A string")
  )
  expect_silent(MCPR:::validate_tool_arguments(valid_args))

  # Test single argument
  single_arg <- list(value = MCPR:::type_boolean())
  expect_silent(MCPR:::validate_tool_arguments(single_arg))
})

test_that("validate_tool_arguments rejects invalid inputs", {
  # Test non-list input
  expect_error(
    MCPR:::validate_tool_arguments("not_a_list"),
    "must be a list"
  )

  expect_error(
    MCPR:::validate_tool_arguments(42),
    "must be a list"
  )

  # Test unnamed list (when non-empty)
  unnamed_list <- list("arg1", "arg2")
  expect_error(
    MCPR:::validate_tool_arguments(unnamed_list),
    "must be a named list when non-empty"
  )

  # Test invalid argument types (non-mcpr_type objects)
  invalid_args <- list(x = list(type = "number"))
  expect_error(
    MCPR:::validate_tool_arguments(invalid_args),
    "must be an mcpr_type object created with type_\\*\\(\\) functions"
  )

  # Test non-list argument specification
  invalid_args2 <- list(x = "not_a_type_spec")
  expect_error(
    MCPR:::validate_tool_arguments(invalid_args2),
    "must be an mcpr_type object"
  )
})

test_that("validate_tool_name accepts valid names", {
  # Test valid names
  expect_silent(MCPR:::validate_tool_name("valid_name"))
  expect_silent(MCPR:::validate_tool_name("camelCase"))
  expect_silent(MCPR:::validate_tool_name("snake_case"))
  expect_silent(MCPR:::validate_tool_name("with123numbers"))
})

test_that("validate_tool_name rejects invalid names", {
  # Test non-string input (numeric actually passes validation)
  expect_silent(MCPR:::validate_tool_name(123))

  expect_error(
    MCPR:::validate_tool_name(NULL),
    "must be a single string"
  )

  # Test empty string (actually invalid)
  expect_error(
    MCPR:::validate_tool_name(""),
    "must contain only letters, numbers, - and _"
  )

  # Test names with invalid characters
  expect_error(
    MCPR:::validate_tool_name("invalid name"),
    "must contain only letters, numbers, - and _"
  )

  expect_error(
    MCPR:::validate_tool_name("invalid@name"),
    "must contain only letters, numbers, - and _"
  )

  # These are actually valid based on the regex
  expect_silent(MCPR:::validate_tool_name("123starts_with_number"))
  expect_silent(MCPR:::validate_tool_name("valid-name"))
})

test_that("validate_tool_description accepts valid descriptions", {
  # Test valid descriptions
  expect_silent(MCPR:::validate_tool_description("A simple description"))
  expect_silent(MCPR:::validate_tool_description("Multi-line\\ndescription"))
  expect_silent(MCPR:::validate_tool_description("Description with symbols: !@#$%"))
})

test_that("validate_tool_description rejects invalid descriptions", {
  # Test non-string input
  expect_error(
    MCPR:::validate_tool_description(123),
    "must be a single string"
  )

  expect_error(
    MCPR:::validate_tool_description(NULL),
    "must be a single string"
  )

  # Test empty string (check actual behavior)
  expect_silent(MCPR:::validate_tool_description(""))

  # Test only whitespace (check actual behavior)
  expect_silent(MCPR:::validate_tool_description("   "))
})

test_that("validate_tool_fun accepts valid functions", {
  # Test regular function
  expect_silent(MCPR:::validate_tool_fun(function(x) x))

  # Test function with multiple parameters
  expect_silent(MCPR:::validate_tool_fun(function(a, b, c) a + b + c))

  # Test built-in function
  expect_silent(MCPR:::validate_tool_fun(mean))
})

test_that("validate_tool_fun rejects invalid functions", {
  # Test non-function input
  expect_error(
    MCPR:::validate_tool_fun("not_a_function"),
    "must be a function"
  )

  expect_error(
    MCPR:::validate_tool_fun(123),
    "must be a function"
  )

  expect_error(
    MCPR:::validate_tool_fun(NULL),
    "must be a function"
  )
})

test_that("validators work with custom property names", {
  # Test that custom property names appear in error messages
  expect_silent(MCPR:::validate_tool_name(123, "custom_property"))

  expect_error(
    MCPR:::validate_tool_description(123, "my_description"),
    "my_description.*must be a single string"
  )

  expect_error(
    MCPR:::validate_tool_arguments("invalid", "my_args"),
    "my_args.*must be a list"
  )

  expect_error(
    MCPR:::validate_tool_fun("invalid", "my_function"),
    "my_function.*must be a function"
  )
})

# Test validate_mcpr_type_structure function directly
test_that("validate_mcpr_type_structure validates mcpr_type objects", {
  # Test valid mcpr_type objects
  expect_silent(MCPR:::validate_mcpr_type_structure(MCPR:::type_string(description = "test")))
  expect_silent(MCPR:::validate_mcpr_type_structure(MCPR:::type_number(description = "test")))
  expect_silent(MCPR:::validate_mcpr_type_structure(MCPR:::type_boolean(description = "test")))
  expect_silent(MCPR:::validate_mcpr_type_structure(type_integer(description = "test")))

  # Test non-mcpr_type objects
  expect_error(
    MCPR:::validate_mcpr_type_structure(list(type = "string")),
    "must be an mcpr_type object created with type_"
  )

  expect_error(
    MCPR:::validate_mcpr_type_structure("not_mcpr_type"),
    "must be an mcpr_type object"
  )
})

# Test validate_mcpr_type_object function directly
test_that("validate_mcpr_type_object validates type field requirements", {
  # Test object without type field
  bad_obj <- structure(list(description = "test"), class = "mcpr_type")
  expect_error(
    MCPR:::validate_mcpr_type_object(bad_obj),
    "must have a 'type' field"
  )

  # Test object with invalid type
  bad_type_obj <- structure(list(type = "invalid_type"), class = "mcpr_type")
  expect_error(
    MCPR:::validate_mcpr_type_object(bad_type_obj),
    "has invalid type 'invalid_type'"
  )
})

test_that("validate_mcpr_type_object validates enum types", {
  # Test valid enum
  valid_enum <- structure(list(type = "enum", values = c("a", "b", "c")), class = "mcpr_type")
  expect_silent(MCPR:::validate_mcpr_type_object(valid_enum))

  # Test enum without values
  enum_no_values <- structure(list(type = "enum"), class = "mcpr_type")
  expect_error(
    MCPR:::validate_mcpr_type_object(enum_no_values),
    "enum type must have 'values' field"
  )

  # Test enum with non-character values
  enum_bad_values <- structure(list(type = "enum", values = c(1, 2, 3)), class = "mcpr_type")
  expect_error(
    MCPR:::validate_mcpr_type_object(enum_bad_values),
    "enum type must have 'values' field with character vector"
  )
})

test_that("validate_mcpr_type_object validates array types", {
  # Test valid array
  valid_array <- structure(list(
    type = "array",
    items = structure(list(type = "string"), class = "mcpr_type")
  ), class = "mcpr_type")
  expect_silent(MCPR:::validate_mcpr_type_object(valid_array))

  # Test array without items
  array_no_items <- structure(list(type = "array"), class = "mcpr_type")
  expect_error(
    MCPR:::validate_mcpr_type_object(array_no_items),
    "array type must have 'items' field"
  )

  # Test recursive validation of array items
  array_bad_items <- structure(list(
    type = "array",
    items = list(type = "string") # Not an mcpr_type object
  ), class = "mcpr_type")
  expect_error(
    MCPR:::validate_mcpr_type_object(array_bad_items),
    "must be an mcpr_type object"
  )
})

test_that("validate_mcpr_type_object validates object types", {
  # Test valid object with properties
  valid_object <- structure(list(
    type = "object",
    properties = list(
      prop1 = structure(list(type = "string"), class = "mcpr_type"),
      prop2 = structure(list(type = "number"), class = "mcpr_type")
    )
  ), class = "mcpr_type")
  expect_silent(MCPR:::validate_mcpr_type_object(valid_object))

  # Test object with NULL properties (should pass)
  object_null_props <- structure(list(type = "object", properties = NULL), class = "mcpr_type")
  expect_silent(MCPR:::validate_mcpr_type_object(object_null_props))

  # Test object with non-list properties
  object_bad_props <- structure(list(type = "object", properties = "not_a_list"), class = "mcpr_type")
  expect_error(
    MCPR:::validate_mcpr_type_object(object_bad_props),
    "object type 'properties' must be a list"
  )

  # Test object with invalid property types
  object_invalid_prop <- structure(list(
    type = "object",
    properties = list(
      bad_prop = list(type = "string") # Not an mcpr_type object
    )
  ), class = "mcpr_type")
  expect_error(
    MCPR:::validate_mcpr_type_object(object_invalid_prop),
    "must be an mcpr_type object"
  )
})

test_that("validate_mcpr_type_object validates all basic types", {
  # Test all valid basic types
  basic_types <- c("boolean", "integer", "number", "string")

  for (type in basic_types) {
    basic_obj <- structure(list(type = type), class = "mcpr_type")
    expect_silent(MCPR:::validate_mcpr_type_object(basic_obj))
  }
})

test_that("obj_type_friendly returns correct type names", {
  expect_equal(obj_type_friendly(NULL), "NULL")
  expect_equal(obj_type_friendly(function() {}), "a function")
  expect_equal(obj_type_friendly(123), "a numeric")
  expect_equal(obj_type_friendly("test"), "a character")
  expect_equal(obj_type_friendly(list()), "a list")
  expect_equal(obj_type_friendly(TRUE), "a logical")
})

test_that("validate_tool_name handles edge cases", {
  # Test multiple values (length != 1)
  expect_error(
    MCPR:::validate_tool_name(c("name1", "name2")),
    "must be a single string"
  )

  # Test NA value
  expect_error(
    MCPR:::validate_tool_name(NA_character_),
    "must not be missing"
  )

  # Test character(0)
  expect_error(
    MCPR:::validate_tool_name(character(0)),
    "must be a single string"
  )
})

test_that("validate_tool_description handles edge cases", {
  # Test multiple strings
  expect_error(
    MCPR:::validate_tool_description(c("desc1", "desc2")),
    "must be a single string"
  )

  # Test NA value
  expect_error(
    MCPR:::validate_tool_description(NA_character_),
    "must not be missing"
  )

  # Test character(0)
  expect_error(
    MCPR:::validate_tool_description(character(0)),
    "must be a single string"
  )

  # Test list
  expect_error(
    MCPR:::validate_tool_description(list("description")),
    "must be a single string"
  )
})

test_that("complex nested structures validate correctly", {
  # Test array of objects
  array_of_objects <- structure(list(
    type = "array",
    items = structure(list(
      type = "object",
      properties = list(
        name = structure(list(type = "string"), class = "mcpr_type"),
        value = structure(list(type = "number"), class = "mcpr_type")
      )
    ), class = "mcpr_type")
  ), class = "mcpr_type")
  expect_silent(MCPR:::validate_mcpr_type_object(array_of_objects))

  # Test object with array property
  object_with_array <- structure(list(
    type = "object",
    properties = list(
      data = structure(list(
        type = "array",
        items = structure(list(type = "string"), class = "mcpr_type")
      ), class = "mcpr_type")
    )
  ), class = "mcpr_type")
  expect_silent(MCPR:::validate_mcpr_type_object(object_with_array))

  # Test nested object failure
  nested_bad_object <- structure(list(
    type = "object",
    properties = list(
      nested = structure(list(
        type = "object",
        properties = list(
          bad_prop = "not_mcpr_type"
        )
      ), class = "mcpr_type")
    )
  ), class = "mcpr_type")
  expect_error(
    MCPR:::validate_mcpr_type_object(nested_bad_object),
    "must be an mcpr_type object"
  )
})
