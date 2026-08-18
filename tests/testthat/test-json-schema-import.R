test_that("JSON object import preserves required and open-object semantics", {
  source_schema <- list(
    type = "object",
    properties = list(
      required_field = list(type = "string"),
      optional_field = list(type = "integer")
    ),
    required = list("required_field")
  )

  spec <- MCPR:::map_type_schema(source_schema, input_type = "json")

  expect_true(spec$properties$required_field$required)
  expect_false(spec$properties$optional_field$required)
  expect_true(spec$additional_properties)

  emitted <- MCPR:::mcpr_type_to_json_schema(spec)
  expect_equal(as.character(emitted$required), "required_field")
  expect_true(emitted$additionalProperties)

  expect_no_error(MCPR:::normalize_arg_by_type(
    list(required_field = "value", undeclared_field = TRUE),
    spec
  ))
  expect_error(
    MCPR:::normalize_arg_by_type(list(optional_field = 1L), spec),
    "missing required field"
  )
})

test_that("JSON object import preserves explicit closed-object semantics", {
  source_schema <- list(
    type = "object",
    properties = list(known = list(type = "string")),
    additionalProperties = FALSE
  )

  spec <- MCPR:::map_type_schema(source_schema, input_type = "json")

  expect_false(spec$properties$known$required)
  expect_false(spec$additional_properties)
  expect_error(
    MCPR:::normalize_arg_by_type(list(extra = "value"), spec),
    "unexpected field"
  )
})

test_that("schema-valued additionalProperties remains permissive on import", {
  source_schema <- list(
    type = "object",
    properties = list(known = list(type = "string")),
    additionalProperties = list(type = "integer")
  )

  spec <- MCPR:::map_type_schema(source_schema, input_type = "json")

  expect_true(spec$additional_properties)
  expect_no_error(MCPR:::normalize_arg_by_type(list(extra = 1L), spec))
})

test_that("mcprClient derives top-level argument requiredness from inputSchema", {
  client <- mcprClient$new(config = tempfile())
  tool <- list(inputSchema = list(
    type = "object",
    properties = list(
      required_arg = list(type = "string"),
      optional_arg = list(type = "integer")
    ),
    required = list("required_arg")
  ))

  arguments <- client$as_mcpr_types(tool)

  expect_true(arguments$required_arg$required)
  expect_false(arguments$optional_arg$required)
  expect_equal(
    as.character(MCPR:::convert_arguments_to_schema(arguments)$required),
    "required_arg"
  )

  imported_tool <- ToolDef$new(
    fun = function(required_arg, optional_arg = NULL) {
      list(required_arg = required_arg, optional_arg = optional_arg)
    },
    name = "imported_tool",
    description = "Imported tool fixture",
    arguments = arguments
  )
  expect_no_error(imported_tool$call(required_arg = "value"))
  expect_error(imported_tool$call(optional_arg = 1L), "Missing required parameter")
})

test_that("JSON import fix does not weaken MCPR-authored type defaults", {
  expect_true(type_string()$required)
  expect_true(type_object(field = type_string())$required)
  expect_true(type_object(field = type_string())$properties$field$required)
  expect_false(type_object(field = type_string())$additional_properties)
})
