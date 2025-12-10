test_that("tool() creates valid ToolDef objects", {
  # Test basic tool creation
  simple_tool <- tool(
    fun = function(x) x + 1,
    description = "Add 1 to a number",
    arguments = list(x = "number")
  )

  expect_true(inherits(simple_tool, "ToolDef"))
  expect_true(inherits(simple_tool, "R6"))
  expect_equal(simple_tool$description, "Add 1 to a number")
  expect_true(is.function(simple_tool$fun))
  expect_equal(names(simple_tool$arguments), "x")
})

test_that("tool() handles name parameter correctly", {
  # Test explicit name
  named_tool <- tool(
    fun = function(x) x * 2,
    name = "double",
    description = "Double a number",
    arguments = list(x = "number")
  )

  expect_equal(named_tool$name, "double")

  # Test auto-generated name from function
  test_fun <- function(x) x
  auto_named_tool <- tool(
    fun = test_fun,
    description = "Identity function",
    arguments = list(x = MCPR:::type_number(description = "Input value"))
  )

  expect_equal(auto_named_tool$name, "test_fun")
})

test_that("tool() validates input parameters", {
  # Test invalid function
  expect_error(tool(
    fun = "not_a_function",
    description = "Test"
  ))

  # Test missing description
  expect_error(tool(
    fun = function(x) x,
    arguments = list()
  ))

  # Test invalid arguments
  expect_error(tool(
    fun = function(x) x,
    description = "Test",
    arguments = "not_a_list"
  ))
})

test_that("ToolDef class has proper structure", {
  test_tool <- tool(
    fun = function(a, b) a + b,
    description = "Add two numbers",
    arguments = list(
      a = MCPR:::type_number(),
      b = MCPR:::type_number()
    )
  )

  # Test field access
  expect_true(is.function(test_tool$fun))
  expect_equal(test_tool$description, "Add two numbers")
  expect_true(test_tool$convert)
  expect_equal(length(test_tool$arguments), 2)

  # Test method exists
  expect_true(is.function(test_tool$call))
})

test_that("ToolDef$call method works correctly", {
  add_tool <- tool(
    fun = function(x, y) x + y,
    description = "Add two numbers",
    arguments = list(
      x = MCPR:::type_number(),
      y = MCPR:::type_number()
    )
  )

  result <- add_tool$call(x = 5, y = 3)
  expect_equal(result, 8)
})

test_that("tool_annotations creates proper annotation lists", {
  # Test basic annotations
  annotations <- MCPR:::tool_annotations(
    title = "Test Tool",
    read_only_hint = TRUE,
    open_world_hint = FALSE
  )

  expect_true(is.list(annotations))
  expect_equal(annotations$title, "Test Tool")
  expect_true(annotations$read_only_hint)
  expect_false(annotations$open_world_hint)

  # Test with additional parameters
  custom_annotations <- MCPR:::tool_annotations(
    title = "Custom Tool",
    custom_field = "custom_value"
  )

  expect_equal(custom_annotations$custom_field, "custom_value")
})

test_that("tool_reject throws proper errors", {
  # Test default rejection
  expect_error(MCPR:::tool_reject(), "Tool call rejected")

  # Test custom reason
  expect_error(
    MCPR:::tool_reject("Custom rejection reason"),
    "Custom rejection reason"
  )

  # Test error class
  tryCatch(
    MCPR:::tool_reject("Test"),
    error = function(e) {
      expect_true("mcpr_tool_reject" %in% class(e))
    }
  )
})

test_that("unique_tool_name generates unique names", {
  # Clear counter for testing
  the_env <- MCPR:::the
  if (exists("cur_tool_id", envir = the_env)) {
    the_env$cur_tool_id <- 0
  }

  name1 <- unique_tool_name()
  name2 <- unique_tool_name()

  expect_true(is.character(name1))
  expect_true(is.character(name2))
  expect_false(name1 == name2)
  expect_true(grepl("tool_", name1))
})

test_that("tool with convert=FALSE doesn't convert arguments", {
  no_convert_tool <- tool(
    fun = function(x) class(x),
    description = "Return class of input",
    arguments = list(x = MCPR:::type_string(description = "Input value")),
    convert = FALSE
  )

  expect_false(no_convert_tool$convert)

  # The call method should respect the convert flag
  result <- no_convert_tool$call(x = "test")
  expect_equal(result, "character")
})
