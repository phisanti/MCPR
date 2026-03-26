get_local_or_installed_path <- function(...) {
  candidates <- c(...)
  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0) existing[[1]] else NULL
}

.client_tools_env <- new.env(parent = asNamespace("MCPR"))
.client_tools_path <- get_local_or_installed_path(
  "R/mcpr-client-tools.R",
  "../R/mcpr-client-tools.R",
  "../../R/mcpr-client-tools.R"
)
source(.client_tools_path, local = .client_tools_env)

test_that("coerce_args_by_schema parses stringified objects when schema says object", {
  schema <- list(
    config = structure(list(type = "object"), class = "mcpr_type"),
    name = structure(list(type = "string"), class = "mcpr_type")
  )

  args <- list(config = '{"key": "value"}', name = "test")
  result <- .client_tools_env$coerce_args_by_schema(args, schema)

  expect_type(result$config, "list")
  expect_equal(result$config$key, "value")
  expect_equal(result$name, "test") # string left alone
})

test_that("coerce_args_by_schema parses stringified arrays when schema says array", {
  schema <- list(
    items = structure(list(type = "array"), class = "mcpr_type")
  )

  args <- list(items = '[1, 2, 3]')
  result <- .client_tools_env$coerce_args_by_schema(args, schema)

  expect_type(result$items, "list")
  expect_equal(result$items, list(1, 2, 3))
})

test_that("coerce_args_by_schema unlists unnamed lists for scalar types", {
  schema <- list(
    x = structure(list(type = "number"), class = "mcpr_type")
  )

  args <- list(x = list(1, 2, 3))
  result <- .client_tools_env$coerce_args_by_schema(args, schema)

  expect_equal(result$x, c(1, 2, 3))
})

test_that("coerce_args_by_schema leaves correct types untouched", {
  schema <- list(
    config = structure(list(type = "object"), class = "mcpr_type"),
    name = structure(list(type = "string"), class = "mcpr_type")
  )

  # Already correct types
  args <- list(config = list(key = "value"), name = "test")
  result <- .client_tools_env$coerce_args_by_schema(args, schema)
  expect_equal(result, args)
})

test_that("coerce_args_by_schema handles missing schema gracefully", {
  schema <- list(
    known = structure(list(type = "string"), class = "mcpr_type")
  )

  # Extra arg not in schema — left untouched
  args <- list(known = "hello", unknown = list(1, 2))
  result <- .client_tools_env$coerce_args_by_schema(args, schema)
  expect_equal(result$unknown, list(1, 2))
})

test_that("coerce_args_by_schema does not parse strings for string schema", {
  schema <- list(
    data = structure(list(type = "string"), class = "mcpr_type")
  )

  # A string that happens to be valid JSON — should NOT be parsed
  args <- list(data = '{"key": "value"}')
  result <- .client_tools_env$coerce_args_by_schema(args, schema)
  expect_equal(result$data, '{"key": "value"}')
})

test_that("coerce_args_by_schema passes through when schema has non-mcpr_type entries", {
  # Simulates ToolDef$call with string-shorthand arguments (e.g. arguments = list(x = "number"))
  schema <- list(x = "number", y = "string")

  args <- list(x = 42, y = "hello")
  result <- .client_tools_env$coerce_args_by_schema(args, schema)
  expect_equal(result, args) # no coercion, safe passthrough
})

test_that("encode_tool_results handles simple text results", {
  # Test simple character result
  test_data <- list(id = 1)
  simple_result <- "Simple text result"

  output <- .client_tools_env$encode_tool_results(test_data, simple_result)

  expect_true(is.list(output))
  expect_equal(output$jsonrpc, "2.0")
  expect_equal(output$id, 1)
  expect_equal(output$result$content[[1]]$type, "text")
  expect_equal(output$result$content[[1]]$text, simple_result)
  expect_false(output$result$isError)
})

test_that("encode_tool_results handles complex objects", {
  # Test complex result that should use mcpr_serialize
  test_data <- list(id = 2)
  complex_result <- data.frame(
    x = 1:3,
    y = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )

  output <- .client_tools_env$encode_tool_results(test_data, complex_result)

  expect_true(is.list(output))
  expect_equal(output$jsonrpc, "2.0")
  expect_equal(output$id, 2)
  expect_equal(output$result$content[[1]]$type, "text")
  expect_false(output$result$isError)

  # The text should be JSON serialized
  expect_true(is.character(output$result$content[[1]]$text))
  expect_true(nchar(output$result$content[[1]]$text) > 0)
})

test_that("encode_tool_results handles numeric vectors", {
  # Test numeric vector (should trigger mcpr_serialize)
  test_data <- list(id = 3)
  numeric_result <- c(1, 2, 3, 4, 5)

  output <- .client_tools_env$encode_tool_results(test_data, numeric_result)

  expect_true(is.list(output))
  expect_equal(output$id, 3)
  expect_false(output$result$isError)

  # Should use mcpr_serialize since it's not a single character
  expect_true(is.character(output$result$content[[1]]$text))
})

test_that("encode_tool_results handles lists", {
  # Test list result
  test_data <- list(id = 4)
  list_result <- list(status = "success", data = 1:5, message = "Complete")

  output <- .client_tools_env$encode_tool_results(test_data, list_result)

  expect_true(is.list(output))
  expect_equal(output$id, 4)
  expect_false(output$result$isError)

  # Should be serialized as JSON
  text_content <- output$result$content[[1]]$text
  expect_true(is.character(text_content))
  expect_true(grepl("success", text_content))
})

test_that("encode_tool_results handles image content with explicit audience", {
  test_data <- list(id = 5)
  image_result <- list(
    type = "image",
    data = "abc",
    mimeType = "image/png",
    annotations = list(audience = list("mcp_app"))
  )

  output <- .client_tools_env$encode_tool_results(test_data, image_result)

  expect_equal(output$id, 5)
  expect_equal(output$result$content[[1]]$type, "image")
  expect_equal(output$result$content[[1]]$data, "abc")
  expect_equal(output$result$content[[1]]$mimeType, "image/png")
  expect_equal(output$result$content[[1]]$annotations$audience, list("mcp_app"))
})

test_that("encode_tool_results defaults audience to assistant for image content", {
  test_data <- list(id = 6)
  image_result <- list(type = "image", data = "xyz", mimeType = "image/png")

  output <- .client_tools_env$encode_tool_results(test_data, image_result)

  expect_equal(output$result$content[[1]]$type, "image")
  expect_equal(output$result$content[[1]]$annotations$audience, list("assistant"))
})

test_that("encode_tool_results no longer special-cases pre-built content arrays", {
  test_data <- list(id = 7)
  content_result <- list(
    content = list(
      list(type = "text", text = "hi"),
      list(type = "image", data = "x", mimeType = "image/png")
    )
  )

  output <- .client_tools_env$encode_tool_results(test_data, content_result)

  expect_equal(length(output$result$content), 1)
  expect_equal(output$result$content[[1]]$type, "text")
  expect_true(grepl("\"content\"", output$result$content[[1]]$text, fixed = TRUE))
})

test_that("encode_tool_results handles text content with metadata", {
  test_data <- list(id = 8)
  text_result <- list(
    type = "text",
    content = "Plot displayed",
    `_meta` = list(ui = list(resourceUri = "ui://mcpr/plots"))
  )

  output <- .client_tools_env$encode_tool_results(test_data, text_result)

  expect_equal(output$result$content[[1]]$type, "text")
  expect_equal(output$result$content[[1]]$text, "Plot displayed")
  expect_equal(output$result$content[[1]]$annotations$audience, list("user"))
  expect_equal(output$result$`_meta`$ui$resourceUri, "ui://mcpr/plots")
})

test_that("encode_tool_results passes structuredContent image alongside content array", {
  test_data <- list(id = 82)
  result <- list(
    content = list(list(type = "text", text = "Plot rendered.")),
    structuredContent = list(
      kind = "image",
      mimeType = "image/png",
      data = "iVBORw0KGgo="
    )
  )

  output <- .client_tools_env$encode_tool_results(test_data, result)

  expect_equal(output$id, 82)
  expect_equal(output$result$content[[1]]$type, "text")
  expect_equal(output$result$content[[1]]$text, "Plot rendered.")
  expect_equal(output$result$structuredContent$kind, "image")
  expect_equal(output$result$structuredContent$data, "iVBORw0KGgo=")
  expect_null(output$result[["_meta"]])
})

test_that("encode_tool_results passes structuredContent plotly alongside content array", {
  test_data <- list(id = 83)
  result <- list(
    content = list(list(type = "text", text = "Widget rendered.")),
    structuredContent = list(
      kind = "plotly",
      spec = list(data = list(), layout = list(), config = list())
    )
  )

  output <- .client_tools_env$encode_tool_results(test_data, result)

  expect_equal(output$id, 83)
  expect_equal(output$result$content[[1]]$text, "Widget rendered.")
  expect_equal(output$result$structuredContent$kind, "plotly")
  expect_true(!is.null(output$result$structuredContent$spec))
  expect_null(output$result[["_meta"]])
})

test_that("encode_tool_results structuredContent serializes correctly with auto_unbox", {
  test_data <- list(id = 84)
  result <- list(
    content = list(list(type = "text", text = "ok")),
    structuredContent = list(
      kind = "image",
      mimeType = "image/png",
      data = "abc123"
    )
  )

  output <- .client_tools_env$encode_tool_results(test_data, result)

  # Round-trip via jsonlite to verify serialization
  json_str <- jsonlite::toJSON(output, auto_unbox = TRUE, null = "null")
  parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

  expect_equal(parsed$result$structuredContent$kind, "image")
  expect_equal(parsed$result$structuredContent$mimeType, "image/png")
  expect_equal(parsed$result$structuredContent$data, "abc123")
  expect_equal(parsed$result$content[[1]]$type, "text")
  expect_equal(parsed$result$content[[1]]$text, "ok")
})

test_that("encode_tool_results does not trigger structuredContent branch for empty structuredContent", {
  test_data <- list(id = 85)
  result <- list(
    content = list(list(type = "text", text = "should fall through")),
    structuredContent = list()
  )

  output <- .client_tools_env$encode_tool_results(test_data, result)

  # Empty structuredContent should fall through to mcpr_serialize, not the structuredContent branch
  expect_equal(output$id, 85)
  expect_null(output$result$structuredContent)
  expect_equal(output$result$content[[1]]$type, "text")
  # Result was serialized as a complex object, not passed through as-is
  expect_true(grepl("structuredContent", output$result$content[[1]]$text))
})

test_that("encode_tool_results rejects partially-named structuredContent", {
  test_data <- list(id = 86)
  result <- list(
    content = list(list(type = "text", text = "bad")),
    structuredContent = list(kind = "image", "orphan_value")
  )

  output <- .client_tools_env$encode_tool_results(test_data, result)

  # Should fall through to mcpr_serialize, not the structuredContent branch
  expect_null(output$result$structuredContent)
  expect_equal(output$result$content[[1]]$type, "text")
  expect_true(grepl("structuredContent", output$result$content[[1]]$text))
})

test_that("encode_tool_results preserves _meta passthrough for image", {
  test_data <- list(id = 9)
  image_result <- list(
    type = "image",
    data = "base64data",
    mimeType = "image/png",
    `_meta` = list(ui = list(resourceUri = "ui://mcpr/plots"))
  )

  output <- .client_tools_env$encode_tool_results(test_data, image_result)

  expect_equal(output$result$`_meta`$ui$resourceUri, "ui://mcpr/plots")
  expect_equal(output$result$content[[1]]$annotations$audience, list("user"))
})
