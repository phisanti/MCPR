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

test_that("decode_tool_args handles basic arguments", {
  # Test basic argument passthrough
  basic_args <- list(x = 42, y = "test", z = TRUE)
  result <- .client_tools_env$decode_tool_args(basic_args)
  expect_equal(result, basic_args)

  # Test non-list input
  expect_equal(.client_tools_env$decode_tool_args("not a list"), "not a list")
  expect_equal(.client_tools_env$decode_tool_args(42), 42)
})

test_that("decode_tool_args detects MCP type markers", {
  # Test arguments with MCP type markers
  mcp_args <- list(
    simple = 42,
    complex = list(
      "_mcp_type" = "numeric",
      "value" = list(1, 2, "Inf")
    )
  )

  # Should trigger from_mcp_json processing
  result <- .client_tools_env$decode_tool_args(mcp_args)
  expect_true(is.list(result))
  # The exact structure depends on from_mcpr_json implementation
  expect_true(!is.null(result))
})

test_that("decode_tool_args handles empty and NULL inputs", {
  # Test NULL
  expect_null(.client_tools_env$decode_tool_args(NULL))

  # Test empty list
  expect_equal(.client_tools_env$decode_tool_args(list()), list())

  # Test list without MCP markers
  no_markers <- list(a = 1, b = list(x = 2, y = 3))
  expect_equal(.client_tools_env$decode_tool_args(no_markers), no_markers)
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

test_that("encode_tool_results keeps hidden graph metadata behind assistant text", {
  test_data <- list(id = 81)
  text_result <- list(
    content = "This tool call rendered a plot in the viewer.",
    `_meta` = list(
      ui = list(resourceUri = "ui://mcpr/plots"),
      mcpr_graph = list(
        kind = "image",
        mimeType = "image/png",
        data = "base64data"
      )
    )
  )

  output <- .client_tools_env$encode_tool_results(test_data, text_result)

  expect_equal(output$result$content[[1]]$type, "text")
  expect_equal(output$result$content[[1]]$text, "This tool call rendered a plot in the viewer.")
  expect_equal(output$result$content[[1]]$annotations$audience, list("assistant"))
  expect_equal(output$result$`_meta`$ui$resourceUri, "ui://mcpr/plots")
  expect_equal(output$result$`_meta`$mcpr_graph$kind, "image")
  expect_equal(output$result$`_meta`$mcpr_graph$data, "base64data")
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
