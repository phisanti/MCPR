# Tests for MCPResourceRegistry and make_default_mcp_resource_registry

reader_text <- function(text = "hello", mime = "text/plain") {
  force(text); force(mime)
  function() list(text = text, mimeType = mime)
}

test_that("register() stores a resource and has(uri) returns TRUE", {
  reg <- MCPResourceRegistry$new()
  reg$register("data://x", "X", reader_text())
  expect_true(reg$has("data://x"))
})

test_that("has() returns FALSE for mcp_app_only resources when client lacks support", {
  reg <- MCPResourceRegistry$new()
  reg$register("ui://x", "X", reader_text(), mcp_app_only = TRUE)
  expect_true(reg$has("ui://x", mcp_apps_supported = TRUE))
  expect_false(reg$has("ui://x", mcp_apps_supported = FALSE))
})

test_that("duplicate URI aborts unless overwrite = TRUE", {
  reg <- MCPResourceRegistry$new()
  reg$register("data://x", "X", reader_text())
  expect_error(
    reg$register("data://x", "Y", reader_text()),
    regexp = "already registered"
  )
})

test_that("overwrite = TRUE replaces an existing entry", {
  reg <- MCPResourceRegistry$new()
  reg$register("data://x", "X", reader_text("first"))
  expect_silent(
    reg$register("data://x", "X2", reader_text("second"), overwrite = TRUE)
  )
  res <- reg$read("data://x")
  expect_equal(res$contents[[1]]$text, "second")
})

test_that("list(mcp_apps_supported = FALSE) excludes mcp_app_only entries", {
  reg <- MCPResourceRegistry$new()
  reg$register("data://a", "A", reader_text())
  reg$register("ui://b", "B", reader_text(), mcp_app_only = TRUE)
  visible <- reg$list(mcp_apps_supported = FALSE)
  uris <- vapply(visible, function(e) e$uri, character(1))
  expect_setequal(uris, "data://a")
})

test_that("list(mcp_apps_supported = TRUE) includes mcp_app_only entries", {
  reg <- MCPResourceRegistry$new()
  reg$register("data://a", "A", reader_text())
  reg$register("ui://b", "B", reader_text(), mcp_app_only = TRUE)
  visible <- reg$list(mcp_apps_supported = TRUE)
  uris <- vapply(visible, function(e) e$uri, character(1))
  expect_setequal(uris, c("data://a", "ui://b"))
})

test_that("read() invokes resource_reader and returns contents list", {
  reg <- MCPResourceRegistry$new()
  reg$register("data://x", "X", reader_text("payload", "text/plain"))
  res <- reg$read("data://x")
  expect_true(is.list(res))
  expect_true(is.list(res$contents))
  expect_equal(res$contents[[1]]$text, "payload")
  expect_equal(res$contents[[1]]$uri, "data://x")
  expect_equal(res$contents[[1]]$mimeType, "text/plain")
})

test_that("read() wraps simple text reader output into contents shape", {
  reg <- MCPResourceRegistry$new()
  reg$register("data://x", "X", function() list(text = "hi", mimeType = "text/plain"))
  res <- reg$read("data://x")
  expect_equal(res$contents[[1]]$text, "hi")
  expect_equal(res$contents[[1]]$mimeType, "text/plain")
})

test_that("read() wraps blob reader output correctly", {
  reg <- MCPResourceRegistry$new()
  reg$register(
    "data://b", "B",
    function() list(blob = "aGVsbG8=", mimeType = "application/octet-stream")
  )
  res <- reg$read("data://b")
  expect_equal(res$contents[[1]]$blob, "aGVsbG8=")
  expect_equal(res$contents[[1]]$mimeType, "application/octet-stream")
  expect_null(res$contents[[1]]$text)
})

test_that("read() returns NULL for mcp_app_only resource when client lacks support", {
  reg <- MCPResourceRegistry$new()
  reg$register("ui://x", "X", reader_text(), mcp_app_only = TRUE)
  expect_null(reg$read("ui://x", mcp_apps_supported = FALSE))
})

test_that("read() returns NULL for unknown URI", {
  reg <- MCPResourceRegistry$new()
  expect_null(reg$read("unknown://nope"))
})

test_that("read() re-throws when resource_reader errors", {
  reg <- MCPResourceRegistry$new()
  reg$register("data://x", "X", function() stop("boom"))
  expect_error(reg$read("data://x"), "boom")
})

test_that("read() aborts when reader returns invalid output (no text/blob)", {
  reg <- MCPResourceRegistry$new()
  reg$register("data://x", "X", function() list(mimeType = "text/plain"))
  expect_error(reg$read("data://x"), regexp = "text.*blob")
})

test_that("make_default_mcp_resource_registry registers the plot viewer URI", {
  reg <- make_default_mcp_resource_registry("1.0.0")
  expect_true(reg$has(MCPR:::MCPR_PLOT_VIEWER_RESOURCE_URI))
})

test_that("list() descriptor shape includes required fields and omits NULL optionals", {
  reg <- MCPResourceRegistry$new()
  reg$register("data://x", "X", reader_text())
  desc <- reg$list()[[1]]
  expect_equal(desc$uri, "data://x")
  expect_equal(desc$name, "X")
  expect_false("description" %in% names(desc))
  expect_false("mimeType" %in% names(desc))
  expect_false("title" %in% names(desc))
  expect_false("annotations" %in% names(desc))
  expect_false("size" %in% names(desc))
  expect_false("_meta" %in% names(desc))

  reg$register(
    "data://y", "Y", reader_text(),
    description = "d", mimeType = "text/plain", title = "T",
    annotations = list(a = 1), size = 42, meta = list(k = "v")
  )
  desc2 <- reg$list()[[2]]
  expect_equal(desc2$description, "d")
  expect_equal(desc2$mimeType, "text/plain")
  expect_equal(desc2$title, "T")
  expect_equal(desc2$annotations, list(a = 1))
  expect_equal(desc2$size, 42)
  expect_equal(desc2[["_meta"]], list(k = "v"))
})
