tools_dir <- system.file(package = "MCPR", mustWork = TRUE)

# Capture the finalized message cat_json would write to stdout.
capture_wire_message <- function(server, request) {
  captured <- NULL
  local_mocked_bindings(
    cat_json = function(x) {
      captured <<- MCPR:::finalize_result_message(x)
      invisible(NULL)
    },
    .package = "MCPR"
  )
  server$.__enclos_env__$private$handle_message_from_client(request)
  captured
}

# Build a stateless (2026-07-28) request with per-request _meta.
modern_request <- function(method, id = 1L, params = list(),
                           version = "2026-07-28",
                           capabilities = setNames(list(), character()),
                           client_name = "test-client") {
  meta <- list()
  meta[["io.modelcontextprotocol/protocolVersion"]] <- version
  meta[["io.modelcontextprotocol/clientInfo"]] <- list(name = client_name, version = "1.0")
  meta[["io.modelcontextprotocol/clientCapabilities"]] <- capabilities
  params[["_meta"]] <- meta
  jsonlite::toJSON(
    list(jsonrpc = "2.0", id = id, method = method, params = params),
    auto_unbox = TRUE
  )
}

ui_capabilities <- function() {
  list(extensions = list(`io.modelcontextprotocol/ui` = setNames(list(), character())))
}

test_that("server/discover advertises stateless versions as a cacheable result", {
  server <- mcprServer$new(.tools_dir = tools_dir)
  resp <- capture_wire_message(server, modern_request("server/discover"))

  expect_null(resp$error)
  expect_equal(unlist(resp$result$supportedVersions), MCPR:::MODERN_VERSIONS)
  expect_true(all(c("tools", "resources", "prompts") %in% names(resp$result$capabilities)))
  expect_equal(resp$result$resultType, "complete")
  expect_equal(resp$result$cacheScope, "public")
  expect_true(resp$result$ttlMs > 0)
  expect_equal(
    resp$result[["_meta"]][["io.modelcontextprotocol/serverInfo"]]$name,
    "R MCPR server"
  )
})

test_that("unsupported _meta protocol version returns UnsupportedProtocolVersionError", {
  server <- mcprServer$new(.tools_dir = tools_dir)
  resp <- capture_wire_message(server, modern_request("tools/list", version = "2099-01-01"))

  expect_equal(resp$error$code, -32022L)
  expect_equal(resp$error$data$requested, "2099-01-01")
  expect_equal(unlist(resp$error$data$supported), MCPR:::MODERN_VERSIONS)
})

test_that("stateless tools/list works without initialize and is cacheable", {
  server <- mcprServer$new(.tools_dir = tools_dir)
  resp <- capture_wire_message(server, modern_request("tools/list"))

  expect_null(resp$error)
  expect_gt(length(resp$result$tools), 0)
  expect_equal(resp$result$resultType, "complete")
  expect_equal(resp$result$cacheScope, "public")
  expect_true(is.numeric(resp$result$ttlMs))
})

test_that("stateless requests take client capabilities per request, not from the connection", {
  server <- mcprServer$new(.tools_dir = tools_dir)
  viewer <- MCPR:::MCPR_PLOT_VIEWER_RESOURCE_URI
  uris <- function(resp) vapply(resp$result$resources, function(r) r$uri, character(1))

  with_ui <- capture_wire_message(
    server, modern_request("resources/list", capabilities = ui_capabilities())
  )
  expect_true(viewer %in% uris(with_ui))

  without_ui <- capture_wire_message(server, modern_request("resources/list", id = 2L))
  expect_false(viewer %in% uris(without_ui))

  # Stateless requests must not leak into the handshake-era connection state.
  expect_false(server$mcp_apps_supported())
})

test_that("resource-not-found error code follows the request's protocol era", {
  server <- mcprServer$new(.tools_dir = tools_dir)

  modern <- capture_wire_message(
    server, modern_request("resources/read", params = list(uri = "data://missing"))
  )
  expect_equal(modern$error$code, -32602L)

  legacy <- capture_wire_message(
    server,
    '{"jsonrpc":"2.0","id":3,"method":"resources/read","params":{"uri":"data://missing"}}'
  )
  expect_equal(legacy$error$code, -32002L)
})

test_that("handshake-era initialize still negotiates a legacy version", {
  server <- mcprServer$new(.tools_dir = tools_dir)
  init <- jsonlite::toJSON(list(
    jsonrpc = "2.0", id = 0L, method = "initialize",
    params = list(
      protocolVersion = "2025-11-25",
      capabilities = setNames(list(), character()),
      clientInfo = list(name = "legacy-client", version = "1.0")
    )
  ), auto_unbox = TRUE)

  resp <- suppressMessages(capture_wire_message(server, init))
  expect_equal(resp$result$protocolVersion, "2025-11-25")
  expect_equal(resp$result$resultType, "complete")
})

test_that("finalize_result_message stamps results and leaves errors alone", {
  existing_meta <- list(`ui/resourceUri` = "ui://x")
  msg <- list(jsonrpc = "2.0", id = 1, result = list(content = list(), `_meta` = existing_meta))
  out <- MCPR:::finalize_result_message(msg, server_version = "9.9.9")

  expect_equal(out$result$resultType, "complete")
  expect_equal(out$result[["_meta"]][["ui/resourceUri"]], "ui://x")
  expect_equal(
    out$result[["_meta"]][["io.modelcontextprotocol/serverInfo"]],
    list(name = "R MCPR server", version = "9.9.9")
  )

  kept <- MCPR:::finalize_result_message(
    list(jsonrpc = "2.0", id = 2, result = list(resultType = "input_required"))
  )
  expect_equal(kept$result$resultType, "input_required")

  err <- list(jsonrpc = "2.0", id = 3, error = list(code = -32603L, message = "x"))
  expect_identical(MCPR:::finalize_result_message(err), err)
})

test_that("cat_json writes finalized results", {
  written <- NULL
  local_mocked_bindings(
    to_json = function(x, ...) {
      written <<- x
      ""
    },
    .package = "MCPR"
  )
  MCPR:::cat_json(list(jsonrpc = "2.0", id = 1, result = list(tools = list())))
  expect_equal(written$result$resultType, "complete")
})

test_that("unkeyed session responses are stamped and keep JSON nulls", {
  written <- NULL
  local_mocked_bindings(
    write_stdout = function(x) {
      written <<- x
      invisible(NULL)
    },
    .package = "nanonext"
  )
  private <- new.env()
  private$log_comm <- function(...) invisible(NULL)

  MCPR:::mcpr_forwarding_handle_message(
    private,
    '{"jsonrpc":"2.0","id":5,"result":{"content":[],"structuredContent":{"x":null}}}'
  )
  parsed <- jsonlite::parse_json(written)
  expect_equal(parsed$result$resultType, "complete")
  expect_true(grepl('"x":null', written, fixed = TRUE))
})

test_that("mcprClient probes server/discover and falls back to initialize", {
  client <- mcprClient$new()
  priv <- client$.__enclos_env__$private

  probe <- priv$mcp_request_discover()
  expect_equal(probe$method, "server/discover")
  expect_equal(
    probe$params[["_meta"]][["io.modelcontextprotocol/protocolVersion"]],
    max(MCPR:::MODERN_VERSIONS)
  )

  respond <- function(response) {
    unlockBinding("send_and_receive", priv)
    priv$send_and_receive <- function(process, message) response
    lockBinding("send_and_receive", priv)
  }

  respond(list(result = list(supportedVersions = list("2026-07-28"))))
  expect_equal(priv$discover_protocol_version(NULL), "2026-07-28")

  respond(list(error = list(code = -32601L, message = "Method not found")))
  expect_null(priv$discover_protocol_version(NULL))

  respond(NULL)
  expect_null(priv$discover_protocol_version(NULL))

  respond(list(error = list(code = -32022L, data = list(supported = list("2099-01-01")))))
  expect_error(priv$discover_protocol_version(NULL), "no protocol version")
})

test_that("mcprClient attaches _meta only for stateless servers", {
  client <- mcprClient$new()
  priv <- client$.__enclos_env__$private
  request <- priv$mcp_request_tools_list()

  expect_identical(priv$with_request_meta(request, NULL), request)

  modern <- priv$with_request_meta(request, "2026-07-28")
  meta <- modern$params[["_meta"]]
  expect_equal(meta[["io.modelcontextprotocol/protocolVersion"]], "2026-07-28")
  expect_equal(meta[["io.modelcontextprotocol/clientInfo"]]$name, "MCPR Client")
  expect_match(
    as.character(jsonlite::toJSON(modern, auto_unbox = TRUE)),
    '"io.modelcontextprotocol/clientCapabilities":{}',
    fixed = TRUE
  )
})
