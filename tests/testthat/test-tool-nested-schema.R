# Nested object / enum roxygen type declarations.
#
# Covers the declaration grammar, the emitted JSON Schema, and the requirement
# that a malformed declaration aborts the registry build instead of quietly
# dropping the tool it belongs to.

build_type <- function(declaration, description = "A description.") {
  MCPR:::map_definition_type_schema(
    declaration,
    description = description,
    parameter_name = "query",
    function_name = "some_tool",
    file_path = "tool-some.R"
  )
}

# Writes a single-tool file and returns the registry built over it.
registry_for <- function(lines, env = parent.frame()) {
  tools_dir <- withr::local_tempdir(.local_envir = env)
  writeLines(lines, file.path(tools_dir, "tool-fixture.R"))
  ToolRegistry$new(tools_dir = tools_dir, verbose = FALSE)
}

nested_tool_lines <- function(declaration) {
  c(
    "#' @title Fixture",
    "#' @description A fixture tool.",
    paste0("#' @param query ", declaration, " The search request."),
    "#' @keywords mcpr_tool",
    "fixture_tool <- function(query) query"
  )
}

# --- declaration grammar ---

test_that("object refinement builds typed properties with per-field requiredness", {
  spec <- build_type("object{terms: array, mode?: enum(auto|exact), max_hits?: integer}")

  expect_equal(spec$type, "object")
  expect_equal(names(spec$properties), c("terms", "mode", "max_hits"))
  expect_equal(spec$properties$terms$type, "array")
  expect_equal(spec$properties$mode$values, c("auto", "exact"))
  expect_equal(spec$properties$max_hits$type, "integer")

  expect_true(spec$properties$terms$required)
  expect_false(spec$properties$mode$required)
  expect_false(spec$properties$max_hits$required)

  # A declared field list is closed.
  expect_false(spec$additional_properties)
})

test_that("enum refinement is available as a standalone parameter type", {
  spec <- build_type("enum(nes_desc|q_asc|name)")

  expect_equal(spec$type, "enum")
  expect_equal(spec$values, c("nes_desc", "q_asc", "name"))
  expect_equal(spec$description, "A description.")
})

test_that("object refinements nest", {
  spec <- build_type("object{filter: object{gene: string, dir?: enum(up|down)}}")

  inner <- spec$properties$filter
  expect_equal(inner$type, "object")
  expect_equal(names(inner$properties), c("gene", "dir"))
  expect_equal(inner$properties$dir$values, c("up", "down"))
})

test_that("refinement whitespace is optional", {
  spec <- build_type("object{terms:array,mode?:enum(auto|exact)}")

  expect_equal(names(spec$properties), c("terms", "mode"))
  expect_equal(spec$properties$mode$values, c("auto", "exact"))
})

test_that("bare object keeps its open-ended legacy meaning", {
  spec <- build_type("object")

  expect_equal(spec$type, "object")
  expect_length(spec$properties, 0)
  expect_true(spec$additional_properties)
})

# --- malformed declarations ---

test_that("malformed refinements abort with the shared unsupported-type class", {
  malformed <- list(
    "unterminated brace" = "object{terms: array",
    "empty field list" = "object{}",
    "field without a type" = "object{terms}",
    "duplicate field" = "object{a: string, a: integer}",
    "unknown nested type" = "object{terms: date}",
    "enum without values" = "enum",
    "empty enum parens" = "enum()",
    "empty enum value" = "enum(auto||exact)",
    "whitespace-only field list" = "object{ }",
    "field name with a space" = "object{max hits: integer}",
    "field name shadowing a type_object argument" = "object{.required: string}",
    "wrong delimiter for object" = "object(terms|mode)",
    "wrong delimiter for enum" = "enum{auto|exact}",
    "refinement on a scalar type" = "string(auto|exact)",
    # strsplit() drops a trailing empty field, so this used to read as a
    # well-formed one-value enum.
    "trailing enum separator" = "enum(auto|)",
    "trailing enum separator with space" = "enum(auto| )"
  )

  for (label in names(malformed)) {
    expect_error(
      build_type(malformed[[label]]),
      class = "mcpr_unsupported_type_error",
      info = label
    )
  }
})

test_that("an unknown type is reported as unsupported even when it carries a refinement", {
  expect_error(build_type("date{x: string}"), "Unsupported MCPR type declaration")
})

test_that("type names are case-insensitive but enum values are not", {
  spec <- build_type("OBJECT{Mode: ENUM(Auto|Exact)}")

  expect_equal(spec$type, "object")
  expect_equal(spec$properties$Mode$values, c("Auto", "Exact"))
})

test_that("nested errors name the offending field path", {
  expect_error(
    build_type("object{filter: object{dir: date}}"),
    "parameter = query.filter.dir"
  )
})

test_that("a malformed declaration aborts the registry instead of dropping the tool", {
  registry <- registry_for(nested_tool_lines("object{terms: array"))

  # roxygen2 discards a @param whose braces are unbalanced, so this never
  # reaches the type parser at all: the parameter simply goes missing. Without
  # the formals check that surfaces as a warning and a vanished tool.
  expect_error(
    registry$search_tools(),
    class = "mcpr_unsupported_type_error"
  )
  expect_error(registry$search_tools(), "Undocumented parameter")
})

test_that("registry re-raise preserves a message containing braces", {
  # Regression: re-raising via cli_abort(conditionMessage(e)) fed the braces in
  # the message back through glue and reported a parse error instead. The
  # fixture has to be a *malformed* refinement, because only those messages
  # carry braces — the `i` bullet spells out `object{field: type, ...}`.
  # An unsupported-type message like `object{terms: date}` has none, and so
  # passes even against the old handler.
  registry <- registry_for(nested_tool_lines("enum(a|b The mode."))

  expect_error(registry$search_tools(), class = "mcpr_unsupported_type_error")
  expect_error(registry$search_tools(), "Malformed MCPR type declaration")
  expect_error(registry$search_tools(), "Unterminated")
})

test_that("an unsupported nested type is still reported through the registry", {
  registry <- registry_for(nested_tool_lines("object{terms: date}"))

  expect_error(registry$search_tools(), "Unsupported MCPR type declaration")
})

# --- emitted schema and runtime agreement ---

test_that("registry emits properties, required and additionalProperties for a nested param", {
  registry <- registry_for(
    nested_tool_lines("object{terms: array, mode?: enum(auto|exact|contains|regex), max_hits?: integer}")
  )
  registry$search_tools()

  schema <- MCPR:::tool_as_json(registry$get_tool("fixture_tool"))$inputSchema
  query <- schema$properties$query

  expect_equal(names(query$properties), c("terms", "mode", "max_hits"))
  expect_equal(query$properties$mode$type, "string")
  # I()-wrapped, like `required` below, so that auto_unbox keeps it an array.
  expect_equal(
    as.character(query$properties$mode$enum),
    c("auto", "exact", "contains", "regex")
  )
  expect_equal(as.character(query$required), "terms")
  expect_false(query$additionalProperties)

  parsed <- jsonlite::fromJSON(MCPR:::to_json(schema), simplifyVector = FALSE)
  expect_equal(parsed$properties$query$required, list("terms"))
  expect_equal(
    parsed$properties$query$properties$mode$enum,
    list("auto", "exact", "contains", "regex")
  )
})

test_that("the emitted shape is the shape the runtime accepts", {
  spec <- build_type("object{terms: array, mode?: enum(auto|exact), max_hits?: integer}")

  accepted <- MCPR:::normalize_arg_by_type(
    list(terms = list("BRCA1", "TP53"), mode = "exact"),
    spec,
    path = "query"
  )
  expect_equal(accepted$terms, c("BRCA1", "TP53"))
  expect_equal(accepted$mode, "exact")

  expect_error(
    MCPR:::normalize_arg_by_type(list(mode = "exact"), spec, path = "query"),
    "missing required field"
  )
  expect_error(
    MCPR:::normalize_arg_by_type(list(terms = list("BRCA1"), mode = "fuzzy"), spec, path = "query"),
    "should be one of"
  )
  expect_error(
    MCPR:::normalize_arg_by_type(list(terms = list("BRCA1"), term = "x"), spec, path = "query"),
    "unexpected field"
  )
})

# --- the resolved `list` token ---

test_that("list and named_list are aliases of json_object", {
  # Assert the concrete target type, not equality with the same function's own
  # output for another token: all three regressing together must fail here.
  for (token in c("json_object", "named_list", "list")) {
    spec <- MCPR:::dispatch_definition_type_schema(token, "d")
    expect_equal(spec$type, "json_object", info = token)
    expect_equal(spec, MCPR:::type_json_object(description = "d"), info = token)
  }

  # An open-ended `object` is a different thing and must not collapse into it.
  expect_equal(MCPR:::dispatch_definition_type_schema("object", "d")$type, "object")
})

test_that("dispatch refuses a token it has no mapping for", {
  expect_error(MCPR:::dispatch_definition_type_schema("date", "d"))
})

# --- edges that used to fail silently ---

test_that("a single-value enum still emits an array", {
  schema <- MCPR:::mcpr_type_to_json_schema(build_type("enum(only)"))

  expect_equal(as.character(schema$enum), "only")
  # Regression: without I() auto_unbox collapsed this to `"enum":"only"`,
  # which is not valid JSON Schema.
  expect_equal(
    jsonlite::fromJSON(MCPR:::to_json(schema), simplifyVector = FALSE)$enum,
    list("only")
  )
})

test_that("a nested single-value enum also emits an array", {
  schema <- MCPR:::mcpr_type_to_json_schema(build_type("object{mode?: enum(only)}"))
  parsed <- jsonlite::fromJSON(MCPR:::to_json(schema), simplifyVector = FALSE)

  expect_equal(parsed$properties$mode$enum, list("only"))
})

test_that("a space before a refinement is rejected rather than silently ignored", {
  ctx <- MCPR:::definition_type_context("query", "some_tool", "tool-some.R")

  # Previously degraded to a bare open-ended object with `{a: string}` left
  # sitting in the description.
  expect_error(
    MCPR:::split_definition_declaration("object {a: string} A thing.", ctx),
    class = "mcpr_unsupported_type_error"
  )
  expect_error(
    MCPR:::split_definition_declaration("enum (a|b) A thing.", ctx),
    class = "mcpr_unsupported_type_error"
  )

  # A type that takes no refinement is unaffected: the brace is just prose.
  expect_equal(
    MCPR:::split_definition_declaration("string {literal} A thing.", ctx)$type,
    "string"
  )
})

test_that("mcpr_type_to_json_schema tolerates a non-mcpr_type property", {
  # The function documents itself as accepting "an mcpr_type object or fallback
  # value", and the properties loop must honour that too.
  spec <- MCPR:::type_object(.description = "d", a = "a string")

  expect_no_error(MCPR:::mcpr_type_to_json_schema(spec))
  expect_equal(MCPR:::mcpr_type_to_json_schema(spec)$properties$a$type, "string")
})

# --- formals coverage check ---

test_that("`...` does not count as an undocumented parameter", {
  expect_silent(
    MCPR:::check_schema_covers_formals(
      list(x = MCPR:::type_string()),
      formals(function(x, ...) x),
      "f",
      "tool-f.R"
    )
  )
})

test_that("one @param tag may document several parameters", {
  registry <- registry_for(c(
    "#' @title Fixture",
    "#' @description A fixture tool.",
    "#' @param x,y integer Two bounds.",
    "#' @keywords mcpr_tool",
    "fixture_tool <- function(x, y) x + y"
  ))
  registry$search_tools()

  args <- registry$get_tool("fixture_tool")$arguments
  expect_equal(names(args), c("x", "y"))
  expect_equal(args$x$type, "integer")
  expect_equal(args$y$description, "Two bounds.")
})

test_that("a genuinely undocumented parameter still aborts the registry", {
  registry <- registry_for(c(
    "#' @title Fixture",
    "#' @description A fixture tool.",
    "#' @param x integer A bound.",
    "#' @keywords mcpr_tool",
    "fixture_tool <- function(x, y) x + y"
  ))

  expect_error(registry$search_tools(), "Undocumented parameter")
})
