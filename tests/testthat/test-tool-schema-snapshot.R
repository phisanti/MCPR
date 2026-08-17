# Pins the JSON Schema emitted for every supported roxygen type token.
#
# The schema is protocol surface: agents plan tool calls against it, so any
# drift in what a token emits is a breaking change for every downstream tool.
# These snapshots exist to make that drift impossible to introduce silently.

flat_token_tool_code <- "
#' @title Flat Token Surface
#' @description Exercises every supported roxygen type token.
#' @param a string A string.
#' @param b character A character.
#' @param c number A number.
#' @param d numeric A numeric.
#' @param e integer An integer.
#' @param f int An int.
#' @param g boolean A boolean.
#' @param h logical A logical.
#' @param i bool A bool.
#' @param j array An array.
#' @param k object An object.
#' @param l json_object A json object.
#' @param m json_array A json array.
#' @param n named_list A named list.
#' @param o list A list.
#' @keywords mcpr_tool
flat_token_surface <- function(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o = NULL) {
  invisible(NULL)
}
"

nested_tool_code <- "
#' @title Nested Query Surface
#' @description Exercises the nested object declaration syntax.
#' @param query object{terms: array, mode?: enum(auto|exact|contains|regex), max_hits?: integer} Structured search request. Terms are OR-matched.
#' @param rank_by enum(score_desc|score_asc|name) Sort key for the result table.
#' @keywords mcpr_tool
nested_query_surface <- function(query, rank_by = \"score_desc\") {
  invisible(NULL)
}
"

# Builds the registry over a throwaway tools dir and returns the wire-format
# JSON for one tool, exactly as `tools/list` would emit it.
emitted_tool_schema <- function(code, tool_name, env = parent.frame()) {
  tools_dir <- withr::local_tempdir(.local_envir = env)
  writeLines(code, file.path(tools_dir, "tool-snapshot.R"))

  registry <- ToolRegistry$new(tools_dir = tools_dir, verbose = FALSE)
  registry$search_tools()

  emitted <- MCPR:::tool_as_json(registry$get_tool(tool_name))
  # Provenance annotations carry the throwaway tools dir, which differs on
  # every run. Everything else here is the protocol surface under test.
  emitted$annotations <- NULL

  jsonlite::toJSON(emitted, auto_unbox = TRUE, pretty = TRUE)
}

# cran = TRUE because the default skips whenever NOT_CRAN is unset — which
# includes the project's own `cd tests && Rscript testthat.R`. A snapshot that
# only runs under devtools is not the guard this file claims to be. The output
# is pure jsonlite over a fixed schema: no network, no timing, no locale.
test_that("flat type tokens emit a stable JSON Schema", {
  expect_snapshot(
    cat(emitted_tool_schema(flat_token_tool_code, "flat_token_surface")),
    cran = TRUE
  )
})

test_that("nested object and enum tokens emit a stable JSON Schema", {
  expect_snapshot(
    cat(emitted_tool_schema(nested_tool_code, "nested_query_surface")),
    cran = TRUE
  )
})
