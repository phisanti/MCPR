# Unit tests for view-vignette.R
# Tests topic parsing, source resolution, description extraction, index/fetch/section
# rendering, and error paths for view(what="vignette").

# ---- Helpers ----

skip_if_no_dplyr_vignettes <- function() {
  testthat::skip_if_not_installed("dplyr")
  info <- tryCatch(tools::getVignetteInfo("dplyr"), error = function(e) NULL)
  testthat::skip_if(is.null(info) || nrow(info) == 0, "dplyr has no installed vignettes")
}

make_vignette_info_matrix <- function(package, dir, topic, file, title, r, pdf) {
  matrix(
    c(package, dir, topic, file, title, r, pdf),
    nrow = length(topic),
    ncol = 7,
    dimnames = list(NULL, c("Package", "Dir", "Topic", "File", "Title", "R", "PDF"))
  )
}

# ---- parse_vignette_topic() ----

test_that("parse_vignette_topic splits bare package name", {
  result <- MCPR:::parse_vignette_topic("dplyr")
  expect_equal(result$package, "dplyr")
  expect_null(result$name)
  expect_null(result$section)
})

test_that("parse_vignette_topic splits pkg::name", {
  result <- MCPR:::parse_vignette_topic("dplyr::colwise")
  expect_equal(result$package, "dplyr")
  expect_equal(result$name, "colwise")
  expect_null(result$section)
})

test_that("parse_vignette_topic splits pkg::name#Section", {
  result <- MCPR:::parse_vignette_topic("dplyr::colwise#Basic usage")
  expect_equal(result$package, "dplyr")
  expect_equal(result$name, "colwise")
  expect_equal(result$section, "Basic usage")
})

test_that("parse_vignette_topic rejects empty package segment", {
  expect_error(MCPR:::parse_vignette_topic("::colwise"), "package name")
})

test_that("parse_vignette_topic rejects empty name after ::", {
  expect_error(MCPR:::parse_vignette_topic("dplyr::"), "cannot be empty")
})

test_that("parse_vignette_topic rejects empty topic", {
  expect_error(MCPR:::parse_vignette_topic(""), "must not be empty")
  expect_error(MCPR:::parse_vignette_topic(NULL), "must not be empty")
})

test_that("parse_vignette_topic trims whitespace", {
  result <- MCPR:::parse_vignette_topic("  dplyr :: colwise # Basic usage  ")
  expect_equal(result$package, "dplyr")
  expect_equal(result$name, "colwise")
  expect_equal(result$section, "Basic usage")
})

# ---- resolve_vignette_source() ----

test_that("resolve_vignette_source returns existing paths for an installed package", {
  skip_if_no_dplyr_vignettes()

  result <- MCPR:::resolve_vignette_source("dplyr")
  expect_true(nrow(result) > 0)
  expect_true(all(file.exists(result$path)))
  expect_true(all(c("name", "title", "path", "is_tangled_only") %in% colnames(result)))
})

test_that("resolve_vignette_source returns zero rows for a package with no vignettes", {
  info <- make_vignette_info_matrix(character(0), character(0), character(0), character(0), character(0), character(0), character(0))
  local_mocked_bindings(
    getVignetteInfo = function(package) info,
    .package = "tools"
  )
  result <- MCPR:::resolve_vignette_source("nopkg")
  expect_equal(nrow(result), 0)
})

test_that("resolve_vignette_source falls back to tangled R file when source is missing", {
  tmp_dir <- withr::local_tempdir()
  doc_dir <- file.path(tmp_dir, "doc")
  dir.create(doc_dir)
  writeLines("df <- 1:10", file.path(doc_dir, "example.R"))

  info <- make_vignette_info_matrix("fakepkg", tmp_dir, "example", "example.Rmd", "Example", "example.R", "example.html")
  local_mocked_bindings(
    getVignetteInfo = function(package) info,
    .package = "tools"
  )

  result <- MCPR:::resolve_vignette_source("fakepkg")
  expect_true(result$is_tangled_only[1])
  expect_true(file.exists(result$path[1]))
  expect_true(grepl("example\\.R$", result$path[1]))
})

test_that("resolve_vignette_source aborts when neither source nor tangled file exists", {
  tmp_dir <- withr::local_tempdir()
  dir.create(file.path(tmp_dir, "doc"))

  info <- make_vignette_info_matrix("fakepkg", tmp_dir, "ghost", "ghost.Rmd", "Ghost", "ghost.R", "ghost.html")
  local_mocked_bindings(
    getVignetteInfo = function(package) info,
    .package = "tools"
  )

  expect_error(MCPR:::resolve_vignette_source("fakepkg"), "No readable source")
})

# ---- extract_vignette_description() ----

test_that("extract_vignette_description reads a YAML folded block scalar", {
  tmp <- withr::local_tempfile(fileext = ".Rmd")
  writeLines(c(
    "---",
    "title: \"Example\"",
    "description: >",
    "  Learn how to easily repeat the same operation across multiple",
    "  columns using `across()`.",
    "output: rmarkdown::html_vignette",
    "---",
    "",
    "Body text here."
  ), tmp)

  result <- MCPR:::extract_vignette_description(tmp)
  expect_true(grepl("^Learn how to easily repeat", result))
  expect_true(grepl("across\\(\\)", result))
})

test_that("extract_vignette_description falls back to first prose paragraph", {
  tmp <- withr::local_tempfile(fileext = ".Rmd")
  writeLines(c(
    "---",
    "title: \"Example\"",
    "output: rmarkdown::html_vignette",
    "---",
    "",
    "```{r, include = FALSE}",
    "knitr::opts_chunk$set(collapse = TRUE)",
    "```",
    "",
    "# Heading",
    "",
    "This is the first real paragraph of prose in the vignette."
  ), tmp)

  result <- MCPR:::extract_vignette_description(tmp)
  expect_equal(result, "This is the first real paragraph of prose in the vignette.")
})

test_that("extract_vignette_description truncates long descriptions", {
  tmp <- withr::local_tempfile(fileext = ".Rmd")
  long_line <- paste(rep("word", 60), collapse = " ")
  writeLines(c(
    "---",
    "description: >",
    paste0("  ", long_line),
    "---",
    ""
  ), tmp)

  result <- MCPR:::extract_vignette_description(tmp, max_length = 50)
  expect_true(nchar(result) <= 50)
  expect_true(grepl("\\.\\.\\.$", result))
})

# ---- view_vignette_index() ----

test_that("view_vignette_index renders a package-level index for dplyr", {
  skip_if_no_dplyr_vignettes()

  result <- MCPR:::view_vignette_index("dplyr")
  expect_type(result, "character")
  expect_true(grepl("^Vignettes: dplyr \\(9\\)", result))
  expect_true(grepl("colwise", result))
  expect_true(grepl("lines,.*R chunks", result))
  expect_true(grepl("Use topic=\"dplyr::<name>\"", result))
  expect_true(grepl("dplyr::<name>#<section>", result))
})

test_that("view_vignette_index reports zero vignettes clearly", {
  info <- make_vignette_info_matrix(character(0), character(0), character(0), character(0), character(0), character(0), character(0))
  local_mocked_bindings(
    getVignetteInfo = function(package) info,
    .package = "tools"
  )
  result <- MCPR:::view_vignette_index("nopkg")
  expect_true(grepl("Vignettes: nopkg \\(0\\)", result))
  expect_true(grepl("No vignettes found", result))
})

# ---- view_vignette_fetch() ----

test_that("view_vignette_fetch returns full source with truncation footer", {
  skip_if_no_dplyr_vignettes()

  result <- MCPR:::view_vignette_fetch("dplyr", "colwise", max_lines = 20)
  expect_true(grepl("Vignette: dplyr::colwise", result))
  expect_true(grepl("package:dplyr", result))
  expect_true(grepl("File:.*colwise\\.Rmd", result))
  expect_true(grepl("more lines", result))
})

test_that("view_vignette_fetch errors for an unknown vignette name", {
  skip_if_no_dplyr_vignettes()
  expect_error(MCPR:::view_vignette_fetch("dplyr", "not-a-real-vignette"), "No vignette named")
})

# ---- view_vignette_section() and fence-tracking ----

test_that("scan_markdown_headings ignores ATX-like lines inside fenced code blocks", {
  skip_if_no_dplyr_vignettes()

  vignettes <- MCPR:::resolve_vignette_source("dplyr")
  path <- vignettes$path[vignettes$name == "colwise"]
  skip_if(length(path) == 0, "colwise vignette not found")

  lines <- readLines(path, warn = FALSE)
  headings <- MCPR:::scan_markdown_headings(lines)

  # colwise.Rmd contains "# ->" R-comment lines inside ```{r} chunks; these
  # must never be picked up as level-1 Markdown headings.
  expect_false(any(headings$title == "->"))
  expect_true("Basic usage" %in% headings$title)
})

test_that("view_vignette_section slices exactly the requested section body", {
  skip_if_no_dplyr_vignettes()

  result <- MCPR:::view_vignette_section("dplyr", "colwise", "Basic usage")
  expect_true(grepl("Vignette: dplyr::colwise", result))
  expect_true(grepl("§Basic usage", result))
  expect_true(grepl("package:dplyr", result))
  # Body of "Basic usage" should not include the sibling section header text
  # for "`_if`, `_at`, `_all`" (next same-level heading, which terminates the slice).
  expect_false(grepl("^`_if`, `_at`, `_all`$", result))
})

test_that("view_vignette_section matches section case-insensitively and trimmed", {
  skip_if_no_dplyr_vignettes()

  result <- MCPR:::view_vignette_section("dplyr", "colwise", "  BASIC USAGE  ")
  expect_true(grepl("§Basic usage", result))
})

test_that("view_vignette_section lists real headings on no match", {
  skip_if_no_dplyr_vignettes()

  expect_error(
    MCPR:::view_vignette_section("dplyr", "colwise", "Not A Real Section"),
    "Did you mean"
  )
})

test_that("view_vignette_section errors for an unknown vignette name", {
  skip_if_no_dplyr_vignettes()
  expect_error(MCPR:::view_vignette_section("dplyr", "not-a-real-vignette", "x"), "No vignette named")
})

# ---- Missing-source fallback ----

test_that("view_vignette_fetch serves tangled R code with a note when source is absent", {
  tmp_dir <- withr::local_tempdir()
  doc_dir <- file.path(tmp_dir, "doc")
  dir.create(doc_dir)
  writeLines(c("## Example tangled code", "1 + 1"), file.path(doc_dir, "example.R"))

  info <- make_vignette_info_matrix("fakepkg", tmp_dir, "example", "example.Rmd", "Example", "example.R", "example.html")
  local_mocked_bindings(
    getVignetteInfo = function(package) info,
    .package = "tools"
  )

  result <- MCPR:::view_vignette_fetch("fakepkg", "example", max_lines = 10)
  expect_true(grepl("code only", result, ignore.case = TRUE))
  expect_true(grepl("1 \\+ 1", result))
})

test_that("view_vignette_fetch aborts when both source and tangled file are absent", {
  tmp_dir <- withr::local_tempdir()
  dir.create(file.path(tmp_dir, "doc"))

  info <- make_vignette_info_matrix("fakepkg", tmp_dir, "ghost", "ghost.Rmd", "Ghost", "ghost.R", "ghost.html")
  local_mocked_bindings(
    getVignetteInfo = function(package) info,
    .package = "tools"
  )

  expect_error(MCPR:::view_vignette_fetch("fakepkg", "ghost"), "No readable source")
})

test_that("resolving a non-installed package errors clearly", {
  expect_error(
    MCPR:::resolve_vignette_source("thispackagedoesnotexist12345"),
    "is not installed"
  )
})

# ---- view_vignette() dispatcher ----

test_that("view_vignette dispatches to index, fetch, and section based on topic depth", {
  skip_if_no_dplyr_vignettes()

  index_result <- MCPR:::view_vignette("dplyr")
  expect_true(grepl("^Vignettes: dplyr", index_result))

  fetch_result <- MCPR:::view_vignette("dplyr::colwise", max_lines = 10)
  expect_true(grepl("^Vignette: dplyr::colwise", fetch_result))

  section_result <- MCPR:::view_vignette("dplyr::colwise#Basic usage")
  expect_true(grepl("§Basic usage", section_result))
})
