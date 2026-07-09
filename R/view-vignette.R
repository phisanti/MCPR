# View Vignette Functions
# Package vignette browsing for view(what="vignette"): index, full-source fetch, and section slicing.
# Reads raw .Rmd/.Rnw/.qmd source from disk via tools::getVignetteInfo() — never renders to PDF/HTML.

# ---- Topic Parsing ----

#' Parse a vignette topic string into package / name / section
#' @param topic Topic string: "pkg", "pkg::name", or "pkg::name#Section"
#' @return list(package = chr, name = chr|NULL, section = chr|NULL)
#' @include view-utils.R
#' @noRd
parse_vignette_topic <- function(topic) {
  if (is.null(topic) || !is.character(topic) || length(topic) != 1 || is.na(topic) || nchar(trimws(topic)) == 0) {
    cli::cli_abort("Vignette topic must not be empty.")
  }

  sep_pos <- regexpr("::", topic, fixed = TRUE)
  has_sep <- sep_pos != -1

  package <- trimws(if (has_sep) substr(topic, 1, sep_pos - 1) else topic)

  if (nchar(package) == 0) {
    cli::cli_abort("Vignette topic must start with a package name, e.g. {.code pkg} or {.code pkg::name}.")
  }

  if (!has_sep) {
    return(list(package = package, name = NULL, section = NULL))
  }

  remainder <- substr(topic, sep_pos + 2, nchar(topic))

  hash_pos <- regexpr("#", remainder, fixed = TRUE)
  if (hash_pos == -1) {
    name <- trimws(remainder)
    section <- NULL
  } else {
    name <- trimws(substr(remainder, 1, hash_pos - 1))
    section <- trimws(substr(remainder, hash_pos + 1, nchar(remainder)))
    if (nchar(section) == 0) {
      section <- NULL
    }
  }

  if (nchar(name) == 0) {
    cli::cli_abort("Vignette name after '{.code ::}' cannot be empty in topic {.val {topic}}.")
  }

  list(package = package, name = name, section = section)
}

# ---- Source Resolution ----

#' Resolve vignette source file paths for an installed package
#' @param package Package name
#' @return data.frame with columns: name, title, path, is_tangled_only
#' @noRd
resolve_vignette_source <- function(package) {
  info <- tryCatch(
    tools::getVignetteInfo(package),
    error = function(e) cli::cli_abort("Package {.pkg {package}} is not installed.")
  )
  info <- as.data.frame(info, stringsAsFactors = FALSE)

  if (nrow(info) == 0) {
    return(info[0, , drop = FALSE])
  }

  source_path <- file.path(info$Dir, "doc", info$File)
  source_exists <- file.exists(source_path)

  tangled_path <- file.path(info$Dir, "doc", info$R)
  tangled_exists <- nzchar(info$R) & file.exists(tangled_path)

  path <- ifelse(source_exists, source_path, tangled_path)
  is_tangled_only <- !source_exists & tangled_exists

  missing <- !source_exists & !tangled_exists
  if (any(missing)) {
    missing_names <- info$Topic[missing]
    cli::cli_abort(c(
      "No readable source found for {.val {missing_names}} in package {.pkg {package}}.",
      "i" = "Neither the .Rmd/.Rnw/.qmd source nor a tangled .R file exists on disk."
    ))
  }

  data.frame(
    name = info$Topic,
    title = info$Title,
    path = path,
    is_tangled_only = is_tangled_only,
    stringsAsFactors = FALSE
  )
}

# ---- Package-level Index ----

#' Extract the YAML `description:` field from a frontmatter block
#' @param frontmatter Character vector of lines between the `---` fences
#' @return Description string, or "" if the field is absent
#' @noRd
extract_yaml_description <- function(frontmatter) {
  # Folded/literal block scalar: description: > (or >-) followed by indented lines
  desc_idx <- grep("^description:\\s*(>-?)?\\s*$", frontmatter)
  if (length(desc_idx) > 0) {
    start <- desc_idx[1] + 1
    field_indent <- NA
    desc_lines <- character(0)
    i <- start
    while (i <= length(frontmatter)) {
      line <- frontmatter[i]
      if (!nzchar(trimws(line))) {
        break
      }
      indent <- nchar(sub("^(\\s*).*$", "\\1", line))
      if (is.na(field_indent)) {
        field_indent <- indent
      }
      if (indent < field_indent) {
        break
      }
      desc_lines <- c(desc_lines, trimws(line))
      i <- i + 1
    }
    if (length(desc_lines) > 0) {
      return(paste(desc_lines, collapse = " "))
    }
  }

  # Inline scalar: description: "some text" or description: some text
  inline_idx <- grep("^description:\\s*\\S", frontmatter)
  if (length(inline_idx) > 0) {
    inline_text <- sub("^description:\\s*", "", frontmatter[inline_idx[1]])
    inline_text <- gsub('^"|"$', "", trimws(inline_text))
    if (nzchar(inline_text)) {
      return(inline_text)
    }
  }

  ""
}

#' Find the first non-empty prose paragraph in a vignette body
#' @param body Character vector of lines after the frontmatter block
#' @return First prose line, skipping headings and fenced code; "" if none
#' @noRd
extract_first_prose_paragraph <- function(body) {
  in_fence <- FALSE
  for (line in body) {
    if (grepl("^```", line)) {
      in_fence <- !in_fence
      next
    }
    if (in_fence) {
      next
    }
    trimmed <- trimws(line)
    if (!nzchar(trimmed) || grepl("^#+\\s", trimmed)) {
      next
    }
    return(trimmed)
  }

  ""
}

#' Extract a vignette's description from YAML frontmatter or first paragraph
#' @param path Path to vignette source file
#' @param max_length Maximum description length before truncation
#' @return Description string, or "" if none found
#' @noRd
extract_vignette_description <- function(path, max_length = 110) {
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
  if (length(lines) == 0) {
    return("")
  }

  fm_bounds <- grep("^---\\s*$", lines)
  frontmatter <- character(0)
  body_start <- 1

  if (length(fm_bounds) >= 2) {
    frontmatter <- lines[(fm_bounds[1] + 1):(fm_bounds[2] - 1)]
    body_start <- fm_bounds[2] + 1
  }

  description <- extract_yaml_description(frontmatter)
  if (!nzchar(description)) {
    description <- extract_first_prose_paragraph(lines[body_start:length(lines)])
  }

  if (!nzchar(description)) {
    return("")
  }

  truncate_text(description, max_length)
}

#' Count fenced R code chunks in a vignette source file
#' @param lines Character vector of file lines
#' @return Integer count of ```{r...} chunks
#' @noRd
count_r_chunks <- function(lines) {
  length(grep("^```\\{r", lines))
}

#' Format a single vignette's row in the package-level index
#' @param v One-row data.frame slice from resolve_vignette_source()
#' @param indent Whitespace prefix aligning wrapped lines under the name column
#' @param name_width Column width to left-pad the vignette name to
#' @return Formatted multi-line row block (no leading/trailing blank lines)
#' @noRd
format_vignette_index_row <- function(v, indent, name_width) {
  lines <- tryCatch(readLines(v$path, warn = FALSE), error = function(e) character(0))
  description <- extract_vignette_description(v$path)
  n_chunks <- count_r_chunks(lines)

  desc_block <- ""
  if (nzchar(description)) {
    quoted <- paste0('"', description, '"')
    desc_wrapped <- strwrap(quoted, width = 58)
    desc_block <- paste0(paste0(indent, desc_wrapped), collapse = "\n")
    desc_block <- paste0(desc_block, "\n")
  }

  stats_line <- if (isTRUE(v$is_tangled_only)) {
    paste0(indent, length(lines), " lines (tangled code only)")
  } else {
    paste0(indent, length(lines), " lines, ", n_chunks, " R chunks")
  }

  name_padded <- formatC(v$name, width = -name_width)
  paste0(
    "  ", name_padded, "  ", v$title, "\n",
    desc_block,
    stats_line
  )
}

#' Build the package-level vignette index
#' @param package Package name
#' @return Formatted index string
#' @noRd
view_vignette_index <- function(package) {
  vignettes <- resolve_vignette_source(package)

  if (nrow(vignettes) == 0) {
    return(paste0("Vignettes: ", package, " (0)\n\nNo vignettes found for package '", package, "'."))
  }

  header <- paste0("Vignettes: ", package, " (", nrow(vignettes), ")")
  name_width <- max(nchar(vignettes$name))
  indent <- strrep(" ", name_width + 4)

  rows <- vapply(seq_len(nrow(vignettes)), function(i) {
    format_vignette_index_row(vignettes[i, ], indent, name_width)
  }, character(1))

  footer <- paste0(
    "Use topic=\"", package, "::<name>\" to read the full source,\n",
    "or topic=\"", package, "::<name>#<section>\" for a single section."
  )

  paste0(header, "\n\n", paste(rows, collapse = "\n\n"), "\n\n", footer)
}

# ---- Full Fetch ----

#' Format the right-padded "Vignette: <label> ... package:<pkg>" header line
#' @param label Text shown after "Vignette: " (e.g. "pkg::name" or "pkg::name  §Section")
#' @param package Package name shown after the padding
#' @param width Minimum column width to right-align "package:" against
#' @return Single formatted header line (no trailing newline)
#' @noRd
format_vignette_header <- function(label, package, width) {
  sprintf("Vignette: %s%spackage:%s", label, strrep(" ", max(1, width - nchar(label))), package)
}

#' Fetch full vignette source with truncation
#' @param package Package name
#' @param name Vignette name
#' @param max_lines Maximum lines to display
#' @return Formatted vignette source string
#' @noRd
view_vignette_fetch <- function(package, name, max_lines = 100) {
  vignettes <- resolve_vignette_source(package)
  match_idx <- which(vignettes$name == name)

  if (length(match_idx) == 0) {
    cli::cli_abort(c(
      "No vignette named {.val {name}} in package {.pkg {package}}.",
      "i" = "Available vignettes: {.val {vignettes$name}}"
    ))
  }

  v <- vignettes[match_idx[1], ]
  rel_path <- file.path(package, "doc", basename(v$path))
  header <- format_vignette_header(paste0(package, "::", name), package, width = 40)
  header <- paste0(header, "\nFile: ", rel_path)
  if (isTRUE(v$is_tangled_only)) {
    header <- paste0(header, "\nNote: source not installed, showing tangled R code only.")
  }

  content <- get_file_content_preview(v$path, max_lines = max_lines)
  if (is.null(content)) {
    cli::cli_abort("Failed to read vignette source at {.path {v$path}}.")
  }

  paste0(header, "\n\n", paste(content, collapse = "\n"))
}

# ---- Section Slicing ----

#' Scan markdown lines for ATX headings, ignoring fenced code blocks
#' @param lines Character vector of file lines
#' @return data.frame with columns: line, level, title
#' @noRd
scan_markdown_headings <- function(lines) {
  in_fence <- FALSE
  results <- list()

  for (i in seq_along(lines)) {
    line <- lines[i]
    if (grepl("^```", line)) {
      in_fence <- !in_fence
      next
    }
    if (in_fence) {
      next
    }
    m <- regmatches(line, regexec("^(#{1,6})\\s+(.*)$", line))[[1]]
    if (length(m) == 3) {
      results[[length(results) + 1]] <- data.frame(
        line = i,
        level = nchar(m[2]),
        title = trimws(m[3]),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(results) == 0) {
    return(data.frame(line = integer(0), level = integer(0), title = character(0), stringsAsFactors = FALSE))
  }

  do.call(rbind, results)
}

#' Slice a single section out of vignette source by heading title
#' @param package Package name
#' @param name Vignette name
#' @param section Section heading to match (case-insensitive, trimmed)
#' @return Formatted section string
#' @noRd
view_vignette_section <- function(package, name, section) {
  vignettes <- resolve_vignette_source(package)
  match_idx <- which(vignettes$name == name)

  if (length(match_idx) == 0) {
    cli::cli_abort(c(
      "No vignette named {.val {name}} in package {.pkg {package}}.",
      "i" = "Available vignettes: {.val {vignettes$name}}"
    ))
  }

  v <- vignettes[match_idx[1], ]
  lines <- tryCatch(readLines(v$path, warn = FALSE), error = function(e) character(0))
  headings <- scan_markdown_headings(lines)

  target_idx <- which(tolower(trimws(headings$title)) == tolower(trimws(section)))

  if (length(target_idx) == 0) {
    if (nrow(headings) == 0) {
      cli::cli_abort("Vignette {.pkg {package}}::{.val {name}} has no Markdown headings to select from.")
    }
    cli::cli_abort(c(
      "No section {.val {section}} in vignette {.pkg {package}}::{.val {name}}.",
      "i" = "Did you mean one of: {.val {headings$title}}"
    ))
  }

  target <- headings[target_idx[1], ]
  later_same_or_higher <- which(headings$line > target$line & headings$level <= target$level)
  end_line <- if (length(later_same_or_higher) > 0) {
    headings$line[later_same_or_higher[1]] - 1
  } else {
    length(lines)
  }

  body <- lines[(target$line + 1):end_line]
  while (length(body) > 0 && !nzchar(trimws(body[1]))) {
    body <- body[-1]
  }
  while (length(body) > 0 && !nzchar(trimws(body[length(body)]))) {
    body <- body[-length(body)]
  }

  topic_label <- paste0(package, "::", name, "  §", target$title)
  header <- format_vignette_header(topic_label, package, width = 42)

  paste0(header, "\n\n", paste(body, collapse = "\n"))
}

# ---- Dispatch ----

#' View a package vignette: index, full source, or a single section
#' @param topic Vignette topic: "pkg", "pkg::name", or "pkg::name#Section"
#' @param max_lines Maximum lines to display
#' @return Formatted vignette information
#' @noRd
view_vignette <- function(topic, max_lines = 100) {
  parsed <- parse_vignette_topic(topic)

  if (is.null(parsed$name)) {
    return(view_vignette_index(parsed$package))
  }

  if (is.null(parsed$section)) {
    return(view_vignette_fetch(parsed$package, parsed$name, max_lines))
  }

  view_vignette_section(parsed$package, parsed$name, parsed$section)
}
