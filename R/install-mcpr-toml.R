# TOML Configuration I/O for Codex
# Dependency-free TOML reader/writer for MCP server configuration files.
# Uses surgical text manipulation to preserve existing configuration and comments.

#' Read TOML configuration restricted to MCP sections
#' @param path Path to TOML configuration file
#' @return List with `mcp` element containing server entries
#' @noRd
read_toml_config <- function(path) {
  if (!file.exists(path)) {
    return(list(mcp = list()))
  }

  tryCatch(
    {
      lines <- readLines(path, warn = FALSE)
      list(mcp = extract_mcp_sections(lines))
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to read configuration file: {.path {path}}",
          "x" = "TOML parsing error: {e$message}"
        )
      )
    }
  )
}

#' Write TOML configuration for MCP sections atomically
#' @param config Configuration list with `mcp` element
#' @param path Destination path for configuration file
#' @noRd
write_toml_config <- function(config, path) {
  temp_path <- paste0(path, ".tmp")

  tryCatch(
    {
      existing_lines <- if (file.exists(path)) readLines(path, warn = FALSE) else character(0)
      preserved_lines <- remove_all_mcp_sections(existing_lines)
      mcp_config <- if (is.null(config[["mcp"]])) list() else config[["mcp"]]
      mcp_lines <- format_mcp_sections(mcp_config)

      combined <- if (length(preserved_lines) > 0 && length(mcp_lines) > 0) {
        c(preserved_lines, "", mcp_lines)
      } else {
        c(preserved_lines, mcp_lines)
      }

      writeLines(combined, temp_path)
      file.rename(temp_path, path)
    },
    error = function(e) {
      if (file.exists(temp_path)) {
        unlink(temp_path)
      }

      cli::cli_abort(
        c(
          "Failed to write configuration file: {.path {path}}",
          "x" = "Error: {e$message}",
          "i" = "Check file permissions and disk space"
        )
      )
    }
  )
}

#' Extract MCP server sections from TOML lines
#' @param lines Character vector of TOML lines
#' @return Named list of MCP servers
#' @noRd
extract_mcp_sections <- function(lines) {
  mcp_servers <- list()
  current_section <- NULL
  current_lines <- character(0)

  for (line in lines) {
    mcp_header <- grepl("^\\s*\\[mcp\\.([^]]+)\\]\\s*$", line)
    other_header <- !mcp_header && grepl("^\\s*\\[.+\\]\\s*$", line)

    if (mcp_header) {
      if (!is.null(current_section)) {
        mcp_servers[[current_section]] <- parse_toml_section(current_lines)
      }

      current_section <- sub("^\\s*\\[mcp\\.([^]]+)\\].*$", "\\1", line)
      current_lines <- character(0)
      next
    }

    if (other_header) {
      if (!is.null(current_section)) {
        mcp_servers[[current_section]] <- parse_toml_section(current_lines)
      }

      current_section <- NULL
      current_lines <- character(0)
      next
    }

    if (!is.null(current_section)) {
      current_lines <- c(current_lines, line)
    }
  }

  if (!is.null(current_section)) {
    mcp_servers[[current_section]] <- parse_toml_section(current_lines)
  }

  mcp_servers
}

#' Parse a single TOML section into a list
#' @param lines Lines within a section
#' @return List of key/value pairs
#' @noRd
parse_toml_section <- function(lines) {
  config <- list()

  for (line in lines) {
    trimmed <- trimws(line)
    if (trimmed == "" || grepl("^#", trimmed)) {
      next
    }

    if (!grepl("=", trimmed, fixed = TRUE)) {
      next
    }

    key <- trimws(sub("=.*$", "", trimmed))
    value <- trimws(sub("^[^=]+=", "", trimmed))

    parsed <- if (grepl("^\\[", value)) {
      parse_toml_array(value)
    } else if (grepl("^\\{", value)) {
      parse_toml_inline_table(value)
    } else {
      parse_toml_string(value)
    }

    config[[key]] <- parsed
  }

  config
}

#' Parse a TOML array of strings
#' @param value String representation of an array
#' @return Character vector
#' @noRd
parse_toml_array <- function(value) {
  stripped <- sub("^\\[\\s*", "", value)
  stripped <- sub("\\s*\\]$", "", stripped)

  if (nchar(stripped) == 0) {
    return(character(0))
  }

  items <- strsplit(stripped, ",")[[1]]
  vapply(items, function(x) parse_toml_string(trimws(x)), character(1), USE.NAMES = FALSE)
}

#' Parse a TOML inline table with string values
#' @param value String representation of an inline table
#' @return Named list
#' @noRd
parse_toml_inline_table <- function(value) {
  stripped <- sub("^\\{\\s*", "", value)
  stripped <- sub("\\s*\\}$", "", stripped)

  if (nchar(stripped) == 0) {
    return(list())
  }

  pairs <- strsplit(stripped, ",")[[1]]
  result <- list()

  for (pair in pairs) {
    kv <- strsplit(trimws(pair), "=", fixed = TRUE)[[1]]
    if (length(kv) >= 2) {
      key <- trimws(kv[1])
      val <- parse_toml_string(trimws(paste(kv[-1], collapse = "=")))
      result[[key]] <- val
    }
  }

  result
}

#' Parse a TOML string value (quoted or unquoted)
#' @param value String to parse
#' @return Scalar character
#' @noRd
parse_toml_string <- function(value) {
  trimmed <- trimws(value)

  if ((startsWith(trimmed, "\"") && endsWith(trimmed, "\"")) ||
    (startsWith(trimmed, "'") && endsWith(trimmed, "'"))) {
    trimmed <- substr(trimmed, 2, nchar(trimmed) - 1)
  }

  trimws(trimmed)
}

#' Remove all MCP sections from TOML lines
#' @param lines Character vector of TOML lines
#' @return Lines without any [mcp.*] sections
#' @noRd
remove_all_mcp_sections <- function(lines) {
  result <- character(0)
  in_mcp <- FALSE

  for (line in lines) {
    is_mcp_header <- grepl("^\\s*\\[mcp\\.", line)
    is_any_header <- grepl("^\\s*\\[.+\\]\\s*$", line)

    if (is_mcp_header) {
      in_mcp <- TRUE
      next
    }

    if (is_any_header && !is_mcp_header) {
      in_mcp <- FALSE
    }

    if (!in_mcp) {
      result <- c(result, line)
    }
  }

  result
}

#' Format MCP sections into TOML lines
#' @param mcp_list Named list of MCP server configurations
#' @return Character vector of TOML lines
#' @noRd
format_mcp_sections <- function(mcp_list) {
  if (length(mcp_list) == 0) {
    return(character(0))
  }

  lines <- character(0)

  for (server_name in names(mcp_list)) {
    server_config <- mcp_list[[server_name]]
    lines <- c(lines, sprintf("[mcp.%s]", server_name))

    for (key in names(server_config)) {
      value <- server_config[[key]]

      if (is.list(value)) {
        entries <- vapply(names(value), function(k) sprintf('%s = "%s"', k, value[[k]]), character(1))
        lines <- c(lines, sprintf('%s = { %s }', key, paste(entries, collapse = ", ")))
      } else if (is.character(value) && length(value) > 1) {
        entries <- sprintf('"%s"', value)
        lines <- c(lines, sprintf('%s = [%s]', key, paste(entries, collapse = ", ")))
      } else {
        scalar <- if (length(value) == 0) "" else value[1]
        lines <- c(lines, sprintf('%s = "%s"', key, scalar))
      }
    }

    lines <- c(lines, "")
  }

  lines
}
