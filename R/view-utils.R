# View Utility Functions
# Shared utility functions for the view tool implementation.
# Provides common formatting, error handling, and data processing functions.

# ---- Output Capture and Formatting ----

#' Capture print output with consistent options
#' @param x Object to capture print output for
#' @param max_print Maximum items to print
#' @return Character vector of captured output lines
#' @noRd
capture_print <- function(x, max_print = 100) {
  # Set reproducible output options
  old_options <- list(
    max.print = getOption("max.print"),
    width = getOption("width"),
    digits = getOption("digits")
  )

  on.exit({
    options(old_options)
  })

  # Set consistent options for reproducible output
  options(
    max.print = max_print,
    width = 80,
    digits = 7
  )

  # Capture print output
  out <- utils::capture.output(print(x))

  # If no output, try capturing messages
  if (length(out) == 0 || !any(nzchar(out))) {
    out <- utils::capture.output(print(x), type = "message")
  }

  # Strip ANSI codes if present
  if (requireNamespace("cli", quietly = TRUE)) {
    out <- cli::ansi_strip(out)
  }

  return(out)
}

#' Build result string with consistent formatting
#' @param title Title/header for the result
#' @param ... Additional content lines
#' @return Formatted result string
#' @noRd
build_result <- function(title, ...) {
  content_lines <- list(...)
  content_lines <- content_lines[!sapply(content_lines, is.null)]

  if (length(content_lines) == 0) {
    return(title)
  }

  return(paste0(title, "\n", paste(content_lines, collapse = "\n")))
}

#' Format object counts and sizes
#' @param count Number of items
#' @param item_name Name of items (singular)
#' @param max_show Maximum to show
#' @return Formatted count string
#' @noRd
format_count <- function(count, item_name, max_show = NULL) {
  if (count == 0) {
    return(paste0("No ", item_name, "s"))
  }

  if (count == 1) {
    return(paste0("1 ", item_name))
  }

  base_text <- paste0(count, " ", item_name, "s")

  if (!is.null(max_show) && count > max_show) {
    return(paste0(base_text, " (showing first ", max_show, ")"))
  }

  return(base_text)
}

#' Format file size in human readable format
#' @param size_bytes Size in bytes
#' @return Human readable size string
#' @noRd
format_file_size <- function(size_bytes) {
  if (is.na(size_bytes)) {
    return("")
  }

  if (size_bytes < 1024) {
    return(paste0(" (", size_bytes, " bytes)"))
  } else if (size_bytes < 1024^2) {
    return(paste0(" (", round(size_bytes / 1024, 1), " KB)"))
  } else if (size_bytes < 1024^3) {
    return(paste0(" (", round(size_bytes / 1024^2, 1), " MB)"))
  } else {
    return(paste0(" (", round(size_bytes / 1024^3, 1), " GB)"))
  }
}

#' Truncate text with ellipsis if too long
#' @param text Text to truncate
#' @param max_length Maximum length
#' @return Truncated text
#' @noRd
truncate_text <- function(text, max_length = 100) {
  if (nchar(text) <= max_length) {
    return(text)
  }

  return(paste0(substr(text, 1, max_length - 3), "..."))
}

# ---- Error Handling Utilities ----

#' Safe wrapper for potentially failing operations
#' @param expr Expression to evaluate safely
#' @param fallback_message Fallback message on error
#' @param include_error Whether to include error message
#' @return Result or fallback message
#' @noRd
safe_eval <- function(expr, fallback_message = "Operation failed", include_error = TRUE) {
  tryCatch(
    {
      expr
    },
    error = function(e) {
      if (include_error) {
        return(paste0(fallback_message, ": ", e$message))
      } else {
        return(fallback_message)
      }
    }
  )
}

#' Extract meaningful error information
#' @param error Error object
#' @return Formatted error description
#' @noRd
format_error_info <- function(error) {
  error_msg <- if (is.null(error$message)) "Unknown error" else error$message

  # Clean up common R error patterns
  error_msg <- gsub("^Error in [^:]+: ", "", error_msg)
  error_msg <- gsub("^Error: ", "", error_msg)

  return(error_msg)
}

# ---- File System Utilities ----

#' Determine if file is likely text-based
#' @param path File path
#' @param file_ext File extension
#' @return TRUE if likely text file
#' @noRd
is_likely_text_file <- function(path, file_ext) {
  # Common text file extensions
  text_extensions <- c(
    "txt", "md", "r", "R", "py", "js", "html", "css", "xml", "json",
    "yaml", "yml", "csv", "tsv", "log", "conf", "cfg", "ini",
    "sh", "bat", "sql", "c", "h", "cpp", "hpp", "java", "go"
  )

  if (tolower(file_ext) %in% text_extensions) {
    return(TRUE)
  }

  # For files without clear extensions, try to detect by content
  tryCatch(
    {
      # Read first few bytes to check for binary content
      con <- file(path, "rb")
      on.exit(close(con))

      first_bytes <- readBin(con, "raw", n = 512)

      # Look for null bytes (common in binary files)
      null_bytes <- sum(first_bytes == 0)

      # If more than 5% null bytes, likely binary
      if (length(first_bytes) > 0 && null_bytes / length(first_bytes) > 0.05) {
        return(FALSE)
      }

      # Convert to character and check for printable characters
      text_content <- rawToChar(first_bytes)

      # Check for mostly printable ASCII characters
      chars <- utf8ToInt(text_content)
      printable_chars <- sum(chars >= 32 & chars <= 126) + sum(chars %in% c(9, 10, 13)) # printable + tab/LF/CR

      if (length(chars) > 0 && printable_chars / length(chars) > 0.8) {
        return(TRUE)
      }

      return(FALSE)
    },
    error = function(e) {
      # If we can't analyze, default to FALSE for safety
      return(FALSE)
    }
  )
}

#' Get content preview for text files
#' @param path File path
#' @param max_lines Maximum lines to read
#' @return Character vector of file lines or NULL on error
#' @noRd
get_file_content_preview <- function(path, max_lines = 10) {
  tryCatch(
    {
      # Try to read as text file
      con <- file(path, "r", encoding = "UTF-8")
      on.exit(close(con))

      lines <- character(0)
      line_count <- 0

      while (line_count < max_lines) {
        line <- readLines(con, n = 1, warn = FALSE)
        if (length(line) == 0) {
          break # End of file
        }

        lines <- c(lines, line)
        line_count <- line_count + 1
      }

      # Check if there are more lines
      if (line_count == max_lines) {
        more_lines <- readLines(con, n = 1, warn = FALSE)
        if (length(more_lines) > 0) {
          total_lines <- safe_eval(
            length(readLines(path, warn = FALSE)),
            fallback_message = "unknown",
            include_error = FALSE
          )
          if (is.numeric(total_lines)) {
            remaining <- total_lines - max_lines
            lines <- c(lines, paste0("... (", remaining, " more lines)"))
          } else {
            lines <- c(lines, "... (more content available)")
          }
        }
      }

      return(lines)
    },
    error = function(e) {
      # Try with different encoding
      tryCatch(
        {
          con <- file(path, "r", encoding = "latin1")
          on.exit(close(con))

          lines <- readLines(con, n = max_lines, warn = FALSE)
          return(lines)
        },
        error = function(e2) {
          return(NULL)
        }
      )
    }
  )
}
