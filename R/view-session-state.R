# View Session State Functions
# Functions for viewing R session information including objects, terminal output, and errors.
# Handles runtime session state and memory-based information.

# ---- Session Information ----

#' View R session information including objects and session details
#' @param max_lines Maximum lines to display
#' @return Formatted session information
#' @noRd
view_session <- function(max_lines = 100) {
  result <- "R Session Information"

  tryCatch(
    {
      # R version and session info
      r_version <- R.version.string
      result <- paste0(result, "\nR version: ", r_version)

      # System information
      sys_info <- Sys.info()
      result <- paste0(result, "\nSystem: ", sys_info[["sysname"]], " ", sys_info[["release"]])
      result <- paste0(result, "\nUser: ", sys_info[["user"]])

      # Current working directory
      result <- paste0(result, "\nWorking directory: ", getwd())

      # Get objects in global environment
      all_objects <- ls(envir = .GlobalEnv, all.names = TRUE)
      visible_objects <- ls(envir = .GlobalEnv, all.names = FALSE)
      hidden_objects <- setdiff(all_objects, visible_objects)

      env_size <- length(all_objects)
      result <- paste0(result, "\n\nGlobal Environment: ", env_size, " objects")

      # Show visible objects first (most relevant)
      if (length(visible_objects) > 0) {
        n_show <- min(max_lines %/% 3, length(visible_objects), 15)
        result <- paste0(result, "\n\nVisible objects (first ", n_show, "):")

        # Get object types for better relevance
        for (i in 1:n_show) {
          obj_name <- visible_objects[i]
          obj_info <- safe_eval(
            {
              obj <- get(obj_name, envir = .GlobalEnv)
              obj_class <- class(obj)[1]
              obj_type <- typeof(obj)

              if (is.function(obj)) {
                paste0(obj_name, " (function)")
              } else if (is.data.frame(obj)) {
                paste0(obj_name, " (data.frame: ", nrow(obj), "x", ncol(obj), ")")
              } else if (is.vector(obj) && length(obj) > 1) {
                paste0(obj_name, " (", obj_type, "[", length(obj), "])")
              } else {
                paste0(obj_name, " (", obj_class, ")")
              }
            },
            paste0(obj_name, " (inaccessible)"),
            include_error = FALSE
          )

          result <- paste0(result, "\n  ", obj_info)
        }

        if (length(visible_objects) > n_show) {
          result <- paste0(result, "\n  ... and ", length(visible_objects) - n_show, " more visible objects")
        }
      }

      # Show count of hidden objects
      if (length(hidden_objects) > 0) {
        result <- paste0(result, "\n\nHidden objects: ", length(hidden_objects))
        if (length(hidden_objects) <= 5) {
          result <- paste0(result, " (", paste(hidden_objects, collapse = ", "), ")")
        } else {
          result <- paste0(result, " (first 3: ", paste(hidden_objects[1:3], collapse = ", "), " ...)")
        }
      }

      # Show loaded namespaces count
      loaded_ns <- loadedNamespaces()
      result <- paste0(result, "\n\nLoaded namespaces: ", length(loaded_ns))

      # Show attached packages
      search_path <- search()
      attached_packages <- search_path[grepl("^package:", search_path)]
      if (length(attached_packages) > 0) {
        pkg_names <- sub("^package:", "", attached_packages)
        result <- paste0(result, "\nAttached packages: ")
        if (length(pkg_names) <= 5) {
          result <- paste0(result, paste(pkg_names, collapse = ", "))
        } else {
          result <- paste0(result, paste(pkg_names[1:5], collapse = ", "))
          result <- paste0(result, " ... and ", length(pkg_names) - 5, " more")
        }
      }

      # Memory usage summary
      if (env_size > 0) {
        total_size <- safe_eval(
          {
            sum(sapply(visible_objects[1:min(length(visible_objects), 20)], function(name) {
              as.numeric(utils::object.size(get(name, envir = .GlobalEnv)))
            }))
          },
          0,
          include_error = FALSE
        )

        if (is.numeric(total_size) && total_size > 0) {
          size_text <- format_file_size(total_size)
          result <- paste0(result, "\nMemory usage (top objects):", size_text)
        }
      }
    },
    error = function(e) {
      result <<- paste0(result, "\nError retrieving session information: ", e$message)
    }
  )

  return(result)
}

# ---- Terminal Output ----

#' Parse radian history format to extract R commands from current session
#' @param radian_lines Character vector of raw radian history lines
#' @param session_start_time POSIXct timestamp of current R session start
#' @param max_commands Maximum number of commands to return
#' @return Character vector of R commands from current session
#' @noRd
parse_radian_history <- function(radian_lines, session_start_time = NULL, max_commands = 100) {
  if (length(radian_lines) == 0) {
    return(character(0))
  }

  # If no session start time provided, get R session start time
  if (is.null(session_start_time)) {
    # Approximate R session start time (when R process started)
    # This is imperfect but better than showing all historical commands
    session_start_time <- Sys.time() - as.difftime(as.numeric(Sys.time() - .POSIXct(0)), units = "secs")

    # Try to get more accurate session start from .GlobalEnv creation time
    # or use a conservative approach (last 4 hours)
    session_start_time <- Sys.time() - as.difftime(4, units = "hours")
  }

  commands <- character(0)
  current_time <- NULL
  current_mode <- NULL

  i <- 1
  while (i <= length(radian_lines)) {
    line <- radian_lines[i]

    # Parse time line: # time: 2025-08-20 08:58:02 UTC
    if (grepl("^# time:", line)) {
      time_match <- regmatches(line, regexpr("\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2} UTC", line))
      if (length(time_match) > 0) {
        tryCatch(
          {
            current_time <- as.POSIXct(time_match, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
          },
          error = function(e) {
            current_time <<- NULL
          }
        )
      }
    }
    # Parse mode line: # mode: r
    else if (grepl("^# mode:", line)) {
      mode_match <- regmatches(line, regexpr("(?<=^# mode: )\\w+", line, perl = TRUE))
      if (length(mode_match) > 0) {
        current_mode <- mode_match
      }
    }
    # Parse command line: +command
    else if (grepl("^\\+", line) && !is.null(current_time) && !is.null(current_mode)) {
      # Only include R commands from current session
      if (current_mode == "r" && current_time >= session_start_time) {
        command <- substring(line, 2) # Remove the '+' prefix
        command <- trimws(command)
        if (nzchar(command)) {
          commands <- c(commands, command)
        }
      }
    }

    i <- i + 1
  }

  # Return most recent commands up to max_commands
  if (length(commands) > max_commands) {
    commands <- utils::tail(commands, max_commands)
  }

  return(commands)
}

#' Get current R session approximate start time
#' @return POSIXct timestamp of estimated session start
#' @noRd
get_session_start_time <- function() {
  # Strategy 1: Check if we can get process start time
  tryCatch(
    {
      # Try to get R process start time using ps (Unix-like systems)
      if (.Platform$OS.type == "unix") {
        pid <- Sys.getpid()
        ps_result <- system(paste0("ps -o lstart= -p ", pid), intern = TRUE)
        if (length(ps_result) > 0 && nzchar(ps_result)) {
          # This is system-dependent and may not always work
          # Fallback to conservative estimate
        }
      }
    },
    error = function(e) {
      # ps command failed or not available
    }
  )

  # Strategy 2: Conservative estimate - assume session started recently
  # Use a reasonable window (last 2 hours) to avoid showing too much history
  session_start <- Sys.time() - as.difftime(2, units = "hours")

  # Strategy 3: If there are objects in GlobalEnv, session has been active
  global_objects <- ls(envir = .GlobalEnv, all.names = TRUE)
  if (length(global_objects) > 0) {
    # If we have many objects, session might be longer running
    # Expand window but keep it reasonable
    if (length(global_objects) > 10) {
      session_start <- Sys.time() - as.difftime(8, units = "hours")
    }
  }

  return(session_start)
}

#' Get command history from multiple sources
#' @param max_lines Maximum lines to retrieve
#' @return Character vector of history lines or NULL if none found
#' @noRd
get_command_history <- function(max_lines = 100) {
  # Strategy 1: Try R's built-in savehistory (works in base R)
  tryCatch(
    {
      history_file <- tempfile()
      savehistory(history_file)

      if (file.exists(history_file)) {
        history_lines <- readLines(history_file, warn = FALSE)
        unlink(history_file)

        if (length(history_lines) > 0) {
          return(history_lines)
        }
      }
    },
    error = function(e) {
      # savehistory failed, try other methods
    }
  )

  # Strategy 2: Try radian history file with smart parsing
  radian_history <- file.path(path.expand("~"), ".radian_history")
  if (file.exists(radian_history)) {
    tryCatch(
      {
        raw_history <- readLines(radian_history, warn = FALSE)
        if (length(raw_history) > 0) {
          # Parse radian format to get only R commands from current session
          session_start <- get_session_start_time()
          parsed_commands <- parse_radian_history(raw_history, session_start, max_lines)

          if (length(parsed_commands) > 0) {
            return(parsed_commands)
          }
          # If no recent R commands found, fall back to raw history (filtered)
          # This might happen if session is very new
          if (length(parsed_commands) == 0) {
            # Take recent raw lines and basic cleanup
            recent_raw <- utils::tail(raw_history, max_lines * 3)
            # Extract just command lines (starting with +)
            command_lines <- recent_raw[grepl("^\\+", recent_raw)]
            if (length(command_lines) > 0) {
              cleaned_commands <- substring(command_lines, 2) # Remove '+'
              cleaned_commands <- trimws(cleaned_commands)
              cleaned_commands <- cleaned_commands[nzchar(cleaned_commands)]
              return(utils::tail(cleaned_commands, max_lines))
            }
          }
        }
      },
      error = function(e) {
        # Radian history file exists but can't be read
      }
    )
  }

  # Strategy 3: Try standard .Rhistory file in home directory
  r_history <- file.path(path.expand("~"), ".Rhistory")
  if (file.exists(r_history)) {
    tryCatch(
      {
        history_lines <- readLines(r_history, warn = FALSE)
        if (length(history_lines) > 0) {
          return(history_lines)
        }
      },
      error = function(e) {
        # R history file exists but can't be read
      }
    )
  }

  # Strategy 4: Try .Rhistory in current working directory
  local_history <- file.path(getwd(), ".Rhistory")
  if (file.exists(local_history)) {
    tryCatch(
      {
        history_lines <- readLines(local_history, warn = FALSE)
        if (length(history_lines) > 0) {
          return(history_lines)
        }
      },
      error = function(e) {
        # Local R history file exists but can't be read
      }
    )
  }

  # No history found from any source
  return(NULL)
}

#' View recent terminal output and command history
#' @param max_lines Maximum lines to display
#' @return Formatted terminal information
#' @noRd
view_terminal <- function(max_lines = 100) {
  result <- "Terminal Output Summary"

  tryCatch(
    {
      # Get command history using multi-strategy approach
      history_lines <- get_command_history(max_lines * 2) # Get more than needed to filter

      if (!is.null(history_lines) && length(history_lines) > 0) {
        # Filter out empty lines and basic cleanup
        history_lines <- history_lines[nzchar(trimws(history_lines))]

        if (length(history_lines) > 0) {
          n_show <- min(max_lines, length(history_lines))
          recent_history <- utils::tail(history_lines, n_show)

          result <- paste0(result, "\nRecent commands (last ", n_show, "):")
          for (i in seq_along(recent_history)) {
            line_num <- length(history_lines) - n_show + i
            # Clean up the command (remove extra whitespace, truncate if too long)
            clean_cmd <- trimws(recent_history[i])
            if (nchar(clean_cmd) > 120) {
              clean_cmd <- paste0(substr(clean_cmd, 1, 117), "...")
            }
            result <- paste0(result, "\n", sprintf("%3d: %s", line_num, clean_cmd))
          }
        } else {
          result <- paste0(result, "\nNo command history available (empty history)")
        }
      } else {
        result <- paste0(result, "\nNo command history available (no history sources found)")
      }

      # Show last computed value if available
      if (exists(".Last.value", envir = .GlobalEnv)) {
        last_val <- get(".Last.value", envir = .GlobalEnv)
        if (!is.null(last_val)) {
          result <- paste0(result, "\n\nLast computed value:")
          if (is.atomic(last_val) && length(last_val) <= 5) {
            val_output <- capture_print(last_val)
            result <- paste0(result, "\n", paste(val_output, collapse = "\n"))
          } else {
            val_summary <- capture_print(last_val)
            val_preview <- val_summary[1:min(3, length(val_summary))]
            result <- paste0(result, "\n", paste(val_preview, collapse = "\n"))
            if (length(val_summary) > 3) {
              result <- paste0(result, "\n... (output truncated)")
            }
          }
        }
      }

      # Check for recent warnings
      last_warnings <- warnings()
      if (!is.null(last_warnings) && length(last_warnings) > 0) {
        result <- paste0(result, "\n\nRecent warnings:")
        n_warnings <- min(3, length(last_warnings))
        for (i in 1:n_warnings) {
          warning_msg <- names(last_warnings)[i]
          if (is.null(warning_msg)) warning_msg <- as.character(last_warnings[[i]])
          result <- paste0(result, "\n", sprintf("  %d: %s", i, warning_msg))
        }
        if (length(last_warnings) > 3) {
          result <- paste0(result, "\n  ... and ", length(last_warnings) - 3, " more warnings")
        }
      }

      # Show current working directory for context
      result <- paste0(result, "\n\nCurrent working directory: ", getwd())
    },
    error = function(e) {
      result <<- paste0(result, "\nError capturing terminal output: ", e$message)
    }
  )

  return(result)
}

# ---- Error Information ----

#' View last error details and traceback
#' @param max_lines Maximum lines to display
#' @return Formatted error information
#' @noRd
view_last_error <- function(max_lines = 100) {
  result <- "Last Error Information"

  tryCatch(
    {
      # Get the last error message
      last_error_msg <- geterrmessage()

      if (is.null(last_error_msg) || last_error_msg == "" || last_error_msg == "No error") {
        result <- paste0(result, "\nNo recent errors detected")
        return(result)
      }

      result <- paste0(result, "\nError message: ", last_error_msg)

      # Get traceback information
      traceback_info <- NULL

      tryCatch(
        {
          # Use capture.output to get traceback as text
          tb_output <- utils::capture.output(traceback())
          if (length(tb_output) > 0 && !all(tb_output == "No traceback available")) {
            traceback_info <- tb_output
          }
        },
        error = function(e) {
          # Traceback not available or already consumed
        }
      )

      if (!is.null(traceback_info) && length(traceback_info) > 0) {
        result <- paste0(result, "\n\nCall stack (traceback):")

        # Limit traceback lines to prevent overwhelming output
        max_traceback_lines <- min(max_lines - 5, 10) # Reserve some lines for other info
        if (length(traceback_info) <= max_traceback_lines) {
          for (i in seq_along(traceback_info)) {
            result <- paste0(result, "\n", sprintf("%2d: %s", i, traceback_info[i]))
          }
        } else {
          # Show first few and last few lines
          for (i in 1:3) {
            result <- paste0(result, "\n", sprintf("%2d: %s", i, traceback_info[i]))
          }
          result <- paste0(result, "\n    ... (", length(traceback_info) - 6, " intermediate calls)")
          for (i in (length(traceback_info) - 2):length(traceback_info)) {
            result <- paste0(result, "\n", sprintf("%2d: %s", i, traceback_info[i]))
          }
        }
      } else {
        result <- paste0(result, "\nNo call stack information available")
      }

      # Check if rlang is available for enhanced error information
      if (requireNamespace("rlang", quietly = TRUE)) {
        tryCatch(
          {
            # Try to get the last condition/error from rlang
            if (exists("last_error", envir = rlang::global_env(), inherits = FALSE)) {
              last_rlang_error <- get("last_error", envir = rlang::global_env())
              if (!is.null(last_rlang_error)) {
                result <- paste0(result, "\n\nEnhanced error info (rlang):")
                error_summary <- utils::capture.output(print(last_rlang_error))
                preview_lines <- min(5, length(error_summary))
                result <- paste0(result, "\n", paste(error_summary[1:preview_lines], collapse = "\n"))
                if (length(error_summary) > 5) {
                  result <- paste0(result, "\n... (output truncated)")
                }
              }
            }
          },
          error = function(e) {
            # rlang error info not available
          }
        )
      }

      # Provide some context about the error
      result <- paste0(result, "\n\nTroubleshooting tips:")
      result <- paste0(result, "\n- Check the error message for specific details")
      result <- paste0(result, "\n- Review the call stack to identify where the error occurred")
      result <- paste0(result, "\n- Verify variable names, function arguments, and data types")
    },
    error = function(e) {
      result <<- paste0(result, "\nError retrieving error information: ", e$message)
    }
  )

  return(result)
}

# ---- Warning Information ----

#' View recent warnings from the session
#' @param max_lines Maximum lines to display
#' @return Formatted warnings information
#' @noRd
view_warnings <- function(max_lines = 100) {
  result <- "Recent Warnings Summary"

  tryCatch(
    {
      # Get recent warnings
      last_warnings <- warnings()

      if (is.null(last_warnings) || length(last_warnings) == 0) {
        result <- paste0(result, "\nNo recent warnings")
        return(result)
      }

      result <- paste0(result, "\nTotal warnings: ", length(last_warnings))

      # Show warnings with context
      n_show <- min(max_lines %/% 2, length(last_warnings), 20)
      result <- paste0(result, "\n\nWarnings (showing first ", n_show, "):")

      for (i in 1:n_show) {
        warning_msg <- names(last_warnings)[i]
        if (is.null(warning_msg)) {
          warning_msg <- as.character(last_warnings[[i]])
        }

        # Clean up warning message
        clean_msg <- gsub("^Warning: ", "", warning_msg)
        clean_msg <- gsub("^Warning in [^:]+: ", "", clean_msg)

        result <- paste0(result, "\n", sprintf("%2d: %s", i, clean_msg))
      }

      if (length(last_warnings) > n_show) {
        result <- paste0(result, "\n... and ", length(last_warnings) - n_show, " more warnings")
      }

      # Check if warnings can be cleared
      result <- paste0(result, "\n\nNote: Use warnings() to see all warnings")
      result <- paste0(result, "\n      Use assign('last.warning', NULL, envir=baseenv()) to clear")
    },
    error = function(e) {
      result <<- paste0(result, "\nError retrieving warning information: ", e$message)
    }
  )

  return(result)
}

# ---- Workspace (File System) ----

#' View current workspace directory structure
#' @param max_lines Maximum lines to display
#' @return Formatted workspace information
#' @noRd
view_workspace <- function(max_lines = 100) {
  result <- paste0("Workspace Directory: ", getwd())

  tryCatch(
    {
      # Get files and directories
      files <- list.files(".", full.names = FALSE, all.files = FALSE, include.dirs = TRUE)

      if (length(files) == 0) {
        result <- paste0(result, "\n  (empty directory)")
        return(result)
      }

      # Limit files shown based on max_lines
      max_files <- min(max_lines - 10, length(files), 30) # Reserve lines for header/footer
      if (length(files) > max_files) {
        files_to_show <- files[1:max_files]
        truncated <- TRUE
      } else {
        files_to_show <- files
        truncated <- FALSE
      }

      # Get file info
      file_info <- file.info(files_to_show)
      dirs <- files_to_show[file_info$isdir & !is.na(file_info$isdir)]
      regular_files <- files_to_show[!file_info$isdir & !is.na(file_info$isdir)]

      dirs <- sort(dirs)
      regular_files <- sort(regular_files)

      # Show directories first
      if (length(dirs) > 0) {
        result <- paste0(result, "\n\nDirectories:")
        for (dir in dirs) {
          result <- paste0(result, "\n  [DIR] ", dir, "/")
        }
      }

      # Show regular files
      if (length(regular_files) > 0) {
        result <- paste0(result, "\n\nFiles:")
        for (file in regular_files) {
          file_size <- file_info[file, "size"]
          size_text <- format_file_size(file_size)

          # Add file type indication for common types
          file_ext <- tools::file_ext(file)
          type_hint <- if (nzchar(file_ext)) {
            paste0(" [", toupper(file_ext), "]")
          } else {
            ""
          }

          result <- paste0(result, "\n  ", file, size_text, type_hint)
        }
      }

      if (truncated) {
        total_files <- length(list.files(".", all.files = FALSE, include.dirs = TRUE))
        result <- paste0(result, "\n\n... and ", total_files - max_files, " more items")
      }

      # Show summary
      total_dirs <- length(list.dirs(".", recursive = FALSE))
      total_files <- length(list.files(".", recursive = FALSE))
      result <- paste0(result, "\n\nSummary: ", total_dirs, " directories, ", total_files, " files")

      # Look for common project files
      project_files <- c(
        "DESCRIPTION", "NAMESPACE", ".Rproj", "requirements.txt",
        "package.json", "Makefile", "README.md", "README.txt"
      )
      found_project_files <- intersect(files, project_files)
      if (length(found_project_files) > 0) {
        result <- paste0(result, "\nProject files found: ", paste(found_project_files, collapse = ", "))
      }
    },
    error = function(e) {
      result <<- paste0(result, "\nError reading workspace directory: ", e$message)
    }
  )

  return(result)
}
