# View System Information Functions
# Functions for viewing installed packages and search path information.
# Handles static system-level information that changes less frequently.

# ---- Package Information ----

#' View installed packages with metadata
#' @param max_lines Maximum lines to display
#' @return Formatted installed packages information
#' @noRd
view_installed_packages <- function(max_lines = 100) {
  result <- "Installed Packages Summary"

  tryCatch(
    {
      # Get installed packages info
      pkg_info <- installed.packages()

      if (nrow(pkg_info) == 0) {
        result <- paste0(result, "\nNo packages found")
        return(result)
      }

      total_packages <- nrow(pkg_info)
      result <- paste0(result, "\nTotal packages: ", total_packages)

      # Get unique libraries
      libraries <- unique(pkg_info[, "LibPath"])
      result <- paste0(result, "\nLibrary paths: ", length(libraries))
      for (lib in libraries) {
        lib_count <- sum(pkg_info[, "LibPath"] == lib)
        result <- paste0(result, "\n  ", lib, " (", lib_count, " packages)")
      }

      # Show most relevant packages first (prioritize recently installed, commonly used)
      pkg_df <- data.frame(
        Package = pkg_info[, "Package"],
        Version = pkg_info[, "Version"],
        Priority = ifelse("Priority" %in% colnames(pkg_info), pkg_info[, "Priority"], NA),
        Built = ifelse("Built" %in% colnames(pkg_info), pkg_info[, "Built"], NA),
        stringsAsFactors = FALSE
      )

      # Identify priority packages
      priority_pkgs <- pkg_df[!is.na(pkg_df$Priority), ]
      regular_pkgs <- pkg_df[is.na(pkg_df$Priority), ]

      # Calculate how many packages to show based on max_lines
      lines_used <- 5 + length(libraries) # header + library paths
      remaining_lines <- max_lines - lines_used - 10 # reserve for footer

      # Show priority packages first (base R packages)
      if (nrow(priority_pkgs) > 0) {
        result <- paste0(result, "\n\nBase R packages (", nrow(priority_pkgs), "):")
        n_show_priority <- min(8, nrow(priority_pkgs), remaining_lines %/% 3)
        for (i in 1:n_show_priority) {
          pkg <- priority_pkgs[i, ]
          result <- paste0(result, "\n  ", pkg$Package, " (", pkg$Version, ")")
        }
        if (nrow(priority_pkgs) > n_show_priority) {
          result <- paste0(result, "\n  ... and ", nrow(priority_pkgs) - n_show_priority, " more base packages")
        }
        remaining_lines <- remaining_lines - n_show_priority - 2
      }

      # Show selection of regular packages (most relevant)
      if (nrow(regular_pkgs) > 0 && remaining_lines > 5) {
        result <- paste0(result, "\n\nAdditional packages (", nrow(regular_pkgs), " total):")

        # Prioritize commonly used data science packages
        common_packages <- c(
          "dplyr", "ggplot2", "tidyr", "readr", "tibble",
          "stringr", "lubridate", "purrr", "tidyverse",
          "data.table", "shiny", "knitr", "rmarkdown",
          "devtools", "roxygen2", "testthat", "usethis"
        )

        common_found <- regular_pkgs[regular_pkgs$Package %in% common_packages, ]
        other_pkgs <- regular_pkgs[!regular_pkgs$Package %in% common_packages, ]

        # Show common packages first
        if (nrow(common_found) > 0 && remaining_lines > 3) {
          result <- paste0(result, "\n  Common packages:")
          n_common <- min(nrow(common_found), remaining_lines %/% 2)
          for (i in 1:n_common) {
            pkg <- common_found[i, ]
            result <- paste0(result, "\n    ", pkg$Package, " (", pkg$Version, ")")
          }
          if (nrow(common_found) > n_common) {
            result <- paste0(result, "\n    ... and ", nrow(common_found) - n_common, " more common packages")
          }
          remaining_lines <- remaining_lines - n_common - 1
        }

        # Show sample of other packages
        if (nrow(other_pkgs) > 0 && remaining_lines > 3) {
          n_show_other <- min(10, nrow(other_pkgs), remaining_lines - 2)
          result <- paste0(result, "\n  Other packages (showing ", n_show_other, " of ", nrow(other_pkgs), "):")
          for (i in 1:n_show_other) {
            pkg <- other_pkgs[i, ]
            result <- paste0(result, "\n    ", pkg$Package, " (", pkg$Version, ")")
          }

          if (nrow(other_pkgs) > n_show_other) {
            result <- paste0(result, "\n    ... and ", nrow(other_pkgs) - n_show_other, " more packages")
          }
        }
      }

      # Show R version info for context
      r_version <- R.version.string
      result <- paste0(result, "\n\nR version: ", r_version)

      # Quick usage tip
      result <- paste0(result, "\n\nTip: Use installed.packages() for full details")
    },
    error = function(e) {
      result <<- paste0(result, "\nError retrieving package information: ", e$message)
      result <<- paste0(result, "\nTry: installed.packages() or .libPaths()")
    }
  )

  return(result)
}

# ---- Search Path Information ----

#' View package search path and namespace information
#' @param max_lines Maximum lines to display
#' @return Formatted search path information
#' @noRd
view_search_path <- function(max_lines = 100) {
  result <- "Package Search Path"

  tryCatch(
    {
      # Get search path
      search_path <- search()
      result <- paste0(result, "\nSearch path entries: ", length(search_path))

      # Calculate how many entries to show
      max_entries <- min(max_lines - 10, length(search_path))

      result <- paste0(result, "\n\nSearch path (showing first ", max_entries, "):")
      for (i in 1:max_entries) {
        entry <- search_path[i]

        # Add context about what each entry is
        if (entry == ".GlobalEnv") {
          context <- " (user workspace)"
        } else if (startsWith(entry, "package:")) {
          pkg_name <- sub("^package:", "", entry)
          # Try to get package version
          version <- safe_eval(
            {
              as.character(packageVersion(pkg_name))
            },
            "unknown",
            include_error = FALSE
          )
          context <- paste0(" (version ", version, ")")
        } else if (startsWith(entry, "Autoloads")) {
          context <- " (auto-loading functions)"
        } else if (entry == "package:base") {
          context <- " (base R functions)"
        } else {
          context <- ""
        }

        result <- paste0(result, "\n", sprintf("%2d: %s%s", i, entry, context))
      }

      if (length(search_path) > max_entries) {
        result <- paste0(result, "\n... and ", length(search_path) - max_entries, " more entries")
      }

      # Show loaded namespaces info
      loaded_ns <- loadedNamespaces()
      attached_packages <- search_path[grepl("^package:", search_path)]
      attached_pkg_names <- sub("^package:", "", attached_packages)
      loaded_only <- setdiff(loaded_ns, attached_pkg_names)

      result <- paste0(result, "\n\nNamespace Summary:")
      result <- paste0(result, "\n  Attached packages: ", length(attached_pkg_names))
      result <- paste0(result, "\n  Loaded only (not attached): ", length(loaded_only))

      if (length(loaded_only) > 0) {
        max_loaded <- min(10, length(loaded_only))
        result <- paste0(result, "\n  Loaded namespaces (first ", max_loaded, "): ")
        result <- paste0(result, paste(loaded_only[1:max_loaded], collapse = ", "))
        if (length(loaded_only) > max_loaded) {
          result <- paste0(result, " ... and ", length(loaded_only) - max_loaded, " more")
        }
      }

      # Show namespace conflicts if any
      conflicts_result <- safe_eval(
        {
          conflicts_info <- conflicts(detail = TRUE)
          if (length(conflicts_info) > 0) {
            conflict_summary <- paste0("\n\nFunction conflicts detected: ", length(conflicts_info))
            if (length(conflicts_info) <= 5) {
              for (conflict_name in names(conflicts_info)) {
                sources <- conflicts_info[[conflict_name]]
                conflict_summary <- paste0(conflict_summary, "\n  ", conflict_name, ": ", paste(sources, collapse = ", "))
              }
            } else {
              conflict_names <- names(conflicts_info)[1:3]
              for (conflict_name in conflict_names) {
                sources <- conflicts_info[[conflict_name]]
                conflict_summary <- paste0(conflict_summary, "\n  ", conflict_name, ": ", paste(sources, collapse = ", "))
              }
              conflict_summary <- paste0(conflict_summary, "\n  ... and ", length(conflicts_info) - 3, " more conflicts")
            }
            conflict_summary
          } else {
            "\n\nNo function conflicts detected"
          }
        },
        "\n\nConflict information unavailable",
        include_error = FALSE
      )

      result <- paste0(result, conflicts_result)

      # Usage tips
      result <- paste0(result, "\n\nTips:")
      result <- paste0(result, "\n  - Use search() to see full search path")
      result <- paste0(result, "\n  - Use conflicts() to check for function conflicts")
      result <- paste0(result, "\n  - Use loadedNamespaces() to see all loaded packages")
    },
    error = function(e) {
      result <<- paste0(result, "\nError retrieving search path information: ", e$message)
      result <<- paste0(result, "\nTry: search() or loadedNamespaces()")
    }
  )

  return(result)
}
