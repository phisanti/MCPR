# Tool Registry
# Discovers MCP tool definitions from roxygen-tagged files across one or more directories.
# Converts tagged functions into validated `ToolDef` objects for server registration.

#' @title Tool Registry
#' @include tool-registry-helpers.R
#' @include utils.R
#' @description Discovers and registers R functions as MCP tools through
#'   roxygen2 parsing. Scans one or more directories for functions tagged
#'   with the mcpr_tool keyword, converts documentation into structured
#'   tool specifications, and supports post-discovery filtering.
#' @details Provides comprehensive tool management:
#' \itemize{
#'   \item \strong{Multi-Directory Discovery}: Scans directories for
#'     tagged functions
#'   \item \strong{Documentation Parsing}: Converts roxygen2 comments to
#'     tool specs
#'   \item \strong{Type Mapping}: Maps parameter types to MCPR
#'     specifications
#'   \item \strong{Filtering}: Include/exclude tools by name for tailored
#'     server profiles
#'   \item \strong{Provenance}: Tracks which directory contributed each tool
#'   \item \strong{Validation}: Checks for naming conflicts and protocol
#'     compliance
#' }
#'
#' @param tools_dir Character vector of directory paths to scan for tool files
#' @param pattern File pattern to match (regex)
#' @param recursive Whether to search subdirectories
#' @param verbose Enable verbose output during search
#'
#' @examples
#' \dontrun{
#' # Basic usage — single directory
#' registry <- ToolRegistry$new()
#' tools <- registry$search_tools()
#'
#' # Multi-directory discovery
#' registry <- ToolRegistry$new(
#'   tools_dir = c(
#'     system.file(package = "MCPR"),
#'     system.file("tools", package = "myPkg")
#'   )
#' )
#'
#' # Filter after discovery
#' registry$search_tools()
#' registry$filter(exclude = "execute_r_code")
#'
#' # Cherry-pick tools from MCPR + all from another package
#' registry$filter(include = c("manage_r_sessions", "my_tool_a", "my_tool_b"))
#'
#' # Inspect what you got
#' registry$get_tool_summary()
#' }
#'
#' @export
ToolRegistry <- R6::R6Class("ToolRegistry",
  public = list(
    #' @description Create a new ToolRegistry instance with specified
    #'   configuration
    #' @param tools_dir Character vector of directory paths to scan for
    #'   tool files (default: "inst"). Each path is scanned independently;
    #'   missing directories are skipped with a warning.
    #' @param pattern File pattern to match (regex) (default: "tool-.*\\.R$")
    #' @param recursive Whether to search subdirectories (default: FALSE)
    #' @param verbose Enable verbose output during search (default: FALSE)
    #' @return New ToolRegistry instance
    initialize = function(tools_dir = "inst",
                          pattern = "tool-.*\\.R$",
                          recursive = FALSE,
                          verbose = FALSE) {
      private$.tools_dir <- tools_dir
      private$.pattern <- pattern
      private$.recursive <- recursive
      private$.verbose <- verbose
      private$.tools <- list()
      private$.all_tools <- list()
      private$.tool_files <- character()
      private$.include <- NULL
      private$.exclude <- NULL
    },

    #' @description Scan configured directories for functions tagged with
    #'   mcpr_tool keyword. Applies any active include/exclude filters
    #'   before returning results.
    #' @param force_refresh Force re-scanning even if tools are cached
    #'   (default: FALSE)
    #' @return List of MCPR tool objects (filtered if filters are active)
    search_tools = function(force_refresh = FALSE) {
      if (!force_refresh && length(private$.all_tools) > 0) {
        if (private$.verbose) {
          cli::cli_inform(
            "Using cached tools ({length(private$.tools)} active)."
          )
        }
        return(private$.tools)
      }

      # Collect files from all configured directories
      private$.tool_files <- character()
      for (dir in private$.tools_dir) {
        if (!dir.exists(dir)) {
          cli::cli_warn("Skipping missing directory {.path {dir}}")
          next
        }
        dir_files <- list.files(
          path = dir,
          pattern = private$.pattern,
          full.names = TRUE,
          recursive = private$.recursive,
          ignore.case = TRUE
        )
        private$.tool_files <- c(private$.tool_files, dir_files)
      }

      if (length(private$.tool_files) == 0) {
        if (private$.verbose) {
          cli::cli_inform("No tool files found.")
        }
        private$.all_tools <- list()
        private$.tools <- list()
        return(list())
      }

      if (private$.verbose) {
        n_dirs <- sum(vapply(private$.tools_dir, dir.exists, logical(1)))
        cli::cli_inform(
          "Searching {length(private$.tool_files)} file{?s} across \\
          {n_dirs} director{?y/ies}..."
        )
      }

      private$.all_tools <- list()

      for (tool_file in private$.tool_files) {
        tryCatch(
          {
            file_tools <- private$parse_file(tool_file)
            private$.all_tools <- c(private$.all_tools, file_tools)
            if (private$.verbose && length(file_tools) > 0) {
              cli::cli_inform(
                "Loaded {length(file_tools)} tool{?s} from \\
                {.file {basename(tool_file)}}"
              )
            }
          },
          error = function(e) {
            cli::cli_warn(
              "Failed to load {.file {basename(tool_file)}}: \\
              {conditionMessage(e)}"
            )
          }
        )
      }

      private$validate_tools(private$.all_tools)

      # Apply filters to produce the active tool set
      private$apply_filters()

      if (private$.verbose) {
        n_all <- length(private$.all_tools)
        n_active <- length(private$.tools)
        if (n_all == n_active) {
          cli::cli_inform("Successfully found {n_active} tool{?s}.")
        } else {
          cli::cli_inform(
            "Found {n_all} tool{?s}, {n_active} active after filtering."
          )
        }
      }

      private$.tools
    },

    #' @description Return currently loaded tools (after filtering)
    #'   without re-scanning
    #' @return List of MCPR tool objects
    get_tools = function() {
      private$.tools
    },

    #' @description Generate data.frame summary of loaded tools with
    #'   metadata
    #' @return Data.frame with columns: name, description, parameters,
    #'   source_dir
    get_tool_summary = function() {
      if (length(private$.tools) == 0) {
        return(data.frame(
          name = character(), description = character(),
          parameters = integer(), source_dir = character(),
          stringsAsFactors = FALSE
        ))
      }

      tool_info <- lapply(private$.tools, function(tool) {
        data.frame(
          name = tool$name,
          description = substr(tool$description, 1, 50),
          parameters = length(tool$arguments),
          source_dir = tool$annotations$source_dir %||% "",
          stringsAsFactors = FALSE
        )
      })

      do.call(rbind, tool_info)
    },

    #' @description Check if tool with specified name exists in registry
    #' @param name Name of the tool to check
    #' @return TRUE if tool exists, FALSE otherwise
    has_tool = function(name) {
      if (length(private$.tools) == 0) {
        return(FALSE)
      }
      tool_names <- vapply(private$.tools, function(x) x$name, character(1))
      name %in% tool_names
    },

    #' @description Retrieve specific tool by name from registry
    #' @param name Name of the tool to retrieve
    #' @return MCPR tool object or NULL if not found
    get_tool = function(name) {
      for (tool in private$.tools) {
        if (tool$name == name) {
          return(tool)
        }
      }
      NULL
    },

    #' @description Update search configuration and reset cached tools.
    #'   Accepts a character vector for tools_dir to enable
    #'   multi-directory discovery. Filters set via \code{filter()} are
    #'   preserved across \code{configure()} calls.
    #' @param tools_dir Character vector of directory paths (optional)
    #' @param pattern New file pattern (optional)
    #' @param recursive New recursive setting (optional)
    #' @return Self (invisibly) for method chaining
    configure = function(tools_dir = NULL, pattern = NULL, recursive = NULL) {
      if (!is.null(tools_dir)) private$.tools_dir <- tools_dir
      if (!is.null(pattern)) private$.pattern <- pattern
      if (!is.null(recursive)) private$.recursive <- recursive

      private$.tools <- list()
      private$.all_tools <- list()
      private$.tool_files <- character()
      invisible(self)
    },

    #' @description Set include/exclude filters for tool names.
    #'   Filters are applied immediately to already-discovered tools and
    #'   persist across subsequent \code{search_tools()} calls.
    #'   Call with no arguments to clear all filters.
    #' @param include Character vector of tool names to keep (allowlist).
    #'   When set, only tools whose names appear in this vector are
    #'   retained.
    #' @param exclude Character vector of tool names to drop (denylist).
    #'   Applied after include. Cannot overlap with include.
    #' @return Self (invisibly) for method chaining
    filter = function(include = NULL, exclude = NULL) {
      if (!is.null(include) && !is.null(exclude)) {
        overlap <- intersect(include, exclude)
        if (length(overlap) > 0) {
          cli::cli_abort(
            "Tool names appear in both {.arg include} and \\
            {.arg exclude}: {.val {overlap}}"
          )
        }
      }
      private$.include <- include
      private$.exclude <- exclude
      private$apply_filters()
      invisible(self)
    },

    #' @description Enable or disable verbose output during search operations
    #' @param verbose TRUE to enable verbose output, FALSE to disable
    #' @return Self (invisibly) for method chaining
    set_verbose = function(verbose) {
      private$.verbose <- verbose
      invisible(self)
    },

    #' @description Print summary of ToolRegistry with directory, filter,
    #'   and tool information
    #' @return Self (invisibly)
    print = function() {
      cli::cli_text("<ToolRegistry>")
      for (dir in private$.tools_dir) {
        cli::cli_text("  Directory: {.path {dir}}")
      }
      n_all <- length(private$.all_tools)
      n_active <- length(private$.tools)
      if (n_all == n_active) {
        cli::cli_text("  Tools: {n_active}")
      } else {
        cli::cli_text("  Tools: {n_active} active ({n_all} discovered)")
      }
      if (!is.null(private$.include)) {
        cli::cli_text("  Include: {.val {private$.include}}")
      }
      if (!is.null(private$.exclude)) {
        cli::cli_text("  Exclude: {.val {private$.exclude}}")
      }
      if (n_active > 0) {
        names <- vapply(private$.tools, function(x) x$name, character(1))
        cli::cli_text("  Names: {.field {names}}")
      }
      invisible(self)
    }
  ),
  private = list(
    .tools_dir = NULL,
    .pattern = NULL,
    .recursive = NULL,
    .tools = NULL,
    .all_tools = NULL,
    .tool_files = NULL,
    .verbose = NULL,
    .include = NULL,
    .exclude = NULL,

    # Applies include/exclude filters from .all_tools to produce .tools
    apply_filters = function() {
      filtered <- private$.all_tools
      if (!is.null(private$.include)) {
        filtered <- Filter(function(t) t$name %in% private$.include, filtered)
      }
      if (!is.null(private$.exclude)) {
        filtered <- Filter(function(t) !t$name %in% private$.exclude, filtered)
      }
      private$.tools <- filtered
    },

    # Parses R file using roxygen2 to extract functions with mcpr_tool keyword
    parse_file = function(file_path) {
      if (!file.exists(file_path)) {
        cli::cli_abort("Tool file {.file {file_path}} does not exist.")
      }

      if (private$.verbose) {
        cli::cli_inform("Parsing: {.file {basename(file_path)}}")
      }

      # Parse roxygen blocks using roxygen2
      parsed_blocks <- tryCatch(
        roxygen2::parse_file(file_path),
        error = function(e) {
          cli::cli_warn(
            "Failed to parse {.file {basename(file_path)}}: \\
            {conditionMessage(e)}"
          )
          NULL
        }
      )
      if (is.null(parsed_blocks)) return(list())

      # Filter blocks that have @keywords mcpr_tool tag
      tool_blocks <- Filter(function(block) {
        any(vapply(block$tags, function(tag) {
          inherits(tag, "roxy_tag_keywords") && "mcpr_tool" %in% tag$val
        }, logical(1)))
      }, parsed_blocks)

      if (length(tool_blocks) == 0) {
        return(list())
      }

      # Source the file to get functions
      sourced_env <- new.env()
      source(file_path, local = sourced_env)

      # Stamp provenance into each tool's annotations
      source_dir <- dirname(file_path)

      # Create tools from parsed blocks
      tools <- list()
      for (block in tool_blocks) {
        tool <- create_tool_from_block(block, sourced_env, file_path)
        if (!is.null(tool)) {
          tool$annotations$source_dir <- source_dir
          tool$annotations$source_file <- basename(file_path)
          tools[[length(tools) + 1]] <- tool
        }
      }

      tools
    },

    # Validates tool collection for naming conflicts and protocol compliance
    validate_tools = function(tools) {
      if (length(tools) == 0) {
        return(TRUE)
      }

      tool_names <- vapply(tools, function(x) x$name, character(1))
      duplicates <- tool_names[duplicated(tool_names)]
      if (length(duplicates) > 0) {
        # Report provenance for duplicates
        for (dup in unique(duplicates)) {
          dup_tools <- Filter(function(t) t$name == dup, tools)
          dirs <- vapply(
            dup_tools,
            function(t) t$annotations$source_dir %||% "unknown",
            character(1)
          )
          cli::cli_warn(
            "Duplicate tool name {.field {dup}} found in: \\
            {.path {dirs}}"
          )
        }
        return(FALSE)
      }

      TRUE
    }
  )
)

#' Register Tools from Directory
#'
#' @title Register Tools from Directory
#' @description Convenience function creating ToolRegistry instance and
#'   returning loaded tools. Combines registry creation, tool discovery,
#'   and validation in single call for simplified tool registration
#'   workflow. Provides direct access to discovered tools without manual
#'   registry management.
#'
#' @param tools_dir Directory path to scan for tool files (default: "inst")
#' @param pattern File pattern to match (regex) (default: "tool-.*\\.R$")
#' @param recursive Whether to search subdirectories (default: FALSE)
#' @param verbose Enable verbose output during search (default: FALSE)
#' @return List of MCPR tool objects
#'
#' @seealso \code{\link{ToolRegistry}} for the underlying class
#' @noRd
register_tools <- function(tools_dir = "inst",
                           pattern = "tool-.*\\.R$",
                           recursive = FALSE,
                           verbose = FALSE) {
  registry <- ToolRegistry$new(tools_dir, pattern, recursive, verbose)
  registry$search_tools()
}
