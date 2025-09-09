#' Tool Registry for MCPR Framework
#'
#' @title Tool Registry
#' @description Automatically discovers and registers R functions as MCP tools through roxygen2 parsing.
#' Scans R files for functions tagged with mcpr_tool keyword and converts documentation
#' into structured tool specifications. Enables tool discovery and validation for MCP
#' protocol integration through automated function metadata extraction.
#' @details Provides comprehensive tool management:
#' \itemize{
#'   \item \strong{Automatic Discovery}: Scans directories for tagged functions
#'   \item \strong{Documentation Parsing}: Converts roxygen2 comments to tool specs
#'   \item \strong{Type Mapping}: Maps parameter types to MCPR specifications
#'   \item \strong{Validation}: Checks for naming conflicts and protocol compliance
#' }
#'
#' @param tools_dir Directory path to scan for tool files
#' @param pattern File pattern to match (regex)
#' @param recursive Whether to search subdirectories
#' @param verbose Enable verbose output during search
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' registry <- ToolRegistry$new()
#' tools <- registry$search_tools()
#'
#' # Custom configuration
#' registry <- ToolRegistry$new(
#'   tools_dir = "custom/tools",
#'   pattern = "\\.R$",
#'   recursive = TRUE
#' )
#'
#' # Get tool information
#' summary <- registry$get_tool_summary()
#' if (registry$has_tool("my_function")) {
#'   tool <- registry$get_tool("my_function")
#' }
#' }
#'
#' @export
ToolRegistry <- R6::R6Class("ToolRegistry",
  public = list(
    #' @description Create a new ToolRegistry instance with specified configuration
    #' @param tools_dir Directory path to scan for tool files (default: "inst")
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
      private$.tool_files <- character()
    },

    #' @description Scan configured directory for functions tagged with mcpr_tool keyword
    #' @param force_refresh Force re-scanning even if tools are cached (default: FALSE)
    #' @return List of MCPR tool objects
    search_tools = function(force_refresh = FALSE) {
      if (!force_refresh && length(private$.tools) > 0) {
        return(private$.tools)
      }

      if (!dir.exists(private$.tools_dir)) {
        if (private$.verbose) {
          cli::cli_inform("Tools directory {.path {private$.tools_dir}} does not exist.")
        }
        return(list())
      }

      private$.tool_files <- list.files(
        path = private$.tools_dir,
        pattern = private$.pattern,
        full.names = TRUE,
        recursive = private$.recursive,
        ignore.case = TRUE
      )

      if (length(private$.tool_files) == 0) {
        if (private$.verbose) {
          cli::cli_inform("No tool files found.")
        }
        return(list())
      }

      if (private$.verbose) {
        cli::cli_inform("Searching for tools in {length(private$.tool_files)} file{?s}...")
      }

      private$.tools <- list()

      for (tool_file in private$.tool_files) {
        tryCatch(
          {
            file_tools <- private$parse_file(tool_file)
            private$.tools <- c(private$.tools, file_tools)
            if (private$.verbose && length(file_tools) > 0) {
              cli::cli_inform("Loaded {length(file_tools)} tool{?s} from {.file {basename(tool_file)}}")
            }
          },
          error = function(e) {
            cli::cli_warn("Failed to load {.file {basename(tool_file)}}: {conditionMessage(e)}")
          }
        )
      }

      private$validate_tools(private$.tools)

      if (private$.verbose) {
        cli::cli_inform("Successfully found {length(private$.tools)} tool{?s}.")
      }

      private$.tools
    },

    #' @description Return currently loaded tools without re-scanning
    #' @return List of MCPR tool objects
    get_tools = function() {
      private$.tools
    },

    #' @description Generate data.frame summary of loaded tools with metadata
    #' @return Data.frame with columns: name, description, parameters
    get_tool_summary = function() {
      if (length(private$.tools) == 0) {
        return(data.frame(name = character(), description = character(), stringsAsFactors = FALSE))
      }

      tool_info <- lapply(private$.tools, function(tool) {
        data.frame(
          name = tool$name,
          description = substr(tool$description, 1, 50),
          parameters = length(tool$arguments),
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

    #' @description Update search configuration and reset cached tools
    #' @param tools_dir New directory path (optional)
    #' @param pattern New file pattern (optional)
    #' @param recursive New recursive setting (optional)
    #' @return Self (invisibly) for method chaining
    configure = function(tools_dir = NULL, pattern = NULL, recursive = NULL) {
      if (!is.null(tools_dir)) private$.tools_dir <- tools_dir
      if (!is.null(pattern)) private$.pattern <- pattern
      if (!is.null(recursive)) private$.recursive <- recursive

      private$.tools <- list()
      private$.tool_files <- character()
      invisible(self)
    },

    #' @description Enable or disable verbose output during search operations
    #' @param verbose TRUE to enable verbose output, FALSE to disable
    #' @return Self (invisibly) for method chaining
    set_verbose = function(verbose) {
      private$.verbose <- verbose
      invisible(self)
    },

    #' @description Print summary of ToolRegistry with directory and tool information
    #' @return Self (invisibly)
    print = function() {
      cat("<ToolRegistry>\n")
      cat("  Directory: ", private$.tools_dir, "\n")
      cat("  Tools: ", length(private$.tools), "\n")
      if (length(private$.tools) > 0) {
        names <- vapply(private$.tools, function(x) x$name, character(1))
        cat("  Names: ", paste(names, collapse = ", "), "\n")
      }
      invisible(self)
    }
  ),
  private = list(
    .tools_dir = NULL,
    .pattern = NULL,
    .recursive = NULL,
    .tools = NULL,
    .tool_files = NULL,
    .verbose = NULL,

    # Parses R file using roxygen2 to extract functions with mcpr_tool keyword
    parse_file = function(file_path) {
      if (!file.exists(file_path)) {
        cli::cli_abort("Tool file {.file {file_path}} does not exist.")
      }

      if (private$.verbose) {
        cli::cli_inform("Parsing: {.file {basename(file_path)}}")
      }

      # Parse roxygen blocks using roxygen2
      tryCatch(
        {
          parsed_blocks <- roxygen2::parse_file(file_path)
        },
        error = function(e) {
          cli::cli_warn("Failed to parse {.file {basename(file_path)}}: {conditionMessage(e)}")
          return(list())
        }
      )

      # Filter blocks that have @keywords mcpr_tool tag
      tool_blocks <- Filter(function(block) {
        any(sapply(block$tags, function(tag) {
          inherits(tag, "roxy_tag_keywords") && "mcpr_tool" %in% tag$val
        }))
      }, parsed_blocks)

      if (length(tool_blocks) == 0) {
        return(list())
      }

      # Source the file to get functions
      sourced_env <- new.env()
      source(file_path, local = sourced_env)

      # Create tools from parsed blocks
      tools <- list()
      for (block in tool_blocks) {
        tool <- create_tool_from_block(block, sourced_env, file_path)
        if (!is.null(tool)) {
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
        cli::cli_warn("Duplicate tool names: {.field {unique(duplicates)}}")
        return(FALSE)
      }

      TRUE
    }
  )
)

#' Register Tools from Directory
#'
#' @title Register Tools from Directory
#' @description Convenience function creating ToolRegistry instance and returning loaded tools.
#' Combines registry creation, tool discovery, and validation in single call for
#' simplified tool registration workflow. Provides direct access to discovered
#' tools without manual registry management.
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
