#' Tool Registry for MCPR
#'
#' The `ToolRegistry` class automatically discovers and registers R functions
#' as tools for AI coding assistants within the MCPR framework. It scans R files
#' for functions tagged with ` @keywords mcpr_tool` and converts their roxygen2
#' documentation into ellmer tool specifications.
#'
#' @details
#' This class uses roxygen2 parsing to extract function metadata and convert
#' parameter documentation into structured tool definitions. Tools are validated
#' for naming conflicts and compatibility with the MCPR protocol.
#'
#' The discovery process looks for functions with the ` @keywords mcpr_tool` tag
#' and automatically converts ` @param` documentation into ellmer type specifications.
#' Supported parameter types include: character/string, numeric/number, integer/int,
#' logical/boolean/bool, list/array.
#'
#' @section Public Methods:
#' \describe{
#'   \item{`initialize(tools_dir, pattern, recursive, verbose)`}{Create new ToolRegistry instance}
#'   \item{`search_tools(force_refresh = FALSE)`}{Scan directory and discover tools}
#'   \item{`get_tools()`}{Return list of discovered tools}
#'   \item{`get_tool_summary()`}{Get data.frame summary of tools}
#'   \item{`has_tool(name)`}{Check if tool exists by name}
#'   \item{`get_tool(name)`}{Retrieve specific tool by name}
#'   \item{`configure(tools_dir, pattern, recursive)`}{Update discovery configuration}
#'   \item{`set_verbose(verbose)`}{Enable/disable verbose output}
#' }
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
  private = list(
    .tools_dir = NULL,
    .pattern = NULL,
    .recursive = NULL,
    .discovered_tools = NULL,
    .tool_files = NULL,
    .verbose = NULL,

    # @description Parses a single R file to find functions with `@keywords mcpr_tool`.
    # @param file_path character. The path to the R file.
    # @return A list of `ellmer::tool` objects found in the file.
    parse_file = function(file_path) {
      if (!file.exists(file_path)) {
        cli::cli_abort("Tool file {.file {file_path}} does not exist.")
      }
      
      if (private$.verbose) {
        cli::cli_inform("Parsing: {.file {basename(file_path)}}")
      }
      
      # Parse roxygen blocks using roxygen2
      tryCatch({
        parsed_blocks <- roxygen2::parse_file(file_path)
      }, error = function(e) {
        cli::cli_warn("Failed to parse {.file {basename(file_path)}}: {conditionMessage(e)}")
        return(list())
      })
      
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

    # @description Checks for duplicate names and conflicts with reserved names.
    # @param tools A list of `ellmer::tool` objects.
    # @return `TRUE` if validation passes, `FALSE` otherwise.
    validate_tools = function(tools) {
      if (length(tools) == 0) return(TRUE)
      
      tool_names <- vapply(tools, function(x) x@name, character(1))
      duplicates <- tool_names[duplicated(tool_names)]
      
      if (length(duplicates) > 0) {
        cli::cli_warn("Duplicate tool names: {.field {unique(duplicates)}}")
        return(FALSE)
      }
      
      reserved_names <- c("list_r_sessions", "select_r_session")
      conflicts <- intersect(tool_names, reserved_names)
      
      if (length(conflicts) > 0) {
        cli::cli_warn("Reserved name conflicts: {.field {conflicts}}")
        return(FALSE)
      }
      
      TRUE
    }
  ),

  public = list(
    #' @description Create a new ToolRegistry instance with specified configuration.
    #' @param tools_dir character. Directory path to scan for tool files. Default: "inst/mcpr_tools"
    #' @param pattern character. File pattern to match (regex). Default: "\\.R$"
    #' @param recursive logical. Whether to search subdirectories. Default: FALSE
    #' @param verbose logical. Enable verbose output during discovery. Default: TRUE
    #' @examples
    #' \dontrun{
    #' registry <- ToolRegistry$new(
    #'   tools_dir = "inst/mcpr_tools",
    #'   recursive = TRUE,
    #'   verbose = FALSE
    #' )
    #' }
    initialize = function(tools_dir = "inst/mcpr_tools", 
                         pattern = "\\.R$", 
                         recursive = FALSE,
                         verbose = TRUE) {
      private$.tools_dir <- tools_dir
      private$.pattern <- pattern
      private$.recursive <- recursive
      private$.verbose <- verbose
      private$.discovered_tools <- list()
      private$.tool_files <- character()
    },

    #' @description Scan the configured directory for R files containing functions
    #' tagged with ` @keywords mcpr_tool` and convert them to ellmer tools.
    #' @param force_refresh logical. Force re-discovery even if tools already cached. Default: FALSE
    #' @return list of ellmer tool objects
    #' @examples
    #' \dontrun{
    #' registry <- ToolRegistry$new()
    #' registry$search_tools()
    #' tools <- registry$search_tools(force_refresh = TRUE)
    #' }
    search_tools = function(force_refresh = FALSE) {
      if (!force_refresh && length(private$.discovered_tools) > 0) {
        return(private$.discovered_tools)
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
        cli::cli_inform("Discovering tools from {length(private$.tool_files)} file{?s}...")
      }
      
      private$.discovered_tools <- list()
      
      for (tool_file in private$.tool_files) {
        tryCatch({
          file_tools <- private$parse_file(tool_file)
          private$.discovered_tools <- c(private$.discovered_tools, file_tools)
          if (private$.verbose && length(file_tools) > 0) {
            cli::cli_inform("✓ Loaded {length(file_tools)} tool{?s} from {.file {basename(tool_file)}}")
          }
        }, error = function(e) {
          cli::cli_warn("Failed to load {.file {basename(tool_file)}}: {conditionMessage(e)}")
        })
      }
      
      private$validate_tools(private$.discovered_tools)
      
      if (private$.verbose) {
        cli::cli_inform("Successfully discovered {length(private$.discovered_tools)} tool{?s}.")
      }
      
      private$.discovered_tools
    },

    #' @description Return the list of currently discovered tools without re-scanning.
    #' @return list of ellmer tool objects
    #' @examples
    #' \dontrun{
    #' registry <- ToolRegistry$new()
    #' registry$search_tools()
    #' tools <- registry$get_tools()
    #' }
    get_tools = function() {
      private$.discovered_tools
    },

    #' @description Generate a data.frame summary of discovered tools with names,
    #' descriptions, and parameter counts.
    #' @return data.frame with columns: name, description, parameters
    #' @examples
    #' \dontrun{
    #' registry <- ToolRegistry$new()
    #' registry$search_tools()
    #' summary <- registry$get_tool_summary()
    #' print(summary)
    #' }
    get_tool_summary = function() {
      if (length(private$.discovered_tools) == 0) {
        return(data.frame(name = character(), description = character(), stringsAsFactors = FALSE))
      }
      
      tool_info <- lapply(private$.discovered_tools, function(tool) {
        data.frame(
          name = tool@name,
          description = substr(tool@description, 1, 50),
          parameters = length(tool@arguments),
          stringsAsFactors = FALSE
        )
      })
      
      do.call(rbind, tool_info)
    },

    #' @description Check if a tool with the specified name has been discovered.
    #' @param name character. Name of the tool to check
    #' @return logical. TRUE if tool exists, FALSE otherwise
    #' @examples
    #' \dontrun{
    #' registry <- ToolRegistry$new()
    #' registry$search_tools()
    #' if (registry$has_tool("my_function")) {
    #'   # Use the tool
    #' }
    #' }
    has_tool = function(name) {
      if (length(private$.discovered_tools) == 0) return(FALSE)
      tool_names <- vapply(private$.discovered_tools, function(x) x@name, character(1))
      name %in% tool_names
    },

    #' @description Retrieve a specific tool by name.
    #' @param name character. Name of the tool to retrieve
    #' @return ellmer tool object or NULL if not found
    #' @examples
    #' \dontrun{
    #' registry <- ToolRegistry$new()
    #' registry$search_tools()
    #' tool <- registry$get_tool("my_function")
    #' if (!is.null(tool)) {
    #'   # Use the tool
    #' }
    #' }
    get_tool = function(name) {
      for (tool in private$.discovered_tools) {
        if (tool@name == name) return(tool)
      }
      NULL
    },

    #' @description Update the discovery configuration and reset cached tools.
    #' @param tools_dir character. New directory path (optional)
    #' @param pattern character. New file pattern (optional)
    #' @param recursive logical. New recursive setting (optional)
    #' @return self (invisibly) for method chaining
    #' @examples
    #' \dontrun{
    #' registry <- ToolRegistry$new()
    #' registry$configure(
    #'   tools_dir = "new/path",
    #'   recursive = TRUE
    #' )$search_tools()
    #' }
    configure = function(tools_dir = NULL, pattern = NULL, recursive = NULL) {
      if (!is.null(tools_dir)) private$.tools_dir <- tools_dir
      if (!is.null(pattern)) private$.pattern <- pattern
      if (!is.null(recursive)) private$.recursive <- recursive
      
      private$.discovered_tools <- list()
      private$.tool_files <- character()
      invisible(self)
    },

    #' @description Enable or disable verbose output during discovery operations.
    #' @param verbose logical. TRUE to enable verbose output, FALSE to disable
    #' @return self (invisibly) for method chaining
    #' @examples
    #' \dontrun{
    #' registry <- ToolRegistry$new()
    #' registry$set_verbose(FALSE)$search_tools()
    #' }
    set_verbose = function(verbose) {
      private$.verbose <- verbose
      invisible(self)
    },

    #' @description
    #' Prints a summary of the ToolRegistry, including the tools directory,
    #' the number of discovered tools, and their names.
    #' @return The `ToolRegistry` object, invisibly.
    print = function() {
      cat("<ToolRegistry>\n")
      cat("  Directory: ", private$.tools_dir, "\n")
      cat("  Tools: ", length(private$.discovered_tools), "\n")
      if (length(private$.discovered_tools) > 0) {
        names <- vapply(private$.discovered_tools, function(x) x@name, character(1))
        cat("  Names: ", paste(names, collapse = ", "), "\n")
      }
      invisible(self)
    }
  )
)

#' Register Tools (Convenience Function)
#'
#' A convenience function that creates a ToolRegistry instance, discovers tools,
#' and returns the discovered tools in a single call.
#'
#' @param tools_dir character. Directory path to scan for tool files. Default: "inst/mcpr_tools"
#' @param pattern character. File pattern to match (regex). Default: "\\.R$"
#' @param recursive logical. Whether to search subdirectories. Default: FALSE
#' @param verbose logical. Enable verbose output during discovery. Default: TRUE
#' @return list of ellmer tool objects
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' tools <- register_tools()
#' 
#' # Custom configuration
#' tools <- register_tools(
#'   tools_dir = "custom/tools",
#'   recursive = TRUE,
#'   verbose = FALSE
#' )
#' }
#'
#' @seealso \code{\link{ToolRegistry}} for the underlying class
#' @export
register_tools <- function(tools_dir = "inst/mcpr_tools", 
                                       pattern = "\\.R$", 
                                       recursive = FALSE,
                                       verbose = TRUE) {
  registry <- ToolRegistry$new(tools_dir, pattern, recursive, verbose)
  registry$search_tools()
}