#' Tool Discovery Engine using roxygen2 parser
#' @export
ToolDiscovery <- R6::R6Class("ToolDiscovery",
  private = list(
    .tools_dir = NULL,
    .pattern = NULL,
    .recursive = NULL,
    .discovered_tools = NULL,
    .tool_files = NULL,
    .verbose = NULL,

    parse_tool_file = function(file_path) {
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
      
      # Filter blocks that have @mcpr_tool tag
      tool_blocks <- Filter(function(block) {
        any(sapply(block$tags, function(tag) inherits(tag, "roxy_tag") && tag$tag == "mcpr_tool"))
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
        tool <- private$create_tool_from_block(block, sourced_env, file_path)
        if (!is.null(tool)) {
          tools[[length(tools) + 1]] <- tool
        }
      }
      
      tools
    },

    create_tool_from_block = function(block, env, file_path) {
      # Extract function name from the block object
      func_name <- block$object$alias
      
      if (is.null(func_name) || !exists(func_name, envir = env)) {
        cli::cli_warn("Function {.fn {func_name %||% 'unknown'}} not found in {.file {basename(file_path)}}")
        return(NULL)
      }
      
      func <- get(func_name, envir = env)
      if (!is.function(func)) {
        cli::cli_warn("{.fn {func_name}} is not a function")
        return(NULL)
      }
      
      # Extract description
      description <- private$extract_description(block)
      
      # Extract parameters
      param_tags <- Filter(function(tag) inherits(tag, "roxy_tag_param"), block$tags)
      ellmer_args <- private$convert_params_to_ellmer(param_tags)
      
      # Create the tool
      tryCatch({
        tool_args <- c(
          list(.fun = func, .description = description),
          ellmer_args
        )
        do.call(ellmer::tool, tool_args)
      }, error = function(e) {
        cli::cli_warn("Failed to create tool for {.fn {func_name}}: {conditionMessage(e)}")
        NULL
      })
    },

    extract_description = function(block) {
      # Look for @description tag first
      desc_tag <- Find(function(tag) inherits(tag, "roxy_tag_description"), block$tags)
      if (!is.null(desc_tag)) {
        return(paste(desc_tag$val, collapse = " "))
      }
      
      # Fall back to title/introduction
      intro_tag <- Find(function(tag) inherits(tag, "roxy_tag_intro"), block$tags)
      if (!is.null(intro_tag)) {
        return(paste(intro_tag$val, collapse = " "))
      }
      
      # Default
      return("No description available")
    },

    convert_params_to_ellmer = function(param_tags) {
      ellmer_args <- list()
      
      for (param_tag in param_tags) {
        param_name <- param_tag$name
        param_desc <- paste(param_tag$description, collapse = " ")
        
        # Extract type from description (look for type at start)
        type_pattern <- "^(character|string|numeric|number|integer|int|logical|boolean|bool|list|array)\\s+"
        type_match <- regexpr(type_pattern, param_desc, ignore.case = TRUE)
        
        if (type_match != -1) {
          type_str <- regmatches(param_desc, type_match)
          type_str <- trimws(gsub("\\s+$", "", type_str))
          param_desc <- sub(type_pattern, "", param_desc, ignore.case = TRUE)
        } else {
          type_str <- "string"  # default
        }
        
        ellmer_args[[param_name]] <- private$convert_type_to_ellmer(type_str, param_desc)
      }
      
      ellmer_args
    },

    convert_type_to_ellmer = function(type_str, description) {
      switch(tolower(type_str),
        "character" = , "string" = ellmer::type_string(description = description),
        "numeric" = , "number" = ellmer::type_number(description = description),
        "integer" = , "int" = ellmer::type_integer(description = description),
        "logical" = , "boolean" = , "bool" = ellmer::type_boolean(description = description),
        "list" = , "array" = ellmer::type_array(description = description),
        ellmer::type_string(description = description)
      )
    },

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

    discover_tools = function(force_refresh = FALSE) {
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
          file_tools <- private$parse_tool_file(tool_file)
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

    get_tools = function() {
      private$.discovered_tools
    },

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

    has_tool = function(name) {
      if (length(private$.discovered_tools) == 0) return(FALSE)
      tool_names <- vapply(private$.discovered_tools, function(x) x@name, character(1))
      name %in% tool_names
    },

    get_tool = function(name) {
      for (tool in private$.discovered_tools) {
        if (tool@name == name) return(tool)
      }
      NULL
    },

    configure = function(tools_dir = NULL, pattern = NULL, recursive = NULL) {
      if (!is.null(tools_dir)) private$.tools_dir <- tools_dir
      if (!is.null(pattern)) private$.pattern <- pattern
      if (!is.null(recursive)) private$.recursive <- recursive
      
      private$.discovered_tools <- list()
      private$.tool_files <- character()
      invisible(self)
    },

    set_verbose = function(verbose) {
      private$.verbose <- verbose
      invisible(self)
    },

    print = function() {
      cat("<ToolDiscovery>\n")
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

#' @export
discover_and_register_tools <- function(tools_dir = "inst/mcpr_tools", 
                                       pattern = "\\.R$", 
                                       recursive = FALSE,
                                       verbose = TRUE) {
  discoverer <- ToolDiscovery$new(tools_dir, pattern, recursive, verbose)
  discoverer$discover_tools()
}
