# R6 Class Implementation for MCP Client
# This file implements the mcpClient R6 class, bringing existing functionality
# from the functional implementation in client.R

#' @name mcpClient
#' @title MCP Client R6 Class
#'
#' @description
#' The mcpClient class provides a persistent, object-oriented interface for managing 
#' Model Context Protocol (MCP) servers within R sessions. It enables AI coding 
#' assistants to maintain workspace state and collaborate iteratively with R users.
#'
#' @details
#' The mcpClient class manages:
#' \itemize{
#'   \item \strong{Server Connections}: Establishes and maintains connections to MCP servers
#'   \item \strong{Tool Discovery}: Automatically discovers and registers available tools
#'   \item \strong{Protocol Communication}: Handles JSON-RPC messaging with servers
#'   \item \strong{State Management}: Maintains server status and tool metadata
#' }
#'
#' ## Configuration File Format
#' The configuration file should be JSON format with mcpServers entry:
#' \preformatted{
#' {
#'   "mcpServers": {
#'     "filesystem": {
#'       "command": "npx",
#'       "args": ["-y", " @modelcontextprotocol/server-filesystem", "/path/to/directory"],
#'       "env": {
#'         "NODE_ENV": "production"
#'       }
#'     }
#'   }
#' }
#' }
#'
#' ## Default Configuration Location
#' \code{~/.config/mcptools/config.json}
#'
#' ## Environment Variables
#' Set \code{options(.mcptools_config = "path")} to override default config location
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' client <- mcpClient$new()
#' client$connect_servers()
#' tools <- client$get_mcpr_tools()
#'
#' # With custom configuration
#' client <- mcpClient$new(config = "path/to/config.json")
#' client$connect_servers()
#'
#' # Method chaining
#' client <- mcpClient$new()$connect_servers()
#'
#' # Call specific tool
#' result <- client$call_tool(
#'   path = "/home/user/file.txt",
#'   server = "filesystem", 
#'   tool = "read_file"
#' )
#'
#' # Check server status
#' status <- client$get_server_status()
#' }
#'
#' @export
mcpClient <- R6::R6Class("mcpClient",
  public = list(
    #' @description Creates new mcpClient instance
    #' @param config Path to configuration file (character). Uses default location if NULL
    #' @return Self (invisible)
    #' @examples
    #' \dontrun{
    #' client <- mcpClient$new()
    #' client <- mcpClient$new(config = "~/.config/mcptools/config.json")
    #' }
    initialize = function(config = NULL) {
      private$.servers <- list()
      private$.server_processes <- list()
      private$.messenger <- MessageHandler$new(logger = self$log_communication)
      
      if (is.null(config)) {
        config <- getOption(
          ".mcptools_config",
          default = private$default_mcp_client_config()
        )
      }
      
      private$.config_path <- config
      
      if (file.exists(config)) {
        private$.config <- private$read_mcp_config(config)
      }
    },
    
    #' Connect to MCP servers
    #'
    #' @description Establishes connections to all configured MCP servers
    #' @return Self (invisible, enables method chaining)
    #' @examples
    #' \dontrun{
    #' client$connect_servers()
    #' # Method chaining
    #' client <- mcpClient$new()$connect_servers()
    #' }
    connect_servers = function() {
      if (is.null(private$.config) || length(private$.config) == 0) {
        cli::cli_warn("No servers configured")
        return(invisible(self))
      }
      
      for (i in seq_along(private$.config)) {
        config_i <- private$.config[[i]]
        name_i <- names(private$.config)[i]
        config_i_env <- if ("env" %in% names(config_i)) {
          unlist(config_i$env)
        } else {
          NULL
        }

        process <- processx::process$new(
          command = Sys.which(config_i$command),
          args = config_i$args,
          env = config_i_env,
          stdin = "|",
          stdout = "|",
          stderr = "|"
        )

        private$.server_processes <- c(
          private$.server_processes,
          rlang::list2(
            !!paste0(c(config_i$command, config_i$args), collapse = " ") := process
          )
        )

        private$add_mcp_server(process = process, name = name_i)
      }
      
      invisible(self)
    },
    
    #' Get MCPR-compatible tools
    #'
    #' @description Retrieves all discovered tools in MCPR-compatible format
    #' @return List of MCPR tool objects
    #' @examples
    #' \dontrun{
    #' tools <- client$get_mcpr_tools()
    #' # Tools can be used with MCPR package for AI integration
    #' }
    get_mcpr_tools = function() {
      if (length(private$.servers) == 0) {
        self$connect_servers()
      }
      
      unname(unlist(
        lapply(private$.servers, private$server_as_mcpr_tools),
        recursive = FALSE
      ))
    },
    
    #' Call a specific tool
    #'
    #' @description Executes specific tool on designated server
    #' @param ... Named arguments for the tool
    #' @param server Server name (character)
    #' @param tool Tool name (character)
    #' @return Tool execution result
    #' @examples
    #' \dontrun{
    #' result <- client$call_tool(
    #'   path = "/home/user/file.txt",
    #'   server = "filesystem",
    #'   tool = "read_file"
    #' )
    #' 
    #' # With error handling
    #' result <- tryCatch({
    #'   client$call_tool(param = "value", server = "myserver", tool = "mytool")
    #' }, error = function(e) {
    #'   cat("Tool execution failed:", e$message)
    #'   NULL
    #' })
    #' }
    call_tool = function(..., server, tool) {
      # Delegate to unified execution engine
      execute_tool_call(create_execution_context(
        id = private$next_id(server),
        tool_name = tool,
        arguments = list(...),
        process = private$.servers[[server]]$process,
        client = self
      ))
    },
    
    #' Get server status
    #'
    #' @description Retrieves status information for all connected servers
    #' @return Named list with server status details including name, connected status, tools_count, and last_id
    #' @examples
    #' \dontrun{
    #' status <- client$get_server_status()
    #' # Returns: list(server_name = list(name, connected, tools_count, last_id))
    #' 
    #' # Check server status before tool calls
    #' status <- client$get_server_status()
    #' if (status$filesystem$connected) {
    #'   result <- client$call_tool(server = "filesystem", tool = "list_files")
    #' }
    #' }
    get_server_status = function() {
      lapply(private$.servers, function(server) {
        list(
          name = server$name,
          connected = server$process$is_alive(),
          tools_count = length(server$tools$tools %||% list()),
          last_id = server$id
        )
      })
    },
    
    #' Log communication for debugging
    #'
    #' @description Logs communication messages for debugging purposes
    #' @param message Message to log (character)
    #' @examples
    #' \dontrun{
    #' client$log_communication("Custom debug message")
    #' }
    log_communication = function(message) {
      log_file <- "~/mcp_client_test.txt"
      cat(message, "\n\n", sep = "", append = TRUE, file = log_file)
    },
    
    #' Convert tool schema to MCPR types
    #'
    #' @description Converts tool schema definitions to MCPR-compatible types
    #' @param tool Tool definition object
    #' @return Converted tool types
    #' @examples
    #' \dontrun{
    #' # Internal method typically not called directly
    #' types <- client$as_mcpr_types(tool_definition)
    #' }
    as_mcpr_types = function(tool) {
      if (is.null(tool$inputSchema) || is.null(tool$inputSchema$properties)) {
        return(list())
      }
      
      properties <- tool$inputSchema$properties
      mcpr_types <- list()
      
      for (prop_name in names(properties)) {
        prop <- properties[[prop_name]]
        mcpr_types[[prop_name]] <- map_type_schema(prop, input_type = "json")
      }
      
      mcpr_types
    }
  ),
  
  private = list(
    .servers = NULL,
    .server_processes = NULL,
    .config_path = NULL,
    .config = NULL,
    .messenger = NULL,
    
    
    add_mcp_server = function(process, name) {
      # Validate server process is alive
      if (!process$is_alive()) {
        cli::cli_abort("Server process {.val {name}} is not running")
      }
      
      response_initialize <- private$.messenger$send_receive(
        process, 
        private$.messenger$create_initialize_request(),
        context = name
      )
      
      # Validate initialization response
      if (is.null(response_initialize$result) || !is.list(response_initialize$result)) {
        cli::cli_abort("Server {.val {name}} failed initialization")
      }
      response_tools_list <- private$.messenger$send_receive(
        process,
        private$.messenger$create_tools_list_request(),
        context = name
      )

      private$.servers[[name]] <- list(
        name = name,
        process = process,
        tools = response_tools_list$result,
        id = 3 # TODO: Implement proper ID management
      )

      private$.servers[[name]]
    },
    
    tool_ref = function(server, tool, arguments) {
      f <- function() {}
      formals(f) <- setNames(
        rep(list(quote(expr = )), length(arguments)),
        arguments
      )

      body(f) <- substitute(
        {
          call_info <- match.call()
          tool_args <- lapply(call_info[-1], eval)
          do.call(
            self$call_tool,
            c(tool_args, list(server = server_val, tool = tool_val))
          )
        },
        list(server_val = server, tool_val = tool)
      )

      f
    },
    
    # ID Management
    
    next_id = function(server_name) {
      # Simple ID management - increment and return
      current_id <- private$.servers[[server_name]]$id
      private$.servers[[server_name]]$id <- current_id + 1
      current_id
    },
    
    server_as_mcpr_tools = function(server) {
      tools <- server$tools$tools
      tools_out <- list()
      
      for (i in seq_along(tools)) {
        tool <- tools[[i]]
        tool_arguments <- self$as_mcpr_types(tool)
        tools_out[[i]] <- ToolDef$new(
          fun = private$tool_ref(
            server = server$name,
            tool = tool$name,
            arguments = names(tool_arguments)
          ),
          description = tool$description,
          name = tool$name,
          arguments = tool_arguments
        )
      }
      
      tools_out
    },
    
    read_mcp_config = function(config_path) {
      # TODO: Add environment variable substitution
      if (!file.exists(config_path)) {
        private$error_no_mcp_config()
      }

      config_lines <- readLines(config_path)
      if (length(config_lines) == 0) {
        return(list())
      }

      tryCatch(
        {
          config <- jsonlite::fromJSON(config_lines)
        },
        error = function(e) {
          cli::cli_abort(
            c(
              "Configuration processing failed",
              i = "The configuration file {.arg config} must be valid JSON."
            ),
            parent = e
          )
        }
      )

      if (!"mcpServers" %in% names(config)) {
        cli::cli_abort(
          c(
            "Configuration processing failed.",
            i = "{.arg config} must have a top-level {.field mcpServers} entry."
          )
        )
      }

      config$mcpServers
    },
    
    error_no_mcp_config = function() {
      cli::cli_abort(
        c(
          "The mcptools MCP client configuration file does not exist.",
          i = "Supply a non-NULL file {.arg config} or create a file at the default \n               configuration location {.file {private$default_mcp_client_config()}}."
        )
      )
    },
    
    default_mcp_client_config = function() {
      file.path("~", ".config", "mcptools", "config.json")
    },
    
    
    finalize = function() {
      timeout_ms <- 5000  
      
      for (process in private$.server_processes) {
        if (process$is_alive()) {
          # Attempt graceful shutdown
          try(process$signal(tools::SIGTERM), silent = TRUE)
          
          # Wait with timeout, then force kill
          start_time <- Sys.time()
          while (process$is_alive() && 
                 as.numeric(difftime(Sys.time(), start_time, units = "secs")) < (timeout_ms/1000)) {
            Sys.sleep(0.1)
          }
          
          if (process$is_alive()) {
            process$kill()
          }
        }
      }
    }
  )
)

#' Create MCP tools (Backward Compatibility)
#'
#' @description
#' This function maintains backward compatibility with the existing functional API
#' while internally using the new mcpClient R6 class. It creates a client instance,
#' connects to servers, and returns MCPR-compatible tools.
#'
#' @param config Path to configuration file (character). Uses default location if NULL
#' @return List of MCPR tools
#' @examples
#' \dontrun{
#' # Legacy usage - creates client internally
#' tools <- mcp_tools()
#' tools <- mcp_tools(config = "config.json")
#' 
#' # Integration with MCPR workflows
#' tools <- mcp_tools()
#' # Use tools with MCPR framework
#' }
#' @export
mcp_tools <- function(config = NULL) {
  client <- mcpClient$new(config = config)
  
  client$connect_servers()
  tools <- client$get_mcpr_tools()
  
  return(tools)
}