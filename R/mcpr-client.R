# MCP Client Implementation
# Provides R6 class for connecting to MCP servers with tool discovery and execution.
# Handles JSON-RPC communication, session management, and server interaction.

#' MCP Client for R Sessions
#'
#' @title MCP Client
#' @description Provides persistent interface for managing Model Context Protocol servers.
#' Establishes connections through JSON-RPC messaging, discovers available tools, and
#' maintains server state. Enables AI assistants to interact with R sessions beyond
#' stateless script execution through centralized protocol communication and tool
#' registry management.
#' @details Creates dedicated environment for MCP server lifecycle management:
#' \itemize{
#'   \item \strong{Server Connections}: Establishes and maintains connections to MCP servers
#'   \item \strong{Tool Discovery}: Automatically discovers and registers available tools
#'   \item \strong{Protocol Communication}: Handles JSON-RPC messaging with servers
#'   \item \strong{State Management}: Maintains server status and tool metadata
#' }
#'
#' @param config Path to JSON configuration file containing mcpServers definitions
#' @examples
#' \dontrun{
#' # Basic usage
#' client <- mcprClient$new()
#' client$connect_servers()
#' tools <- client$get_mcpr_tools()
#'
#' # With custom configuration
#' client <- mcprClient$new(config = "path/to/config.json")
#' client$connect_servers()
#'
#' # Method chaining
#' client <- mcprClient$new()$connect_servers()
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
mcprClient <- R6::R6Class("mcprClient",
  public = list(
#' @description Creates new mcprClient instance
    #' @param config Path to configuration file (character). Uses default location if NULL
    #' @return Self (invisible)
    initialize = function(config = NULL) {
      private$.servers <- list()
      private$.server_processes <- list()
      private$.messenger <- MessageHandler$new(logger = private$log_communication)
      
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
    
#' @description Establishes connections to all configured MCP servers
    #' @return Self (invisible, enables method chaining)
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

        private$add_mcpr_server(process = process, name = name_i)
      }
      
      invisible(self)
    },
    
#' @description Retrieves all discovered tools in MCPR-compatible format
    #' @return List of MCPR tool objects
    get_mcpr_tools = function() {
      if (length(private$.servers) == 0) {
        self$connect_servers()
      }
      
      unname(unlist(
        lapply(private$.servers, private$server_as_mcpr_tools),
        recursive = FALSE
      ))
    },
    
#' @description Executes specific tool on designated server
    #' @param ... Named arguments for the tool
    #' @param server Server name (character)
    #' @param tool Tool name (character)
    #' @return Tool execution result
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
    
#' @description Retrieves status information for all connected servers
    #' @return Named list with server status details including name, connected status, tools_count, and last_id
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

    #' @description Convert tool schema to MCPR types
    #' @param tool Tool definition object
    #' @return Converted tool types
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
    
    
    # Validates server process and performs MCP initialization handshake
    add_mcpr_server = function(process, name) {
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
    
    # Creates function wrapper for tool calls with dynamic argument binding
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
    
    # Generates sequential request IDs for JSON-RPC protocol compliance
    next_id = function(server_name) {
      # Simple ID management - increment and return
      current_id <- private$.servers[[server_name]]$id
      private$.servers[[server_name]]$id <- current_id + 1
      current_id
    },
    
    # Converts MCP server tools to MCPR-compatible ToolDef objects
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
    
    # Parses JSON configuration file and validates mcpServers structure
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
    
    # Provides helpful error message for missing configuration files
    error_no_mcp_config = function() {
      cli::cli_abort(
        c(
          "The mcptools MCP client configuration file does not exist.",
          i = "Supply a non-NULL file {.arg config} or create a file at the default \n               configuration location {.file {private$default_mcp_client_config()}}."
        )
      )
    },
    
    # Returns standard configuration file location following XDG specification
    default_mcp_client_config = function() {
      file.path("~", ".config", "mcptools", "config.json")
    },
    
    
    # Gracefully terminates server processes with timeout-based cleanup
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
    },
    
    # Logs JSON-RPC communication for protocol debugging and troubleshooting
    log_communication = function(message) {
      log_file <- "~/mcp_client_test.txt"
      cat(message, "\n\n", sep = "", append = TRUE, file = log_file)
    }
  )
)

#' Create MCP Tools from Configuration
#'
#' @title MCP Tools Factory Function
#' @description Creates client instance, establishes server connections, and returns
#' MCPR-compatible tools. Provides functional interface over class implementation
#' for backward compatibility. Internally manages client lifecycle and connection
#' state through automated server discovery and tool registration.
#'
#' @param config Path to configuration file (character). Uses default location if NULL
#' @return List of MCPR tools
#' @export
mcpr_tools <- function(config = NULL) {
  client <- mcprClient$new(config = config)
  
  client$connect_servers()
  tools <- client$get_mcpr_tools()
  
  return(tools)
}