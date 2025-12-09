# MCP Client Implementation
# Provides R6 class for connecting to MCP servers with tool discovery and execution.
# Handles JSON-RPC communication, session management, and server interaction.

#' @title MCP Client
#' @description
#' Persistent interface for managing Model Context Protocol servers within R sessions.
#' • **Server connections**: Establishes and maintains connections to MCP servers
#' • **Tool discovery**: Automatically discovers and registers available tools
#' • **Protocol communication**: Handles JSON-RPC messaging with servers
#' • **State management**: Maintains server status and tool metadata
#' • **Configuration support**: Uses JSON config files for server specifications
#'
#' @param config Path to configuration file (uses default location if NULL)
#' @return New mcprClient instance
#' @export
mcprClient <- R6::R6Class("mcprClient",
  inherit = BaseMCPR,
  public = list(
    #' @description Creates new mcprClient instance
    #' @param config Path to configuration file (character). Uses default location if NULL
    #' @return Self (invisible)
    initialize = function(config = NULL) {
      self$initialize_base("CLIENT")

      private$.servers <- list()
      private$.server_processes <- list()

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

    #' Establish connections to all configured MCP servers
    #' @return Self (invisible, enables method chaining)
    connect_servers = function() {
      if (is.null(private$.config) || length(private$.config) == 0) {
        private$log_warn("No servers configured")
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

    #' Retrieve all discovered tools in MCPR-compatible format
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

    #' Execute specific tool on designated server
    #' @param ... Named arguments for the tool
    #' @param server Server name (character)
    #' @param tool Tool name (character)
    #' @return Tool execution result
    call_tool = function(..., server, tool) {
      server_process <- private$.servers[[server]]$process
      private$send_and_receive(
        server_process,
        private$mcp_request_tool_call(
          id = private$jsonrpc_id(server),
          tool = tool,
          arguments = list(...)
        )
      )
    },

    #' Retrieve status information for all connected servers
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

    # Send JSON-RPC message and receive response from process
    send_and_receive = function(process, message) {
      json_msg <- jsonlite::toJSON(message, auto_unbox = TRUE)
      private$log_comm("FROM CLIENT", json_msg)
      process$write_input(paste0(json_msg, "\n"))

      output <- NULL
      attempts <- 0
      max_attempts <- 20

      while (length(output) == 0 && attempts < max_attempts) {
        Sys.sleep(0.2)
        output <- process$read_output_lines()
        attempts <- attempts + 1
      }

      if (!is.null(output) && length(output) > 0) {
        private$log_comm("FROM SERVER", output[1])
        return(jsonlite::parse_json(output[1]))
      }

      private$log_warn(paste("No response received after", attempts, "attempts"))
      return(NULL)
    },

    # Initialize and register MCP server with handshake and tool discovery
    add_mcp_server = function(process, name) {
      response_initialize <- private$send_and_receive(process, private$mcp_request_initialize())
      response_tools_list <- private$send_and_receive(process, private$mcp_request_tools_list())

      private$.servers[[name]] <- list(
        name = name,
        process = process,
        tools = response_tools_list$result,
        id = 3
      )

      private$.servers[[name]]
    },

    # Create function reference for remote tool execution
    tool_ref = function(server, tool, arguments) {
      f <- function() {}
      formals(f) <- stats::setNames(
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

    # Create JSON-RPC initialize request message
    mcp_request_initialize = function() {
      protocol_version <- max(SUPPORTED_VERSIONS)
      list(
        jsonrpc = "2.0",
        id = 1,
        method = "initialize",
        params = list(
          protocolVersion = protocol_version,
          capabilities = create_client_capabilities(protocol_version),
          clientInfo = list(
            name = "MCPR Client",
            version = "1.0.0"
          )
        )
      )
    },

    # Create JSON-RPC tools list request message
    mcp_request_tools_list = function() {
      list(
        jsonrpc = "2.0",
        id = 2,
        method = "tools/list"
      )
    },

    # Create JSON-RPC tool call request message
    mcp_request_tool_call = function(id, tool, arguments) {
      if (length(arguments) == 0) {
        params <- list(name = tool)
      } else {
        params <- list(
          name = tool,
          arguments = arguments
        )
      }
      list(
        jsonrpc = "2.0",
        id = id,
        method = "tools/call",
        params = params
      )
    },

    # Generate and increment JSON-RPC ID for server
    jsonrpc_id = function(server_name) {
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

    # Read and parse MCP configuration file
    read_mcp_config = function(config_path) {
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
          error_msg <- "Configuration processing failed: The configuration file must be valid JSON."
          private$log_error(paste(error_msg, "Error:", e$message))
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
        error_msg <- "Configuration processing failed: config must have a top-level mcpServers entry."
        private$log_error(error_msg)
        cli::cli_abort(
          c(
            "Configuration processing failed.",
            i = "{.arg config} must have a top-level {.field mcpServers} entry."
          )
        )
      }

      config$mcpServers
    },

    # Throw error when MCP configuration file is missing
    error_no_mcp_config = function() {
      error_msg <- "The mcptools MCP client configuration file does not exist."
      private$log_error(error_msg)
      cli::cli_abort(
        c(
          "The mcptools MCP client configuration file does not exist.",
          i = "Supply a non-NULL file {.arg config} or create a file at the default \n               configuration location {.file {private$default_mcp_client_config()}}."
        )
      )
    },

    # Get default MCP client configuration file path
    default_mcp_client_config = function() {
      file.path("~", ".config", "mcptools", "config.json")
    },

    # Gracefully terminate server processes with timeout-based cleanup
    finalize = function() {
      timeout_ms <- 5000

      for (process in private$.server_processes) {
        if (process$is_alive()) {
          # Attempt graceful shutdown with SIGTERM
          try(process$signal(tools::SIGTERM), silent = TRUE)

          # Wait with timeout, then force kill if still alive
          start_time <- Sys.time()
          while (process$is_alive() &&
            as.numeric(difftime(Sys.time(), start_time, units = "secs")) < (timeout_ms / 1000)) {
            Sys.sleep(0.1)
          }

          # Force kill if still running after timeout
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

#' Create MCP Tools (Updated to ToolDef)
#'
#' @description
#' Creates MCP tools using the modern ToolDef system.
#' • **Client management**: Creates client instance and connects to servers
#' • **Tool discovery**: Automatically discovers and registers available tools
#' • **ToolDef objects**: Returns modern ToolDef objects instead of legacy formats
#'
#' @param config Path to configuration file (uses default location if NULL)
#' @return List of ToolDef objects
#' @export
mcpr_tools <- function(config = NULL) {
  client <- mcprClient$new(config = config)

  client$connect_servers()
  tools <- client$get_mcpr_tools()
  return(tools)
}
