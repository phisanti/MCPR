# MCPR Installation Utilities
# Functions for installing and configuring MCPR with various AI agents and IDEs.
# Handles cross-platform configuration file management with atomic operations and validation.

#' Install MCPR for AI Agents
#'
#' @title Install MCPR for AI Agents
#' @description Configures MCPR MCP server for specified AI agents with cross-platform support.
#' Automatically detects configuration file locations, safely modifies existing configurations,
#' and provides validation instructions. Supports Claude Code, Claude Desktop, GitHub Copilot,
#' and Gemini CLI with appropriate error handling and user permission requests.
#'
#' @param agent Character string specifying which agent to configure.
#'   Must be one of: "claude", "gemini", "copilot"
#' @param scope Character. For Claude Code only - configuration scope: "local", "project", or "user" (default: "local")
#' @param server_name Character. Name for the MCPR server in configuration (default: "mcpr")
#' @param force Logical. If TRUE, overwrites existing MCPR configuration without asking (default: FALSE)
#'
#' @return Invisible list with installation result for the specified agent
#' @export
#'
#' @examples
#' \dontrun{
#' # Install for Claude Code (local scope)
#' install_mcpr("claude")
#'
#' # Install for different agents (one at a time)
#' install_mcpr("gemini")
#' install_mcpr("copilot")
#'
#' # Install with custom server name
#' install_mcpr("claude", server_name = "my-r-server")
#'
#' # Force overwrite existing configuration
#' install_mcpr("gemini", force = TRUE)
#' }
install_mcpr <- function(agent = c("claude", "gemini", "copilot"),
                         scope = c("local", "project", "user"),
                         server_name = "mcpr",
                         force = FALSE) {
  # Validate inputs - only allow single agent
  if (length(agent) == 0) {
    cli::cli_abort(
      c(
        "No agent specified.",
        "i" = "Please specify one agent: {.val claude}, {.val gemini}, or {.val copilot}"
      )
    )
  }

  if (length(agent) > 1) {
    cli::cli_abort(
      c(
        "Multiple agents specified.",
        "x" = "Only one agent can be configured at a time.",
        "i" = "Please run {.fn install_mcpr} separately for each agent."
      )
    )
  }

  agent <- match.arg(agent, c("claude", "gemini", "copilot"))
  scope <- match.arg(scope)
  check_string(server_name, arg = "server_name")
  check_bool(force, arg = "force")

  # Configure the single agent directly
  cli::cli_alert_info("Configuring MCPR for {.val {agent}}...")

  result <- tryCatch(
    {
      agent_spec <- get_agent_specification(agent)

      # Handle scope parameter for Claude only
      config_result <- if (agent == "claude") {
        get_agent_config_path(agent, scope = scope)
      } else {
        get_agent_config_path(agent)
      }

      # Handle metadata key difference
      config_metadata <- if (agent == "copilot") {
        list(config_location = config_result$type)
      } else {
        list(config_type = config_result$type)
      }

      install_mcpr_unified(
        agent_name = agent,
        config_path = config_result$path,
        config_metadata = config_metadata,
        agent_spec = agent_spec,
        server_name = server_name,
        force = force
      )
    },
    error = function(e) {
      cli::cli_alert_danger("Failed to configure {.val {agent}}: {e$message}")
      list(success = FALSE, error = e$message)
    }
  )

  # Success messaging
  if (result$success) {
    cli::cli_alert_success("Successfully configured {.val {agent}}")
    if (!is.null(result$test_command)) {
      cli::cli_alert_info("Test with: {.code {result$test_command}}")
    }
    if (!is.null(result$restart_required) && result$restart_required) {
      cli::cli_alert_warning("Please restart {.val {agent}} to activate the configuration")
    }
    cli::cli_alert_info("You can now use MCPR in your R sessions with {.fn mcpr_session}")
  }

  invisible(result)
}


# Unified configuration system

#' Get Agent Specification
#' @param agent Agent name ("claude", "gemini", "copilot")
#' @return List with agent configuration details
#' @noRd
get_agent_specification <- function(agent) {
  base_server_config <- list(
    command = "R",
    args = c("--quiet", "--slave", "-e", "MCPR::mcpr_server()")
  )

  specs <- list(
    claude = list(
      server_section = "mcpServers",
      server_config = base_server_config,
      paths = list(
        desktop = list(
          Darwin = c(Sys.getenv("HOME"), "Library", "Application Support", "Claude", "claude_desktop_config.json"),
          Windows = c(Sys.getenv("APPDATA"), "Claude", "claude_desktop_config.json"),
          Linux = c(Sys.getenv("HOME"), ".config", "Claude", "claude_desktop_config.json")
        ),
        code_user = list(
          Darwin = c(Sys.getenv("HOME"), ".config", "claude", "mcp.json"),
          Windows = c(Sys.getenv("APPDATA"), "claude", "mcp.json"),
          Linux = c(Sys.getenv("HOME"), ".config", "claude", "mcp.json")
        ),
        code_local = ".mcp.json"
      ),
      test_commands = list(
        desktop = NULL,
        code = "claude mcp list"
      ),
      restart_required = list(
        desktop = TRUE,
        code = FALSE
      )
    ),
    gemini = list(
      server_section = "mcpServers",
      server_config = c(base_server_config, list(type = "stdio")),
      paths = list(
        main = c(Sys.getenv("HOME"), ".gemini", "settings.json")
      ),
      test_commands = list(main = "gemini --help"),
      restart_required = list(main = FALSE)
    ),
    copilot = list(
      server_section = "servers",
      server_config = c(base_server_config, list(type = "stdio")),
      paths = list(
        workspace = c(".vscode", "mcp.json"),
        user = list(
          Darwin = c(Sys.getenv("HOME"), ".config", "Code", "User", "mcp.json"),
          Windows = c(Sys.getenv("APPDATA"), "Code", "User", "mcp.json"),
          Linux = c(Sys.getenv("HOME"), ".config", "Code", "User", "mcp.json")
        )
      ),
      test_commands = list(workspace = NULL, user = NULL),
      restart_required = list(workspace = TRUE, user = TRUE)
    )
  )

  specs[[agent]]
}

#' Get Cross-Platform Path
#' @param path_spec Path specification (vector for simple path, list for cross-platform)
#' @return Character string with resolved file path
#' @noRd
get_cross_platform_path <- function(path_spec) {
  if (is.character(path_spec)) {
    # Simple path
    if (length(path_spec) == 1) {
      return(path_spec)
    } else {
      return(do.call(file.path, as.list(path_spec)))
    }
  } else if (is.list(path_spec)) {
    # Cross-platform path
    os <- Sys.info()[["sysname"]]
    if (!is.null(path_spec[[os]])) {
      return(do.call(file.path, as.list(path_spec[[os]])))
    } else {
      # Fallback to first available
      return(do.call(file.path, as.list(path_spec[[1]])))
    }
  }

  stop("Invalid path specification")
}


# Unified installation workflow

#' Unified Installation Workflow
#' @param agent_name Name of the agent being configured
#' @param config_path Path to configuration file
#' @param config_metadata Additional metadata about configuration
#' @param agent_spec Agent specification from get_agent_specification
#' @param server_name Name for server configuration
#' @param force Whether to overwrite existing config
#' @return List with installation result
#' @noRd
install_mcpr_unified <- function(agent_name, config_path, config_metadata, agent_spec,
                                 server_name, force) {
  # Create MCPR server configuration
  mcpr_config <- agent_spec$server_config
  server_section <- agent_spec$server_section

  # Read or create configuration
  config <- read_or_create_config(config_path, server_section)

  # Check for existing server and get user permission if needed
  if (server_exists(config, server_section, server_name) && !force) {
    if (!get_user_permission(server_name, agent_name, config_path)) {
      return(list(success = FALSE, error = "Installation cancelled by user"))
    }
  }

  # Add MCPR server configuration
  config[[server_section]][[server_name]] <- mcpr_config

  # Write configuration atomically
  write_json_config(config, config_path)

  # Determine test command and restart requirement
  config_type_key <- config_metadata$config_type %||%
    config_metadata$config_location %||%
    "main"

  test_command <- agent_spec$test_commands[[config_type_key]]
  restart_required <- agent_spec$restart_required[[config_type_key]]

  result <- list(
    success = TRUE,
    config_path = config_path,
    server_name = server_name,
    test_command = test_command,
    restart_required = restart_required
  )

  c(result, config_metadata)
}

#' Read or Create Configuration File
#' @param config_path Path to configuration file
#' @param server_section Name of server section ("mcpServers" or "servers")
#' @return Configuration list
#' @noRd
read_or_create_config <- function(config_path, server_section) {
  if (file.exists(config_path)) {
    config <- read_json_config(config_path)

    # Ensure server section exists
    if (is.null(config[[server_section]])) {
      config[[server_section]] <- list()
    }
  } else {
    # Create new configuration
    config <- list()
    config[[server_section]] <- list()

    # Create directory if it doesn't exist
    config_dir <- dirname(config_path)
    if (!dir.exists(config_dir)) {
      dir.create(config_dir, recursive = TRUE, mode = "0755")
    }
  }

  config
}

#' Check if Server Already Exists
#' @param config Configuration list
#' @param server_section Name of server section
#' @param server_name Name of server to check
#' @return Logical indicating if server exists
#' @noRd
server_exists <- function(config, server_section, server_name) {
  !is.null(config[[server_section]][[server_name]])
}

#' Get User Permission for Overwriting Server Configuration
#' @param server_name Name of server
#' @param agent_name Name of agent
#' @param config_path Path to configuration file
#' @return Logical indicating if user granted permission
#' @noRd
get_user_permission <- function(server_name, agent_name, config_path) {
  message_parts <- if (agent_name == "copilot") {
    c("Copilot", "configuration")
  } else if (agent_name == "gemini") {
    c("Gemini", "configuration")
  } else {
    c("", basename(config_path))
  }

  response <- readline(sprintf(
    "MCPR server '%s' already exists in %s %s. Overwrite? (y/N): ",
    server_name, message_parts[1], message_parts[2]
  ))

  grepl("^[Yy]", response)
}

# Configuration file I/O helpers

#' Read JSON Configuration File
#' @param path Path to JSON configuration file
#' @return List with configuration data
#' @noRd
read_json_config <- function(path) {
  tryCatch(
    {
      jsonlite::fromJSON(path, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to read configuration file: {.path {path}}",
          "x" = "JSON parsing error: {e$message}"
        )
      )
    }
  )
}

#' Write JSON Configuration File Atomically
#' @param config Configuration list to write
#' @param path Destination path for configuration file
#' @noRd
write_json_config <- function(config, path) {
  temp_path <- paste0(path, ".tmp")

  tryCatch(
    {
      # Write to temporary file first
      jsonlite::write_json(config, temp_path,
        pretty = TRUE,
        auto_unbox = TRUE
      )

      # Atomic move to final location
      file.rename(temp_path, path)
    },
    error = function(e) {
      # Clean up temp file on error
      if (file.exists(temp_path)) {
        unlink(temp_path)
      }

      cli::cli_abort(
        c(
          "Failed to write configuration file: {.path {path}}",
          "x" = "Error: {e$message}",
          "i" = "Check file permissions and disk space"
        )
      )
    }
  )
}

# Unified path function

#' Get Agent Configuration Path with Intelligent Defaults
#'
#' @param agent Agent name: "claude", "gemini", or "copilot"
#' @param scope For Claude only: "local", "project", or "user" scope (default: intelligent choice)
#' @return List with path (character) and type (character) indicating the chosen configuration
#' @noRd
get_agent_config_path <- function(agent, scope = NULL) {
  agent_spec <- get_agent_specification(agent)
  paths <- agent_spec$paths

  path_type <- "main" # Default type
  config_path <- NULL

  if (agent == "claude") {
    if (!is.null(scope) && scope == "user") {
      # Force user scope when explicitly requested
      config_path <- get_cross_platform_path(paths$code_user)
      path_type <- "code"
    } else {
      # Intelligent defaults: prioritize Desktop, then local
      desktop_path <- get_cross_platform_path(paths$desktop)
      if (file.exists(desktop_path)) {
        config_path <- desktop_path
        path_type <- "desktop"
      } else {
        # Use local project file by default
        config_path <- get_cross_platform_path(paths$code_local)
        path_type <- "code"
      }
    }
  } else if (agent == "copilot") {
    # For Copilot, prioritize a workspace-level config if in a VS Code project
    if (dir.exists(".vscode")) {
      config_path <- get_cross_platform_path(paths$workspace)
      path_type <- "workspace"
    } else {
      # Otherwise, use the global user config
      config_path <- get_cross_platform_path(paths$user)
      path_type <- "user"
    }
  } else if (agent == "gemini") {
    # Gemini has a single, straightforward path
    config_path <- get_cross_platform_path(paths$main)
    path_type <- "main"
  } else {
    stop("Internal error: Unsupported agent in get_agent_config_path")
  }

  list(path = config_path, type = path_type)
}
