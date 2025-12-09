# MCPR Installation Utilities
# Functions for installing and configuring MCPR with various AI agents and IDEs.
# Handles cross-platform configuration file management with atomic operations and validation.

#' Install MCPR for AI Agents
#'
#' @title Install MCPR for AI Agents
#' @description Configures MCPR MCP server for specified AI agents with cross-platform support.
#' Automatically detects configuration file locations, safely modifies existing configurations,
#' and provides validation instructions. Supports Claude Code, Claude Desktop, GitHub Copilot,
#' Gemini CLI, and OpenAI Codex (TOML) with appropriate error handling and user permission requests.
#'
#' @param agent Character string specifying which agent to configure.
#'   Must be one of: "claude", "gemini", "copilot", "codex". If NULL, displays helpful usage guide.
#' @param scope Character. Configuration scope for the agent. If NULL, uses the recommended default.
#'   For Claude: "local" (desktop), "project" (.mcp.json), "user" (code config).
#'   For Copilot: "workspace" (.vscode/mcp.json), "user" (profile config), "remote" (remote config).
#'   For Gemini: "global" (settings.json), "project" (extensions), "ide" (IDE-specific config).
#'   For Codex: "global" (~/.codex/config.toml).
#' @param server_name Character. Name for the MCPR server in configuration (default: "mcpr")
#' @param force Logical. If TRUE, overwrites existing MCPR configuration without asking (default: FALSE)
#'
#' @return Invisible list with installation result for the specified agent
#'
#' @examples
#' \dontrun{
#' # Install for Claude Code (local scope)
#' install_mcpr("claude")
#'
#' # Install for different agents (one at a time)
#' install_mcpr("gemini")
#' install_mcpr("copilot")
#' install_mcpr("codex")
#'
#' # Install with custom server name
#' install_mcpr("claude", server_name = "my-r-server")
#'
#' # Force overwrite existing configuration
#' install_mcpr("gemini", force = TRUE)
#' }
#' @export
install_mcpr <- function(agent = NULL,
                         scope = NULL,
                         server_name = "mcpr",
                         force = FALSE) {
  # Agent validation
  if (is.null(agent) || length(agent) == 0) {
    cli::cli_abort(
      c(
        "!" = "No agent specified.",
        " " = "",
        "i" = "MCPR can be configured for the following AI agents:",
        "*" = "{.strong claude} - Claude Desktop or Claude Code",
        "*" = "{.strong gemini} - Google Gemini CLI",
        "*" = "{.strong copilot} - GitHub Copilot in VS Code",
        "*" = "{.strong codex} - OpenAI Codex",
        " " = "",
        "i" = "Choose your agent:",
        "*" = "For Claude Desktop: {.code install_mcpr(\"claude\")}",
        "*" = "For Claude Code (project): {.code install_mcpr(\"claude\", scope = \"project\")}",
        "*" = "For Claude Code (user): {.code install_mcpr(\"claude\", scope = \"user\")}",
        "*" = "For Gemini CLI: {.code install_mcpr(\"gemini\")}",
        "*" = "For GitHub Copilot: {.code install_mcpr(\"copilot\")}",
        "*" = "For OpenAI Codex: {.code install_mcpr(\"codex\")}"
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

  if (!agent %in% c("claude", "gemini", "copilot", "codex")) {
    # Handle invalid agent
    cli::cli_abort(
      c(
        "!" = "Invalid agent: {.val {agent}}",
        "i" = "Only {.val claude}, {.val gemini}, {.val copilot}, and {.val codex} are supported."
      )
    )
  }

  # Handle scope validation and defaults for all agents
  if (!is.null(scope)) {
    valid_scopes <- switch(agent,
      "claude" = c("local", "project", "user"),
      "copilot" = c("workspace", "user", "remote"),
      "gemini" = c("global", "project", "ide", "local"),
      "codex" = c("global"),
      stop("Internal error: unknown agent in scope validation")
    )

    if (!scope %in% valid_scopes) {
      scope_guidance <- switch(agent,
        "claude" = c(
          "i" = "For Claude, scope determines the configuration file location:",
          "*" = "{.strong local} (default) - Claude Desktop config for general use",
          "*" = "{.strong project} - Local .mcp.json file for this project only",
          "*" = "{.strong user} - Claude Code user config (~/.config/claude/)"
        ),
        "copilot" = c(
          "i" = "For Copilot, scope determines VS Code configuration location:",
          "*" = "{.strong workspace} (default) - VS Code workspace config (.vscode/mcp.json)",
          "*" = "{.strong user} - VS Code user profile config (global to all workspaces)",
          "*" = "{.strong remote} - Remote development server config"
        ),
        "gemini" = c(
          "i" = "For Gemini, scope determines configuration location:",
          "*" = "{.strong global} (default) - Global settings (~/.gemini/settings.json)",
          "*" = "{.strong local} - Local project settings (./.gemini/settings.json)",
          "*" = "{.strong project} - Project extension (~/.gemini/extensions/mcpr/)",
          "*" = "{.strong ide} - IDE-specific config (IntelliJ mcp.json)"
        ),
        "codex" = c(
          "i" = "For Codex, scope determines configuration location:",
          "*" = "{.strong global} (default) - Global settings (~/.codex/config.toml)"
        )
      )

      cli::cli_abort(c(
        "!" = "Invalid scope for {.strong {agent}}: {.val {scope}}",
        " " = "",
        scope_guidance
      ))
    }
  }

  # Set default scopes for each agent if not specified
  if (is.null(scope)) {
    scope <- switch(agent,
      "claude" = "local", # Claude Desktop
      "copilot" = "workspace", # VS Code workspace
      "gemini" = "global", # Global settings
      "codex" = "global", # Codex global settings
      "local" # fallback
    )
  }
  check_string(server_name, arg = "server_name")
  check_bool(force, arg = "force")

  # Configure the single agent with scope-aware messaging
  scope_msg <- switch(paste(agent, scope, sep = "_"),
    # Claude scopes
    "claude_local" = "Claude Desktop configuration",
    "claude_project" = "local project configuration (.mcp.json)",
    "claude_user" = "Claude Code user configuration",
    # Copilot scopes
    "copilot_workspace" = "VS Code workspace configuration (.vscode/mcp.json)",
    "copilot_user" = "VS Code user profile configuration",
    "copilot_remote" = "remote development configuration",
    # Gemini scopes
    "gemini_global" = "global settings configuration (~/.gemini/settings.json)",
    "gemini_local" = "local project settings configuration (./.gemini/settings.json)",
    "gemini_project" = "project extension configuration",
    "gemini_ide" = "IDE-specific configuration",
    # Codex scopes
    "codex_global" = "global settings configuration (~/.codex/config.toml)",
    # Fallback
    paste(scope, "configuration")
  )

  cli::cli_alert_info("Configuring MCPR for {.strong {agent}} using {scope_msg}...")

  result <- tryCatch(
    {
      agent_spec <- get_agent_specification(agent)

      # Get agent configuration path with scope
      config_result <- get_agent_config_path(agent, scope = scope)

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
        force = force,
        scope = scope
      )
    },
    error = function(e) {
      cli::cli_alert_danger("Failed to configure {.val {agent}}: {e$message}")
      list(success = FALSE, error = e$message)
    }
  )

  # Success messaging with configuration details
  if (result$success) {
    cli::cli_alert_success("Successfully configured {.strong {agent}} MCPR server!")

    # Show configuration file path
    cli::cli_alert_info("Configuration saved to: {.path {result$config_path}}")

    if (!is.null(result$test_command)) {
      cli::cli_alert_info("Test installation: {.code {result$test_command}}")
    }

    if (!is.null(result$restart_required) && result$restart_required) {
      cli::cli_alert_warning("Please restart {.strong {agent}} to activate the configuration")
    }

    cli::cli_rule()
    cli::cli_alert_info("Next steps:")
    cli::cli_bullets(c(
      "*" = "Start an R session where you want to collaborate with AI",
      "*" = "Run {.code mcpr_session_start()} to make the session discoverable",
      "*" = "Your AI agent can now execute R code and access your workspace!"
    ))
  }

  invisible(result)
}


# Unified configuration system

#' Get Agent Specification
#' @param agent Agent name ("claude", "gemini", "copilot", "codex")
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
      config_format = "json",
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
      config_format = "json",
      server_config = list(
        command = "Rscript",
        args = c("-e", "MCPR::mcpr_server()"),
        description = "R statistical computing and data analysis server"
      ),
      paths = list(
        global = c(Sys.getenv("HOME"), ".gemini", "settings.json"),
        local = c(".", ".gemini", "settings.json"),
        project = c(Sys.getenv("HOME"), ".gemini", "extensions", "mcpr", "gemini-extension.json"),
        ide = c(Sys.getenv("HOME"), ".config", "JetBrains", "mcp.json")
      ),
      test_commands = list(global = "gemini --help", local = "gemini --help", project = "gemini --help", ide = NULL),
      restart_required = list(global = FALSE, local = FALSE, project = FALSE, ide = TRUE),
      extension_metadata = list(
        name = "mcpr",
        version = "1.0.0"
      )
    ),
    copilot = list(
      server_section = "mcpServers",
      config_format = "json",
      server_config = list(
        workspace = base_server_config,
        user = base_server_config,
        remote = base_server_config
      ),
      paths = list(
        workspace = c(".vscode", "mcp.json"),
        user = list(
          Darwin = c(Sys.getenv("HOME"), ".config", "Code", "User", "mcp.json"),
          Windows = c(Sys.getenv("APPDATA"), "Code", "User", "mcp.json"),
          Linux = c(Sys.getenv("HOME"), ".config", "Code", "User", "mcp.json")
        ),
        remote = c(Sys.getenv("HOME"), "mcp.json") # Simplified remote path
      ),
      test_commands = list(workspace = "code --help", user = "code --help", remote = NULL),
      restart_required = list(workspace = FALSE, user = FALSE, remote = TRUE)
    ),
    codex = list(
      server_section = "mcp",
      config_format = "toml",
      server_config = base_server_config,
      paths = list(
        global = c(Sys.getenv("HOME"), ".codex", "config.toml")
      ),
      test_commands = list(global = "codex mcp list"),
      restart_required = list(global = FALSE)
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
                                 server_name, force, scope = NULL) {
  config_format <- agent_spec$config_format %||% "json"
  # Get server configuration
  if (is.list(agent_spec$server_config) && "command" %in% names(agent_spec$server_config)) {
    # Direct configuration structure (e.g., claude, gemini with command + args)
    mcpr_config <- agent_spec$server_config
  } else if (is.list(agent_spec$server_config) && !is.null(scope) && scope %in% names(agent_spec$server_config)) {
    # Scope-based configuration (e.g., copilot with workspace/user/remote)
    mcpr_config <- agent_spec$server_config[[scope]]
  } else if (!is.list(agent_spec$server_config)) {
    # Non-list configuration (shouldn't happen with current implementation)
    mcpr_config <- agent_spec$server_config
  } else {
    # Fallback: try to get a reasonable configuration
    if (length(agent_spec$server_config) > 0) {
      mcpr_config <- agent_spec$server_config[[1]]
    } else {
      stop("No valid server configuration found for agent: ", agent_name)
    }
  }

  server_section <- agent_spec$server_section

  # Read or create configuration
  config <- read_or_create_config(config_path, server_section, format = config_format)

  # Check for existing server and get user permission if needed
  if (server_exists(config, server_section, server_name) && !force) {
    if (!get_user_permission(server_name, agent_name, config_path)) {
      return(list(success = FALSE, error = "Installation cancelled by user"))
    }
  }

  # Handle Gemini extension format (only for project scope)
  if (agent_name == "gemini" && scope == "project" && !is.null(agent_spec$extension_metadata)) {
    config[["name"]] <- agent_spec$extension_metadata$name
    config[["version"]] <- agent_spec$extension_metadata$version
  }

  # Add MCPR server configuration
  config[[server_section]][[server_name]] <- mcpr_config

  # Write configuration atomically
  write_config(config, config_path, format = config_format)

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
#' @param format Configuration format ("json" or "toml")
#' @return Configuration list
#' @noRd
read_or_create_config <- function(config_path, server_section, format = "json") {
  if (file.exists(config_path)) {
    config <- switch(format,
      "json" = read_json_config(config_path),
      "toml" = read_toml_config(config_path),
      stop("Unsupported config format: ", format)
    )

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

#' Write Configuration File Atomically
#' @param config Configuration list to write
#' @param path Destination path for configuration file
#' @param format Configuration format ("json" or "toml")
#' @noRd
write_config <- function(config, path, format = "json") {
  switch(format,
    "json" = write_json_config(config, path),
    "toml" = write_toml_config(config, path),
    stop("Unsupported config format: ", format)
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
#' @param agent Agent name: "claude", "gemini", "copilot", or "codex"
#' @param scope Scope for the agent (default handled by install_mcpr)
#' @return List with path (character) and type (character) indicating the chosen configuration
#' @noRd
get_agent_config_path <- function(agent, scope = NULL) {
  agent_spec <- get_agent_specification(agent)
  paths <- agent_spec$paths

  # Use scope directly as the path type for all agents
  if (agent == "claude") {
    config_path <- switch(scope,
      "local" = get_cross_platform_path(paths$desktop),
      "project" = get_cross_platform_path(paths$code_local),
      "user" = get_cross_platform_path(paths$code_user),
      stop("Invalid Claude scope: ", scope)
    )
    path_type <- if (scope == "local") "desktop" else "code"
  } else if (agent == "copilot") {
    config_path <- switch(scope,
      "workspace" = get_cross_platform_path(paths$workspace),
      "user" = get_cross_platform_path(paths$user),
      "remote" = get_cross_platform_path(paths$remote),
      stop("Invalid Copilot scope: ", scope)
    )
    path_type <- scope
  } else if (agent == "gemini") {
    config_path <- switch(scope,
      "global" = get_cross_platform_path(paths$global),
      "local" = get_cross_platform_path(paths$local),
      "project" = get_cross_platform_path(paths$project),
      "ide" = get_cross_platform_path(paths$ide),
      stop("Invalid Gemini scope: ", scope)
    )
    path_type <- scope
  } else if (agent == "codex") {
    config_path <- switch(scope,
      "global" = get_cross_platform_path(paths$global),
      stop("Invalid Codex scope: ", scope)
    )
    path_type <- scope
  } else {
    stop("Internal error: Unsupported agent in get_agent_config_path")
  }

  list(path = config_path, type = path_type)
}
