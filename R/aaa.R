# Global State Container
# Dedicated environment for package-level MCPR state and shared registries.
# Keeps server/session state isolated from user workspaces while remaining package-wide.

#' @title Global State Container
#' @description Creates a dedicated environment for managing MCPR package state.
#' Provides centralized storage for server processes, tools registry, and
#' inter-process communication channels. Ensures state isolation from user
#' workspace while maintaining package-wide accessibility for session
#' management and tool execution coordination.
#' @noRd
the <- rlang::new_environment()

#' Server Process Registry
#'
#' @name server_processes
#' @description Initialize empty list for tracking active MCP server processes
the$server_processes <- list()

#' Daemon Session Registry
#'
#' @name daemon_sessions
#' @description Named integer vector tracking MCPR-owned secondary sessions.
#' Keys are internal secondary-session IDs, values are session socket IDs.
the$daemon_sessions <- integer(0)

#' Daemon Socket Registry
#'
#' @name daemon_sockets
#' @description Named list of nanonext sockets for MCPR-owned secondary sessions.
#' Keys are internal secondary-session IDs, values are nanonext socket objects.
the$daemon_sockets <- list()

#' Daemon Process Registry
#'
#' @name daemon_processes
#' @description Named list of processx process handles for secondary-session lifecycle management.
#' Keys are internal secondary-session IDs, values are processx::process objects.
the$daemon_processes <- list()

#' User Session Registry
#'
#' @name user_sessions
#' @description Named list of nanonext sockets for joined interactive user sessions.
#' Keys are session IDs (character), values are nanonext socket objects.
the$user_sessions <- list()
