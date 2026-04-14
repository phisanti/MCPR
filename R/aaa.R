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
#' @description Named integer vector tracking agent-owned daemon sessions.
#' Keys are client IDs, values are session IDs (nanonext socket port numbers).
the$daemon_sessions <- integer(0)

#' Daemon Socket Registry
#'
#' @name daemon_sockets
#' @description Named list of nanonext sockets for daemon communication.
#' Keys are client IDs, values are nanonext socket objects.
the$daemon_sockets <- list()

#' Daemon Process Registry
#'
#' @name daemon_processes
#' @description Named list of processx process handles for daemon lifecycle management.
#' Keys are client IDs, values are processx::process objects.
the$daemon_processes <- list()
