#' Global Environment for MCPR State Management
#'
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
