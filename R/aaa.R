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

#' Plot Store
#'
#' @name plot_store
#' @description Environment of recorded plot pages (`recordedplot`) addressable
#' as `plot:N`; created lazily by `plot_store()`.
the$plot_store <- NULL

#' Owned Plot Device State
#'
#' @name plot_state
#' @description List (`mode`, `device`, `url`, `browser_opened`) for the single
#' plot device owned by an MCPR-owned session; NULL in other processes.
the$plot_state <- NULL

#' Session Activity Log
#'
#' @name activity_log
#' @description Bounded list of user console commands and agent code runs.
the$activity_log <- NULL

#' Plot Capture State
#'
#' @name plot_capture_state
#' @description `the$plot_capture_source` ("agent"/"user") labels pages recorded
#' during an execute_r_code capture, `the$plot_capture_touched` collects their
#' ids, and `the$plot_recording_suspended` disables recording for MCPR's own
#' renders. All are NULL outside a capture.
the$plot_capture_source <- NULL
the$plot_capture_touched <- NULL
the$plot_recording_suspended <- NULL

#' Joined-Session Display Device
#'
#' @name display_device
#' @description List (`device`, `url`) for the httpgd device MCPR opened in a
#' joined user session; NULL until the first user-facing plot.
the$display_device <- NULL
