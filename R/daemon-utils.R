# Daemon Session Utilities
# Registry functions for managing agent-owned background R sessions.
# Provides spawn, register, unregister, and lookup for daemon lifecycle management.

#' Register a daemon session
#'
#' @description Adds a client_id -> session_id mapping to the daemon registry.
#' @param client_id Character. Identifies the agent or client owning this daemon.
#' @param session_id Integer. The nanonext socket port number for this daemon.
#' @return Called for side effects; returns NULL invisibly.
#' @noRd
register_daemon <- function(client_id, session_id) {
  the$daemon_sessions[[client_id]] <- as.integer(session_id)
}

#' Unregister a daemon session
#'
#' @description Removes a client from all three daemon registries, closes the
#' nanonext socket, and kills the process if it is still alive. Safe to call
#' on an unknown client_id (no-op).
#' @param client_id Character. The client to remove.
#' @return Called for side effects; returns NULL invisibly.
#' @noRd
unregister_daemon <- function(client_id) {
  # Remove from session registry
  the$daemon_sessions <- the$daemon_sessions[names(the$daemon_sessions) != client_id]

  # Close socket if exists
  sock <- the$daemon_sockets[[client_id]]
  if (!is.null(sock)) {
    tryCatch(nanonext::reap(sock), error = function(e) NULL)
  }
  the$daemon_sockets[[client_id]] <- NULL

  # Kill process if alive
  proc <- the$daemon_processes[[client_id]]
  if (!is.null(proc) && inherits(proc, "process") && proc$is_alive()) {
    tryCatch(proc$kill(), error = function(e) NULL)
  }
  the$daemon_processes[[client_id]] <- NULL
}

#' Look up a daemon session ID by client ID
#'
#' @description Returns the session ID (socket port number) for a registered daemon, or NULL.
#' @param client_id Character. The client to look up.
#' @return Integer(1) or NULL.
#' @noRd
get_daemon_session <- function(client_id) {
  if (client_id %in% names(the$daemon_sessions)) {
    the$daemon_sessions[[client_id]]
  } else {
    NULL
  }
}

#' List all registered daemon sessions
#'
#' @description Returns the full daemon session registry as a named integer vector.
#' @return Named integer vector (client_id -> session_id). May be length-0.
#' @noRd
list_daemon_sessions <- function() {
  the$daemon_sessions
}

#' Build a process label for a daemon session
#'
#' @description Returns "MCPR-{session_id}" for use in Activity Monitor / ps output.
#' @param session_id Integer. The session port number.
#' @return Character(1).
#' @noRd
daemon_process_label <- function(session_id) {
  sprintf("MCPR-%d", as.integer(session_id))
}

#' Find an available daemon socket port
#'
#' @description Scans ports 1..1023 via nanonext::listen to find the first
#' unoccupied slot. Creates and tears down a temporary socket to avoid
#' interfering with existing listeners.
#' @return Integer(1) port number.
#' @noRd
find_daemon_port <- function() {
  socket_base <- get_system_socket_url()
  sock <- nanonext::socket("poly")
  on.exit(nanonext::reap(sock), add = TRUE)

  i <- 1L
  while (i < 1024L) {
    if (nanonext::listen(sock, url = sprintf("%s%d", socket_base, i), fail = "none")) {
      i <- i + 1L
    } else {
      return(i)
    }
  }
  cli::cli_abort("No available socket ports found for daemon session.")
}

#' Spawn a daemon R session
#'
#' @description Launches a new R process running MCPR::mcp_session() in daemon mode.
#' Uses Rscript (not R) so the process exits on error instead of dropping to
#' an interactive prompt. Stores the process handle in the$daemon_processes.
#' @param client_id Character. The owning client identifier.
#' @param session_id Integer. The port number the daemon will listen on.
#' @param working_dir Character. Working directory for the daemon process.
#' @return The processx::process object.
#' @noRd
spawn_daemon <- function(client_id, session_id, working_dir = getwd()) {
  rscript <- the$rscript_path %||% file.path(R.home("bin"), "Rscript")
  r_expr <- sprintf(
    'MCPR::mcp_session(session_id = %dL, working_dir = "%s", daemon = TRUE)',
    as.integer(session_id), working_dir
  )

  proc <- processx::process$new(
    command = rscript,
    args = c("-e", r_expr),
    stdin = NULL,
    stdout = "|",
    stderr = "|",
    env = c(
      "current",
      MCPR_DAEMON = "true",
      MCPR_CLIENT_ID = client_id,
      MCPR_PROCESS_LABEL = daemon_process_label(session_id)
    )
  )

  the$daemon_processes[[client_id]] <- proc
  proc
}

#' Wait for a daemon session to become connectable
#'
#' @description Polls the daemon's nanonext socket URL until a connection is
#' established or the timeout is reached. Returns a connected poly socket on
#' success, or NULL on timeout.
#'
#' Note: IPC sockets don't support async reconnection (unlike TCP), so the
#' mirai pipe_notify + until pattern cannot be used here. Our architecture
#' requires the server to dial out to the daemon's listener, which must be
#' polled. The mirai pattern works because mirai uses reverse-dial (daemon
#' dials into host).
#'
#' @param session_id Integer. The port number to dial.
#' @param timeout_ms Numeric. Milliseconds to wait before giving up (default 15000).
#' @return A connected nanonext poly socket, or NULL on timeout.
#' @noRd
await_daemon_ready <- function(session_id, timeout_ms = 15000) {
  url <- sprintf("%s%d", get_system_socket_url(), as.integer(session_id))

  start_time <- proc.time()[["elapsed"]]
  timeout_s <- timeout_ms / 1000

  repeat {
    sock <- nanonext::socket("poly")
    result <- nanonext::dial(sock, url = url, fail = "none")
    if (!result) {
      # Give the pipe a moment to establish after successful dial
      Sys.sleep(0.2)
      if (nanonext::stat(sock, "pipes") > 0L) {
        return(sock)
      }
    }
    nanonext::reap(sock)

    elapsed <- proc.time()[["elapsed"]] - start_time
    if (elapsed >= timeout_s) return(NULL)

    Sys.sleep(0.5)
  }
}
