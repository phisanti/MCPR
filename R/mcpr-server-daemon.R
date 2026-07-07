# MCP Server Secondary Session Transport
# Server-side helpers for MCPR-owned secondary sessions and joined user sockets.
# Used by mcprServer to spawn, connect, register, and clean attached session transports.

#' Register a secondary session
#'
#' @description Adds a key -> session_id mapping to the legacy secondary-session registry.
#' @param client_id Character. Internal registry key for this secondary session.
#' @param session_id Integer. The nanonext socket port number for this secondary session.
#' @return Called for side effects; returns NULL invisibly.
#' @noRd
register_daemon <- function(client_id, session_id) {
  the$daemon_sessions[[client_id]] <- as.integer(session_id)
}

#' Unregister a secondary session
#'
#' @description Removes a key from all three legacy secondary-session registries, closes the
#' nanonext socket, and kills the process if it is still alive. Safe to call
#' on an unknown registry key (no-op).
#' @param client_id Character. The internal registry key to remove.
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

#' Build a process label for a secondary session
#'
#' @description Returns "MCPR-{session_id}" for use in Activity Monitor / ps output.
#' @param session_id Integer. The session port number.
#' @return Character(1).
#' @noRd
daemon_process_label <- function(session_id) {
  sprintf("MCPR-%d", as.integer(session_id))
}

#' Find an available secondary-session socket port
#'
#' @description Scans ports 2..1023 via nanonext::listen to find the first
#' unoccupied slot. Port 1 is reserved for user session connections.
#' Creates and tears down a temporary socket to probe availability without
#' interfering with existing listeners.
#' @return Integer(1) port number.
#' @noRd
find_daemon_port <- function() {
  socket_base <- get_system_socket_url()
  sock <- nanonext::socket("poly")
  on.exit(nanonext::reap(sock), add = TRUE)

  # Start at 2: port 1 is pre-dialed by the server for user session connections
  i <- 2L
  while (i < 1024L) {
    if (nanonext::listen(sock, url = sprintf("%s%d", socket_base, i), fail = "none")) {
      i <- i + 1L
    } else {
      return(i)
    }
  }
  cli::cli_abort("No available socket ports found for secondary session.")
}

#' Spawn a secondary R session
#'
#' @description Launches a new R process running MCPR::mcp_session() in secondary mode.
#' Uses Rscript with --no-init-file --no-site-file to avoid .Rprofile/renv delays.
#' Stores the process handle in the$daemon_processes.
#' @param client_id Character. Internal registry key for this secondary session.
#' @param session_id Integer. The port number the secondary session will listen on.
#' @param working_dir Character. Working directory for the secondary process.
#' @return The processx::process object.
#' @noRd
# Spawned secondary sessions stay owned by the daemon registries until cleanup.
# The process handle and session key are tracked together so teardown can unwind
# them without guessing which resources were created.
spawn_daemon <- function(client_id, session_id, working_dir = getwd()) {
  rscript <- the$rscript_path %||% file.path(R.home("bin"), "Rscript")
  r_expr <- sprintf(
    'MCPR::mcp_session(session_id = %dL, working_dir = "%s", daemon = TRUE)',
    as.integer(session_id), working_dir
  )
  label <- daemon_process_label(session_id)

  proc <- processx::process$new(
    command = rscript,
    args = c("--no-init-file", "--no-site-file", "-e", r_expr),
    stdin = NULL,
    stdout = "|",
    stderr = "|",
    env = c(
      "current",
      MCPR_DAEMON = "true",
      MCPR_CLIENT_ID = client_id,
      MCPR_PROCESS_LABEL = label,
      MCPR_PARENT_PID = as.character(Sys.getpid())
    )
  )

  the$daemon_processes[[client_id]] <- proc
  cli::cli_inform(c(
    "i" = "Secondary session {session_id} spawned (PID {proc$get_pid()})"
  ))
  proc
}

#' Connect to any session via IPC socket
#'
#' @description Dials the session's IPC URL and waits for a pipe connection using
#'   the pipe_notify + until pattern. Works for both interactive and secondary sessions.
#'   Returns a connected socket on success, or NULL on timeout.
#' @param session_id Integer. The session port number to connect to.
#' @param timeout_ms Integer. Connection timeout in milliseconds (default: 15000).
#' @return A nanonext socket object, or NULL if connection failed.
#' @noRd
# Dial the session URL and wait for the pipe-notify handshake before returning.
# Callers only receive a socket once the remote session is actually listening.
connect_ipc_socket <- function(session_id, timeout_ms = 15000L) {
  url <- sprintf("%s%d", get_system_socket_url(), as.integer(session_id))
  sock <- nanonext::socket("poly")
  cv <- nanonext::cv()
  nanonext::pipe_notify(sock, cv, add = TRUE)
  nanonext::dial(sock, url = url, fail = "none")
  connected <- nanonext::until(cv, as.integer(timeout_ms))
  nanonext::pipe_notify(sock, NULL, add = TRUE)
  if (!connected) {
    nanonext::reap(sock)
    return(NULL)
  }
  sock
}

#' Register a joined user session socket
#'
#' @description Stores an IPC socket under its session ID in the user sessions registry.
#' @param session_id Integer. The session port number.
#' @param socket A nanonext socket connected to the user session.
#' @return Called for side effects; returns NULL invisibly.
#' @noRd
register_user_session <- function(session_id, socket) {
  the$user_sessions[[as.character(as.integer(session_id))]] <- socket
}

#' Look up a joined user session socket by session ID
#'
#' @description Returns the nanonext socket for a registered user session, or NULL.
#' @param session_id Integer. The session port number.
#' @return A nanonext socket, or NULL.
#' @noRd
get_user_session <- function(session_id) {
  the$user_sessions[[as.character(as.integer(session_id))]]
}

#' Unregister a joined user session
#'
#' @description Closes the socket and removes the entry from the user sessions registry.
#' @param session_id Integer. The session port number.
#' @return Called for side effects; returns NULL invisibly.
#' @noRd
unregister_user_session <- function(session_id) {
  key <- as.character(as.integer(session_id))
  sock <- the$user_sessions[[key]]
  if (!is.null(sock)) tryCatch(nanonext::reap(sock), error = function(e) NULL)
  the$user_sessions[[key]] <- NULL
}
