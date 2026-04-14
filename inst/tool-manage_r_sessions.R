# Manage R Sessions Tool for MCPR
# Unified tool for listing and joining R sessions with enhanced status information.
# Combines functionality from list_r_sessions and select_r_session.

#' Format Session List as Table
#'
#' @description Helper function to format session list as aligned table
#' @param session_data Character vector of session descriptions
#' @return Formatted table string
#' @noRd
format_sessions_table <- function(session_data) {
  if (length(session_data) == 0) {
    return("No active R sessions found.")
  }

  # Parse session data - expect format: "ID: directory (IDE) - timestamp" or "No session: directory (IDE) - timestamp"
  sessions <- list()

  for (i in seq_along(session_data)) {
    line <- session_data[i]

    # Try to parse "ID: directory (IDE) - timestamp"
    if (grepl("^\\d+:", line)) {
      parts <- regmatches(line, regexec("^(\\d+): (.+) \\((.+)\\) - (.+)$", line))[[1]]
      if (length(parts) == 5) {
        sessions[[i]] <- list(
          id = parts[2],
          directory = basename(parts[3]), # Use basename for cleaner display
          ide = parts[4],
          timestamp = parts[5]
        )
      }
    } else if (grepl("^No session:", line)) {
      parts <- regmatches(line, regexec("^No session: (.+) \\((.+)\\) - (.+)$", line))[[1]]
      if (length(parts) == 4) {
        sessions[[i]] <- list(
          id = "?",
          directory = basename(parts[2]),
          ide = parts[3],
          timestamp = "Unknown"
        )
      }
    }
  }

  # Remove any failed parses
  sessions <- sessions[!sapply(sessions, is.null)]

  if (length(sessions) == 0) {
    return("No parseable session data found.")
  }

  # Convert parsed sessions to data frame for generic table formatting
  sessions_df <- data.frame(
    ID = sapply(sessions, function(s) s$id),
    `Working Directory` = sapply(sessions, function(s) s$directory),
    IDE = sapply(sessions, function(s) s$ide),
    Timestamp = sapply(sessions, function(s) s$timestamp),
    stringsAsFactors = FALSE
  )
  
  # Use generic table formatting function
  MCPR:::format_table_for_agent(sessions_df, "No parseable session data found.")
}

#' Format Daemon Sessions as Table
#'
#' @description Helper to list daemon sessions from the registry with status info.
#' @return Formatted table string, or NULL if no daemon sessions exist.
#' @noRd
format_daemon_sessions_table <- function() {
  daemons <- MCPR:::list_daemon_sessions()
  if (length(daemons) == 0) return(NULL)

  rows <- lapply(names(daemons), function(key) {
    sid <- daemons[[key]]
    proc <- MCPR:::the$daemon_processes[[key]]
    status <- if (!is.null(proc) && inherits(proc, "process") && proc$is_alive()) {
      "running"
    } else {
      "stopped"
    }
    data.frame(
      `Session ID` = as.character(sid),
      Status = status,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  df <- do.call(rbind, rows)
  MCPR:::format_table_for_agent(df, "No daemon sessions found.")
}

#' @description Manage R sessions - list available sessions with detailed status, join a specific session, or start/stop a daemon session. Use action="list" to see all available sessions (interactive and daemon) with working directory and timestamp. Use action="join" with session parameter to connect to a specific session. Use action="start" to spawn a new isolated background daemon R session - each call creates a separate session with its own workspace. Use action="close" with session parameter to close a specific daemon session. Do not use this tool unless specifically asked to manage R sessions.
#' @param action character The action to perform: "list", "join", "start", or "close"
#' @param session integer Optional. The R session number to join (required when action="join"), or the daemon session to close (required when action="close").
#' @keywords mcpr_tool
#' @return For "list": formatted table of all sessions. For "join": success message. For "start": the session ID to use with other tools. For "close": status message.
manage_r_sessions <- function(action = "list", session = NULL) {
  if (!action %in% c("list", "join", "start", "close", "stop")) {
    stop("action must be one of: 'list', 'join', 'start', 'close'")
  }

  # Normalize "stop" to "close" (back-compat alias)
  if (action == "stop") action <- "close"

  # Get platform-specific socket URL once and reuse
  socket_base <- MCPR:::get_system_socket_url()

  if (action == "list") {
    # Enhanced listing with working directory and timestamp
    # Use manual socket management to avoid BaseMCPR cleanup conflicts
    sock <- nanonext::socket("poly")
    on.exit(nanonext::reap(sock), add = TRUE)

    cv <- nanonext::cv()
    monitor <- nanonext::monitor(sock, cv)

    for (i in seq_len(1024L)) {
      if (
        nanonext::dial(
          sock,
          url = sprintf("%s%d", socket_base, i),
          autostart = NA,
          fail = "none"
        ) &&
          i > 8L
      ) {
        break
      }
    }
    pipes <- nanonext::read_monitor(monitor)
    # Get session data from all active sessions
    res <- lapply(
      pipes,
      function(x) nanonext::recv_aio(sock, mode = "string", timeout = 5000L)
    )
    lapply(
      pipes,
      function(x) nanonext::send_aio(sock, character(), mode = "serial", pipe = x)
    )

    # Collect and format session data as table
    session_data <- sort(as.character(nanonext::collect_aio_(res)))
    interactive_table <- format_sessions_table(session_data)

    # Append daemon sessions
    daemon_table <- format_daemon_sessions_table()

    parts <- character(0)
    if (nchar(interactive_table) > 0) {
      parts <- c(parts, "Interactive Sessions:", interactive_table)
    }
    if (!is.null(daemon_table)) {
      parts <- c(parts, "", "Daemon Sessions:", daemon_table)
    }
    paste(parts, collapse = "\n")
  } else if (action == "join") {
    # Join existing session (renamed from select)
    if (is.null(session)) {
      stop("session parameter is required when action='join'")
    }
    if (!is.numeric(session) || length(session) != 1) {
      stop("session must be a single integer")
    }

    server_socket <- if (exists("server_socket", envir = the) && !is.null(the$server_socket)) {
      the$server_socket
    } else {
      stop("No server socket available - server may not be running")
    }

    nanonext::reap(server_socket[["dialer"]][[1L]])
    attr(server_socket, "dialer") <- NULL
    nanonext::dial(
      server_socket,
      url = sprintf("%s%d", socket_base, session)
    )
    sprintf("Joined session %d successfully.", session)
  } else if (action == "start") {
    # Always create a new daemon with a unique key based on session_id.
    # Each call creates a separate isolated session with no sharing.
    session_id <- MCPR:::find_daemon_port()
    daemon_key <- sprintf("daemon-%d", session_id)
    MCPR:::spawn_daemon(daemon_key, session_id, getwd())
    MCPR:::register_daemon(daemon_key, session_id)
    sprintf("Daemon session %d started. Use session=%d in execute_r_code and other tools to target this session. Connection will be established on first use.", session_id, session_id)
  } else if (action == "close") {
    if (is.null(session)) {
      stop("session parameter is required when action='close' - specify which daemon session to close")
    }
    daemon_key <- sprintf("daemon-%d", as.integer(session))
    existing <- MCPR:::get_daemon_session(daemon_key)
    if (is.null(existing)) {
      return(sprintf("No daemon session %d found.", as.integer(session)))
    }
    MCPR:::unregister_daemon(daemon_key)
    sprintf("Daemon session %d closed.", as.integer(session))
  }
}

#' @export
manage_r_sessions <- manage_r_sessions
