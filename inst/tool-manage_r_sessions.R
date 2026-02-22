# Manage R Sessions Tool for MCPR
# Unified tool for listing, joining, and starting R sessions with enhanced status information.
# Combines functionality from list_r_sessions and select_r_session with session creation capabilities.

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

#* @mcp_tool
#' @description Manage R sessions - list available sessions with detailed status, join a specific session. Use action="list" to see all available sessions with working directory and timestamp. Use action="join" with session parameter to connect to a specific session. Do not use this tool unless specifically asked to manage R sessions.
#' @param action character The action to perform: "list" or "join"
#' @param session integer Optional. The R session number to join (required when action="join")
#' @keywords mcpr_tool
#' @return For "list": vector of detailed session descriptions. For "join": success message.
manage_r_sessions <- function(action = "list", session = NULL) {
  if (!action %in% c("list", "join")) {
    stop("action must be one of: 'list', 'join'")
  }

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
    format_sessions_table(session_data)
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
  }
}

#' @export
manage_r_sessions <- manage_r_sessions
