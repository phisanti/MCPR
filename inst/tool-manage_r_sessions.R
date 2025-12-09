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
#' @description Manage R sessions - list available sessions with detailed status, join a specific session, or start a new session. Use action="list" to see all available sessions with working directory and timestamp. Use action="join" with session parameter to connect to a specific session. Use action="start" to create a new R session. Do not use this tool unless specifically asked to manage R sessions.
#' @param action character The action to perform: "list", "join", or "start"
#' @param session integer Optional. The R session number to join (required when action="join")
#' @keywords mcpr_tool
#' @return For "list": vector of detailed session descriptions. For "join": success message. For "start": new session information.
manage_r_sessions <- function(action = "list", session = NULL) {
  if (!action %in% c("list", "join", "start")) {
    stop("action must be one of: 'list', 'join', 'start'")
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
  } else if (action == "start") {
    # Start new R session using processx
    tryCatch(
      {
        # Find next available session number
        sock <- nanonext::socket("poly")
        on.exit(nanonext::reap(sock), add = TRUE)

        next_session <- 1L
        for (i in seq_len(1024L)) {
          if (!nanonext::dial(
            sock,
            url = sprintf("%s%d", socket_base, i),
            autostart = NA,
            fail = "none"
          )) {
            next_session <- i
            break
          }
        }

        # Start new R process with MCPR session in daemon mode
        working_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
        r_cmd <- file.path(R.home("bin"), "R")
        r_expr <- sprintf(
          "MCPR::mcp_session(session_id = %d, working_dir = %s, daemon = TRUE)",
          next_session,
          encodeString(working_dir, quote = "\"")
        )
        args <- c(
          "--vanilla", "-e",
          r_expr
        )

        proc <- processx::process$new(
          command = r_cmd,
          args = args,
          stdout = "|",
          stderr = "|"
        )

        # Give the process a moment to start
        Sys.sleep(1)

        if (proc$is_alive()) {
          sprintf("Started new R session %d (PID: %d)", next_session, proc$get_pid())
        } else {
          stop("Failed to start new R session")
        }
      },
      error = function(e) {
        sprintf("Error starting new session: %s", e$message)
      }
    )
  }
}

#' @export
manage_r_sessions <- manage_r_sessions
