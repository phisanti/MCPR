# Manage R Sessions Tool for MCPR
# Unified tool for listing, joining, and starting R sessions with enhanced status information.
# Combines functionality from list_r_sessions and select_r_session with session creation capabilities.

#' Create a descriptive string for the current R session with enhanced details
#'
#' Used for the `manage_r_sessions` tool with action "list".
#' @return A string like "1: /path/to/project (RStudio) - 2024-08-08 10:30:15"
describe_session_detailed <- function() {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  sprintf("%d: %s (%s) - %s", 
          the$session, 
          getwd(), 
          infer_ide(), 
          timestamp)
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
  
  if (action == "list") {
    # Enhanced listing with working directory and timestamp
    sock <- nanonext::socket("poly")
    on.exit(nanonext::reap(sock))
    cv <- nanonext::cv()
    monitor <- nanonext::monitor(sock, cv)
    for (i in seq_len(1024L)) {
      if (
        nanonext::dial(
          sock,
          url = sprintf("%s%d", the$socket_url, i),
          autostart = NA,
          fail = "none"
        ) &&
          i > 8L
      ) {
        break
      }
    }
    pipes <- nanonext::read_monitor(monitor)
    res <- lapply(
      pipes,
      function(x) nanonext::recv_aio(sock, mode = "string", timeout = 5000L)
    )
    lapply(
      pipes,
      function(x) nanonext::send_aio(sock, character(), mode = "serial", pipe = x)
    )
    sort(as.character(nanonext::collect_aio_(res)))
    
  } else if (action == "join") {
    # Join existing session (renamed from select)
    if (is.null(session)) {
      stop("session parameter is required when action='join'")
    }
    if (!is.numeric(session) || length(session) != 1) {
      stop("session must be a single integer")
    }
    
    nanonext::reap(the$server_socket[["dialer"]][[1L]])
    attr(the$server_socket, "dialer") <- NULL
    nanonext::dial(
      the$server_socket,
      url = sprintf("%s%d", the$socket_url, session)
    )
    sprintf("Joined session %d successfully.", session)
    
  } else if (action == "start") {
    # Start new R session using processx
    tryCatch({
      # Find next available session number
      sock <- nanonext::socket("poly")
      on.exit(nanonext::reap(sock))
      
      next_session <- 1L
      for (i in seq_len(1024L)) {
        if (!nanonext::dial(
          sock,
          url = sprintf("%s%d", the$socket_url, i),
          autostart = NA,
          fail = "none"
        )) {
          next_session <- i
          break
        }
      }
      
      # Start new R process with MCPR session
      r_cmd <- file.path(R.home("bin"), "R")
      args <- c("--vanilla", "-e", 
                sprintf("MCPR::mcp_session(%d); readline('Press Enter to continue...')", 
                        next_session))
      
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
      
    }, error = function(e) {
      sprintf("Error starting new session: %s", e$message)
    })
  }
}

#' @export
manage_r_sessions <- manage_r_sessions