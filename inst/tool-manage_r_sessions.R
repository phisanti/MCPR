# Manage R Sessions Tool for MCPR
# Discovery schema for server-owned active session controls.
# Runtime behavior is intercepted by mcprServer before ordinary tool forwarding.

#' @description Manage R sessions - list available sessions, attach a human session, start an MCPR-owned secondary session, detach back to the private session, or close a secondary session. This is a server-owned control tool; ordinary tools run in the current active session and do not take a session argument. Do not use this tool unless specifically asked to manage R sessions.
#' @param action character The action to perform: "list", "join", "start", "detach", or "close"
#' @param session integer Optional. The R session number to join (required when action="join"), or the secondary session to close (required when action="close").
#' @keywords mcpr_tool
#' @return Server-owned session control status.
manage_r_sessions <- function(action = "list", session = NULL) {
  if (is.null(action)) {
    action <- "list"
  }
  action <- tolower(as.character(action))
  if (identical(action, "stop")) {
    action <- "close"
  }
  if (action %in% c("local", "private")) {
    action <- "detach"
  }

  if (!action %in% c("list", "join", "start", "detach", "close")) {
    cli::cli_abort("action must be one of: 'list', 'join', 'start', 'detach', 'close'")
  }
  if (identical(action, "join") && is.null(session)) {
    cli::cli_abort("session parameter is required when action='join'")
  }
  if (identical(action, "close") && is.null(session)) {
    cli::cli_abort("session parameter is required when action='close'")
  }
  if (!is.null(session) && (!is.numeric(session) || length(session) != 1L || is.na(session))) {
    cli::cli_abort("session must be a single integer")
  }

  cli::cli_abort(
    paste0(
      "manage_r_sessions is handled by mcprServer. ",
      "Call it through the MCP server tools/call path, not by sourcing the tool body directly."
    )
  )
}

#' @export
manage_r_sessions <- manage_r_sessions
