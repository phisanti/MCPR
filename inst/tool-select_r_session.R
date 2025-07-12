#' Select R Session Tool for MCPR
#'
#' This file defines the select_r_session tool that allows AI agents to
#' choose which R session to work with.

#* @mcp_tool
#* @description Choose the R session of interest. Use the `list_r_sessions` tool to discover potential sessions. In general, do not use this tool unless asked to select a specific R session; the tools available to you have a default R session that is usually the one the user wants. Do not call this tool immediately after calling list_r_sessions unless you've been asked to select an R session and haven't yet called list_r_sessions. Your choice of session will persist after the tool is called; only call this tool more than once if you need to switch between sessions.
#* @param session integer The R session number to select.
#' Select an R session
#'
#' @param session integer The R session number to select.
#' @keywords mcpr_tool
#' @return Success message
select_r_session <- function(session) {
  nanonext::reap(the$server_socket[["dialer"]][[1L]])
  attr(the$server_socket, "dialer") <- NULL
  nanonext::dial(
    the$server_socket,
    url = sprintf("%s%d", the$socket_url, session)
  )
  sprintf("Selected session %d successfully.", session)
}

# LEGACY ELLMER TOOL DEFINITION (commented out for Phase 2 migration)  
# This will be removed in Phase 3 after full migration validation
#
# select_r_session_tool <-
#   ellmer::tool(
#     .fun = select_r_session,
#     .description = paste(
#       "Choose the R session of interest.",
#       "Use the `list_r_sessions` tool to discover potential sessions.",
#       "In general, do not use this tool unless asked to select a specific R",
#       "session; the tools available to you have a default R session",
#       "that is usually the one the user wants.",
#       "Do not call this tool immediately after calling list_r_sessions",
#       "unless you've been asked to select an R session and haven't yet",
#       "called list_r_sessions.",
#       "Your choice of session will persist after the tool is called; only",
#       "call this tool more than once if you need to switch between sessions."
#     ),
#     session = ellmer::type_integer("The R session number to select.")
#   )

#' @export
select_r_session <- select_r_session
