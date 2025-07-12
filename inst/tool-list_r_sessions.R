#' List R Sessions Tool for MCPR
#'
#' This file defines the list_r_sessions tool that allows AI agents to
#' discover R sessions which have called `MCPR::mcp_session()`.

#* @mcp_tool
#' @description List the R sessions that are available to access. R sessions which have run `MCPR::mcp_session()` will appear here. In the output, start each session with 'Session #' and do NOT otherwise prefix any index numbers to the output. In general, do not use this tool unless asked to list or select a specific R session. Given the output of this tool, report the users to the user. Do NOT make a choice of R session based on the results of the tool and call select_r_session unless the user asks you to specifically.
#' List available R sessions
#' @keywords mcpr_tool
#' @return A vector of session names/identifiers
list_r_sessions <- function() {
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
}

# LEGACY ELLMER TOOL DEFINITION (commented out for Phase 2 migration)
# This will be removed in Phase 3 after full migration validation
#
# list_r_sessions_tool <-
#   ellmer::tool(
#     .fun = list_r_sessions,
#     .description = paste(
#       "List the R sessions that are available to access.",
#       "R sessions which have run `MCPR::mcp_session()` will appear here.",
#       "In the output, start each session with 'Session #' and do NOT otherwise",
#       "prefix any index numbers to the output.",
#       "In general, do not use this tool unless asked to list or",
#       "select a specific R session.",
#       "Given the output of this tool, report the users to the user.",
#       "Do NOT make a choice of R session based on the results of the tool",
#       "and call select_r_session unless the user asks you to specifically."
#     )
#   )

#' @export
list_r_sessions <- list_r_sessions
