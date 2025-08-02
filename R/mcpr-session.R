#' Make an R session available to the MCP server
#'
#' @description
#' Call this function in an interactive R session to make it discoverable by the
#' MCP server. This allows MCP clients (like AI assistants) to execute code and
#' tools within this specific R session.
#'
#' It is recommended to add `mcptools::mcp_session()` to your `.Rprofile` to
#' automatically make every interactive session available.
#'
#' @details
#' The function works by creating a `nanonext` socket and listening on a
#' unique URL. The MCP server can then connect to this socket to communicate
#' with the R session. It only runs in interactive sessions.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   mcp_session()
#' }
mcp_session <- function() {
  if (!rlang::is_interactive()) {
    return(invisible())
  }

  the$session_socket <- nanonext::socket("poly")
  the$last_activity <- Sys.time()
  reg.finalizer(environment(), function(e) mcp_session_stop(), onexit = TRUE)
  
  i <- 1L
  # Attempt to find an available socket to listen on
  while (i < 1024L) {
    # prevent indefinite loop
    nanonext::listen(
      the$session_socket,
      url = sprintf("%s%d", the$socket_url, i),
      fail = "none"
    ) ||
      break
    i <- i + 1L
  }
  the$session <- i

  # Start listening for messages from the server asynchronously
  schedule_handle_message_from_server()
  # Schedule periodic cleanup check
  later::later(check_session_timeout, delay = 900) # 15 min
}


#' Asynchronously listen for the next message from the server
schedule_handle_message_from_server <- function() {
  the$raio <- nanonext::recv_aio(the$session_socket, mode = "serial")
  promises::as.promise(the$raio)$then(handle_message_from_server)$catch(
    \(e) {
      # Connection lost - cleanup and stop
      if (!is.null(the$session_socket)) mcp_session_stop()
    }
  )
}

#' Process a message received from the MCP server
handle_message_from_server <- function(data) {
  pipe <- nanonext::pipe_id(the$raio)
  the$last_activity <- Sys.time()
  # Immediately schedule the next listen to be ready for the next message
  schedule_handle_message_from_server()

  # If the message is empty, it's a discovery ping; respond with session info
  if (length(data) == 0) {
    return(
      nanonext::send_aio(
        the$session_socket,
        describe_session(),
        mode = "raw",
        pipe = pipe
      )
    )
  }

  # Process the request and create a response
  body <- if (data$method == "tools/call") {
    execute_tool_call(data)
  } else {
    jsonrpc_response(
      data$id,
      error = list(code = -32601, message = "Method not found")
    )
  }

  # Send the response back to the server
  nanonext::send_aio(
    the$session_socket,
    to_json(body),
    mode = "raw",
    pipe = pipe
  )
}


#' Stop the MCP session and clean up resources
mcp_session_stop <- function() {
  if (!is.null(the$session_socket)) {
    nanonext::reap(the$session_socket)
    the$session_socket <- NULL
    the$session <- NULL
    the$raio <- NULL
    the$last_activity <- NULL
  }
}

check_session_timeout <- function() {
  if (!is.null(the$last_activity) && 
      difftime(Sys.time(), the$last_activity, units = "mins") > 15) {
    mcp_session_stop()
  } else if (!is.null(the$session_socket)) {
    later::later(check_session_timeout, delay = 900) # Check again in 15 min
  }
}