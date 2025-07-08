# This file contains the implementation of the core, built-in tools
# provided by the MCPR package.

# --- Session Management Tools ---

#' List available R sessions
#'
#' @description
#' This tool discovers and lists all interactive R sessions that have been made
#' available by calling `mcp_session()`. It is executed on the server side.
#'
#' @return A character vector of available sessions, each described by its
#'   ID, working directory, and IDE.
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

#' Select an R session to be the target for subsequent tool calls
#'
#' @description
#' This tool changes the active R session that the MCP server communicates with.
#' It is executed on the server side.
#'
#' @param session The integer ID of the session to select.
#' @return A confirmation message.
select_r_session <- function(session) {
  # Disconnect from the current session and connect to the new one
  nanonext::reap(the$server_socket[["dialer"]][[1L]])
  attr(the$server_socket, "dialer") <- NULL
  nanonext::dial(
    the$server_socket,
    url = sprintf("%s%d", the$socket_url, session)
  )
  sprintf("Selected session %d successfully.", session)
}

# --- Code Execution Tool ---

#' Execute arbitrary R code in the current session
#'
#' @description
#' This tool allows an AI agent to execute any R code in the selected R session.
#' It enables stateful, interactive programming.
#'
#' @param code A string containing valid R code to execute.
#' @return A formatted string containing the execution results, including
#'   output, warnings, and errors.
execute_r_code <- function(code) {
  if (!is.character(code) || length(code) != 1) {
    stop("Code must be a single character string")
  }
  if (nchar(trimws(code)) == 0) {
    stop("Code cannot be empty")
  }

  # Capture all output, warnings, and results
  warnings_list <- character(0)
  warning_handler <- function(w) {
    warnings_list <<- c(warnings_list, w$message)
    invokeRestart("muffleWarning")
  }

  output_capture <- capture.output({
    eval_result <- tryCatch({
      withCallingHandlers({
        parsed_code <- parse(text = code)
        last_result <- NULL
        for (i in seq_along(parsed_code)) {
          last_result <- withVisible(eval(parsed_code[[i]], envir = .GlobalEnv))
        }
        list(success = TRUE, result = last_result)
      }, warning = warning_handler)
    }, error = function(e) {
      list(success = FALSE, error = e$message)
    })
  }, type = "output")

  # Format the response for the AI agent
  response_parts <- c()
  if (eval_result$success) {
    response_parts <- c(response_parts, "✓ Code executed successfully.")
    if (length(output_capture) > 0) {
      response_parts <- c(response_parts, paste("Output:", paste(output_capture, collapse = "\n"), sep = "\n"))
    }
    if (eval_result$result$visible && !is.null(eval_result$result$value)) {
      response_parts <- c(response_parts, paste("Result:", capture.output(print(eval_result$result$value)), collapse = "\n"))
    }
    if (length(warnings_list) > 0) {
      response_parts <- c(response_parts, paste("Warnings:", paste(warnings_list, collapse = "; ")))
    }
  } else {
    response_parts <- c(response_parts, paste("✗ Error:", eval_result$error))
  }
  paste(response_parts, collapse = "\n\n")
}


# --- Ellmer Tool Definitions ---

list_r_sessions_tool <- ellmer::tool(
  .fun = list_r_sessions,
  .description = "List the R sessions that are available to access. Use this to discover potential sessions to connect to."
)

select_r_session_tool <- ellmer::tool(
  .fun = select_r_session,
  .description = "Choose the R session of interest. Use `list_r_sessions` to discover potential sessions.",
  session = ellmer::type_integer("The R session number to select.")
)

execute_r_code_tool <- ellmer::tool(
  .fun = execute_r_code,
  .description = "Execute R code in the current R session. The code will be executed in the global environment, so variables and objects will persist for future tool calls.",
  code = ellmer::type_string("The R code to execute.", required = TRUE)
)

#' @export
execute_r_code <- execute_r_code
