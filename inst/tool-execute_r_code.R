# Execute R Code Tool
# Runs arbitrary R code inside the current session with persistent workspace state.
# Captures results, output, warnings, errors, and plot pages (as plot:N) for agent workflows.

#' Execute R code in the current session
#'
#' @description Execute R code in the current R session. Variables persist in the global environment across calls. Returns results, output, warnings, and errors. IMPORTANT: Output goes to the agent context only - invisible to the user. Note that cat() and message() calls also have no visible side-effect to the user; their output is captured and returned to the agent only. Avoid comments, headers, and decorative print statements in code. Communicate findings to the user via chat. Any plot the code draws is recorded and reported as plot:N; view it with show_plot(plot = "plot:N") instead of wrapping code in capture_plot().
#' @param code character The R code to execute. Can be a single expression or multiple statements.
#' @param timeout integer Seconds to wait for the session to respond before returning a timeout error (default: 300). Increase for long-running computations such as model fitting or large data processing. If you receive a timeout error, retry with a higher value (e.g. timeout=600).
#' @keywords mcpr_tool
#' @return A list containing the results, output, and any warnings/errors
execute_r_code <- function(code, timeout = 300L) {
  # timeout is read by the server before forwarding to cap the response wait; unused here
  if (!is.character(code) || length(code) != 1) {
    cli::cli_abort("Code must be a single character string")
  }

  if (nchar(trimws(code)) == 0) {
    cli::cli_abort("Code cannot be empty")
  }

  # Capture all output types
  result <- list(
    code = code,
    success = FALSE,
    result = NULL,
    output = NULL,
    warnings = NULL,
    error = NULL,
    visible = FALSE
  )

  # Capture warnings
  warnings_list <- character(0)
  warning_handler <- function(w) {
    warnings_list <<- c(warnings_list, w$message)
    invokeRestart("muffleWarning")
  }

  # Record every plot page the code draws, even if it later errors
  plot_token <- MCPR:::plot_capture_begin("agent")
  # An interrupt skips the normal end; restore capture state regardless
  capture_open <- TRUE
  on.exit(if (capture_open) MCPR:::plot_capture_end(plot_token), add = TRUE)

  # Capture console output
  output_capture <- utils::capture.output(
    {
      tryCatch(
        {
          # Execute the code with warning handling
          withCallingHandlers(
            {
              # Parse and evaluate the code
              parsed_code <- parse(text = code)
              last_result <- NULL

              # Execute each expression
              for (i in seq_along(parsed_code)) {
                last_result <- withVisible(eval(parsed_code[[i]], envir = .GlobalEnv))
              }

              result$result <- last_result$value
              result$visible <- last_result$visible
              result$success <- TRUE
            },
            warning = warning_handler
          )
        },
        error = function(e) {
          result$error <<- e$message
          result$success <<- FALSE
        }
      )
    },
    type = "output"
  )

  plot_ids <- MCPR:::plot_capture_end(plot_token)
  capture_open <- FALSE
  MCPR:::activity_record(
    source = "agent",
    code = code,
    ok = result$success,
    value_class = if (result$success && result$visible) class(result$result) else NULL,
    plots = plot_ids,
    error = result$error
  )

  # Store captured output
  if (length(output_capture) > 0) {
    result$output <- paste(output_capture, collapse = "\n")
  }

  # Store warnings
  if (length(warnings_list) > 0) {
    result$warnings <- warnings_list
  }

  # Format the response for better readability
  response_parts <- character(0)

  if (result$success) {
    response_parts <- c(response_parts, "Code executed successfully")

    # Add output if any
    if (!is.null(result$output) && nchar(result$output) > 0) {
      response_parts <- c(
        response_parts,
        paste("Output:", result$output, sep = "\n")
      )
    }

    # Add result if visible and not NULL
    if (result$visible && !is.null(result$result)) {
      result_str <- tryCatch(
        {
          if (is.data.frame(result$result) || is.matrix(result$result)) {
            # For large objects, show structure instead of full content
            if (nrow(result$result) > 10 || ncol(result$result) > 10) {
              paste(
                "Result: Large object -", class(result$result)[1],
                "with", nrow(result$result), "rows and",
                ncol(result$result), "columns"
              )
            } else {
              paste("Result:", utils::capture.output(print(result$result)), collapse = "\n")
            }
          } else if (length(result$result) > 20) {
            # For long vectors, show summary
            paste(
              "Result: Vector of length", length(result$result),
              "- first few values:", paste(utils::head(result$result, 10), collapse = ", ")
            )
          } else {
            paste("Result:", utils::capture.output(print(result$result)), collapse = "\n")
          }
        },
        error = function(e) {
          paste("Result: [Object of class", class(result$result)[1], "]")
        }
      )

      response_parts <- c(response_parts, result_str)
    }

    # Add warnings if any
    if (!is.null(result$warnings)) {
      response_parts <- c(
        response_parts,
        paste("Warnings:", paste(result$warnings, collapse = "; "))
      )
    }
  } else {
    response_parts <- c(
      response_parts,
      paste("Error:", result$error)
    )
  }

  plot_note <- MCPR:::format_plot_capture(plot_ids)
  if (!is.null(plot_note)) {
    response_parts <- c(response_parts, plot_note)
  }

  return(paste(response_parts, collapse = "\n\n"))
}
