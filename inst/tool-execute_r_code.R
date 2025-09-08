#' Execute R Code Tool for MCPR
#'
#' This file defines the execute_r_code tool that allows AI agents to execute
#' arbitrary R code within the current R session. This enables stateful,
#' interactive programming where the agent can build upon previous commands
#' and maintain workspace state.

#* @mcp_tool
#' Execute R code in the current session
#'
#' @description Execute R code in the current R session. This tool allows you to run any valid R code and see the results. The code will be executed in the global environment, so variables and objects created will persist for future tool calls. Use this tool to perform data analysis, create visualizations, load packages, manipulate data, or any other R operation. The tool will return the results, any output, warnings, and errors. For complex multi-line code, ensure proper syntax and use semicolons or newlines to separate statements.
#' @param code character The R code to execute. Can be a single expression or multiple statements.
#' @keywords mcpr_tool
#' @return A list containing the results, output, and any warnings/errors
execute_r_code <- function(code) {
  if (!is.character(code) || length(code) != 1) {
    stop("Code must be a single character string")
  }

  if (nchar(trimws(code)) == 0) {
    stop("Code cannot be empty")
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

  return(paste(response_parts, collapse = "\n\n"))
}

# LEGACY ELLMER TOOL DEFINITION (commented out for Phase 2 migration)
# This will be removed in Phase 3 after full migration validation
#
# #' The ellmer tool definition for executing R code
# #'
# #' This tool allows AI agents to execute arbitrary R code within the current
# #' session, enabling stateful interactions and iterative development.
# execute_r_code_tool <- ellmer::tool(
#   .fun = execute_r_code,
#   .description = paste(
#     "Execute R code in the current R session.",
#     "This tool allows you to run any valid R code and see the results.",
#     "The code will be executed in the global environment, so variables",
#     "and objects created will persist for future tool calls.",
#     "Use this tool to perform data analysis, create visualizations,",
#     "load packages, manipulate data, or any other R operation.",
#     "The tool will return the results, any output, warnings, and errors.",
#     "For complex multi-line code, ensure proper syntax and use semicolons",
#     "or newlines to separate statements."
#   ),
#   code = ellmer::type_string(
#     "The R code to execute. Can be a single expression or multiple statements.",
#     required = TRUE
#   )
# )

#' @export
execute_r_code <- execute_r_code
