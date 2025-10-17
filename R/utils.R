# General Utility Functions
# Common utility functions supporting MCPR package operations and workflows.
# Provides NULL handling, JSON conversion, validation, and helper functions.

#' Drop NULL Values from Vector or List
#'
#' @title Drop NULL Values from Vector or List
#' @description Removes all NULL elements from vector or list through logical filtering.
#' Provides clean data structure processing by eliminating NULL entries for consistent
#' data handling workflows. Maintains non-NULL elements while preserving original
#' data structure characteristics.
#'
#' @param x Vector or list to process
#' @return Vector or list with NULL values removed
#' @noRd
drop_nulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

#' Create Named List
#'
#' @title Create Named List
#' @description Creates named list ensuring proper named structure even when empty.
#' Handles edge case of empty list creation to maintain named list properties for
#' consistent data structure handling. Provides reliable named list construction
#' for MCPR framework data processing requirements.
#'
#' @param ... Elements to include in named list
#' @return Named list with provided elements
#' @noRd
named_list <- function(...) {
  res <- list(...)
  if (length(res) == 0) {
    # A way of creating an empty named list
    res <- list(a = 1)[0]
  }
  res
}

#' Convert R Object to JSON String
#'
#' @title Convert R Object to JSON String
#' @description Converts R object to JSON string format with automatic unboxing for scalar values.
#' Provides consistent JSON serialization interface for MCPR package data transmission.
#' Simplifies JSON conversion with sensible defaults for MCP protocol communication.
#'
#' @param x R object to convert to JSON
#' @param ... Additional arguments passed to jsonlite::toJSON
#' @return JSON string representation of R object
#' @noRd
to_json <- function(x, ...) {
  jsonlite::toJSON(x, ..., auto_unbox = TRUE)
}


#' Check Session is Not Interactive
#'
#' @title Check Session is Not Interactive
#' @description Validates that current session is not interactive for server functions.
#' Prevents server operations in interactive contexts where they're inappropriate.
#' Provides clear error messaging for proper usage guidance and workflow enforcement
#' in MCP server deployment scenarios.
#'
#' @param call Calling environment for error reporting
#' @return None (throws error if interactive)
#' @noRd
check_not_interactive <- function(call = rlang::caller_env()) {
  if (rlang::is_interactive()) {
    cli::cli_abort(
      c(
        "This function is not intended for interactive use.",
        "i" = "See {.help {.fn mcpr_server}} for instructions on configuring this
       function with applications"
      ),
      call = call
    )
  }
}

#' Remove Empty Elements from List
#'
#' @title Remove Empty Elements from List
#' @description Filters out empty elements from list based on length criterion.
#' Provides data cleaning utility for list processing workflows by removing
#' zero-length elements. Maintains non-empty elements for consistent data
#' structure handling in MCPR framework operations.
#'
#' @param .x List to process
#' @return List with empty elements removed
#' @noRd
compact <- function(.x) {
  Filter(length, .x)
}

#' Compact List by Removing NULL Values
#'
#' @title Compact List by Removing NULL Values
#' @description Removes NULL values from list through negated NULL check filtering.
#' Provides clean data structure processing for consistent list handling workflows.
#' Maintains non-NULL elements while preserving list structure for MCPR framework
#' data processing requirements.
#'
#' @param x List to compact
#' @return List with NULL values removed
#' @noRd
compact_list <- function(x) {
  Filter(Negate(is.null), x)
}

#' Infer Current IDE
#'
#' @title Infer Current IDE
#' @description Infers current IDE from command line arguments for environment detection.
#' Recognizes RStudio, Positron, and other IDE environments through command argument
#' analysis. Enables context-aware behavior adaptation based on development environment
#' for enhanced user experience in different IDE contexts.
#'
#' @return Character string with IDE name
#' @noRd
infer_ide <- function() {
  first_cmd_arg <- commandArgs()[1]
  switch(first_cmd_arg,
    ark = "Positron",
    RStudio = "RStudio",
    first_cmd_arg
  )
}

#' Null Coalescing Operator
#'
#' @name null-coalesce
#' @title Null Coalescing Operator
#' @description Returns left-hand side if not NULL, otherwise returns right-hand side value.
#' Provides convenient NULL value handling for default value assignment and conditional
#' logic. Enables clean code patterns for NULL checking and fallback value provision
#' throughout MCPR framework operations.
#'
#' @param x Left-hand side value to check
#' @param y Right-hand side value (used if x is NULL)
#' @return x if not NULL, otherwise y
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Additional utility functions for MCPR migration

#' Check Object is Function
#'
#' @title Check Object is Function
#' @description Validates that object is callable function with clear error reporting.
#' Provides function type checking utility for parameter validation in MCPR framework.
#' Ensures proper function objects for tool definitions and callback handling through
#' comprehensive validation with informative error messages.
#'
#' @param x Object to check for function type
#' @param arg Argument name for error messages
#' @param call Calling environment for error reporting
#' @return None (throws error if not valid function)
#' @noRd
check_function <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.function(x)) {
    cli::cli_abort("{.arg {arg}} must be a function, not {.obj_type_friendly {x}}", call = call)
  }
}

#' Check Object is String
#'
#' @title Check Object is String
#' @description Validates that object is single non-NA character value with optional NULL allowance.
#' Provides string type checking utility for parameter validation in MCPR framework.
#' Ensures proper character types for names, descriptions, and text parameters through
#' comprehensive validation with clear error reporting.
#'
#' @param x Object to check for string type
#' @param allow_null Whether to allow NULL values (default: FALSE)
#' @param arg Argument name for error messages
#' @param call Calling environment for error reporting
#' @return None (throws error if not valid string)
#' @noRd
check_string <- function(x, allow_null = FALSE, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (allow_null && is.null(x)) {
    return()
  }
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single string, not {.obj_type_friendly {x}}", call = call)
  }
}

#' Check Object is Boolean
#'
#' @title Check Object is Boolean
#' @description Validates that object is single non-NA logical value with optional NULL allowance.
#' Provides boolean type checking utility for parameter validation in MCPR framework.
#' Ensures proper logical types for configuration flags and boolean parameters through
#' comprehensive validation with clear error reporting.
#'
#' @param x Object to check for boolean type
#' @param allow_null Whether to allow NULL values (default: FALSE)
#' @param arg Argument name for error messages
#' @param call Calling environment for error reporting
#' @return None (throws error if not valid boolean)
#' @noRd
check_bool <- function(x, allow_null = FALSE, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (allow_null && is.null(x)) {
    return()
  }
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single logical value, not {.obj_type_friendly {x}}", call = call)
  }
}

#' Check Current Session Socket
#'
#' @title Check Current Session Socket
#' @description Determines the socket number that the current R session is using for MCP communication.
#' Reports basic session and socket information for debugging and logging purposes.
#'
#' @param verbose Logical. If TRUE (default), prints diagnostic messages to console.
#'   If FALSE, returns diagnostic information as a list for programmatic use.
#' @return If verbose=TRUE, returns socket number invisibly and prints messages.
#'   If verbose=FALSE, returns list with socket_number, is_interactive, and has_session components.
#' @noRd
check_session_socket <- function(verbose = TRUE) {
  is_interactive_session <- interactive()
  has_mcp_session <- exists("session", envir = the) && !is.null(the$session)
  socket_num <- if (has_mcp_session && is.numeric(the$session)) the$session else NULL

  if (verbose) {
    if (!is_interactive_session) {
      cli::cli_alert_info("Not in an interactive R session")
    } else if (!has_mcp_session) {
      cli::cli_alert_info("No MCP session detected")
      cli::cli_alert_info("Run {.fn mcp_session} to start an MCP session")
    } else if (is.null(socket_num)) {
      cli::cli_alert_warning("MCP session detected but socket number is invalid")
    } else {
      cli::cli_alert_success("MCP session running on socket: {socket_num}")
    }

    return(invisible(socket_num))
  } else {
    return(list(
      socket_number = socket_num,
      is_interactive = is_interactive_session,
      has_session = has_mcp_session
    ))
  }
}

#' Get System Socket URL Base
#'
#' @title Get System Socket URL Base
#' @description Returns platform-specific socket URL base for nanonext operations.
#' Provides cross-platform socket URLs optimized for each operating system.
#' Uses global state when available, otherwise falls back to platform detection.
#'
#' @return Character string with platform-appropriate socket URL base
#' @noRd
get_system_socket_url <- function() {
  # Use global state (set by .onLoad) with fallback to platform detection
  the$socket_url %||% switch(Sys.info()[["sysname"]],
    Linux = "abstract://MCPR-socket",
    Windows = "ipc://MCPR-socket",
    "ipc:///tmp/MCPR-socket"
  )
}

#' Describe Current Session
#'
#' @title Describe Current Session
#' @description Creates session description with optional detailed information.
#' Used for both discovery ping responses (detailed=FALSE) and session management
#' tools (detailed=TRUE). When detailed=TRUE, includes timestamp for table formatting.
#'
#' @param detailed Logical. If TRUE, includes timestamp for detailed session information.
#'   If FALSE (default), returns basic session description for discovery pings.
#' @return Character string with session description
#' @noRd
describe_session <- function(detailed = FALSE) {
  session_num <- if (exists("session", envir = the) && !is.null(the$session)) {
    the$session
  } else {
    NULL
  }

  if (detailed) {
    # Detailed version with timestamp for manage_r_sessions tool
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    if (is.null(session_num)) {
      sprintf(
        "No session: %s (%s) - %s",
        getwd(),
        infer_ide(),
        timestamp
      )
    } else {
      sprintf(
        "%d: %s (%s) - %s",
        session_num,
        getwd(),
        infer_ide(),
        timestamp
      )
    }
  } else {
    # Basic version for discovery ping responses
    if (is.null(session_num)) {
      sprintf("No session: %s (%s)", basename(getwd()), infer_ide())
    } else {
      sprintf("%d: %s (%s)", session_num, basename(getwd()), infer_ide())
    }
  }
}

#' Format Data Frame as Aligned Table for Agent Consumption
#'
#' @title Format Data Frame as Aligned Table for Agent Consumption
#' @description Converts data frame to formatted table string with aligned columns and separators.
#' Provides consistent table formatting across MCPR tools for agent/LLM readable output.
#' Dynamically calculates column widths and creates properly aligned text table with
#' headers, separator line, and data rows.
#'
#' @param df Data frame to format as table
#' @param empty_message Message to return when data frame is empty (default: "No data found.")
#' @return Character string with formatted table or empty message
#' @noRd
format_table_for_agent <- function(df, empty_message = "No data found.") {
  if (nrow(df) == 0) {
    return(empty_message)
  }
  
  # Calculate column widths dynamically
  col_widths <- sapply(names(df), function(col) {
    max(nchar(c(col, as.character(df[[col]]))))
  })
  
  # Create separator line
  separator <- paste0(rep("-", sum(col_widths) + 3 * (ncol(df) - 1)), collapse = "")
  
  # Format header
  header_format <- paste(paste0("%-", col_widths, "s"), collapse = " | ")
  header <- do.call(sprintf, c(list(header_format), as.list(names(df))))
  
  # Format data rows
  row_format <- paste(paste0("%-", col_widths, "s"), collapse = " | ")
  rows <- apply(df, 1, function(row) {
    do.call(sprintf, c(list(row_format), as.list(as.character(row))))
  })
  
  # Combine header, separator, and rows
  paste(c(header, separator, rows), collapse = "\n")
}

# Mocking for testing
interactive <- NULL
basename <- NULL
getwd <- NULL
commandArgs <- NULL
