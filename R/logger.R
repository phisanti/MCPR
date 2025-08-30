# MCPR Logging System
# Minimal R6-based logging system for MCPR package debugging and monitoring.
# Provides flexible log levels and output formatting for development workflows.

#' @title MCPRLogger - Minimal Flexible Logging
#' @description Ultra-compact R6 logger focused on efficiency and flexibility
#' @noRd
MCPRLogger <- R6::R6Class("MCPRLogger",
  private = list(
    .file = NULL,
    .enabled = TRUE,
    .component = "MCPR",
    write = function(level, message, component = NULL) {
      if (!private$.enabled) {
        return(invisible())
      }

      comp <- component %||% private$.component
      entry <- sprintf(
        "[%s] [%s] [%s] %s\n",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, comp, message
      )
      cat(entry, file = private$.file, append = TRUE)
    }
  ),
  public = list(
    #' @description Initialize logger instance
    #' @param file Log file path (optional, defaults to package log file)
    #' @param component Component name for log entries
    #' @param enabled Whether logging is enabled
    initialize = function(file = NULL, component = "MCPR", enabled = TRUE) {
      private$.file <- file %||% self$get_default_log_file()
      private$.component <- component
      private$.enabled <- enabled
    },

    #' @description Core logging method - everything goes through this
    #' @param message Log message text
    #' @param level Log level indicator (default: "I")
    #' @param component Optional component override
    log = function(message, level = "I", component = NULL) {
      private$write(level, message, component)
      invisible(self)
    },

    #' @description Log info message
    #' @param msg Message text
    #' @param comp Optional component override
    info = function(msg, comp = NULL) self$log(msg, "INFO", comp),

    #' @description Log warning message
    #' @param msg Message text
    #' @param comp Optional component override
    warn = function(msg, comp = NULL) self$log(msg, "WARNING", comp),

    #' @description Log error message
    #' @param msg Message text
    #' @param comp Optional component override
    error = function(msg, comp = NULL) self$log(msg, "ERRO", comp),

    #' @description Log debug message
    #' @param msg Message text
    #' @param comp Optional component override
    debug = function(msg, comp = NULL) self$log(msg, "DEBUG", comp),

    #' @description Log communication message
    #' @param msg Message text
    #' @param comp Optional component override
    comm = function(msg, comp = NULL) self$log(msg, "COMMUNICATION", comp),

    #' @description Enable or disable logging
    #' @param state Logging enabled state (default: TRUE)
    enable = function(state = TRUE) {
      private$.enabled <- state
      invisible(self)
    },

    #' @description Set component name for log entries
    #' @param component Component name
    set_component = function(component) {
      private$.component <- component
      invisible(self)
    },

    #' @description Set log file path
    #' @param file Log file path
    set_file = function(file) {
      private$.file <- file
      invisible(self)
    },

    #' @description Get MCP Tools Log File Path
    #' @return Character string with log file path from environment variable or temporary file
    get_default_log_file = function() {
      Sys.getenv("MCPTOOLS_LOG_FILE", tempfile(fileext = ".txt"))
    }
  )
)
