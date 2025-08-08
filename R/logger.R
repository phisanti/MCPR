
# MCPR Logging System
# Minimal R6-based logging system for MCPR package debugging and monitoring.
# Provides flexible log levels and output formatting for development workflows.

#' @title MCPRLogger - Minimal Flexible Logging
#' @description Ultra-compact R6 logger focused on efficiency and flexibility
#' @export
MCPRLogger <- R6::R6Class("MCPRLogger",
  private = list(
    .file = NULL,
    .enabled = TRUE,
    .component = "MCPR",
    
    write = function(level, message, component = NULL) {
      if (!private$.enabled) return(invisible())
      
      comp <- component %||% private$.component
      entry <- sprintf("[%s] [%s] [%s] %s\n", 
                      format(Sys.time(), "%H:%M:%S"), level, comp, message)
      cat(entry, file = private$.file, append = TRUE)
    }
  ),
  
  public = list(
    initialize = function(file = NULL, component = "MCPR", enabled = TRUE) {
      private$.file <- file %||% mcptools_log_file()
      private$.component <- component
      private$.enabled <- enabled
    },
    
    # Core logging method - everything goes through this
    log = function(message, level = "I", component = NULL) {
      private$write(level, message, component)
      invisible(self)
    },
    
    # Fluent interface methods
    info = function(msg, comp = NULL) self$log(msg, "INFO", comp),
    warn = function(msg, comp = NULL) self$log(msg, "WARNING", comp),
    error = function(msg, comp = NULL) self$log(msg, "ERRO", comp),
    debug = function(msg, comp = NULL) self$log(msg, "DEBUG", comp),
    comm = function(msg, comp = NULL) self$log(msg, "COMMUNICATION", comp),
    
    # Configuration
    enable = function(state = TRUE) { private$.enabled <- state; invisible(self) },
    set_component = function(component) { private$.component <- component; invisible(self) },
    set_file = function(file) { private$.file <- file; invisible(self) }
  )
)
