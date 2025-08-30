# MCPR Base Class Framework
# Provides foundational R6 class for shared functionality across MCPR components.
# Implements controlled global state management, logging, and resource cleanup patterns.

#' @title BaseMCPR - Foundation Class for MCPR Components
#' @description
#' Base R6 class providing common functionality for all MCPR components including
#' controlled global state management, standardized logging, and automatic resource
#' cleanup. Designed to eliminate scattered global state access while preserving
#' inter-process communication capabilities essential for MCP protocol operation.
#'
#' @details Key Features:
#' \itemize{
#'   \item \strong{State Management}: Controlled access to global `the` environment
#'   \item \strong{Logging Integration}: Automatic logger setup with component identification
#'   \item \strong{Resource Cleanup}: Automatic registration and cleanup of resources
#'   \item \strong{Socket Utilities}: Streamlined socket creation with cleanup
#'   \item \strong{State Ownership}: Tracks and manages owned global state keys
#' }
#'
#' @param component_name Component identifier for logging (e.g., "SERVER", "SESSION", "CLIENT")
#' @noRd
BaseMCPR <- R6::R6Class("BaseMCPR",
  public = list(
    #' @description Initialize base MCPR functionality with component identification
    #' @param component_name Component identifier for logging and state management
    #' @return New BaseMCPR instance
    initialize_base = function(component_name) {
      if (missing(component_name) || !is.character(component_name) || length(component_name) != 1) {
        cli::cli_abort("component_name must be a single character string")
      }

      private$.component_name <- component_name
      private$.logger <- MCPRLogger$new(component = component_name)
      private$.cleanup_registry <- list()
      private$.state_keys <- character(0)
      private$.initialized <- TRUE

      # Register automatic cleanup on finalization
      reg.finalizer(self, function(x) x$cleanup_all(), onexit = TRUE)

      private$.logger$debug(sprintf("BASE_INIT: %s component initialized", component_name))
      invisible(self)
    },

    # ============ STATE MANAGEMENT ============

    #' @description Get value from global state with optional default
    #' @param key State key to retrieve
    #' @param default Default value if key doesn't exist
    #' @return Value from global state or default
    state_get = function(key, default = NULL) {
      private$check_initialized()
      if (exists(key, envir = the) && !is.null(the[[key]])) {
        value <- the[[key]]
        private$.logger$debug(sprintf(
          "STATE_GET: %s -> %s", key,
          if (is.null(value)) "NULL" else class(value)[1]
        ))
        return(value)
      } else {
        private$.logger$debug(sprintf(
          "STATE_GET: %s -> DEFAULT (%s)", key,
          if (is.null(default)) "NULL" else class(default)[1]
        ))
        return(default)
      }
    },

    #' @description Set value in global state and track ownership
    #' @param key State key to set
    #' @param value Value to store
    #' @return Self (invisibly) for method chaining
    state_set = function(key, value) {
      private$check_initialized()
      the[[key]] <- value
      private$.state_keys <- unique(c(private$.state_keys, key))
      private$.logger$debug(sprintf(
        "STATE_SET: %s <- %s", key,
        if (is.null(value)) "NULL" else class(value)[1]
      ))
      invisible(self)
    },

    #' @description Check if key exists in global state and has non-NULL value
    #' @param key State key to check
    #' @return TRUE if key exists and is not NULL, FALSE otherwise
    state_has = function(key) {
      private$check_initialized()
      exists(key, envir = the) && !is.null(the[[key]])
    },

    #' @description Remove key from global state and ownership tracking
    #' @param key State key to clear
    #' @return Self (invisibly) for method chaining
    state_clear = function(key) {
      private$check_initialized()
      if (exists(key, envir = the)) {
        the[[key]] <- NULL
        private$.state_keys <- setdiff(private$.state_keys, key)
        private$.logger$debug(sprintf("STATE_CLEAR: %s", key))
      }
      invisible(self)
    },

    #' @description Get list of state keys owned by this instance
    #' @return Character vector of owned state keys
    state_keys_owned = function() {
      private$check_initialized()
      private$.state_keys
    },


    # ============ RESOURCE MANAGEMENT ============

    #' @description Register cleanup function for automatic resource management
    #' @param cleanup_fn Function to call during cleanup (no arguments)
    #' @param description Description of the resource for logging
    #' @return Self (invisibly) for method chaining
    register_cleanup = function(cleanup_fn, description = "resource") {
      private$check_initialized()
      if (!is.function(cleanup_fn)) {
        cli::cli_abort("cleanup_fn must be a function")
      }

      private$.cleanup_registry[[length(private$.cleanup_registry) + 1]] <- list(
        fn = cleanup_fn,
        desc = description
      )
      private$.logger$debug(sprintf("CLEANUP_REGISTER: %s", description))
      invisible(self)
    },

    #' @description Execute all registered cleanup functions in LIFO order
    #' @return Self (invisibly) for method chaining
    cleanup_all = function() {
      if (!private$.initialized) {
        return(invisible(self))
      }

      private$.logger$debug(sprintf(
        "CLEANUP_START: %d resources, %d state keys",
        length(private$.cleanup_registry), length(private$.state_keys)
      ))

      # Execute cleanup functions in reverse order (LIFO)
      for (i in rev(seq_along(private$.cleanup_registry))) {
        cleanup <- private$.cleanup_registry[[i]]
        tryCatch(
          {
            cleanup$fn()
            private$.logger$debug(sprintf("CLEANUP_SUCCESS: %s", cleanup$desc))
          },
          error = function(e) {
            private$.logger$warn(sprintf("CLEANUP_ERROR: %s - %s", cleanup$desc, e$message))
          }
        )
      }

      # Clean up owned global state
      for (key in private$.state_keys) {
        self$state_clear(key)
      }

      # Reset cleanup registry
      private$.cleanup_registry <- list()
      private$.state_keys <- character(0)

      private$.logger$debug("CLEANUP_COMPLETE")
      invisible(self)
    },

    # ============ SOCKET UTILITIES ============

    #' @description Create nanonext socket with automatic cleanup registration
    #' @param type Socket type (default: "poly")
    #' @param description Description for cleanup logging
    #' @return nanonext socket object
    create_socket = function(type = "poly", description = "socket") {
      private$check_initialized()
      socket <- nanonext::socket(type)

      # Register automatic cleanup
      self$register_cleanup(
        function() {
          tryCatch(
            {
              nanonext::reap(socket)
            },
            error = function(e) {
              # Socket may already be closed, ignore errors
            }
          )
        },
        description = sprintf("%s_%s", description, type)
      )

      private$.logger$debug(sprintf("SOCKET_CREATE: %s (%s)", description, type))
      socket
    },

    #' @description Generate socket URL using global socket_url pattern
    #' @param id Socket identifier number
    #' @return Formatted socket URL string
    socket_url = function(id) {
      private$check_initialized()
      base_url <- self$state_get("socket_url")
      if (is.null(base_url)) {
        base_url <- get_system_socket_url()
        private$.logger$warn(sprintf("Socket URL not found in state, using system default: %s", base_url))
      }
      url <- sprintf("%s%d", base_url, id)
      private$.logger$debug(sprintf("SOCKET_URL: %s", url))
      url
    },

    # ============ COMPONENT UTILITIES ============

    #' @description Get component name for this instance
    #' @return Component name string
    get_component_name = function() {
      private$check_initialized()
      private$.component_name
    },

    #' @description Check if base functionality is properly initialized
    #' @return TRUE if initialized, FALSE otherwise
    is_initialized = function() {
      !is.null(private$.initialized) && private$.initialized
    }
  ),
  private = list(
    .logger = NULL,
    .component_name = NULL,
    .cleanup_registry = NULL,
    .state_keys = NULL,
    .initialized = FALSE,

    # Check that initialize_base was called
    check_initialized = function() {
      if (!private$.initialized) {
        cli::cli_abort("BaseMCPR not initialized. Call initialize_base() first.")
      }
    },

    # ============ PRIVATE LOGGER INTERFACE ============

    # Get the logger instance for this component
    get_logger = function() {
      private$check_initialized()
      private$.logger
    },

    # Log info message
    log_info = function(msg) {
      private$check_initialized()
      private$.logger$info(msg)
      invisible(self)
    },

    # Log warning message
    log_warn = function(msg) {
      private$check_initialized()
      private$.logger$warn(msg)
      invisible(self)
    },

    # Log error message
    log_error = function(msg) {
      private$check_initialized()
      private$.logger$error(msg)
      invisible(self)
    },

    # Log debug message
    log_debug = function(msg) {
      private$check_initialized()
      private$.logger$debug(msg)
      invisible(self)
    },

    # Log communication message with optional source prefix
    log_comm = function(source = "", msg) {
      private$check_initialized()
      full_msg <- if (nzchar(source)) {
        paste0(source, ": ", msg)
      } else {
        msg
      }
      private$.logger$comm(full_msg)
      invisible(self)
    }
  )
)
