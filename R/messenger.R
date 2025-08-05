#' @title MCP Message Handler
#' @description
#' Unified message passing system for the MCPR package that consolidates all
#' JSON-RPC communication patterns: client-server via processx, server-session 
#' via nanonext sockets, and type-aware data flow with JSON serialization.
#'
#' @details
#' This class replaces scattered protocol functions with a single, cohesive
#' message handling system that provides:
#' \itemize{
#'   \item Unified JSON-RPC message creation and handling
#'   \item Transport abstraction (processx, nanonext, stdio)
#'   \item Type-aware serialization/deserialization
#'   \item Centralized communication logging
#'   \item Protocol compliance validation
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' messenger <- MessageHandler$new()
#' response <- messenger$create_response(id = 1, result = "success")
#' 
#' # With logging
#' messenger <- MessageHandler$new(logger = function(msg) cat(msg, "\n"))
#' 
#' # Process communication
#' request <- messenger$create_tool_request(1, "read_file", list(path = "/tmp/file"))
#' response <- messenger$send_receive(process, request)
#' }
#'
#' @export
MessageHandler <- R6::R6Class("MessageHandler",
  private = list(
    .logger = NULL,
    .timeout_seconds = 20,
    .retry_delay = 0.2,
    
    # Log communication message
    log_message = function(direction, message, context = NULL) {
      if (!is.null(private$.logger)) {
        prefix <- paste0(direction, if (!is.null(context)) paste0(" (", context, ")") else "", ": ")
        private$.logger(paste0(prefix, message))
      }
    },
    
    # Validate JSON-RPC structure
    validate_jsonrpc = function(obj, require_method = FALSE) {
      if (!is.list(obj)) return(FALSE)
      if (is.null(obj$jsonrpc) || obj$jsonrpc != "2.0") return(FALSE)
      if (require_method && is.null(obj$method)) return(FALSE)
      TRUE
    }
  ),
  
  public = list(
    #' @description Initialize MessageHandler
    #' @param logger Optional logging function
    #' @param timeout_seconds Timeout for message operations (default: 20)
    #' @param retry_delay Delay between retries in seconds (default: 0.2)
    initialize = function(logger = NULL, timeout_seconds = 20, retry_delay = 0.2) {
      private$.logger <- logger
      private$.timeout_seconds <- timeout_seconds
      private$.retry_delay <- retry_delay
    },
    
    #' @description Create JSON-RPC 2.0 response
    #' @param id Request ID
    #' @param result Success result (optional)
    #' @param error Error object (optional)
    #' @return JSON-RPC response list
    create_response = function(id, result = NULL, error = NULL) {
      if (!xor(is.null(result), is.null(error))) {
        warning("Either `result` or `error` must be provided, but not both.")
      }
      
      drop_nulls(list(
        jsonrpc = "2.0",
        id = id,
        result = result,
        error = error
      ))
    },
    
    #' @description Create MCP tool call request
    #' @param id Request ID
    #' @param tool Tool name
    #' @param arguments Tool arguments
    #' @return JSON-RPC request list
    create_tool_request = function(id, tool, arguments) {
      params <- if (length(arguments) == 0) {
        list(name = tool)
      } else {
        list(name = tool, arguments = arguments)
      }
      
      list(
        jsonrpc = "2.0",
        id = id,
        method = "tools/call",
        params = params
      )
    },
    
    #' @description Create capabilities response for MCP handshake
    #' @return Capabilities list
    create_capabilities = function() {
      list(
        protocolVersion = "2024-11-05",
        capabilities = list(
          prompts = named_list(listChanged = FALSE),
          resources = named_list(subscribe = FALSE, listChanged = FALSE),
          tools = named_list(listChanged = FALSE)
        ),
        serverInfo = list(
          name = "R mcptools server",
          version = "0.0.1"
        ),
        instructions = "This provides information about a running R session."
      )
    },
    
    #' @description Create initialization request
    #' @param client_name Client name (default: "MCP Test Client")
    #' @param client_version Client version (default: "0.1.0")
    #' @return Initialization request list
    create_initialize_request = function(client_name = "MCP Test Client", client_version = "0.1.0") {
      list(
        jsonrpc = "2.0",
        id = 1,
        method = "initialize",
        params = list(
          protocolVersion = "2024-11-05",
          capabilities = list(tools = list(listChanged = FALSE)),
          clientInfo = list(name = client_name, version = client_version)
        )
      )
    },
    
    #' @description Create tools list request
    #' @param id Request ID (default: 2)
    #' @return Tools list request
    create_tools_list_request = function(id = 2) {
      list(
        jsonrpc = "2.0",
        id = id,
        method = "tools/list"
      )
    },
    
    #' @description Send message and receive response via processx
    #' @param process processx process object
    #' @param message Message to send
    #' @param context Optional context for logging
    #' @param max_attempts Maximum retry attempts
    #' @param retry_delay Delay between retries
    #' @return Parsed JSON response or NULL on timeout
    send_receive = function(process, message, context = NULL, 
                           max_attempts = NULL, retry_delay = NULL) {
      max_attempts <- max_attempts %||% private$.timeout_seconds / private$.retry_delay
      retry_delay <- retry_delay %||% private$.retry_delay
      
      json_msg <- jsonlite::toJSON(message, auto_unbox = TRUE)
      private$log_message("SEND", json_msg, context)
      
      process$write_input(paste0(json_msg, "\n"))
      
      output <- NULL
      attempts <- 0
      
      while (length(output) == 0 && attempts < max_attempts) {
        Sys.sleep(retry_delay)
        output <- process$read_output_lines()
        attempts <- attempts + 1
      }
      
      if (!is.null(output) && length(output) > 0) {
        private$log_message("RECEIVE", output[1], context)
        return(jsonlite::parse_json(output[1]))
      }
      
      private$log_message("TIMEOUT", paste("No response after", attempts, "attempts"), context)
      return(NULL)
    },
    
    #' @description Output JSON to stdout
    #' @param obj Object to output as JSON
    output_json = function(obj) {
      nanonext::write_stdout(to_json(obj))
    },
    
    #' @description Parse incoming JSON-RPC message
    #' @param line Raw message line
    #' @return Parsed JSON object or NULL if invalid
    parse_message = function(line) {
      if (length(line) == 0) return(NULL)
      
      tryCatch(
        jsonlite::parse_json(line),
        error = function(e) {
          private$log_message("ERROR", paste("Failed to parse:", conditionMessage(e)))
          NULL
        }
      )
    },
    
    #' @description Validate and route incoming message
    #' @param data Parsed message data
    #' @param handlers Named list of method handlers
    #' @return Response object or NULL
    route_message = function(data, handlers) {
      if (!private$validate_jsonrpc(data)) {
        return(self$create_response(
          data$id %||% NULL,
          error = list(code = -32600, message = "Invalid Request")
        ))
      }
      
      method <- data$method
      if (is.null(method)) return(NULL)
      
      handler <- handlers[[method]]
      if (is.null(handler)) {
        return(self$create_response(
          data$id,
          error = list(code = -32601, message = "Method not found")
        ))
      }
      
      tryCatch(
        handler(data),
        error = function(e) {
          self$create_response(
            data$id,
            error = list(code = -32603, message = conditionMessage(e))
          )
        }
      )
    },
    
    #' @description Create error response
    #' @param id Request ID
    #' @param code Error code
    #' @param message Error message
    #' @return Error response object
    create_error = function(id, code, message) {
      self$create_response(
        id = id,
        error = list(code = code, message = message)
      )
    },
    
    #' @description Handle session discovery ping
    #' @return Session description
    handle_discovery_ping = function() {
      # This would call describe_session() if available
      list(
        type = "text",
        text = "R Session Available"
      )
    }
  )
)

#' Create MessageHandler instance (convenience function)
#'
#' @description
#' Creates a new MessageHandler instance with optional logging.
#' This function provides a simple interface for creating message handlers
#' with common configurations.
#'
#' @param logger Optional logging function
#' @param timeout_seconds Timeout for operations (default: 20)
#' @param retry_delay Delay between retries (default: 0.2)
#' @return MessageHandler instance
#'
#' @examples
#' \dontrun{
#' # Simple messenger
#' messenger <- create_messenger()
#' 
#' # With file logging
#' messenger <- create_messenger(logger = function(msg) {
#'   cat(msg, "\n", file = "mcp.log", append = TRUE)
#' })
#' 
#' # With custom timeouts
#' messenger <- create_messenger(timeout_seconds = 30, retry_delay = 0.5)
#' }
#'
#' @export
create_messenger <- function(logger = NULL, timeout_seconds = 20, retry_delay = 0.2) {
  MessageHandler$new(
    logger = logger,
    timeout_seconds = timeout_seconds,
    retry_delay = retry_delay
  )
}
