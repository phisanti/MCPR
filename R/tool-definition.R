#' @include utils.R
#' @include type-conversion-utilities.R
#' @include tool-definition-validators.R

# Tool Definition Framework
# Core framework for defining MCP tools with R6 class structure and type specifications.
# Provides ToolDef class for tool creation, validation, and JSON Schema conversion.

NULL

#' Define Tool for MCP Framework
#'
#' @title Define Tool for MCP Framework
#' @description Annotates function for use in tool calls through comprehensive validation
#' and type definition. Creates ToolDef object with name, description, and argument
#' specifications for MCP protocol integration. Enables automatic JSON type conversion
#' and provides validation to ensure proper tool construction.
#'
#' @param fun The function to be invoked when the tool is called
#' @param name The name of the function. This can be omitted if fun is an existing function
#' @param description A detailed description of what the function does
#' @param arguments A named list that defines the arguments accepted by the function
#' @param annotations Additional properties that describe the tool and its behavior
#' @param convert Should JSON inputs be automatically convert to their R data type equivalents (default: TRUE)
#' @return An R6 ToolDef object
#' @examples
#' # Define a tool for drawing random numbers
#' tool_rnorm <- tool(
#'   rnorm,
#'   description = "Draw numbers from a random normal distribution",
#'   arguments = list(
#'     n = type_integer("The number of observations. Must be a positive integer."),
#'     mean = type_number("The mean value of the distribution."),
#'     sd = type_number("The standard deviation of the distribution. Must be a non-negative number.")
#'   )
#' )
#' 
#' # Call the tool directly
#' tool_rnorm$call(n = 5, mean = 0, sd = 1)
#' @export
tool <- function(
  fun,
  description,
  arguments = list(),
  name = NULL,
  convert = TRUE,
  annotations = list()
) {
  fun_expr <- rlang::enexpr(fun)
  check_function(fun)
  check_string(description)
  check_string(name, allow_null = TRUE)
  check_bool(convert)

  if (is.null(name)) {
    if (rlang::is_symbol(fun_expr)) {
      name <- as.character(fun_expr)
    } else {
      name <- unique_tool_name()
    }
  }
  validate_tool_name(name, "name")

  check_arguments(arguments, formals(fun))

  ToolDef$new(
    fun = fun,
    name = name,
    description = description,
    arguments = arguments,
    convert = convert,
    annotations = annotations
  )
}

#' Tool Definition for MCP Framework
#'
#' @title Tool Definition
#' @description Encapsulates tool metadata and execution logic for MCP protocol integration.
#' Provides comprehensive validation through active bindings, automatic type conversion,
#' and structured tool calling interface. Maintains tool properties with validation
#' and enables direct tool execution with JSON input handling.
#' @details Manages tool lifecycle through validated properties:
#' \itemize{
#'   \item \strong{Function Storage}: Maintains executable function with validation
#'   \item \strong{Metadata Management}: Stores name, description, and annotations
#'   \item \strong{Argument Validation}: Ensures proper type definitions for parameters
#'   \item \strong{Type Conversion}: Handles JSON-to-R type conversion automatically
#' }
#'
#' @param fun The function to be invoked when the tool is called
#' @param name Tool name with validation for MCP protocol compliance
#' @param description Tool description for documentation and discovery
#' @param arguments Named list of argument type definitions
#' @param convert Automatic JSON type conversion flag (default: TRUE)
#' @param annotations Additional metadata for tool behavior hints
ToolDef <- R6::R6Class("ToolDef",
  public = list(
    #' @description Initialize ToolDef with validation
    #' @param fun Function to be invoked
    #' @param name Tool name
    #' @param description Tool description
    #' @param arguments Named list of argument definitions
    #' @param convert Enable JSON type conversion
    #' @param annotations Additional tool metadata
    #' @return New ToolDef instance
    initialize = function(fun, name, description, arguments = list(), convert = TRUE, annotations = list()) {
      # Use active bindings for validation during initialization
      self$fun <- fun
      self$name <- name
      self$description <- description
      self$arguments <- arguments
      self$convert <- convert
      self$annotations <- annotations
    },
    
    #' @description Execute tool with arguments and optional type conversion
    #' @param ... Arguments to pass to the tool function
    #' @return Result of tool function execution
    call = function(...) {
      args <- list(...)
      if (self$convert) {
        args <- convert_json_types(args)
      }
      do.call(self$fun, args)
    },
    
    #' @description Print formatted tool definition with metadata
    #' @param ... Additional arguments (unused)
    #' @return Self (invisibly)
    print = function(...) {
      if (length(self$arguments) > 0) {
        fake_call <- rlang::call2(self$name, !!!rlang::syms(names(self$arguments)))
      } else {
        fake_call <- rlang::call2(self$name)
      }
      
      cli::cli_text("{.comment # <MCPR::ToolDef>} {.code {deparse1(fake_call)}}")
      cli::cli_text("{.comment # @name:} {.field {self$name}}")
      cli::cli_text("{.comment # @description:} {self$description}")
      cli::cli_text("{.comment # @convert:} {.val {self$convert}}")
      cli::cli_text("{.comment #}")
      print(self$fun)
      
      invisible(self)
    }
  ),
  private = list(
    .name = NULL,
    .description = NULL,
    .arguments = NULL,
    .convert = TRUE,
    .annotations = NULL,
    .fun = NULL
  ),
  
  active = list(
    #' @field name Tool name with MCP protocol compliance validation
    name = function(value) {
      if (missing(value)) {
        private$.name
      } else {
        validate_tool_name(value, "name")
        private$.name <- value
      }
    },
    
    #' @field description Tool description ensuring non-empty string format
    description = function(value) {
      if (missing(value)) {
        private$.description
      } else {
        validate_tool_description(value, "description")
        private$.description <- value
      }
    },
    
    #' @field arguments Argument definitions ensuring proper list structure
    arguments = function(value) {
      if (missing(value)) {
        private$.arguments
      } else {
        validate_tool_arguments(value, "arguments")
        private$.arguments <- value
      }
    },
    
    #' @field convert JSON conversion flag ensuring logical type
    convert = function(value) {
      if (missing(value)) {
        private$.convert
      } else {
        # Inline validation equivalent to validate_tool_convert
        if (!is.logical(value) || length(value) != 1 || is.na(value)) {
          cli::cli_abort("Property {.field convert} must be a single logical value, not {.obj_type_friendly {value}}")
        }
        private$.convert <- value
      }
    },
    
    #' @field annotations Tool annotations ensuring basic R type compliance
    annotations = function(value) {
      if (missing(value)) {
        private$.annotations
      } else {
        # Inline validation equivalent to validate_tool_annotations
        if (!is.list(value)) {
          cli::cli_abort("Property {.field annotations} must be a list, not {.obj_type_friendly {value}}")
        }
        if (length(value) > 0) {
          for (i in seq_along(value)) {
            element_name <- names(value)[i] %||% paste0("element_", i)
            element <- value[[i]]
            if (!is.null(element) && 
                !is.character(element) && 
                !is.logical(element) && 
                !is.numeric(element) && 
                !is.list(element)) {
              cli::cli_abort(
                "Property {.field annotations} element {.val {element_name}} must be a basic R type (character, logical, numeric, or list), not {.obj_type_friendly {element}}"
              )
            }
          }
        }
        private$.annotations <- value
      }
    },
    
    #' @field fun Executable function ensuring callable object
    fun = function(value) {
      if (missing(value)) {
        private$.fun
      } else {
        validate_tool_fun(value, "fun")
        private$.fun <- value
      }
    }
  )
)

#' Check Arguments Against Function Formals
#'
#' @title Check Arguments Against Function Formals
#' @description Validates that argument definitions match function formals ensuring proper
#' tool construction. Checks for named list format and validates argument-formal alignment
#' through name comparison. Provides comprehensive error reporting for missing or extra
#' argument definitions.
#'
#' @param arguments Named list of argument type definitions
#' @param formals Function formals to validate against
#' @param call Calling environment for error reporting
#' @return NULL (invisible) if valid, throws error if invalid
check_arguments <- function(arguments, formals, call = rlang::caller_env()) {
  if (!is.list(arguments) || !(length(arguments) == 0 || rlang::is_named(arguments))) {
    cli::cli_abort("Arguments must be a named list", call = call)
  }

  extra_args <- setdiff(names(arguments), names(formals))
  missing_args <- setdiff(names(formals), names(arguments))
  if (length(extra_args) > 0 || length(missing_args) > 0) {
    cli::cli_abort(
      c(
        "Names of {.arg arguments} must match formals of {.arg fun}",
        "*" = if (length(extra_args) > 0) {
          "Extra type definitions: {.val {extra_args}}"
        },
        "*" = if (length(missing_args) > 0) {
          "Missing type definitions: {.val {missing_args}}"
        }
      ),
      call = call
    )
  }

  invisible()
}

#' Check ToolDef Object
#'
#' @title Check ToolDef Object
#' @description Validates that object is proper ToolDef instance through R6 class checking.
#' Ensures object inheritance and type compliance for tool validation workflows.
#' Provides error handling for invalid tool objects in validation pipeline.
#'
#' @param x Object to validate as ToolDef
#' @param arg Argument name for error reporting
#' @param call Calling environment for error reporting
#' @return NULL (invisible) if valid, throws error if invalid
check_tool <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!R6::is.R6(x) || !inherits(x, "ToolDef")) {
    cli::cli_abort("{.arg {arg}} must be a <ToolDef>", call = call)
  }
}


#' Tool Annotations for MCP Protocol
#'
#' @title Tool Annotations for MCP Protocol
#' @description Creates additional properties providing tool behavior information for MCP clients.
#' Implements Model Context Protocol annotation hints for tool characteristics including
#' read-only behavior, world interaction, and operation effects. Enables enhanced tool
#' discovery and usage guidance through structured metadata.
#' @param title A human-readable title for the tool.
#' @param read_only_hint If `TRUE`, the tool does not modify its environment.
#' @param open_world_hint If `TRUE`, the tool may interact with an "open world"
#'   of external entities. If `FALSE`, the tool's domain of interaction is
#'   closed. For example, the world of a web search tool is open, but the world
#'   of a memory tool is not.
#' @param idempotent_hint If `TRUE`, calling the tool repeatedly with the same
#'   arguments will have no additional effect on its environment. (Only
#'   meaningful when `read_only_hint` is `FALSE`.)
#' @param destructive_hint If `TRUE`, the tool may perform destructive updates
#'   to its environment, otherwise it only performs additive updates. (Only
#'   meaningful when `read_only_hint` is `FALSE`.)
#' @param ... Additional named parameters to include in the tool annotations.
#'
#' @return A list of tool annotations.
#'
#' @family tool calling helpers
#' @export
tool_annotations <- function(
  title = NULL,
  read_only_hint = NULL,
  open_world_hint = NULL,
  idempotent_hint = NULL,
  destructive_hint = NULL,
  ...
) {
  if (!is.null(title)) check_string(title)
  if (!is.null(read_only_hint)) check_bool(read_only_hint)
  if (!is.null(open_world_hint)) check_bool(open_world_hint)
  if (!is.null(idempotent_hint)) check_bool(idempotent_hint)
  if (!is.null(destructive_hint)) check_bool(destructive_hint)

  compact_list(list(
    title = title,
    read_only_hint = read_only_hint,
    open_world_hint = open_world_hint,
    idempotent_hint = idempotent_hint,
    destructive_hint = destructive_hint,
    ...
  ))
}

#' Reject Tool Call
#'
#' @title Reject Tool Call
#' @description Throws structured error to reject tool call execution with custom reasoning.
#' Enables tool functions and callbacks to prevent execution through standardized error
#' handling. Provides user-controlled tool call rejection for security and workflow
#' control in MCP interactions.
#'
#' Here's an example where `utils::askYesNo()` is used to ask the user for
#' permission before accessing their current working directory. This happens
#' directly in the tool function and is appropriate when you write the tool
#' definition and know exactly how it will be called.
#'
#' ```r
#' chat <- chat_openai(model = "gpt-4.1-nano")
#'
#' list_files <- function() {
#'   allow_read <- utils::askYesNo(
#'     "Would you like to allow access to your current directory?"
#'   )
#'   if (isTRUE(allow_read)) {
#'     dir(pattern = "[.](r|R|csv)$")
#'   } else {
#'     tool_reject()
#'   }
#' }
#'
#' chat$register_tool(tool(
#'   list_files,
#'   "List files in the user's current directory"
#' ))
#'
#' chat$chat("What files are available in my current directory?")
#' #> [tool call] list_files()
#' #> Would you like to allow access to your current directory? (Yes/no/cancel) no
#' #> #> Error: Tool call rejected. The user has chosen to disallow the tool #' call.
#' #> It seems I am unable to access the files in your current directory right now.
#' #> If you can tell me what specific files you're looking for or if you can #' provide
#' #> the list, I can assist you further.
#'
#' chat$chat("Try again.")
#' #> [tool call] list_files()
#' #> Would you like to allow access to your current directory? (Yes/no/cancel) yes
#' #> #> app.R
#' #> #> data.csv
#' #> The files available in your current directory are "app.R" and "data.csv".
#' ```
#'
#' You can achieve a similar experience with tools written by others by using a
#' `tool_request` callback. In the next example, imagine the tool is provided by
#' a third-party package. This example implements a simple menu to ask the user
#' for consent before running *any*  tool.
#'
#' ```r
#' packaged_list_files_tool <- tool(
#'   function() dir(pattern = "[.](r|R|csv)$"),
#'   "List files in the user's current directory"
#' )
#'
#' chat <- chat_openai(model = "gpt-4.1-nano")
#' chat$register_tool(packaged_list_files_tool)
#'
#' always_allowed <- c()
#'
#' # ContentToolRequest
#' chat$on_tool_request(function(request) {
#'   if (request@name %in% always_allowed) return()
#'
#'   answer <- utils::menu(
#'     title = sprintf("Allow tool `%s()` to run?", request@name),
#'     choices = c("Always", "Once", "No"),
#'     graphics = FALSE
#'   )
#'
#'   if (answer == 1) {
#'     always_allowed <<- append(always_allowed, request@name)
#'   } else if (answer %in% c(0, 3)) {
#'     tool_reject()
#'   }
#' })
#'
#' # Try choosing different answers to the menu each time
#' chat$chat("What files are available in my current directory?")
#' chat$chat("How about now?")
#' chat$chat("And again now?")
#' ```
#'
#' @param reason A character string describing the reason for rejecting the tool call
#' @return Throws an error of class mcpr_tool_reject with the provided reason
#'
#' @family tool calling helpers
#' @export
tool_reject <- function(
  reason = "The user has chosen to disallow the tool call."
) {
  check_string(reason)

  rlang::abort(
    paste("Tool call rejected.", reason),
    class = "mcpr_tool_reject"
  )
}


#' Generate Unique Tool Name
#'
#' @title Generate Unique Tool Name
#' @description Generates unique sequential tool names for anonymous tool definitions.
#' Maintains global counter for tool naming consistency and provides fallback naming
#' for tools without explicit names. Ensures unique identification across tool
#' registry and MCP protocol interactions.
#'
#' @return Character string with unique tool name format
unique_tool_name <- function() {
  the$cur_tool_id <- (the$cur_tool_id %||% 0) + 1
  sprintf("tool_%03d", the$cur_tool_id)
}