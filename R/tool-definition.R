# Tool Definition Framework
# Core framework for defining MCP tools with R6 class structure.
# Simplified version for basic ToolDef integration without complex type system.

#' Define Tool for MCP Framework
#'
#' @title Define Tool for MCP Framework
#' @description Creates ToolDef object with name, description, and argument
#' specifications for MCP protocol integration. Simplified version for initial integration.
#'
#' @param fun The function to be invoked when the tool is called
#' @param name The name of the function. This can be omitted if fun is an existing function
#' @param description A detailed description of what the function does
#' @param arguments A named list that defines the arguments accepted by the function
#' @param annotations Additional properties that describe the tool and its behavior
#' @param convert Should JSON inputs be automatically convert to their R data type equivalents (default: TRUE)
#' @return An R6 ToolDef object
#' @examples
#' # Define a simple tool
#' my_tool <- tool(
#'   function(x) x + 1,
#'   description = "Add 1 to a number",
#'   arguments = list(x = "number")
#' )
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

  # Convert arguments to mcpr_type objects if needed
  converted_arguments <- convert_argument_types(arguments)

  check_arguments(converted_arguments, formals(fun))

  ToolDef$new(
    fun = fun,
    name = name,
    description = description,
    arguments = converted_arguments,
    convert = convert,
    annotations = annotations
  )
}

#' Convert Argument Types to MCPR Types
#'
#' @title Convert Argument Types to MCPR Types
#' @description Converts various argument type formats to standardized MCPR type objects.
#' Supports string-based type definitions, raw list formats, and existing mcpr_type objects.
#' Provides unified type conversion for consistent tool argument processing across different
#' input formats.
#'
#' @param arguments Named list of argument type definitions in various formats
#' @return Named list of mcpr_type objects
#' @noRd
convert_argument_types <- function(arguments) {
  if (!is.list(arguments) || length(arguments) == 0) {
    return(arguments)
  }

  converted_args <- list()

  for (arg_name in names(arguments)) {
    arg_spec <- arguments[[arg_name]]

    # Check if already an mcpr_type object
    if (inherits(arg_spec, "mcpr_type")) {
      converted_args[[arg_name]] <- arg_spec
      next
    }

    # Handle string-based type definitions
    if (is.character(arg_spec) && length(arg_spec) == 1) {
      converted_args[[arg_name]] <- map_type_schema(arg_spec, input_type = "definition")
      next
    }

    # Handle raw list format (backward compatibility)
    if (is.list(arg_spec) && !is.null(arg_spec$type)) {
      description <- arg_spec$description %||% ""
      converted_args[[arg_name]] <- map_type_schema(arg_spec$type, description = description, input_type = "definition")
      next
    }

    # Default fallback - treat as string
    cli::cli_warn("Unknown argument type specification for '{arg_name}', defaulting to string")
    converted_args[[arg_name]] <- map_type_schema("string", input_type = "definition")
  }

  converted_args
}

#' Tool Definition
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
#' @noRd
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
        if (!is.list(value)) {
          cli::cli_abort("Property {.field annotations} must be a list, not {.obj_type_friendly {value}}")
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @param reason A character string describing the reason for rejecting the tool call
#' @return Throws an error of class mcpr_tool_reject with the provided reason
#' @noRd
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
#' @noRd
unique_tool_name <- function() {
  the$cur_tool_id <- (the$cur_tool_id %||% 0) + 1
  sprintf("tool_%03d", the$cur_tool_id)
}
