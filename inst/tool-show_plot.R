# Show Plot Tool
# Routes plotting requests to user-facing or agent-facing rendering paths.
# User path delegates to R/plot-render.R primitives; agent path uses shared device/encoding helpers.

#' Create and display R plots
#'
#' @description Create and display R plots. By default (target='user'), the plot
#' is shown to the user via the active graphics device — the user sees it
#' directly. Set target='agent' when YOU (the agent) need to see and analyze the
#' plot as an image — returns a base64-encoded image optimized for token
#' efficiency. The user does NOT see agent-targeted plots.
#'
#' The width, height, format, token_limit, and warn_threshold parameters only
#' apply when target='agent'. For target='user', the plot is simply printed to
#' the user's active graphics device at its default size.
#'
#' @param expr R code expression to generate the plot
#' @param target Who should see the plot: 'user' (default) prints to the active
#'   graphics device for the user to see; 'agent' returns a base64-encoded image
#'   for agent analysis
#' @param width Width in pixels (agent only, default: 600)
#' @param height Height in pixels (agent only, default: 450)
#' @param format Output format: 'png', 'jpeg', 'pdf', or 'svg' (agent only, default: 'png')
#' @param token_limit Maximum allowed tokens (agent only, default: 25000)
#' @param warn_threshold Token threshold for optimization warnings (agent only, default: 20000)
#' @keywords mcpr_tool
#' @return For target='user': a text confirmation. For target='agent': image
#'   response with base64-encoded plot and optimization metadata.
show_plot <- function(expr, target = "user", width = 600, height = 450, format = "png",
                      token_limit = 25000, warn_threshold = 20000) {
  # Validate inputs
  if (!is.character(expr) || length(expr) != 1) {
    cli::cli_abort("Expression must be a single character string")
  }

  if (nchar(trimws(expr)) == 0) {
    cli::cli_abort("Expression cannot be empty")
  }

  valid_targets <- c("user", "agent")
  if (!target %in% valid_targets) {
    cli::cli_abort("Target must be one of: {paste(valid_targets, collapse = ', ')}")
  }

  if (target == "user") {
    return(show_plot_user(expr))
  }

  # target == "agent"
  show_plot_agent(expr, width, height, format, token_limit, warn_threshold)
}

# Tool-level MCP Apps binding so clients can discover the UI resource via tools/list.
.show_plot_annotations <- list(
  `_meta` = list(
    ui = list(resourceUri = "ui://mcpr/plots")
  )
)

#' Gather MCP App routing context for show_plot diagnostics
#'
#' @return Named list with request-scoped routing signals
#' @noRd
.show_plot_request_context <- function() {
  MCPR:::get_mcpr_request_context()
}

#' Check if the current request originates from an MCP Apps-capable client
#'
#' Delegates to the package-level function when available, falling back to FALSE.
#'
#' @return Logical
#' @noRd
.mcp_apps_supported <- function() {
  ctx <- .show_plot_request_context()
  identical(ctx$mcpr_interface, "mcp_app") || isTRUE(ctx$mcp_apps_supported)
}

#' Display a plot to the user via the best available channel
#'
#' Routes through MCP App (structuredContent) when supported, otherwise
#' delegates to local device rendering via R/plot-render.R primitives.
#'
#' @param expr R code expression to generate the plot
#' @return A text confirmation message or MCP App structured response
#' @noRd
show_plot_user <- function(expr) {
  # Two-level routing: first split on client type, then on plot type.
  # MCP App clients get structuredContent; local clients get device rendering.
  if (.mcp_apps_supported()) {
    return(show_plot_via_mcp_app(expr))
  }

  tryCatch(
    show_plot_local(expr),
    error = function(e) {
      cli::cli_abort("Error displaying plot: {e$message}")
    }
  )
}

#' Display a plot via local device rendering
#'
#' Evaluates the expression, checks for htmlwidgets, and delegates
#' static plots to MCPR:::render_static_plot().
#'
#' @param expr R code expression to generate the plot
#' @return A text confirmation message
#' @noRd
show_plot_local <- function(expr) {
  # Eval once up front for type detection: widgets go to the browser,
  # everything else routes to a graphics device via render_static_plot().
  # We pass the result through so printable objects (gg, grob) aren't re-evaluated.
  result <- MCPR:::eval_plot_expr(expr)

  if (inherits(result, c("htmlwidget", "plotly"))) {
    path <- MCPR:::show_widget_in_browser(result)
    return(list(
      type = "text",
      content = sprintf("Interactive widget opened in browser: %s", path)
    ))
  }

  device_channel <- MCPR:::detect_local_device()
  render_result <- MCPR:::render_static_plot(expr, device_channel, result = result)

  list(
    type = "text",
    content = switch(render_result$channel,
      httpgd = sprintf("Plot displayed to user via httpgd at %s", render_result$info),
      device = render_result$info,
      file = sprintf("Plot saved to file: %s (headless environment, no display available).", render_result$info)
    )
  )
}

#' Display a plot via MCP App inline viewer
#'
#' Captures static plots as PNG and returns a single MCP content item with
#' response-level UI metadata. If the expression yields an htmlwidget/plotly
#' object, delegates to show_plotly_via_mcp_app() for interactive rendering.
#'
#' @param expr R code expression to generate the plot
#' @return A single content item descriptor for encode_tool_results()
#' @noRd
show_plot_via_mcp_app <- function(expr) {
  # tmp needed before eval because PNG device must be open to capture side-effect plots
  tmp <- tempfile(fileext = ".png")
  device_open <- FALSE
  on.exit({
    if (device_open && grDevices::dev.cur() != 1) {
      try(grDevices::dev.off(), silent = TRUE)
    }
    unlink(tmp)
  }, add = TRUE)

  # Open device BEFORE eval so side-effect plots are captured
  grDevices::png(tmp, width = 800, height = 600)
  device_open <- TRUE

  result <- tryCatch(
    MCPR:::eval_plot_expr(expr),
    error = function(e) {
      cli::cli_abort("Error evaluating plot expression: {e$message}")
    }
  )

  # Delegate interactive plots to the plotly path
  if (inherits(result, c("htmlwidget", "plotly"))) {
    grDevices::dev.off()
    device_open <- FALSE
    return(show_plotly_via_mcp_app(result))
  }

  if (inherits(result, c("gg", "ggplot", "grob", "gtable", "trellis", "recordedplot"))) {
    print(result)
  }
  grDevices::dev.off()
  device_open <- FALSE

  # Base64-encode the PNG — guard against empty/partial files
  file_size <- file.info(tmp)$size
  if (is.na(file_size) || file_size == 0) {
    cli::cli_abort("Plot produced an empty image file — the expression may not generate visible output.")
  }
  raw_data <- readBin(tmp, "raw", file_size)
  b64_data <- base64enc::base64encode(raw_data)

  list(
    content = list(list(
      type = "text",
      text = "This tool call rendered a plot in the viewer.",
      annotations = list(audience = list("assistant"))
    )),
    structuredContent = list(
      kind = "image",
      mimeType = "image/png",
      data = b64_data
    )
  )
}

#' Display a plotly/htmlwidget via MCP App inline viewer
#'
#' Builds the plotly spec and returns it in `structuredContent` for the MCP App
#' viewer to render interactively.
#'
#' @param widget A plotly or htmlwidget object
#' @return A single content item descriptor for encode_tool_results()
#' @noRd
show_plotly_via_mcp_app <- function(widget) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    cli::cli_abort("plotly package is required for interactive chart rendering")
  }

  built <- plotly::plotly_build(widget)

  spec <- list(
    data   = built$x$data,
    layout = built$x$layout,
    config = built$x$config
  )

  list(
    content = list(list(
      type = "text",
      text = "This tool call rendered an interactive widget in the viewer.",
      annotations = list(audience = list("assistant"))
    )),
    structuredContent = list(
      kind = "plotly",
      spec = spec
    )
  )
}

#' Render a plot for agent analysis as base64-encoded image
#'
#' @param expr R code expression to generate the plot
#' @param width Width in pixels
#' @param height Height in pixels
#' @param format Output format
#' @param token_limit Maximum allowed tokens
#' @param warn_threshold Token threshold for optimization warnings
#' @return Image response with base64-encoded plot and optimization metadata
#' @noRd
show_plot_agent <- function(expr, width = 600, height = 450, format = "png",
                            token_limit = 25000, warn_threshold = 20000) {
  # Validate format
  valid_formats <- c("png", "jpeg", "pdf", "svg")
  if (!format %in% valid_formats) {
    cli::cli_abort("Format must be one of: {paste(valid_formats, collapse = ', ')}")
  }

  # Validate dimensions
  if (!is.numeric(width) || width <= 0) {
    cli::cli_abort("Width must be a positive number")
  }
  if (!is.numeric(height) || height <= 0) {
    cli::cli_abort("Height must be a positive number")
  }

  # Convert to integer
  width <- as.integer(width)
  height <- as.integer(height)

  tryCatch(
    {
      # Set up graphics device (httpgd if available, standard otherwise)
      device_info <- MCPR:::setup_graphics_device(format, width, height)

      # Execute the plotting code
      result <- MCPR:::eval_plot_expr(expr)

      # Get the plot data with actual token count
      image_response <- MCPR:::get_plot_data(device_info, format, width, height)
      actual_tokens <- image_response$tokens

      # Check if plot exceeds token limits
      if (actual_tokens > token_limit) {
        suggestions <- MCPR:::generate_optimization_suggestions(width, height, actual_tokens, token_limit, format)

        error_msg <- sprintf(
          "Plot too large for agent analysis: %s tokens exceeds %s token limit.
          Try these optimizations:
          - %s
          Or use show_plot with target='user' to display directly to the user instead.",
          format(actual_tokens, big.mark = ","),
          format(token_limit, big.mark = ","),
          paste(suggestions, collapse = "\n- ")
        )

        cli::cli_abort(error_msg)
      }

      # Generate warning for high token usage
      optimization_warning <- NULL
      if (actual_tokens > warn_threshold) {
        suggestions <- MCPR:::generate_optimization_suggestions(width, height, actual_tokens, warn_threshold, format)

        optimization_warning <- sprintf(
          "WARNING: HIGH TOKEN USAGE: %s tokens (%.1f%% of limit)
        Consider optimizing for better efficiency:
        - %s
        Or use show_plot with target='user' to display directly to the user instead.",
          format(actual_tokens, big.mark = ","),
          (actual_tokens / token_limit) * 100,
          paste(suggestions, collapse = "\n- ")
        )

        cli::cli_warn(optimization_warning)
      }

      # Add optimization metadata to response
      image_response$metadata <- list(
        actual_tokens = actual_tokens,
        dimensions = paste0(width, "x", height),
        format = format,
        optimization_applied = if (!is.null(optimization_warning)) "Warning issued" else "None",
        token_efficiency = sprintf("%.1f%% of limit used", (actual_tokens / token_limit) * 100)
      )

      # Add warning to response if applicable
      if (!is.null(optimization_warning)) {
        image_response$optimization_warning <- optimization_warning
      }

      return(image_response)
    },
    error = function(e) {
      # Clean up any active devices on error
      if (grDevices::dev.cur() != 1) {
        try(grDevices::dev.off(), silent = TRUE)
      }
      cli::cli_abort("Error creating plot: {e$message}")
    }
  )
}
