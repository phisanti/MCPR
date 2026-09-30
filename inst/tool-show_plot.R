# Show Plot Tool
# Thin wrapper that resolves a plot variable name from .GlobalEnv and delegates to channel_plot().
# All routing logic lives in R/plot-render.R; this file owns tool description and input validation.

#' Display an R plot object
#'
#' @description Display an R plot object. Pass the NAME of an existing variable - not R code - or a recorded plot reference.
#'
#' Recorded plots: every plot page drawn by execute_r_code (and by the user at
#' the console of a joined session) is recorded and reported as plot:N. Show one
#' with show_plot(plot = "plot:N") or the newest with show_plot(plot = "plot:last").
#' Recorded plots re-render at any width/height.
#'
#' Plot objects - two-step workflow:
#' 1. Create the plot: execute_r_code("my_plot <- ggplot(mtcars, aes(mpg, wt)) + geom_point()")
#' 2. Display it: show_plot(plot = "my_plot")
#'
#' For base R graphics and other side-effect plotting workflows, capture the
#' plotting expression first:
#' 1. execute_r_code("base_plot <- MCPR::capture_plot(plot(cars))")
#' 2. show_plot(plot = "base_plot")
#'
#' This also works for multi-step side-effect plots such as:
#' 1. execute_r_code("model_plot <- MCPR::capture_plot({ model <- lm(dist ~ speed, data = cars); plot(model) })")
#' 2. show_plot(plot = "model_plot")
#'
#' By default (target='user'), the plot is shown to the user. Set target='agent'
#' when YOU need to analyze the plot as an image.
#'
#' @param plot string Name of a variable in the R session containing a plot object
#'   (ggplot, plotly, recordedplot, grob, gtable, trellis, htmlwidget,
#'   or captured_plot), or a recorded plot reference such as "plot:3" or "plot:last"
#' @param target string Who sees the plot: 'user' (default) or 'agent'
#' @param width integer Optional render width in pixels (100-4000) for target='agent' and inline viewers. Default 600 for agent images.
#' @param height integer Optional render height in pixels (100-4000) for target='agent' and inline viewers. Default 450 for agent images.
#' @keywords mcpr_tool
#' @return For target='user': a text confirmation or structuredContent. For target='agent': an
#'   image block, followed by a text warning when token usage is high.
show_plot <- function(plot, target = "user", width = NULL, height = NULL) {
  # Validate: must be a single non-empty string
  if (!is.character(plot) || length(plot) != 1 || nchar(trimws(plot)) == 0) {
    cli::cli_abort("`plot` must be a non-empty variable name string")
  }

  valid_targets <- c("user", "agent")
  if (!target %in% valid_targets) {
    cli::cli_abort("Target must be one of: {paste(valid_targets, collapse = ', ')}")
  }
  mcp_apps <- .mcp_apps_supported()

  # Recorded plot reference: replay the stored page
  plot <- trimws(plot)
  if (grepl(MCPR:::PLOT_REF_PATTERN, plot)) {
    entry <- MCPR:::plot_store_get(plot)
    if (is.null(entry)) {
      recorded <- MCPR:::plot_store_list()$id
      cli::cli_abort(c(
        "No recorded plot {.val {plot}} in this session.",
        "i" = if (length(recorded)) {
          "Recorded plots: {paste0('plot:', recorded, collapse = ', ')}."
        } else {
          "No plots have been recorded yet; draw one with execute_r_code."
        }
      ))
    }
    return(MCPR::channel_plot(
      entry$record,
      mcp_apps_supported = mcp_apps, target = target,
      width = width, height = height
    ))
  }

  # Expression detection: if it looks like code, guide the agent
  expr_patterns <- c("(", "+", "<-", "=", "{", "~", "::", "%%")
  if (any(vapply(expr_patterns, function(p) grepl(p, plot, fixed = TRUE), logical(1)))) {
    return(list(
      type = "text",
      content = paste0(
        "It looks like you passed an R expression instead of a variable name. ",
        "The show_plot tool expects the NAME of an existing plot object. ",
        "If the expression returns a plot object, create it first with execute_r_code:\n\n",
        "  execute_r_code(\"my_plot <- ", plot, "\")\n\n",
        "If the expression draws through side effects, such as plot(cars) or ",
        "{ model <- lm(...); plot(model) }, capture it first:\n\n",
        "  execute_r_code(\"my_plot <- MCPR::capture_plot(", plot, ")\")\n\n",
        "Then display it:\n\n",
        "  show_plot(plot = \"my_plot\")"
      )
    ))
  }

  # Resolve variable name from .GlobalEnv
  if (!exists(plot, envir = .GlobalEnv)) {
    cli::cli_abort("Variable '{plot}' not found in the R session. Create it first with execute_r_code.")
  }
  plot_obj <- get(plot, envir = .GlobalEnv)

  # Delegate to channel_plot
  MCPR::channel_plot(
    plot_obj,
    mcp_apps_supported = mcp_apps, target = target,
    width = width, height = height
  )
}

# Tool-level MCP Apps binding so clients can discover the UI resource via tools/list.
.show_plot_annotations <- list(
  `_meta` = list(
    ui = list(resourceUri = MCPR:::MCPR_PLOT_VIEWER_RESOURCE_URI)
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
