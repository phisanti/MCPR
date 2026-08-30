# Widget Screenshot Renderer
# Standalone Rscript invoked to screenshot a saved htmlwidget HTML file
# via headless Chrome (chromote); never runs inside MCPR's later/nanonext.

# Rscript render-widget.R <html> <out.png> <w> <h> <deadline_ms>
# stdout: status token (OK/NO_CHROMOTE/NO_CHROME/NO_PLOT/TIMEOUT/RENDER_ERROR).
# stderr: diagnostics.
# exit: 0 OK, 2 NO_CHROME, 3 NO_PLOT, 4 TIMEOUT, 5 NO_CHROMOTE, 6 RENDER_ERROR

# If a headless GPU stack renders blank, callers can export
# chromote::set_chrome_args("--use-angle=swiftshader") before invoking it.

# Build a file:// URL that is valid on both POSIX ("/tmp/x") and Windows
# ("C:/Users/x"), where normalizePath() yields no leading slash.
file_url <- function(path) {
  p <- normalizePath(path, winslash = "/", mustWork = TRUE)
  paste0("file://", if (startsWith(p, "/")) "" else "/", p)
}

# Set once the widget's first draw completes. Injected before navigation so the
# listener cannot attach after plotly_afterplot has already fired.
READY_HOOK <- "
window.__mcprDrawn = false;
(function () {
  var iv = setInterval(function () {
    var el = document.querySelector('.js-plotly-plot');
    if (el && typeof el.on === 'function') {
      clearInterval(iv);
      el.on('plotly_afterplot', function () { window.__mcprDrawn = true; });
    }
  }, 50);
})();
"

# Returns [node_count, drawn]. node_count counts every element in the plot
# subtree, not just '.point': line traces render as path.js-line, heatmaps as
# <image>, box as path.box, pie as path.surface -- a marker-only counter never
# leaves zero for those and wedges until the deadline.
PROBE_JS <- "(function () {
  var el = document.querySelector('.js-plotly-plot');
  if (!el) return [-1, 0];
  return [el.querySelectorAll('*').length, window.__mcprDrawn ? 1 : 0];
})()"

render_widget <- function(html, out, width, height, deadline_ms) {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    cli::cli_alert_danger("chromote package is not installed")
    return(list(token = "NO_CHROMOTE", code = 5L))
  }
  if (is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL))) {
    cli::cli_alert_danger("no Chromium-based browser found")
    return(list(token = "NO_CHROME", code = 2L))
  }

  b <- chromote::ChromoteSession$new(width = width, height = height)
  on.exit(try(b$close(), silent = TRUE), add = TRUE)

  b$Page$addScriptToEvaluateOnNewDocument(source = READY_HOOK)
  b$go_to(file_url(html))

  start <- Sys.time()
  # Scale the "widget never appeared" window with the overall budget: a large
  # widget's first paint can exceed a fixed 5s on a loaded machine.
  no_plot_by <- start + max(5, deadline_ms / 4000)
  deadline <- start + deadline_ms / 1000
  prev <- NA_real_

  repeat {
    if (Sys.time() > deadline) {
      cli::cli_alert_danger("plot did not settle within {deadline_ms}ms")
      return(list(token = "TIMEOUT", code = 4L))
    }

    probe <- b$Runtime$evaluate(PROBE_JS, returnByValue = TRUE)$result$value
    count <- if (is.null(probe)) -1 else as.numeric(probe[[1]])
    drawn <- !is.null(probe) && identical(as.numeric(probe[[2]]), 1)

    if (count < 0) {
      if (Sys.time() > no_plot_by) {
        cli::cli_alert_danger("no .js-plotly-plot element appeared in time")
        return(list(token = "NO_PLOT", code = 3L))
      }
    } else if (drawn) {
      # plotly_afterplot fired: the renderer itself says drawing is complete.
      break
    } else if (count > 0 && !is.na(prev) && count == prev) {
      # Fallback for widgets whose event never fires. count > 0 matters: the
      # div gets its class before any child nodes exist.
      break
    }

    prev <- count
    Sys.sleep(0.1)
  }

  invisible(b$screenshot(filename = out, selector = ".js-plotly-plot"))
  list(token = "OK", code = 0L)
}

args <- commandArgs(trailingOnly = TRUE)
result <- tryCatch(
  render_widget(
    html = args[[1]], out = args[[2]],
    width = as.integer(args[[3]]), height = as.integer(args[[4]]),
    deadline_ms = as.numeric(args[[5]])
  ),
  error = function(e) {
    cli::cli_alert_danger("render failed: {conditionMessage(e)}")
    list(token = "RENDER_ERROR", code = 6L)
  }
)
cat(result$token, "\n", sep = "")
quit(save = "no", status = result$code)
