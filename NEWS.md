# MCPR 0.9.83

* Fixed `channel_plot(target = "agent")` silently returning a blank image for
  `plotly`/`htmlwidget` plots. `.channel_plot_agent()` had no branch for
  interactive widgets, so the print step was skipped, the untouched graphics
  device was captured, and the agent received a plausible-looking but empty
  PNG with no error. Interactive plots now render at 900x650 through an
  isolated headless-Chrome subprocess and are encoded the same way as static
  plots; a zero-byte or oversized capture now aborts loudly instead of
  reaching the model silently.
* Corrected agent-image token accounting from base64 text length
  (`nchar(base64) / 4`) to a pixel-based estimate (`width * height / 750`),
  which reflects what a vision model is actually charged for an image block
  rather than the length of its base64 encoding. The token limit and warning
  threshold moved to 3000/2000 to stay meaningful in those units; the previous
  25000/20000 values were unreachable under the corrected formula.
* `channel_plot(target = "agent")` now accepts only `plotly` among interactive
  widgets. Other htmlwidget types (leaflet, DT, visNetwork) abort with a
  message pointing at `target = "user"` instead of failing obscurely inside
  the renderer, which only knows how to locate a plotly plot.
* Rendering interactive plots for agent analysis requires a Chromium-based
  browser (Chrome, Chromium, Brave, or Edge), discoverable via
  `chromote::find_chrome()` or the `CHROMOTE_CHROME` environment variable.
  This is optional — only used by `channel_plot(target = "agent")` on
  `plotly`/`htmlwidget` objects; static plots and `target = "user"` are
  unaffected.
