# Chrome Discovery Helper for Tests
# Points chromote at a Chromium-family browser when CHROMOTE_CHROME is unset.
# Without this the interactive-render tests skip silently on machines that have
# Brave/Edge/Chromium but no Google Chrome, hiding real regressions.

if (!nzchar(Sys.getenv("CHROMOTE_CHROME"))) {
  candidates <- c(
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser",
    "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    "/usr/bin/google-chrome", "/usr/bin/chromium", "/usr/bin/chromium-browser",
    "/usr/bin/microsoft-edge"
  )
  found <- candidates[file.exists(candidates)]
  if (length(found) > 0) {
    Sys.setenv(CHROMOTE_CHROME = found[[1]])
  }
}

# TRUE when the interactive render path can actually be exercised.
chrome_available <- function() {
  requireNamespace("chromote", quietly = TRUE) &&
    !is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL))
}
