# MCP Resource Registry
# Provides MCPResourceRegistry R6 class for registering custom MCP resources with the server.
# Mirrors ToolRegistry ergonomics; resources are registered imperatively, not via file-scanning.

#' @include mcpr-base.R
NULL

#' MIME type used for MCP App HTML resources (profile=mcp-app)
#' @noRd
MCPR_MCP_APP_MIME <- "text/html;profile=mcp-app"

#' Canonical URI for the built-in MCPR plot viewer resource
#' @noRd
MCPR_PLOT_VIEWER_RESOURCE_URI <- "ui://mcpr/plots"

#' MCP Resource Registry
#' @description R6 class for registering custom MCP resources that the server exposes
#' via the `resources/list` and `resources/read` MCP protocol methods. Mirrors the
#' ergonomics of [ToolRegistry] but registration is imperative rather than file-scanned.
#'
#' @details Each registered resource carries a zero-argument `resource_reader` callback
#' that the server invokes when the client issues a `resources/read` request. Readers
#' may return either a simple `list(text = ..., mimeType = ...)` /
#' `list(blob = ..., mimeType = ...)` shape (which the registry wraps into a full
#' MCP `contents` envelope) or a complete `list(contents = list(...))` shape.
#'
#' @examples
#' \dontrun{
#' reg <- MCPResourceRegistry$new()
#' reg$register(
#'   uri = "data://hello",
#'   name = "Hello",
#'   resource_reader = function() list(text = "hi", mimeType = "text/plain")
#' )
#' server <- mcprServer$new(resource_registry = reg)
#' }
#' @export
MCPResourceRegistry <- R6::R6Class("MCPResourceRegistry",
  public = list(
    #' @description Initialize an empty resource registry.
    initialize = function() {
      private$.resources <- list()
    },

    #' @description Register a resource entry.
    #' @param uri Non-empty character scalar identifying the resource.
    #' @param name Non-empty character scalar with a human-readable name.
    #' @param resource_reader Zero-argument function returning the resource contents.
    #' @param description Optional description string.
    #' @param mimeType Optional default MIME type for the resource.
    #' @param title Optional title string.
    #' @param annotations Optional annotations list passed through to the descriptor.
    #' @param size Optional size hint.
    #' @param meta Optional `_meta` list passed through to the descriptor.
    #' @param mcp_app_only Logical; if `TRUE`, the resource is only listed/read for
    #'   clients that announce MCP Apps support.
    #' @param overwrite Logical; if `TRUE`, replace an existing entry with the same URI.
    #' @return The registry, invisibly.
    register = function(uri, name, resource_reader,
                        description = NULL, mimeType = NULL,
                        title = NULL, annotations = NULL,
                        size = NULL, meta = NULL,
                        mcp_app_only = FALSE, overwrite = FALSE) {
      if (!is.character(uri) || length(uri) != 1 || !nzchar(uri))
        cli::cli_abort("{.arg uri} must be a non-empty string")
      if (!is.character(name) || length(name) != 1 || !nzchar(name))
        cli::cli_abort("{.arg name} must be a non-empty string")
      if (!is.function(resource_reader))
        cli::cli_abort("{.arg resource_reader} must be a function")
      if (length(formals(resource_reader)) != 0)
        cli::cli_abort("{.arg resource_reader} must be a zero-argument function")
      if (self$has(uri, mcp_apps_supported = TRUE) && !overwrite)
        cli::cli_abort(
          "A resource with URI {.val {uri}} is already registered. Use {.code overwrite = TRUE} to replace it."
        )

      private$.resources[[uri]] <- list(
        uri             = uri,
        name            = name,
        description     = description,
        mimeType        = mimeType,
        title           = title,
        annotations     = annotations,
        size            = size,
        meta            = meta,
        mcp_app_only    = isTRUE(mcp_app_only),
        resource_reader = resource_reader
      )
      invisible(self)
    },

    #' @description List resource descriptors visible to the calling client.
    #' @param mcp_apps_supported Logical; if `FALSE`, `mcp_app_only` entries are excluded.
    #' @return A list of resource descriptors suitable for the MCP `resources/list` response.
    list = function(mcp_apps_supported = TRUE) {
      entries <- private$.resources
      if (!isTRUE(mcp_apps_supported)) {
        entries <- Filter(function(e) !isTRUE(e$mcp_app_only), entries)
      }
      lapply(unname(entries), function(e) {
        desc <- list(uri = e$uri, name = e$name)
        if (!is.null(e$description))  desc$description  <- e$description
        if (!is.null(e$mimeType))     desc$mimeType     <- e$mimeType
        if (!is.null(e$title))        desc$title        <- e$title
        if (!is.null(e$annotations))  desc$annotations  <- e$annotations
        if (!is.null(e$size))         desc$size         <- e$size
        if (!is.null(e$meta))         desc[["_meta"]]   <- e$meta
        desc
      })
    },

    #' @description Read a resource by URI.
    #' @param uri The URI to read.
    #' @param mcp_apps_supported Logical; if `FALSE`, `mcp_app_only` entries return `NULL`.
    #' @return A list shaped like the MCP `resources/read` result, or `NULL` if not found.
    read = function(uri, mcp_apps_supported = TRUE) {
      entry <- private$.resources[[uri]]
      if (is.null(entry)) return(NULL)
      if (isTRUE(entry$mcp_app_only) && !isTRUE(mcp_apps_supported)) return(NULL)

      raw <- entry$resource_reader()
      private$validate_and_wrap(uri, raw, entry$mimeType)
    },

    #' @description Check whether a URI is registered.
    #' @param uri The URI to check.
    #' @param mcp_apps_supported Logical; if `FALSE`, `mcp_app_only` entries report `FALSE`.
    #' @return Logical.
    has = function(uri, mcp_apps_supported = TRUE) {
      entry <- private$.resources[[uri]]
      if (is.null(entry)) return(FALSE)
      if (isTRUE(entry$mcp_app_only) && !isTRUE(mcp_apps_supported)) return(FALSE)
      TRUE
    }
  ),
  private = list(
    .resources = list(),

    validate_and_wrap = function(uri, raw, registered_mimeType) {
      # Two accepted shapes:
      # Simple: list(text = "...", mimeType = "...") or list(blob = "...", mimeType = "...")
      # Full:   list(contents = list(list(uri = ..., text = ..., mimeType = ...)))
      if (is.list(raw) && !is.null(raw$contents)) {
        if (!is.list(raw$contents))
          cli::cli_abort("resource_reader returned {.code contents} that is not a list")
        for (item in raw$contents) {
          private$validate_content_item(item)
        }
        return(raw)
      }
      private$validate_content_item(raw)
      mime <- raw$mimeType %||% registered_mimeType
      item <- list(uri = uri)
      if (!is.null(raw$text))  item$text     <- raw$text
      if (!is.null(raw$blob))  item$blob     <- raw$blob
      if (!is.null(mime))      item$mimeType <- mime
      list(contents = list(item))
    },

    validate_content_item = function(item) {
      has_text <- !is.null(item$text)
      has_blob <- !is.null(item$blob)
      if (!has_text && !has_blob)
        cli::cli_abort("resource_reader output must contain {.code text} or {.code blob}")
      if (has_text && has_blob)
        cli::cli_abort("resource_reader output must contain {.code text} or {.code blob}, not both")
      if (has_text && (!is.character(item$text) || length(item$text) != 1))
        cli::cli_abort("{.code text} in resource_reader output must be a length-1 character string")
      if (has_blob && (!is.character(item$blob) || length(item$blob) != 1))
        cli::cli_abort("{.code blob} in resource_reader output must be a length-1 character string")
      if (!is.null(item$mimeType) && (!is.character(item$mimeType) || length(item$mimeType) != 1))
        cli::cli_abort("{.code mimeType} in resource_reader output must be a length-1 character string")
    }
  )
)

# Internal helper: builds the default registry with the built-in MCP App plot viewer.
# mcpr_version is captured by the reader closure.
make_default_mcp_resource_registry <- function(mcpr_version) {
  reg <- MCPResourceRegistry$new()
  cache <- NULL
  reg$register(
    uri             = MCPR_PLOT_VIEWER_RESOURCE_URI,
    name            = "MCPR Plot Viewer",
    description     = "Interactive plot viewer for R visualizations",
    mimeType        = MCPR_MCP_APP_MIME,
    mcp_app_only    = TRUE,
    resource_reader = function() {
      if (is.null(cache)) {
        path <- system.file("mcp_app/plot-viewer.html", package = "MCPR")
        if (!nzchar(path) || !file.exists(path))
          cli::cli_abort("Plot viewer HTML resource not found in package installation")
        html <- paste(readLines(path, warn = FALSE), collapse = "\n")
        cache <<- gsub("__MCPR_VERSION__", mcpr_version, html, fixed = TRUE)
      }
      list(text = cache, mimeType = MCPR_MCP_APP_MIME)
    }
  )
  reg
}
