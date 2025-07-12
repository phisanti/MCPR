#' Global registry for custom serializers
#' @keywords internal
.mcp_custom_serializers <- new.env(parent = emptyenv())

#' Register a custom serializer for a specific class
#'
#' @param class_name Character, name of the R class
#' @param serializer_func Function that takes an object and returns JSON-compatible representation
#' @export
#' @examples
#' # Register a custom serializer for spatial data
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   register_mcp_serializer("sf", function(obj) {
#'     list(
#'       type = "geojson",
#'       data = sf::st_as_geojson(obj)
#'     )
#'   })
#' }
register_mcp_serializer <- function(class_name, serializer_func) {
  .mcp_custom_serializers[[class_name]] <- serializer_func
}

#' Get all registered custom serializers
#' @return List of custom serializers
#' @export
get_mcp_serializers <- function() {
  as.list(.mcp_custom_serializers)
}