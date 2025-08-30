# Custom Serializer Registry
# Global registry system for user-defined custom class serializers.
# Enables extensible type conversion for specialized R objects in MCP protocol.

#' Global Registry for Custom Serializers
#'
#' @title Global Registry for Custom Serializers
#' @description Maintains environment-based registry for custom class serializers.
#' Provides isolated storage for user-defined serialization functions enabling
#' extensible type conversion for specialized R objects in MCP protocol workflows.
#'
#' @keywords internal
.mcpr_custom_serializers <- new.env(parent = emptyenv())

#' Register Custom Serializer for Class
#'
#' @title Register Custom Serializer for Class
#' @description Registers custom serialization function for specific R class in global registry.
#' Enables extensible type conversion system for specialized objects through user-defined
#' serialization logic. Supports custom object handling in MCP protocol communication
#' through pluggable serializer architecture.
#'
#' @param class_name Name of the R class to register serializer for
#' @param serializer_func Function taking object and returning JSON-compatible representation
#' @return None (registers serializer in global registry)
#' @examples
#' # Register a custom serializer for spatial data
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   register_mcpr_serializer("sf", function(obj) {
#'     list(
#'       type = "geojson",
#'       data = sf::st_as_geojson(obj)
#'     )
#'   })
#' }
#' @noRd
register_mcpr_serializer <- function(class_name, serializer_func) {
  .mcpr_custom_serializers[[class_name]] <- serializer_func
}

#' Get All Registered Custom Serializers
#'
#' @title Get All Registered Custom Serializers
#' @description Retrieves all registered custom serializers from global registry as named list.
#' Provides access to complete serialization function collection for inspection and
#' usage in type conversion workflows. Enables serializer discovery and management
#' for MCP protocol customization.
#'
#' @return Named list of custom serializer functions
#' @noRd
get_mcpr_serializers <- function() {
  as.list(.mcpr_custom_serializers)
}
