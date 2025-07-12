#' Convert a tool definition to a JSON-compatible list
# TODO: Moved to mcpr-server-tools.R
# tool_as_json <- function(tool) {
#   dummy_provider <- ellmer::Provider("dummy", "dummy", "dummy")
#   as_json <- getNamespace("ellmer")[["as_json"]]
#   inputSchema <- compact(as_json(dummy_provider, tool@arguments))
#   inputSchema$description <- NULL # This field is not needed
# 
#   list(
#     name = tool@name,
#     description = tool@description,
#     inputSchema = inputSchema
#   )
# }


# === Helpers for set_server_tools ===
# COMMENT OUT FOR MIGRATION TO TOOL REGISTRY
#looks_like_r_file <- function(x) {
#  rlang::is_string(x) &&
#    file.exists(x) &&
#    grepl("\\.r$", x, ignore.case = TRUE)
#}
# COMMENT OUT FOR MIGRATION TO TOOL REGISTRY
#source_tools <- function(x) {
#  source(x, local = TRUE)$value
#}
