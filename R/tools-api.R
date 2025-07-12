#' Convert a tool definition to a JSON-compatible list

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
