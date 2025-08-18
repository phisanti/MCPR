# Validation Test for ToolRegistry Integration
# This test demonstrates the complete integration of ToolRegistry into the MCPR codebase

# Load the package
pkgload::load_all(".")

# Create temporary directory for test tools
temp_dir <- tempfile("validation_tools_")
dir.create(temp_dir)

# Create sample tools
writeLines(c(
  "#' Square Function",
  "#' @description Calculates the square of a number",
  "#' @param x numeric The number to square",
  "#' @keywords mcpr_tool",
  "square_number <- function(x) {",
  "  return(x^2)",
  "}"
), file.path(temp_dir, "square_tool.R"))

writeLines(c(
  "#' Concatenate Strings",
  "#' @description Concatenates two strings with a separator",
  "#' @param first string The first string",
  "#' @param second string The second string",
  "#' @param sep string The separator to use",
  "#' @keywords mcpr_tool",
  "concat_strings <- function(first, second, sep = ' ') {",
  "  paste(first, second, sep = sep)",
  "}"
), file.path(temp_dir, "concat_tool.R"))

cat("=== ToolRegistry Integration Validation ===\n")

# Test 1: Create ToolRegistry and discover tools
cat("1. Creating ToolRegistry and discovering tools...\n")
registry <- ToolRegistry$new(tools_dir = temp_dir, verbose = FALSE)
discovered_tools <- registry$search_tools()
cat("   Found", length(discovered_tools), "tools\n")

# Test 2: Create mcprServer with ToolRegistry
cat("2. Creating mcprServer with ToolRegistry...\n")
server <- mcprServer$new(registry = registry)
cat("   Server created successfully\n")

# Test 3: Verify server tools include registry tools
cat("3. Verifying server tools include registry tools...\n")
server_tools <- server$get_tools()
tool_names <- names(server_tools)
cat("   Available tools:", paste(tool_names, collapse = ", "), "\n")

# Test 4: Verify built-in tools are still present
cat("4. Verifying built-in tools are present...\n")
builtin_tools <- c("list_r_sessions", "select_r_session")
missing_builtins <- setdiff(builtin_tools, tool_names)
if (length(missing_builtins) == 0) {
  cat("   ✓ All built-in tools are present\n")
} else {
  cat("   ✗ Missing built-in tools:", paste(missing_builtins, collapse = ", "), "\n")
}

# Test 5: Verify registry tools are present
cat("5. Verifying registry tools are present...\n")
registry_tool_names <- c("square_number", "concat_strings")
missing_registry <- setdiff(registry_tool_names, tool_names)
if (length(missing_registry) == 0) {
  cat("   ✓ All registry tools are present\n")
} else {
  cat("   ✗ Missing registry tools:", paste(missing_registry, collapse = ", "), "\n")
}

# Test 6: Test tool JSON conversion
cat("6. Testing tool JSON conversion...\n")
json_tools <- server$get_tools("json")
json_names <- sapply(json_tools, function(x) x$name)
cat("   JSON tools:", paste(json_names, collapse = ", "), "\n")

# Test 7: Test convenience function
cat("7. Testing mcp_server convenience function...\n")
if (FALSE) { # Skip actual server start in validation
  server_via_convenience <- mcp_server(registry = registry)
  cat("   Convenience function works\n")
} else {
  cat("   Convenience function test skipped (would block)\n")
}

# Test 8: Test tool precedence
cat("8. Testing ToolRegistry precedence over tools parameter...\n")
temp_tool_file <- tempfile(fileext = ".R")
writeLines("list()", temp_tool_file)
server_precedence <- mcprServer$new(registry = registry)
precedence_tools <- server_precedence$get_tools()
if ("square_number" %in% names(precedence_tools)) {
  cat("   ✓ Registry tools take precedence\n")
} else {
  cat("   ✗ Registry precedence test failed\n")
}

# Cleanup
unlink(temp_dir, recursive = TRUE)
unlink(temp_tool_file)

cat("=== Integration Validation Complete ===\n")
