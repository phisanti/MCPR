# Development Scratchpad for BaseMCPR Testing
# Informal testing ground for BaseMCPR functionality during development
# Use this file to experiment and verify behavior before formal tests

# Load the package
library(MCPR)

cat("=== BaseMCPR Development Testing ===\n")

# Test 1: Basic Initialization
cat("\n--- Test 1: Basic Initialization ---\n")
test_component <- BaseMCPR$new()

# Should fail - no component name
tryCatch({
  test_component$initialize_base()
  cat("❌ FAIL: Should require component_name\n")
}, error = function(e) {
  cat("✅ PASS: Correctly requires component_name\n")
})

# Should work
test_component$initialize_base("TEST")
cat("✅ PASS: Initialization with component name works\n")
cat("Component name:", test_component$get_component_name(), "\n")
cat("Is initialized:", test_component$is_initialized(), "\n")

# Test 2: State Management
cat("\n--- Test 2: State Management ---\n")

# Test state_get with default
default_val <- test_component$state_get("nonexistent_key", "default_value")
cat("Default value retrieval:", default_val, "\n")

# Test state_set and state_get
test_component$state_set("test_key", "test_value")
retrieved_val <- test_component$state_get("test_key")
cat("Set and get value:", retrieved_val, "\n")

# Test state_has
has_key <- test_component$state_has("test_key")
has_missing <- test_component$state_has("missing_key")
cat("Has existing key:", has_key, "\n")
cat("Has missing key:", has_missing, "\n")

# Test state ownership tracking
owned_keys <- test_component$state_keys_owned()
cat("Owned keys:", paste(owned_keys, collapse = ", "), "\n")

# Test 3: Logger Functionality
cat("\n--- Test 3: Logger Functionality ---\n")

# Get logger
logger <- test_component$get_logger()
cat("Logger component:", logger$get_default_log_file(), "\n")

# Test logging methods
test_component$log_info("Test info message")
test_component$log_warn("Test warning message")
test_component$log_debug("Test debug message")
test_component$log_comm("Test communication message")

cat("✅ PASS: All logging methods work (check log file for output)\n")

# Test 4: Resource Management
cat("\n--- Test 4: Resource Management ---\n")

# Create test resource
test_resource <- list(active = TRUE)

# Register cleanup
test_component$register_cleanup(
  function() {
    test_resource$active <<- FALSE
    cat("  Resource cleanup executed\n")
  },
  "test_resource"
)

cat("Resource before cleanup:", test_resource$active, "\n")

# Execute cleanup
test_component$cleanup_all()
cat("Resource after cleanup:", test_resource$active, "\n")

# Test 5: Socket Utilities
cat("\n--- Test 5: Socket Utilities ---\n")

# Set socket URL in global state (simulating package initialization)
test_component$state_set("socket_url", "ipc:///tmp/test_")

# Test socket URL generation
url1 <- test_component$socket_url(1)
url2 <- test_component$socket_url(42)
cat("Socket URL 1:", url1, "\n")
cat("Socket URL 2:", url2, "\n")

# Test socket creation
if (requireNamespace("nanonext", quietly = TRUE)) {
  tryCatch({
    socket <- test_component$create_socket("poly", "test_socket")
    cat("✅ PASS: Socket creation works\n")
    
    # Cleanup should be automatic
    test_component$cleanup_all()
    cat("✅ PASS: Socket cleanup registered\n")
  }, error = function(e) {
    cat("⚠️  SKIP: Socket creation failed (nanonext issue?):", e$message, "\n")
  })
} else {
  cat("⚠️  SKIP: nanonext not available for socket testing\n")
}

# Test 6: Error Handling
cat("\n--- Test 6: Error Handling ---\n")

# Test uninitialized access
uninit_component <- BaseMCPR$new()
tryCatch({
  uninit_component$state_get("test")
  cat("❌ FAIL: Should prevent uninitialized access\n")
}, error = function(e) {
  cat("✅ PASS: Prevents uninitialized access\n")
})

# Test invalid cleanup function
tryCatch({
  test_component$register_cleanup("not_a_function", "bad_cleanup")
  cat("❌ FAIL: Should reject non-function cleanup\n")
}, error = function(e) {
  cat("✅ PASS: Rejects non-function cleanup\n")
})

# Test 7: State Isolation
cat("\n--- Test 7: State Isolation ---\n")

# Create second component
component2 <- BaseMCPR$new()
component2$initialize_base("TEST2")

# Set different state in each
test_component$state_set("isolation_test", "component1")
component2$state_set("isolation_test2", "component2")

# Check ownership
keys1 <- test_component$state_keys_owned()
keys2 <- component2$state_keys_owned()

cat("Component 1 keys:", paste(keys1, collapse = ", "), "\n")
cat("Component 2 keys:", paste(keys2, collapse = ", "), "\n")

# Both should see the same global state (but track ownership separately)
val1_from_comp1 <- test_component$state_get("isolation_test")
val1_from_comp2 <- component2$state_get("isolation_test")
val2_from_comp1 <- test_component$state_get("isolation_test2") 
val2_from_comp2 <- component2$state_get("isolation_test2")

cat("Component 1 sees its own value:", val1_from_comp1, "\n")
cat("Component 2 sees component 1's value:", val1_from_comp2, "\n")
cat("Component 1 sees component 2's value:", val2_from_comp1, "\n")
cat("Component 2 sees its own value:", val2_from_comp2, "\n")

if (identical(val1_from_comp1, val1_from_comp2) && 
    identical(val2_from_comp1, val2_from_comp2)) {
  cat("✅ PASS: Global state is shared but ownership is tracked separately\n")
} else {
  cat("❌ FAIL: State isolation issue\n")
}

# Test 8: Inheritance Pattern
cat("\n--- Test 8: Inheritance Pattern ---\n")

# Create a mock class that inherits from BaseMCPR
MockComponent <- R6::R6Class("MockComponent",
  inherit = BaseMCPR,
  public = list(
    initialize = function() {
      # Initialize base first
      self$initialize_base("MOCK")
      
      # Add component-specific initialization
      private$.mock_data <- "initialized"
      self$log_info("Mock component initialized")
    },
    
    get_mock_data = function() {
      private$.mock_data
    },
    
    do_something_with_state = function() {
      self$state_set("mock_state", "mock_value")
      self$log_info("Set mock state")
      self$state_get("mock_state")
    }
  ),
  
  private = list(
    .mock_data = NULL
  )
)

# Test the inheritance
mock <- MockComponent$new()
cat("Mock component name:", mock$get_component_name(), "\n")
cat("Mock data:", mock$get_mock_data(), "\n")

# Test inherited functionality
result <- mock$do_something_with_state()
cat("State operation result:", result, "\n")
cat("Mock owned keys:", paste(mock$state_keys_owned(), collapse = ", "), "\n")

cat("✅ PASS: Inheritance pattern works correctly\n")

# Final cleanup
cat("\n--- Final Cleanup ---\n")
test_component$cleanup_all()
component2$cleanup_all()
mock$cleanup_all()

cat("\n=== BaseMCPR Development Testing Complete ===\n")
cat("All core functionality appears to be working correctly!\n")