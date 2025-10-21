# Read Instructions Tool
# Provides access to user-defined instruction files for domain-specific R analysis guidance.
# Enables custom workflows without modifying MCPR code through structured instruction files.

#* @mcp_tool
#' Read User-Provided Instruction Files
#'
#' @description Read user-provided instruction files for domain-specific R analysis guidance. Two-call pattern: (1) Call without parameters to list available instruction files with their keywords and descriptions. (2) Call with instruction_type to retrieve the full content of a specific instruction file. Searches relative to the current R session's working directory (.mcpr_instructions/). For multi-session environments, use manage_r_sessions() to view and switch between sessions.
#' @param instruction_type character Type of instruction to read (optional - if empty, lists all available)
#' @keywords mcpr_tool
#' @return Character string with instruction content or formatted table of available instructions
read_instructions <- function(instruction_type = NULL) {
  # If no parameter provided, list available instructions
  if (is.null(instruction_type)) {
    return(list_available_instructions())
  }
  
  # Validate input
  available_types <- get_available_instruction_types()
  if (length(available_types) == 0) {
    cli::cli_abort(c(
      "No instruction files found.",
      "i" = "Create {.path .mcpr_instructions/} directory with {.code .md} files to enable custom instructions."
    ))
  }
  
  if (!instruction_type %in% available_types) {
    cli::cli_abort(c(
      "Invalid instruction type {.val {instruction_type}}.",
      "i" = "Available options: {paste(available_types, collapse = ", ")}"
    ))
  }
  
  # Find file by keyword (not just filename)
  file_path <- find_instruction_file_by_keyword(instruction_type)
  if (is.null(file_path)) {
    cli::cli_abort(c(
      "Instruction file for type {.val {instruction_type}} not found.",
      "i" = "Check that {.path .mcpr_instructions/} contains a matching {.code .md} file."
    ))
  }
  
  # Read and return content
  content <- readLines(file_path, warn = FALSE)
  paste(content, collapse = "\n")
}

# Helper function to format available instructions as table
list_available_instructions <- function() {
  if (!dir.exists(".mcpr_instructions")) {
    current_dir <- getwd()

    # Try to detect if multiple sessions exist
    session_count <- tryCatch({
      length(the$server_processes)
    }, error = function(e) NULL)

    session_hint <- if (!is.null(session_count) && session_count > 1) {
      paste0("\n  ", cli::symbol$pointer, " Multiple sessions detected. Use manage_r_sessions() to view all session directories")
    } else {
      ""
    }

    msg <- cli::format_message(c(
      "x" = "No {.path .mcpr_instructions/} directory found in current session",
      "i" = "Current working directory: {.path {current_dir}}",
      "i" = "Create {.path .mcpr_instructions/} with {.file .md} instruction files",
      "i" = "To switch sessions: {.code manage_r_sessions(action = \"join\", session = N)}{session_hint}"
    ))

    return(msg)
  }
  
  files <- list.files(".mcpr_instructions", pattern = "\\.md$", full.names = FALSE)
  if (length(files) == 0) {
    return("No instruction files found in .mcpr_instructions directory.")
  }
  
  # Parse each file to get metadata
  instructions <- list()
  for (file in files) {
    yaml_header <- extract_yaml_header(file.path(".mcpr_instructions", file))
    if (!is.null(yaml_header) && !is.null(yaml_header$keyword) && !is.null(yaml_header$definition)) {
      instructions[[length(instructions) + 1]] <- list(
        path = file,
        keyword = yaml_header$keyword,
        description = substr(yaml_header$definition, 1, 50)
      )
    }
  }
  
  if (length(instructions) == 0) {
    # Build detailed quality diagnostics
    quality_issues <- character()

    for (file in files) {
      yaml_header <- extract_yaml_header(file.path(".mcpr_instructions", file))
      if (is.null(yaml_header)) {
        quality_issues <- c(quality_issues, cli::format_inline("x {.file {file}}: Missing or malformed YAML header"))
      } else {
        if (is.null(yaml_header$keyword)) {
          quality_issues <- c(quality_issues, cli::format_inline("x {.file {file}}: Missing {.field keyword} field"))
        }
        if (is.null(yaml_header$definition)) {
          quality_issues <- c(quality_issues, cli::format_inline("x {.file {file}}: Missing {.field definition} field"))
        }
      }
    }

    msg_parts <- c(
      cli::format_inline("x No valid instruction files in {.path .mcpr_instructions/}"),
      cli::format_inline("! {length(files)} file(s) found but none have valid YAML headers"),
      quality_issues,
      cli::format_inline("i Required YAML format:"),
      "  ---",
      cli::format_inline("  keyword: {.field your_keyword}"),
      cli::format_inline("  definition: {.field Brief description}"),
      "  ---"
    )

    return(paste(msg_parts, collapse = "\n"))
  }
  
  # Convert to data frame and use existing formatting utility
  instructions_df <- data.frame(
    Path = sapply(instructions, function(x) x$path),
    Keyword = sapply(instructions, function(x) x$keyword), 
    Description = sapply(instructions, function(x) x$description),
    stringsAsFactors = FALSE
  )
  
  # Use generic table formatting function from utils.R
  format_table_for_agent(instructions_df, "No instruction files found.")
}

# Helper function to get available instruction types from YAML headers
get_available_instruction_types <- function() {
  if (!dir.exists(".mcpr_instructions")) return(character(0))
  
  files <- list.files(".mcpr_instructions", pattern = "\\.md$", full.names = TRUE)
  types <- character(0)
  
  for (file in files) {
    yaml_header <- extract_yaml_header(file)
    if (!is.null(yaml_header$keyword)) {
      types <- c(types, yaml_header$keyword)
    }
  }
  
  types
}

# Helper function to find instruction file by keyword
find_instruction_file_by_keyword <- function(keyword) {
  if (!dir.exists(".mcpr_instructions")) return(NULL)
  
  files <- list.files(".mcpr_instructions", pattern = "\\.md$", full.names = TRUE)
  
  for (file in files) {
    yaml_header <- extract_yaml_header(file)
    if (!is.null(yaml_header$keyword) && yaml_header$keyword == keyword) {
      return(file)
    }
  }
  
  NULL
}

# Helper function to extract YAML header from markdown files
extract_yaml_header <- function(file_path) {
  tryCatch({
    lines <- readLines(file_path, warn = FALSE)
    if (length(lines) < 3 || lines[1] != "---") return(NULL)
    
    yaml_end <- which(lines[-1] == "---")[1] + 1
    if (is.na(yaml_end)) return(NULL)
    
    yaml_content <- paste(lines[2:(yaml_end-1)], collapse = "\n")
    yaml::yaml.load(yaml_content)
  }, error = function(e) NULL)
}

#' @export
read_instructions <- read_instructions