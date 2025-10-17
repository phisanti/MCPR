#' Create Instruction Template File
#'
#' @title Create Instruction Template File
#' @description Creates a .mcpr_instructions directory if not present and generates a template
#' instruction markdown file with proper YAML frontmatter and guidance sections.
#' Provides scaffolding for users to create domain-specific instruction files.
#'
#' @param filename Character string. Name for the instruction file (default: 'instructions-template').
#'   If '.md' extension is not present, it will be added automatically.
#' @param overwrite Logical. Whether to overwrite existing files (default: FALSE)
#' @return Character string with path to created file
#' @export
#' @examples
#' \dontrun{
#' # Create default template
#' create_instruction()
#' 
#' # Create specific instruction file
#' create_instruction("financial_analysis")
#' 
#' # Force overwrite existing file
#' create_instruction("my_analysis", overwrite = TRUE)
#' }
create_instruction <- function(filename = "instructions-template", overwrite = FALSE) {
  # Validate inputs
  check_string(filename)
  check_bool(overwrite)
  
  # Ensure .md extension
  if (!grepl("\\.md$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".md")
  }
  
  # Create directory if it doesn't exist
  instructions_dir <- ".mcpr_instructions"
  if (!dir.exists(instructions_dir)) {
    dir.create(instructions_dir, recursive = TRUE)
    cli::cli_alert_success("Created directory: {instructions_dir}")
  }
  
  # Full file path
  file_path <- file.path(instructions_dir, filename)
  
  # Check if file exists and handle overwrite
  if (file.exists(file_path) && !overwrite) {
    cli::cli_abort(
      c(
        "File already exists: {file_path}",
        "i" = "Use {.code overwrite = TRUE} to replace existing file"
      )
    )
  }
  
  # Generate template content
  template_content <- generate_instruction_template(filename)
  
  # Write file
  writeLines(template_content, file_path)
  
  cli::cli_alert_success("Created instruction template: {file_path}")
  cli::cli_alert_info("Edit the file to customize for your specific domain")
  cli::cli_alert_info("Use {.fn read_instructions} tool to access instructions in MCP sessions")
  
  invisible(file_path)
}

#' Generate Instruction Template Content
#'
#' @title Generate Instruction Template Content
#' @description Creates template content that guides users on how to write effective
#' instruction files with proper structure and best practices.
#'
#' @param filename Character string with filename for keyword generation
#' @return Character vector with template lines
#' @noRd
generate_instruction_template <- function(filename) {
  base_name <- gsub("\\.md$", "", filename, ignore.case = TRUE)
  keyword_suggestion <- gsub("[^a-zA-Z0-9_]", "_", base_name)
  keyword_suggestion <- gsub("_+", "_", keyword_suggestion)
  keyword_suggestion <- gsub("^_|_$", "", keyword_suggestion)

  c(
    "---",
    paste0('keyword: "', keyword_suggestion, '"'),
    'definition: "Example Instructions file"',
    'version: "1.0"',
    'author: "Your Name"',
    'tags: ["domain"]',
    "---",
    "",
    "# Your Domain Instructions",
    "",
    "## Purpose",
    "Provide AI models domain-specific R guidance to reduce errors and ensure consistency. Instruction files provide AI models with domain-specific guidance for R analysis. They reduce hallucination and ensure consistent, high-quality results by giving the model concrete examples, specific package recommendations, and best practices.",
    "",
    "## Writing Guidelines",
    "- **Keep it focused**: Single clear purpose",
    "- **Be prescriptive**: 'Use quantmod::getSymbols()' not 'get data'",
    "- **Use examples**: Actual R code, not descriptions",
    "- **Show anti-patterns**: Don't / Do examples",
    "",
    "## Template Sections",
    "",
    "### Required Packages",
    "```r",
    "library(pkg)  # Purpose",
    "```",
    "",
    "### Workflow",
    "1. Load and validate data",
    "2. Clean and transform",
    "3. Analysis",
    "4. Visualization",
    "",
    "### Common Pitfalls",
    "`bad_example()`",
    "`good_example()`",
    "",
    "---",
    paste0("Test: `read_instructions(\"", keyword_suggestion, "\")`")
  )
}
