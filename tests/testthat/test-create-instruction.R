test_that("create_instruction creates directory if not present", {
  # Use temp directory for testing
  temp_dir <- tempdir()
  old_wd <- getwd()
  setwd(temp_dir)

  # Clean up any existing .mcpr_instructions directory
  if (dir.exists(".mcpr_instructions")) {
    unlink(".mcpr_instructions", recursive = TRUE)
  }

  # Create instruction file
  result <- create_instruction("test_file")

  # Check directory was created
  expect_true(dir.exists(".mcpr_instructions"))

  # Check file was created
  expect_true(file.exists(".mcpr_instructions/test_file.md"))

  # Cleanup
  unlink(".mcpr_instructions", recursive = TRUE)
  setwd(old_wd)
})

test_that("create_instruction adds .md extension if missing", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  setwd(temp_dir)

  if (dir.exists(".mcpr_instructions")) {
    unlink(".mcpr_instructions", recursive = TRUE)
  }

  # Create without extension
  create_instruction("no_extension")
  expect_true(file.exists(".mcpr_instructions/no_extension.md"))

  # Create with extension
  create_instruction("with_extension.md", overwrite = TRUE)
  expect_true(file.exists(".mcpr_instructions/with_extension.md"))

  # Cleanup
  unlink(".mcpr_instructions", recursive = TRUE)
  setwd(old_wd)
})

test_that("create_instruction generates valid YAML header", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  setwd(temp_dir)

  if (dir.exists(".mcpr_instructions")) {
    unlink(".mcpr_instructions", recursive = TRUE)
  }

  create_instruction("yaml_test")
  content <- readLines(".mcpr_instructions/yaml_test.md")

  # Check YAML delimiters
  expect_equal(content[1], "---")
  expect_match(content[2], '^keyword: "yaml_test"$')
  expect_match(content[3], '^definition:')

  # Find closing YAML delimiter
  yaml_end <- which(content == "---")[2]
  expect_true(!is.na(yaml_end))

  # Cleanup
  unlink(".mcpr_instructions", recursive = TRUE)
  setwd(old_wd)
})

test_that("create_instruction prevents overwriting without flag", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  setwd(temp_dir)

  if (dir.exists(".mcpr_instructions")) {
    unlink(".mcpr_instructions", recursive = TRUE)
  }

  # Create initial file
  create_instruction("overwrite_test")

  # Try to overwrite without flag - should error
  expect_error(
    create_instruction("overwrite_test"),
    "File already exists"
  )

  # Overwrite with flag - should succeed
  expect_no_error(
    create_instruction("overwrite_test", overwrite = TRUE)
  )

  # Cleanup
  unlink(".mcpr_instructions", recursive = TRUE)
  setwd(old_wd)
})

test_that("create_instruction sanitizes filename to valid keyword", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  setwd(temp_dir)

  if (dir.exists(".mcpr_instructions")) {
    unlink(".mcpr_instructions", recursive = TRUE)
  }

  # Create file with special characters
  create_instruction("my-special file!")
  content <- readLines(".mcpr_instructions/my-special file!.md")

  # Check keyword is sanitized (no spaces, special chars replaced with _)
  expect_match(content[2], '^keyword: "my_special_file"$')

  # Cleanup
  unlink(".mcpr_instructions", recursive = TRUE)
  setwd(old_wd)
})
