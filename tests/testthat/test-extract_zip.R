# Tests that a valid ZIP file is extracted successfully
# and the extraction folder path is returned.
test_that("extract_zip extracts a ZIP file successfully", {
  
  temp_dir <- tempfile()
  fs::dir_create(temp_dir)
  
  source_dir <- file.path(
    temp_dir,
    "source"
  )
  
  fs::dir_create(source_dir)
  
  test_file <- file.path(
    source_dir,
    "test.csv"
  )
  
  writeLines(
    "id,value\n1,test",
    test_file
  )
  
  zip_file <- file.path(
    temp_dir,
    "test.zip"
  )
  
  old_wd <- getwd()
  
  on.exit(
    {
      setwd(old_wd)
      
      unlink(
        temp_dir,
        recursive = TRUE
      )
    },
    add = TRUE
  )
  
  setwd(source_dir)
  
  utils::zip(
    zipfile = zip_file,
    files = "test.csv"
  )
  
  setwd(old_wd)
  
  result <- suppressMessages(
    extract_zip(
      zip_file = zip_file,
      extract_folder = temp_dir
    )
  )
  
  expect_equal(
    result,
    file.path(
      temp_dir,
      "test"
    )
  )
  
  expect_true(
    file.exists(
      file.path(
        result,
        "test.csv"
      )
    )
  )
})


# Tests that the extraction folder is named after
# the ZIP file without the .zip extension.
test_that("extract_zip creates a folder based on the ZIP file name", {
  
  temp_dir <- tempfile()
  fs::dir_create(temp_dir)
  
  source_dir <- file.path(
    temp_dir,
    "source"
  )
  
  fs::dir_create(source_dir)
  
  test_file <- file.path(
    source_dir,
    "example.txt"
  )
  
  writeLines(
    "test",
    test_file
  )
  
  zip_file <- file.path(
    temp_dir,
    "nhs-resource.zip"
  )
  
  old_wd <- getwd()
  
  on.exit(
    {
      setwd(old_wd)
      
      unlink(
        temp_dir,
        recursive = TRUE
      )
    },
    add = TRUE
  )
  
  setwd(source_dir)
  
  utils::zip(
    zipfile = zip_file,
    files = "example.txt"
  )
  
  setwd(old_wd)
  
  result <- suppressMessages(
    extract_zip(
      zip_file = zip_file,
      extract_folder = temp_dir
    )
  )
  
  expect_equal(
    basename(result),
    "nhs-resource"
  )
})


# Tests that an existing extraction folder is removed
# before extracting the current ZIP contents.
test_that("extract_zip removes an existing extraction folder before extraction", {
  
  temp_dir <- tempfile()
  fs::dir_create(temp_dir)
  
  source_dir <- file.path(
    temp_dir,
    "source"
  )
  
  fs::dir_create(source_dir)
  
  current_file <- file.path(
    source_dir,
    "current.csv"
  )
  
  writeLines(
    "id,value\n1,current",
    current_file
  )
  
  zip_file <- file.path(
    temp_dir,
    "test.zip"
  )
  
  old_wd <- getwd()
  
  on.exit(
    {
      setwd(old_wd)
      
      unlink(
        temp_dir,
        recursive = TRUE
      )
    },
    add = TRUE
  )
  
  setwd(source_dir)
  
  utils::zip(
    zipfile = zip_file,
    files = "current.csv"
  )
  
  setwd(old_wd)
  
  existing_output_folder <- file.path(
    temp_dir,
    "test"
  )
  
  fs::dir_create(
    existing_output_folder
  )
  
  old_file <- file.path(
    existing_output_folder,
    "old.csv"
  )
  
  writeLines(
    "old data",
    old_file
  )
  
  result <- suppressMessages(
    extract_zip(
      zip_file = zip_file,
      extract_folder = temp_dir
    )
  )
  
  expect_false(
    file.exists(
      file.path(
        result,
        "old.csv"
      )
    )
  )
  
  expect_true(
    file.exists(
      file.path(
        result,
        "current.csv"
      )
    )
  )
})


# Tests that NA is returned when the ZIP file path is NA.
test_that("extract_zip returns NA when zip_file is NA", {
  
  result <- extract_zip(
    zip_file = NA_character_,
    extract_folder = tempfile()
  )
  
  expect_true(
    is.na(result)
  )
})


# Tests that NA is returned when the ZIP file does not exist.
test_that("extract_zip returns NA when the ZIP file does not exist", {
  
  result <- extract_zip(
    zip_file = "file_that_does_not_exist.zip",
    extract_folder = tempfile()
  )
  
  expect_true(
    is.na(result)
  )
})


# Tests that a failed extraction returns NA and reports
# the failure without stopping the wider extraction process.
test_that("extract_zip handles an extraction failure", {
  
  temp_dir <- tempfile()
  fs::dir_create(temp_dir)
  
  fake_zip <- file.path(
    temp_dir,
    "fake.zip"
  )
  
  writeLines(
    "fake zip content",
    fake_zip
  )
  
  on.exit(
    unlink(
      temp_dir,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  testthat::local_mocked_bindings(
    unzip = function(...) {
      warning(
        "error 1 in extracting from zip file"
      )
    },
    .package = "utils"
  )
  
  expect_message(
    result <- extract_zip(
      zip_file = fake_zip,
      extract_folder = temp_dir
    ),
    "Could not extract"
  )
  
  expect_true(
    is.na(result)
  )
})

# Tests that zip_file must be a single character value.
test_that("extract_zip validates zip_file", {
  
  expect_error(
    extract_zip(
      zip_file = 123,
      extract_folder = tempfile()
    ),
    "`zip_file` must be a single character value"
  )
  
  expect_error(
    extract_zip(
      zip_file = c(
        "file1.zip",
        "file2.zip"
      ),
      extract_folder = tempfile()
    ),
    "`zip_file` must be a single character value"
  )
})


# Tests that extract_folder must be a single non-empty
# character value.
test_that("extract_zip validates extract_folder", {
  
  expect_error(
    extract_zip(
      zip_file = "test.zip",
      extract_folder = ""
    ),
    "`extract_folder` must be a single non-empty character value"
  )
  
  expect_error(
    extract_zip(
      zip_file = "test.zip",
      extract_folder = c(
        "folder1",
        "folder2"
      )
    ),
    "`extract_folder` must be a single non-empty character value"
  )
})