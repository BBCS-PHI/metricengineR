# Tests that CSV files in the supplied folder are read
# and combined into one data frame.
test_that("read_csv_folder reads and combines CSV files", {
  
  test_folder <- tempfile()
  fs::dir_create(test_folder)
  
  on.exit(
    unlink(
      test_folder,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  readr::write_csv(
    tibble::tibble(
      id = c(1, 2),
      value = c("A", "B")
    ),
    file.path(
      test_folder,
      "file1.csv"
    )
  )
  
  readr::write_csv(
    tibble::tibble(
      id = c(3, 4),
      value = c("C", "D")
    ),
    file.path(
      test_folder,
      "file2.csv"
    )
  )
  
  result <- suppressMessages(
    read_csv_folder(
      folder = test_folder
    )
  )
  
  expect_equal(
    nrow(result),
    4L
  )
  
  expect_true(
    all(
      c(
        "source_file",
        "source_folder",
        "id",
        "value"
      ) %in% names(result)
    )
  )
})


# Tests that CSV files in subfolders are also found
# because the folder search is recursive.
test_that("read_csv_folder reads CSV files recursively", {
  
  test_folder <- tempfile()
  subfolder <- file.path(
    test_folder,
    "subfolder"
  )
  
  fs::dir_create(
    subfolder,
    recurse = TRUE
  )
  
  on.exit(
    unlink(
      test_folder,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  readr::write_csv(
    tibble::tibble(
      id = 1,
      value = "test"
    ),
    file.path(
      subfolder,
      "nested.csv"
    )
  )
  
  result <- suppressMessages(
    read_csv_folder(
      folder = test_folder
    )
  )
  
  expect_equal(
    nrow(result),
    1L
  )
  
  expect_equal(
    result$source_file,
    "nested.csv"
  )
  
  expect_equal(
    result$source_folder,
    "subfolder"
  )
})


# Tests that all source data columns are read as character
# when all_columns_character is TRUE.
test_that("read_csv_folder reads all columns as character by default", {
  
  test_folder <- tempfile()
  fs::dir_create(test_folder)
  
  on.exit(
    unlink(
      test_folder,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  readr::write_csv(
    tibble::tibble(
      id = c(1, 2),
      value = c(10.5, 20.5),
      category = c("A", "B")
    ),
    file.path(
      test_folder,
      "test.csv"
    )
  )
  
  result <- suppressMessages(
    read_csv_folder(
      folder = test_folder
    )
  )
  
  expect_type(
    result$id,
    "character"
  )
  
  expect_type(
    result$value,
    "character"
  )
  
  expect_type(
    result$category,
    "character"
  )
})


# Tests that readr is allowed to determine column types
# when all_columns_character is FALSE.
test_that("read_csv_folder can automatically determine column types", {
  
  test_folder <- tempfile()
  fs::dir_create(test_folder)
  
  on.exit(
    unlink(
      test_folder,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  readr::write_csv(
    tibble::tibble(
      id = c(1, 2),
      value = c(10.5, 20.5),
      category = c("A", "B")
    ),
    file.path(
      test_folder,
      "test.csv"
    )
  )
  
  result <- suppressMessages(
    read_csv_folder(
      folder = test_folder,
      all_columns_character = FALSE
    )
  )
  
  expect_true(
    is.numeric(result$id)
  )
  
  expect_true(
    is.numeric(result$value)
  )
  
  expect_type(
    result$category,
    "character"
  )
})


# Tests that source_file and source_folder correctly identify
# where each row originated.
test_that("read_csv_folder adds source information", {
  
  test_folder <- tempfile()
  subfolder <- file.path(
    test_folder,
    "june-2026"
  )
  
  fs::dir_create(
    subfolder,
    recurse = TRUE
  )
  
  on.exit(
    unlink(
      test_folder,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  readr::write_csv(
    tibble::tibble(
      id = 1
    ),
    file.path(
      subfolder,
      "csds.csv"
    )
  )
  
  result <- suppressMessages(
    read_csv_folder(
      folder = test_folder
    )
  )
  
  expect_equal(
    result$source_file,
    "csds.csv"
  )
  
  expect_equal(
    result$source_folder,
    "june-2026"
  )
})


# Tests that CSV file extensions are matched regardless
# of upper or lower case.
test_that("read_csv_folder reads uppercase CSV file extensions", {
  
  test_folder <- tempfile()
  fs::dir_create(test_folder)
  
  on.exit(
    unlink(
      test_folder,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  readr::write_csv(
    tibble::tibble(
      id = 1
    ),
    file.path(
      test_folder,
      "test.CSV"
    )
  )
  
  result <- suppressMessages(
    read_csv_folder(
      folder = test_folder
    )
  )
  
  expect_equal(
    nrow(result),
    1L
  )
})


# Tests that an empty tibble is returned and a message is shown
# when the folder contains no CSV files.
test_that("read_csv_folder returns an empty tibble when no CSV files are found", {
  
  test_folder <- tempfile()
  fs::dir_create(test_folder)
  
  on.exit(
    unlink(
      test_folder,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  expect_message(
    result <- read_csv_folder(
      folder = test_folder
    ),
    "No CSV files found"
  )
  
  expect_s3_class(
    result,
    "tbl_df"
  )
  
  expect_equal(
    nrow(result),
    0L
  )
})


# Tests that the function stops when the supplied folder
# does not exist.
test_that("read_csv_folder stops when the folder does not exist", {
  
  expect_error(
    read_csv_folder(
      folder = file.path(
        tempdir(),
        "folder_that_does_not_exist"
      )
    ),
    "Folder does not exist"
  )
})


# Tests that folder must be a single non-empty character value.
test_that("read_csv_folder validates folder", {
  
  expect_error(
    read_csv_folder(
      folder = ""
    ),
    "`folder` must be a single non-empty character value"
  )
  
  expect_error(
    read_csv_folder(
      folder = c(
        "folder1",
        "folder2"
      )
    ),
    "`folder` must be a single non-empty character value"
  )
  
  expect_error(
    read_csv_folder(
      folder = 123
    ),
    "`folder` must be a single non-empty character value"
  )
})


# Tests that all_columns_character must contain one
# non-missing logical value.
test_that("read_csv_folder validates all_columns_character", {
  
  test_folder <- tempfile()
  fs::dir_create(test_folder)
  
  on.exit(
    unlink(
      test_folder,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  expect_error(
    read_csv_folder(
      folder = test_folder,
      all_columns_character = "Yes"
    ),
    "`all_columns_character` must be TRUE or FALSE"
  )
  
  expect_error(
    read_csv_folder(
      folder = test_folder,
      all_columns_character = NA
    ),
    "`all_columns_character` must be TRUE or FALSE"
  )
})