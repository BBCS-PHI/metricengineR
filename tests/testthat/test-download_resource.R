# Tests that a resource is downloaded successfully and the
# downloaded file path is returned.
test_that("download_resource downloads a file successfully", {
  
  download_folder <- tempfile()
  
  on.exit(
    unlink(
      download_folder,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  testthat::local_mocked_bindings(
    download.file = function(
    url,
    destfile,
    mode,
    quiet
    ) {
      
      writeLines(
        "test file",
        destfile
      )
      
      0
    },
    .package = "utils"
  )
  
  result <- suppressMessages(
    download_resource(
      resource_url = "https://example.com/test-file.zip",
      download_folder = download_folder
    )
  )
  
  expect_equal(
    result,
    file.path(
      download_folder,
      "test-file.zip"
    )
  )
  
  expect_true(
    file.exists(result)
  )
})


# Tests that URL query parameters are removed when creating
# the local file name.
test_that("download_resource removes query parameters from the file name", {
  
  download_folder <- tempfile()
  
  on.exit(
    unlink(
      download_folder,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  testthat::local_mocked_bindings(
    download.file = function(
    url,
    destfile,
    mode,
    quiet
    ) {
      
      writeLines(
        "test file",
        destfile
      )
      
      0
    },
    .package = "utils"
  )
  
  result <- suppressMessages(
    download_resource(
      resource_url = "https://example.com/test-file.zip?download=1",
      download_folder = download_folder
    )
  )
  
  expect_equal(
    basename(result),
    "test-file.zip"
  )
})


# Tests that an existing downloaded file is returned without
# attempting to download the resource again.
test_that("download_resource does not download an existing file again", {
  
  download_folder <- tempfile()
  
  fs::dir_create(
    download_folder
  )
  
  existing_file <- file.path(
    download_folder,
    "test-file.zip"
  )
  
  writeLines(
    "existing file",
    existing_file
  )
  
  on.exit(
    unlink(
      download_folder,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  testthat::local_mocked_bindings(
    download.file = function(...) {
      stop(
        "download.file should not have been called"
      )
    },
    .package = "utils"
  )
  
  expect_message(
    result <- download_resource(
      resource_url = "https://example.com/test-file.zip",
      download_folder = download_folder
    ),
    "Already downloaded"
  )
  
  expect_equal(
    result,
    existing_file
  )
})


# Tests that NA is returned when no resource URL was found.
test_that("download_resource returns NA for a missing resource URL", {
  
  result <- download_resource(
    resource_url = NA_character_,
    download_folder = tempfile()
  )
  
  expect_true(
    is.na(result)
  )
})


# Tests that a failed download returns NA and reports the
# download failure without stopping the wider extraction process.
test_that("download_resource handles a failed download", {
  
  download_folder <- tempfile()
  
  on.exit(
    unlink(
      download_folder,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  testthat::local_mocked_bindings(
    download.file = function(...) {
      stop(
        "Connection failed."
      )
    },
    .package = "utils"
  )
  
  expect_message(
    result <- download_resource(
      resource_url = "https://example.com/test-file.zip",
      download_folder = download_folder
    ),
    "Download failed"
  )
  
  expect_true(
    is.na(result)
  )
})


# Tests that resource_url must be a single character value.
test_that("download_resource validates resource_url", {
  
  expect_error(
    download_resource(
      resource_url = 123,
      download_folder = tempfile()
    ),
    "`resource_url` must be a single character value"
  )
  
  expect_error(
    download_resource(
      resource_url = c(
        "https://example.com/file1.zip",
        "https://example.com/file2.zip"
      ),
      download_folder = tempfile()
    ),
    "`resource_url` must be a single character value"
  )
})


# Tests that download_folder must be a single non-empty
# character value.
test_that("download_resource validates download_folder", {
  
  expect_error(
    download_resource(
      resource_url = "https://example.com/test-file.zip",
      download_folder = ""
    ),
    "`download_folder` must be a single non-empty character value"
  )
  
  expect_error(
    download_resource(
      resource_url = "https://example.com/test-file.zip",
      download_folder = c(
        "folder1",
        "folder2"
      )
    ),
    "`download_folder` must be a single non-empty character value"
  )
})