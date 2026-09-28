# Tests that the full publication workflow combines data from
# successfully identified, downloaded, and extracted resources.
test_that("get_publication_data runs the publication workflow successfully", {
  
  test_folder <- tempfile()
  
  on.exit(
    unlink(
      test_folder,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  testthat::local_mocked_bindings(
    
    get_child_links = function(
    parent_url,
    publication_pattern
    ) {
      
      c(
        "https://example.com/january-2026",
        "https://example.com/february-2026"
      )
    },
    
    get_resource_link = function(
    publication_url,
    ...
    ) {
      
      paste0(
        publication_url,
        ".zip"
      )
    },
    
    download_resource = function(
    resource_url,
    download_folder
    ) {
      
      file.path(
        download_folder,
        basename(resource_url)
      )
    },
    
    extract_zip = function(
    zip_file,
    extract_folder
    ) {
      
      file.path(
        extract_folder,
        tools::file_path_sans_ext(
          basename(zip_file)
        )
      )
    },
    
    read_csv_folder = function(
    folder,
    all_columns_character
    ) {
      
      tibble::tibble(
        source_folder = basename(folder),
        value = "test"
      )
    },
    
    .package = "metricengineR"
  )
  
  
  result <- suppressMessages(
    get_publication_data(
      parent_url = "https://example.com/publications",
      publication_pattern = "[a-z]+-[0-9]{4}$",
      resource_text = "CSV Data",
      download_folder = test_folder
    )
  )
  
  
  expect_equal(
    nrow(result$data),
    2L
  )
  
  expect_equal(
    nrow(result$catalogue),
    2L
  )
  
  expect_equal(
    length(result$publication_links),
    2L
  )
  
  expect_true(
    all(
      c(
        "publication_url",
        "resource_url",
        "downloaded_file",
        "extracted_folder"
      ) %in% names(result$catalogue)
    )
  )
})


# Tests that an empty result is returned when no publication
# pages are found on the parent page.
test_that("get_publication_data handles no publication links", {
  
  test_folder <- tempfile()
  
  on.exit(
    unlink(
      test_folder,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  testthat::local_mocked_bindings(
    
    get_child_links = function(
    parent_url,
    publication_pattern
    ) {
      
      character(0)
    },
    
    .package = "metricengineR"
  )
  
  
  expect_message(
    result <- get_publication_data(
      parent_url = "https://example.com/publications",
      publication_pattern = "test",
      resource_text = "CSV Data",
      download_folder = test_folder
    ),
    "No matching publication pages were found"
  )
  
  
  expect_equal(
    nrow(result$data),
    0L
  )
  
  expect_equal(
    nrow(result$catalogue),
    0L
  )
  
  expect_equal(
    result$publication_links,
    character(0)
  )
})


# Tests that an empty dataset is returned when publication pages
# exist but none contain a matching downloadable resource.
test_that("get_publication_data handles no matching resources", {
  
  test_folder <- tempfile()
  
  on.exit(
    unlink(
      test_folder,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  testthat::local_mocked_bindings(
    
    get_child_links = function(
    parent_url,
    publication_pattern
    ) {
      
      "https://example.com/january-2026"
    },
    
    get_resource_link = function(
    publication_url,
    ...
    ) {
      
      NA_character_
    },
    
    .package = "metricengineR"
  )
  
  
  expect_message(
    result <- get_publication_data(
      parent_url = "https://example.com/publications",
      publication_pattern = "test",
      resource_text = "CSV Data",
      download_folder = test_folder
    ),
    "No matching downloadable resources were found"
  )
  
  
  expect_equal(
    nrow(result$data),
    0L
  )
  
  expect_equal(
    nrow(result$catalogue),
    0L
  )
  
  expect_equal(
    result$publication_links,
    "https://example.com/january-2026"
  )
})


# Tests that failed ZIP extractions are handled safely and
# result in an empty combined dataset.
test_that("get_publication_data handles failed extractions", {
  
  test_folder <- tempfile()
  
  on.exit(
    unlink(
      test_folder,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  testthat::local_mocked_bindings(
    
    get_child_links = function(
    parent_url,
    publication_pattern
    ) {
      
      "https://example.com/january-2026"
    },
    
    get_resource_link = function(
    publication_url,
    ...
    ) {
      
      "https://example.com/january-2026.zip"
    },
    
    download_resource = function(
    resource_url,
    download_folder
    ) {
      
      file.path(
        download_folder,
        "january-2026.zip"
      )
    },
    
    extract_zip = function(
    zip_file,
    extract_folder
    ) {
      
      NA_character_
    },
    
    .package = "metricengineR"
  )
  
  
  expect_message(
    result <- get_publication_data(
      parent_url = "https://example.com/publications",
      publication_pattern = "test",
      resource_text = "CSV Data",
      download_folder = test_folder
    ),
    "No resources were successfully extracted"
  )
  
  
  expect_equal(
    nrow(result$data),
    0L
  )
  
  expect_true(
    is.na(
      result$catalogue$extracted_folder
    )
  )
})


# Tests that only folders extracted during the current workflow
# are passed to read_csv_folder.
test_that("get_publication_data reads only current extracted folders", {
  
  test_folder <- tempfile()
  
  on.exit(
    unlink(
      test_folder,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  folders_read <- character(0)
  
  testthat::local_mocked_bindings(
    
    get_child_links = function(
    parent_url,
    publication_pattern
    ) {
      
      c(
        "https://example.com/january-2026",
        "https://example.com/february-2026"
      )
    },
    
    get_resource_link = function(
    publication_url,
    ...
    ) {
      
      paste0(
        publication_url,
        ".zip"
      )
    },
    
    download_resource = function(
    resource_url,
    download_folder
    ) {
      
      file.path(
        download_folder,
        basename(resource_url)
      )
    },
    
    extract_zip = function(
    zip_file,
    extract_folder
    ) {
      
      file.path(
        extract_folder,
        tools::file_path_sans_ext(
          basename(zip_file)
        )
      )
    },
    
    read_csv_folder = function(
    folder,
    all_columns_character
    ) {
      
      folders_read <<- c(
        folders_read,
        folder
      )
      
      tibble::tibble(
        value = 1
      )
    },
    
    .package = "metricengineR"
  )
  
  
  suppressMessages(
    get_publication_data(
      parent_url = "https://example.com/publications",
      publication_pattern = "test",
      resource_text = "CSV Data",
      download_folder = test_folder
    )
  )
  
  
  expect_equal(
    length(folders_read),
    2L
  )
  
  expect_true(
    all(
      basename(folders_read) %in%
        c(
          "january-2026",
          "february-2026"
        )
    )
  )
})


# Tests that parent_url must be a single non-empty
# character value.
test_that("get_publication_data validates parent_url", {
  
  expect_error(
    get_publication_data(
      parent_url = "",
      publication_pattern = "test",
      resource_text = "CSV Data",
      download_folder = tempfile()
    ),
    "`parent_url` must be a single non-empty character value"
  )
})


# Tests that publication_pattern must be a single non-empty
# character value.
test_that("get_publication_data validates publication_pattern", {
  
  expect_error(
    get_publication_data(
      parent_url = "https://example.com",
      publication_pattern = "",
      resource_text = "CSV Data",
      download_folder = tempfile()
    ),
    "`publication_pattern` must be a single non-empty character value"
  )
})


# Tests that resource_text must be a single non-empty
# character value.
test_that("get_publication_data validates resource_text", {
  
  expect_error(
    get_publication_data(
      parent_url = "https://example.com",
      publication_pattern = "test",
      resource_text = "",
      download_folder = tempfile()
    ),
    "`resource_text` must be a single non-empty character value"
  )
})


# Tests that download_folder must be a single non-empty
# character value.
test_that("get_publication_data validates download_folder", {
  
  expect_error(
    get_publication_data(
      parent_url = "https://example.com",
      publication_pattern = "test",
      resource_text = "CSV Data",
      download_folder = ""
    ),
    "`download_folder` must be a single non-empty character value"
  )
})


# Tests that match_period must contain one non-missing
# logical value.
test_that("get_publication_data validates match_period", {
  
  expect_error(
    get_publication_data(
      parent_url = "https://example.com",
      publication_pattern = "test",
      resource_text = "CSV Data",
      download_folder = tempfile(),
      match_period = "Yes"
    ),
    "`match_period` must be TRUE or FALSE"
  )
})


# Tests that all_columns_character must contain one
# non-missing logical value.
test_that("get_publication_data validates all_columns_character", {
  
  expect_error(
    get_publication_data(
      parent_url = "https://example.com",
      publication_pattern = "test",
      resource_text = "CSV Data",
      download_folder = tempfile(),
      all_columns_character = "Yes"
    ),
    "`all_columns_character` must be TRUE or FALSE"
  )
})