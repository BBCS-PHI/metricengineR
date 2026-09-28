# Tests that the correct resource URL is returned when a matching
# ZIP resource exists for the publication period.
test_that("get_resource_link returns the matching resource URL", {
  
  testthat::local_mocked_bindings(
    read_html = function(x) {
      
      xml2::read_html(
        paste0(
          "<html><body>",
          "<a href='/files/csds-may-2026.zip'>CSV Data (as ZIP) May 2026</a>",
          "<a href='/files/csds-june-2026.zip'>CSV Data (as ZIP) June 2026</a>",
          "</body></html>"
        )
      )
    },
    .package = "rvest"
  )
  
  result <- suppressMessages(
    get_resource_link(
      publication_url = "https://digital.nhs.uk/publications/test/june-2026",
      resource_text = "CSV Data \\(as ZIP\\)"
    )
  )
  
  expect_equal(
    result,
    "https://digital.nhs.uk/files/csds-june-2026.zip"
  )
})


# Tests that period matching selects the resource belonging to the
# publication period rather than another matching resource.
test_that("get_resource_link matches the publication period", {
  
  testthat::local_mocked_bindings(
    read_html = function(x) {
      
      xml2::read_html(
        paste0(
          "<html><body>",
          "<a href='/files/january-2026.zip'>CSV Data (as ZIP) January 2026</a>",
          "<a href='/files/february-2026.zip'>CSV Data (as ZIP) February 2026</a>",
          "</body></html>"
        )
      )
    },
    .package = "rvest"
  )
  
  result <- suppressMessages(
    get_resource_link(
      publication_url = "https://digital.nhs.uk/publications/test/february-2026",
      resource_text = "CSV Data \\(as ZIP\\)",
      match_period = TRUE
    )
  )
  
  expect_equal(
    result,
    "https://digital.nhs.uk/files/february-2026.zip"
  )
})


# Tests that period matching can be disabled and the first matching
# resource is returned regardless of the publication period.
test_that("get_resource_link can retrieve a resource without period matching", {
  
  testthat::local_mocked_bindings(
    read_html = function(x) {
      
      xml2::read_html(
        paste0(
          "<html><body>",
          "<a href='/files/test-resource.zip'>CSV Data (as ZIP)</a>",
          "</body></html>"
        )
      )
    },
    .package = "rvest"
  )
  
  result <- suppressMessages(
    get_resource_link(
      publication_url = "https://digital.nhs.uk/publications/test/june-2026",
      resource_text = "CSV Data \\(as ZIP\\)",
      match_period = FALSE
    )
  )
  
  expect_equal(
    result,
    "https://digital.nhs.uk/files/test-resource.zip"
  )
})


# Tests that NA is returned and a warning alert is displayed when
# no resource matches the requested resource text.
test_that("get_resource_link returns NA when no matching resource is found", {
  
  testthat::local_mocked_bindings(
    read_html = function(x) {
      
      xml2::read_html(
        paste0(
          "<html><body>",
          "<a href='/files/other-file.zip'>Other Download June 2026</a>",
          "</body></html>"
        )
      )
    },
    .package = "rvest"
  )
  
  expect_message(
    result <- get_resource_link(
      publication_url = "https://digital.nhs.uk/publications/test/june-2026",
      resource_text = "CSV Data \\(as ZIP\\)"
    ),
    "No matching resource found"
  )
  
  expect_true(
    is.na(result)
  )
})


# Tests that NA is returned and the function reports the problem
# when the dataset page cannot be read.
test_that("get_resource_link handles a dataset page that cannot be read", {
  
  testthat::local_mocked_bindings(
    read_html = function(x) {
      stop("Connection failed.")
    },
    .package = "rvest"
  )
  
  expect_message(
    result <- get_resource_link(
      publication_url = "https://digital.nhs.uk/publications/test/june-2026",
      resource_text = "CSV Data \\(as ZIP\\)"
    ),
    "Could not read dataset page"
  )
  
  expect_true(
    is.na(result)
  )
})


# Tests that the function stops when match_period is TRUE but
# period_pattern cannot identify a reporting period in the URL.
test_that("get_resource_link stops when the publication period cannot be identified", {
  
  testthat::local_mocked_bindings(
    read_html = function(x) {
      
      xml2::read_html(
        paste0(
          "<html><body>",
          "<a href='/files/test.zip'>CSV Data (as ZIP)</a>",
          "</body></html>"
        )
      )
    },
    .package = "rvest"
  )
  
  expect_error(
    suppressMessages(
      get_resource_link(
        publication_url = "https://digital.nhs.uk/publications/test/latest",
        resource_text = "CSV Data \\(as ZIP\\)",
        match_period = TRUE
      )
    ),
    "period_pattern.*could not identify"
  )
})


# Tests that publication_url must be a single non-empty character value.
test_that("get_resource_link validates publication_url", {
  
  expect_error(
    get_resource_link(
      publication_url = "",
      resource_text = "CSV Data"
    ),
    "`publication_url` must be a single non-empty character value"
  )
  
  expect_error(
    get_resource_link(
      publication_url = c(
        "https://digital.nhs.uk/page1",
        "https://digital.nhs.uk/page2"
      ),
      resource_text = "CSV Data"
    ),
    "`publication_url` must be a single non-empty character value"
  )
})


# Tests that resource_text must be a single non-empty character value.
test_that("get_resource_link validates resource_text", {
  
  expect_error(
    get_resource_link(
      publication_url = "https://digital.nhs.uk/test/june-2026",
      resource_text = ""
    ),
    "`resource_text` must be a single non-empty character value"
  )
})


# Tests that match_period must contain one non-missing logical value.
test_that("get_resource_link validates match_period", {
  
  expect_error(
    get_resource_link(
      publication_url = "https://digital.nhs.uk/test/june-2026",
      resource_text = "CSV Data",
      match_period = "Yes"
    ),
    "`match_period` must be TRUE or FALSE"
  )
})