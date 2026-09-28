test_that("get_child_links returns matching publication links", {
  
  testthat::local_mocked_bindings(
    read_html = function(x) {
      
      xml2::read_html(
        paste0(
          "<html><body>",
          "<a href='/data-and-information/publications/statistical/test/january-2026/'>January 2026</a>",
          "<a href='/data-and-information/publications/statistical/test/january-2026/'>January duplicate</a>",
          "<a href='https://digital.nhs.uk/data-and-information/publications/statistical/test/february-2026/'>February 2026</a>",
          "<a href='/data-and-information/other-page'>Other page</a>",
          "</body></html>"
        )
      )
    },
    .package = "rvest"
  )
  
  result <- suppressMessages(
    get_child_links(
      parent_url = "https://digital.nhs.uk/data-and-information/publications/statistical/test",
      publication_pattern = "test/[a-z]+-[0-9]{4}/?$"
    )
  )
  
  expect_equal(
    result,
    c(
      "https://digital.nhs.uk/data-and-information/publications/statistical/test/january-2026",
      "https://digital.nhs.uk/data-and-information/publications/statistical/test/february-2026"
    )
  )
})


test_that("get_child_links returns no links when nothing matches", {
  
  testthat::local_mocked_bindings(
    read_html = function(x) {
      
      xml2::read_html(
        paste0(
          "<html><body>",
          "<a href='/data-and-information/other-page'>Other page</a>",
          "<a href='/another-page'>Another page</a>",
          "</body></html>"
        )
      )
    },
    .package = "rvest"
  )
  
  result <- suppressMessages(
    get_child_links(
      parent_url = "https://digital.nhs.uk/data-and-information/publications/statistical/test",
      publication_pattern = "test/[a-z]+-[0-9]{4}/?$"
    )
  )
  
  expect_equal(
    result,
    character(0)
  )
})


test_that("get_child_links validates parent_url", {
  
  expect_error(
    get_child_links(
      parent_url = c(
        "https://digital.nhs.uk/page1",
        "https://digital.nhs.uk/page2"
      ),
      publication_pattern = "test"
    ),
    "`parent_url` must be a single non-empty character value"
  )
  
  expect_error(
    get_child_links(
      parent_url = "",
      publication_pattern = "test"
    ),
    "`parent_url` must be a single non-empty character value"
  )
})


test_that("get_child_links validates publication_pattern", {
  
  expect_error(
    get_child_links(
      parent_url = "https://digital.nhs.uk/test",
      publication_pattern = c(
        "pattern1",
        "pattern2"
      )
    ),
    "`publication_pattern` must be a single non-empty character value"
  )
  
  expect_error(
    get_child_links(
      parent_url = "https://digital.nhs.uk/test",
      publication_pattern = ""
    ),
    "`publication_pattern` must be a single non-empty character value"
  )
})


test_that("get_child_links stops when the parent page cannot be read", {
  
  testthat::local_mocked_bindings(
    read_html = function(x) {
      stop(
        "Connection failed."
      )
    },
    .package = "rvest"
  )
  
  expect_error(
    suppressMessages(
      get_child_links(
        parent_url = "https://digital.nhs.uk/test",
        publication_pattern = "test"
      )
    ),
    "Could not read parent page"
  )
})