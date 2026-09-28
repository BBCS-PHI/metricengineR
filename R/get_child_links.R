#' Get Child Publication Links
#'
#' Retrieves publication links from a parent web page that match a supplied
#' regular expression and converts them to absolute URLs.
#'
#' This is useful for publication landing pages that contain links to multiple
#' reporting periods, such as monthly NHS Digital publication pages.
#'
#' @param parent_url URL of the parent publication page containing links to
#'   individual publication pages.
#' @param publication_pattern Regular expression used to identify the required
#'   publication links from all links found on the parent page.
#'
#' @return A character vector containing unique matching publication URLs.
#'   Relative links are converted to absolute URLs and trailing slashes are
#'   removed.
#'
#' @examples
#' \dontrun{
#' publication_links <- get_child_links(
#'   parent_url = paste0(
#'     "https://digital.nhs.uk/data-and-information/publications/",
#'     "statistical/community-services-statistics-for-children-young-people-and-adults"
#'   ),
#'   publication_pattern = paste0(
#'     "community-services-statistics-for-children-young-people-and-adults/",
#'     "[a-z]+-[0-9]{4}/?$"
#'   )
#' )
#'
#' publication_links
#' }
#'
#' @export
get_child_links <- function(
    parent_url,
    publication_pattern
) {
  
  # Validate inputs
  
  if(
    !is.character(parent_url) ||
    length(parent_url) != 1L ||
    is.na(parent_url) ||
    !nzchar(parent_url)
  ){
    stop(
      "`parent_url` must be a single non-empty character value.",
      call. = FALSE
    )
  }
  
  if(
    !is.character(publication_pattern) ||
    length(publication_pattern) != 1L ||
    is.na(publication_pattern) ||
    !nzchar(publication_pattern)
  ){
    stop(
      "`publication_pattern` must be a single non-empty character value.",
      call. = FALSE
    )
  }
  
  
  # Read parent page
  
  cli::cli_alert_info(
    "Reading parent page: {parent_url}"
  )
  
  page <- tryCatch(
    {
      rvest::read_html(
        parent_url
      )
    },
    error = function(e){
      
      stop(
        paste0(
          "FAIL: Could not read parent page: ",
          parent_url,
          " - ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
  
  
  # Extract links
  
  links <- page |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    stats::na.omit() |>
    unique()
  
  
  # Keep publication links matching the requested pattern
  
  links <- links[
    stringr::str_detect(
      links,
      publication_pattern
    )
  ]
  
  
  # Convert to absolute URLs
  
  links <- make_absolute_url(
    links,
    parent_url
  )
  
  
  # Remove trailing slashes and duplicates
  
  links <- links |>
    stringr::str_remove("/$") |>
    unique()
  
  
  cli::cli_alert_success(
    "Found {length(links)} matching publication page(s)."
  )
  
  
  links
}