#' Make a URL Absolute
#'
#' Converts relative or root-relative URLs into absolute URLs using a supplied
#' base URL. URLs that are already absolute are returned unchanged.
#'
#' This helper is useful when extracting links from web pages where the `href`
#' attribute may contain either a complete URL or a relative path.
#'
#' @param link Character vector containing absolute or relative URLs to resolve.
#'   For example, `"/data-and-information/publications/test"`.
#' @param base_url Base URL used to resolve relative links. This can be the
#'   website root or the full URL of the page from which the links were
#'   extracted.
#'
#' @return A character vector containing absolute URLs.
#'
#' @examples
#' make_absolute_url(
#'   link = "/data-and-information/publications/test",
#'   base_url = "https://digital.nhs.uk"
#' )
#'
#' make_absolute_url(
#'   link = "https://digital.nhs.uk/test",
#'   base_url = "https://digital.nhs.uk"
#' )
#'
#' @export
make_absolute_url <- function(
    link,
    base_url
) {
  
  if(!is.character(link)){
    stop(
      "`link` must be a character vector.",
      call. = FALSE
    )
  }
  
  if(
    !is.character(base_url) ||
    length(base_url) != 1L ||
    is.na(base_url)
  ){
    stop(
      "`base_url` must be a single character value.",
      call. = FALSE
    )
  }
  
  xml2::url_absolute(
    link,
    base_url
  )
}