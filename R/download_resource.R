#' Download a Resource File
#'
#' Downloads a file from a supplied resource URL and saves it to a specified
#' folder.
#'
#' The download folder is created automatically if it does not already exist.
#' If the file has already been downloaded, the existing file is returned
#' without downloading it again.
#'
#' @param resource_url URL of the resource file to download.
#' @param download_folder Folder where the downloaded file should be saved.
#'
#' @return A character value containing the path to the downloaded file.
#'   Returns `NA_character_` if the download fails.
#'
#' @examples
#' \dontrun{
#' downloaded_file <- download_resource(
#'   resource_url = paste0(
#'     "https://digital.nhs.uk/example/",
#'     "community-services-data.zip"
#'   ),
#'   download_folder = "data/CSDS_downloads"
#' )
#'
#' downloaded_file
#' }
#'
#' @export
download_resource <- function(
    resource_url,
    download_folder
) {
  
  # Validate inputs
  
  if(
    !is.character(resource_url) ||
    length(resource_url) != 1L
  ){
    stop(
      "`resource_url` must be a single character value.",
      call. = FALSE
    )
  }
  
  if(
    !is.character(download_folder) ||
    length(download_folder) != 1L ||
    is.na(download_folder) ||
    !nzchar(download_folder)
  ){
    stop(
      "`download_folder` must be a single non-empty character value.",
      call. = FALSE
    )
  }
  
  
  # Return NA where no resource URL was found
  
  if(is.na(resource_url)){
    return(
      NA_character_
    )
  }
  
  
  # Create download folder
  
  fs::dir_create(
    download_folder,
    recurse = TRUE
  )
  
  
  # Remove query string before deriving the file name
  
  clean_url <- stringr::str_remove(
    resource_url,
    "\\?.*$"
  )
  
  file_name <- basename(
    clean_url
  )
  
  output_file <- file.path(
    download_folder,
    file_name
  )
  
  
  # Do not download the file again if it already exists
  
  if(file.exists(output_file)){
    
    cli::cli_alert_info(
      "Already downloaded: {file_name}"
    )
    
    return(
      output_file
    )
  }
  
  
  # Download file
  
  cli::cli_alert_info(
    "Downloading: {file_name}"
  )
  
  result <- tryCatch(
    {
      
      utils::download.file(
        resource_url,
        destfile = output_file,
        mode = "wb",
        quiet = TRUE
      )
      
      cli::cli_alert_success(
        "Downloaded successfully: {file_name}"
      )
      
      output_file
      
    },
    
    error = function(e){
      
      cli::cli_alert_warning(
        "Download failed for {resource_url}: {conditionMessage(e)}"
      )
      
      NA_character_
    }
  )
  
  
  result
}