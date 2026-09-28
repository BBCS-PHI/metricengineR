#' Extract a ZIP File
#'
#' Extracts a ZIP file into a subfolder named after the ZIP file.
#'
#' If the extraction folder already exists, it is removed before extraction.
#' This prevents files from previous extractions remaining in the folder when
#' they are no longer present in the current ZIP file.
#'
#' @param zip_file Path to the ZIP file to extract.
#' @param extract_folder Parent folder where the extracted files should be
#'   stored.
#'
#' @return A character value containing the path to the extracted folder.
#'   Returns `NA_character_` if the ZIP file is missing or extraction fails.
#'
#' @examples
#' \dontrun{
#' extracted_folder <- extract_zip(
#'   zip_file = "data/CSDS_downloads/csds-june-2026.zip",
#'   extract_folder = "data/CSDS_downloads"
#' )
#'
#' extracted_folder
#' }
#'
#' @export
extract_zip <- function(
    zip_file,
    extract_folder
) {
  
  # Validate inputs
  
  if(
    !is.character(zip_file) ||
    length(zip_file) != 1L
  ){
    stop(
      "`zip_file` must be a single character value.",
      call. = FALSE
    )
  }
  
  if(
    !is.character(extract_folder) ||
    length(extract_folder) != 1L ||
    is.na(extract_folder) ||
    !nzchar(extract_folder)
  ){
    stop(
      "`extract_folder` must be a single non-empty character value.",
      call. = FALSE
    )
  }
  
  
  # Return NA where no ZIP file was supplied
  
  if(
    is.na(zip_file) ||
    !file.exists(zip_file)
  ){
    return(
      NA_character_
    )
  }
  
  
  # Create output folder name
  
  folder_name <- tools::file_path_sans_ext(
    basename(zip_file)
  )
  
  output_folder <- file.path(
    extract_folder,
    folder_name
  )
  
  
  # Remove previous extraction folder
  
  if(fs::dir_exists(output_folder)){
    fs::dir_delete(
      output_folder
    )
  }
  
  
  # Create extraction folder
  
  fs::dir_create(
    output_folder,
    recurse = TRUE
  )
  
  
  # Extract ZIP file
  
  cli::cli_alert_info(
    "Extracting: {basename(zip_file)}"
  )
  
  result <- tryCatch(
    {
      
      withCallingHandlers(
        {
          utils::unzip(
            zipfile = zip_file,
            exdir = output_folder,
            overwrite = TRUE
          )
        },
        warning = function(w){
          stop(
            conditionMessage(w),
            call. = FALSE
          )
        }
      )
      
      cli::cli_alert_success(
        "Extracted successfully: {basename(zip_file)}"
      )
      
      output_folder
      
    },
    
    error = function(e){
      
      # Remove incomplete extraction folder
      
      if(fs::dir_exists(output_folder)){
        fs::dir_delete(
          output_folder
        )
      }
      
      cli::cli_alert_warning(
        "Could not extract {zip_file}: {conditionMessage(e)}"
      )
      
      NA_character_
    }
  )
  
  result
}