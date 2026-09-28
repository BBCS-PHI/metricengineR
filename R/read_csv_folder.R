#' Read CSV Files from a Folder
#'
#' Reads all CSV files from a folder and its subfolders and combines them into
#' a single data frame.
#'
#' By default, all columns are read as character values. This is useful for
#' source datasets where column types may vary between files or reporting
#' periods and can be standardised later in the processing pipeline.
#'
#' The source file name and source folder are added to each row to retain
#' information about where the data originated.
#'
#' @param folder Path to the folder containing the CSV files.
#' @param all_columns_character Logical. If `TRUE`, all columns are read as
#'   character values. Defaults to `TRUE`. If `FALSE`, column types are
#'   determined automatically by `readr::read_csv()`.
#'
#' @return A tibble containing the combined contents of all CSV files found.
#'   Two additional columns, `source_file` and `source_folder`, are added.
#'   If no CSV files are found, an empty tibble is returned.
#'
#' @examples
#' \dontrun{
#' data <- read_csv_folder(
#'   folder = "data/CSDS_downloads",
#'   all_columns_character = TRUE
#' )
#'
#' data
#' }
#'
#' @export
read_csv_folder <- function(
    folder,
    all_columns_character = TRUE
) {
  
  # Validate inputs
  
  if(
    !is.character(folder) ||
    length(folder) != 1L ||
    is.na(folder) ||
    !nzchar(folder)
  ){
    stop(
      "`folder` must be a single non-empty character value.",
      call. = FALSE
    )
  }
  
  if(
    !is.logical(all_columns_character) ||
    length(all_columns_character) != 1L ||
    is.na(all_columns_character)
  ){
    stop(
      "`all_columns_character` must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  
  if(!dir.exists(folder)){
    stop(
      paste0(
        "Folder does not exist: ",
        folder
      ),
      call. = FALSE
    )
  }
  
  
  # Find CSV files
  
  csv_files <- list.files(
    path = folder,
    pattern = "\\.csv$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  
  # Return empty tibble if no CSV files are found
  
  if(length(csv_files) == 0L){
    
    cli::cli_alert_warning(
      "No CSV files found in: {folder}"
    )
    
    return(
      tibble::tibble()
    )
  }
  
  
  cli::cli_alert_info(
    "Reading {length(csv_files)} CSV file(s) from: {folder}"
  )
  
  
  # Read and combine CSV files
  
  data <- purrr::map_dfr(
    csv_files,
    function(csv_file) {
      
      cli::cli_alert_info(
        "Reading: {basename(csv_file)}"
      )
      
      if(all_columns_character){
        
        file_data <- readr::read_csv(
          csv_file,
          col_types = readr::cols(
            .default = readr::col_character()
          ),
          show_col_types = FALSE,
          progress = FALSE
        )
        
      } else {
        
        file_data <- readr::read_csv(
          csv_file,
          show_col_types = FALSE,
          progress = FALSE
        )
      }
      
      
      # Add source information
      
      file_data |>
        dplyr::mutate(
          source_file = basename(csv_file),
          source_folder = basename(
            dirname(csv_file)
          ),
          .before = 1
        )
    }
  )
  
  
  cli::cli_alert_success(
    "Successfully read {length(csv_files)} CSV file(s) with {nrow(data)} total row(s)."
  )
  
  
  data
}