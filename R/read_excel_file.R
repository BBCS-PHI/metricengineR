#' Read an Excel File
#'
#' Reads a specified sheet from an Excel file and adds the source file name
#' to the returned data.
#'
#' @param file_path Path to the Excel file.
#' @param sheet_name Name or number of the sheet to read. Defaults to `1`.
#'
#' @return A data frame containing the Excel data with an additional
#'   `source_file` column. Returns `NULL` if the file cannot be processed.
#'
#' @details
#' A message is printed when processing starts and when the file is read
#' successfully. If an error occurs while reading the workbook, a warning is
#' returned and the function returns `NULL`.
#'
#' @examples
#' \dontrun{
#' read_excel_file(
#'   file_path = "data/example.xlsx",
#'   sheet_name = "Data"
#' )
#' }
#'
#' @export
read_excel_file <- function(
    file_path, 
    sheet_name = 1) {
  
  # Validate inputs
  if(
    !is.character(file_path) ||
    length(file_path) != 1L ||
    is.na(file_path)
  ){
    stop(
      "`file_path` must be a single file path.",
      call. = FALSE
    )
  }
  
  if(!file.exists(file_path)){
    stop(
      paste0(
        "Excel file does not exist: ",
        file_path
      ),
      call. = FALSE
    )
  }
  
  if(
    length(sheet_name) != 1L ||
    is.na(sheet_name)
  ){
    stop(
      "`sheet_name` must contain a single sheet name or sheet number.",
      call. = FALSE
    )
  }
  
  # Read Excel file
  file_name <- basename(file_path)
  
  cli::cli_alert_info(
    "Processing Excel file: {file_name}"
  )
  
  result <- tryCatch(
    {
      
      df <- readxl::read_excel(
        path = file_path,
        sheet = sheet_name
      ) |>
        dplyr::mutate(
          source_file = file_name
          )
      
      cli::cli_alert_success(
        "Excel file processed successfully: {file_name}"
      )
      
      df
    },
    error = function(e) {
      
      cli::cli_alert_warning(
        "Could not process {file_name}: {conditionMessage(e)}"
      )
      
      NULL
    }
  )
  
  result
  
}
