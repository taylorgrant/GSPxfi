#' Read in the Xfinity Social Matrices list
#'
#' The Xfinity social matrices list is the overall sheet that contains hyperlinks to each
#' social matrix. The social matrices cover all social channels for each LOB of Xfinity.
#'
#' Note that the user is required to have the workbook ID of the sheet saved as an environmental variable `XFI_MATRIX_ID`
#'
#'
#' @param sheet The name or number of the sheet within the workbook
#' @param full_range The lower range that the sheet covers (e.g., B43)
#' @param email A google email to authenticate with \code{\link[googlesheets4]{googlesheets4-package}}
#'
#' @return Dataframe with the LOB, the matrix/social platform name, and the link associated with each
#' @export
#'
#' @examples
#' \dontrun{
#' q3_matrices <- get_matrix_ids(sheet = 3, full_range = "B43", email = first_last.email.com)
#' }
get_matrix_ids <- function(sheet, full_range, email) {
  options(gargle_oauth_email = email)
  # master ID
  id <- Sys.getenv("XFI_MATRIX_ID")
  ss <- googlesheets4::gs4_get(id)
  # read in text format
  list_text <- googlesheets4::read_sheet(ss, sheet = sheet) |>
    janitor::clean_names() |>
    dplyr::select(1:2) |>
    tidyr::fill(lob, .direction = "down")
  # read in hyperlinks
  cells <- googlesheets4::range_read_cells(ss, sheet = sheet, range = glue::glue("A1:{full_range}"), cell_data = "full")
  # function to extract hyperlinks
  extract_hyperlink <- function(cell) {
    if (!is.null(cell$hyperlink)) {
      return(cell$hyperlink)
    } else {
      return(NA)
    }
  }
  # use function
  hyperlinks <- cells |>
    dplyr::mutate(hyperlink = purrr::map_chr(cell, extract_hyperlink)) |>
    dplyr::filter(stringr::str_detect(loc, "^B")) |>
    dplyr::filter(row != 1) |>
    dplyr::select(hyperlink)
  # bind together
  out <- cbind(list_text, hyperlinks)
}













