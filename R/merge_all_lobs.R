#' Merge all LOBs
#'
#' If you want to merge all LOB data at once, this function will do it
#'
#' @param platform Platform of interest (Meta, Pinterest, Snpachat, Reddit, TikTok)
#' @param matrix_ids The file of matrix IDs used
#' @param email_address Email address that has access to the social matrices
#' @param sprinklr_data Platform specific Sprinklr data that had been read into memory
#'
#' @return Merged dataframe with Sprinklr and Social Matrix data
#' @export
#'
#' @examples
#' \dontrun{
#' merged_meta <- merge_all_lobs("Meta", "master_ids_q4", "first_last@gspsf.com", sprinklr_meta)
#' }
merge_all_lobs <- function(platform, matrix_ids, email_address, sprinklr_data) {
  # putting functions together
  merge_all <- function(lob, platform, data) {
    matrix_data <- get_matrix_data(master_ids = matrix_ids, line = lob, platform = platform, email = email_address)
    if (nrow(matrix_data) == 0) {
      return()
    } else {
      merged <- xfi_merge(sprinklr_data = data, matrix_data = matrix_data, lob = lob)
    }
  }
  if (platform %in% c("Meta", "Pinterest")) {
    lobs <- c("Central", "Internet/Product Diff", "NED", "Retargeting",  "West", "Xfinity Mobile")
  } else {
    lobs <- c("Central", "Internet/Product Diff", "NED",  "West", "Xfinity Mobile")
  }
  out <- purrr::map_dfr(lobs, ~merge_all(lob = .x, platform = platform, data = sprinklr_data))
}
