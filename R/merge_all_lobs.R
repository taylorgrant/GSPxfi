#' Merge all LOBs
#'
#' If you want to merge all LOB data at once, this function will do it
#'
#' @param platform Platform of interest (Meta, Pinterest, Snpachat, Reddit, TikTok)
#' @param email_address Email address that has access to the social matrices
#' @param sprinklr_data Platform specific Sprinklr data that had been read into memory
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' merged_meta <- merge_all_lobs("Meta", "first_last@gspsf.com", sprinklr_meta)
#' }
merge_all_lobs <- function(platform, email_address, sprinklr_data) {
  # putting functions together
  merge_all <- function(lob, platform, data) {
    matrix_data <- get_matrix_data(master_ids = master_ids, line = lob, platform = platform, email = email_address)
    merged <- xfi_merge(sprinklr_data = data, matrix_data = matrix_data, lob = lob)
  }
  if (platform %in% c("Meta", "Pinterest")) {
    lobs <- c("Central", "Internet/Product Diff", "NED", "Retargeting",  "West", "Xfinity Mobile")
  } else {
    lobs <- c("Central", "Internet/Product Diff", "NED",  "West", "Xfinity Mobile")
  }
  out <- purrr::map_dfr(lobs, ~merge_all(lob = .x, platform = platform, data = sprinklr_data))
}
