#' Read the downloaded Sprinklr data into memory
#'
#' @param file_location The location where the Sprinklr file is located
#'
#' @return Dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' sprinklr_data <- read_sprinklr(here::here("data", "Meta-Q324.xlsx"))
#' }
read_sprinklr <- function(file_location) {
  readxl::read_excel(file_location, skip = 2) |>
    janitor::clean_names() |>
    # get rid of dollar sign and make sure there is spend
    dplyr::mutate(spent_in_usd_sum = as.numeric(stringr::str_remove_all(spent_in_usd_sum, "\\$"))) |>
    dplyr::filter(spent_in_usd_sum > 0) |>
    # fix the gap spacing in the placement names
    dplyr::mutate(ad_variant_name = stringr::str_replace_all(ad_variant_name, " _|  _", "_"),
                  ad_variant_name = stringr::str_remove_all(ad_variant_name, "\\$"))
}
