#' Merge Social Matrix data with Sprinklr
#'
#' This takes the Sprinklr data, splits out the placement name and applies the nomenclature to it. It then
#' uses \code{\link[fuzzyjoin:fuzzy_full_join]{fuzzy_full_join function}}. Finally,
#' we stanardize the column names - for some reason, Sprinklr column names differ by platform.
#'
#' Running the function also checks how well the merge worked and reports the results in the console.
#'
#' @param lob The line of business of interest
#'
#' @return Dataframe of merged data (Sprinklr with Social Matrix data)
#' @export
#'
#' @examples
#' \dontrun{
#' merged <- xfi_merge("West")
#' }
xfi_merge <- function(lob){
  # filter down to specific LOB
  tmp_sprinklr <- sprinklr_data |>
    dplyr::filter(stringr::str_detect(ad_account, lob)) |>
    apply_nomenclature()

  # fuzzy full join sprinklr data to matrix data
  tmp_full <- tmp_sprinklr |>
    fuzzyjoin::fuzzy_full_join(tidyr::unnest(matrix_data, col = matrix_data),
                               by = c("ad_variant_name" = "placement_name"),
                               match_fun = stringr::str_detect) |>
    clean_headers() |>
    dplyr::filter(!is.na(campaign_name)) |>
    dplyr::distinct(part2, ad_post_permalink, platform_position, ad_objective,
                    total_results, spend, impressions, .keep_all = TRUE)

  # report out on where we're missing
  report1 <- tmp_full |>
    dplyr::mutate(Merged = ifelse(is.na(placement_name), "NOT MATCHED", "MATCHED")) |>
    dplyr::group_by(Merged) |>
    dplyr::summarise(Placements = dplyr::n(),
                     Spend = scales::dollar(sum(spend, na.rm = TRUE))) |>
    knitr::kable(format = "rst")

  cat("\n",crayon::bgBlue(crayon::black(crayon::bold("Checking in on how many placements were successfully merged..."))))
  print(report1)

  report2 <- tmp_full |>
    dplyr::mutate(Merged = ifelse(is.na(placement_name), "NOT MATCHED", "MATCHED")) |>
    dplyr::group_by(campaign_name, Merged) |>
    dplyr::summarise(Placements = dplyr::n(),
                     Spend = scales::dollar(sum(spend, na.rm = TRUE)), .groups = "keep") |>
    knitr::kable(format = "rst")
  cat("\n",crayon::bgBlue(crayon::black(crayon::bold("Breaking down merged placements by campaign..."))))
  print(report2)
  return(tmp_full)
}
