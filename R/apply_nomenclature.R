#' Applying nomenclature splits to placement names
#'
#' @param tbl Dataframe that includes a column with the placement name
#'
#' @return Dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' sprinklr_data |> apply_nomenclature()
#' }
apply_nomenclature <- function(tbl) {
  options(warn = -1) # suppress the warning

  # placement and campaign lookup tables (in sysdata, are needed in this function)

  # taxonomy names
  place_taxonomy <- c("p_division_code", "p_region_code", "p_dma_code", "p_zip_code", "p_retargeting_segment",
                      "p_vendor_name", "p_sub_vendor_name", "p_platform", "p_placement_execution_type",
                      "p_programmatic_type", "p_placement_payment_method", "p_serving_method",
                      "p_video_inventory_type", "p_customer_type_audience", "p_research_study",
                      "p_placement_execution_size", "p_data_onboarder", "p_tactic", "p_data_source_targeting_type",
                      "p_targeting_kpi", "p_creative_target_decisioning_dco", "p_browsers_devices"
  )
  camp_taxonomy <- c("c_client_code", "c_billing_code", "c_po_estimate", "c_target_code", "c_campaign_type",
                     "c_year", "c_quarter", "c_campaign_objective", "c_mmm_category"
  )

  # convert lut to dataframe (so we can switch out error hyphen/dash)
  plut_tbl <- plut |>
    purrr::map_df(tibble::enframe, name = "index", .id = 'name') |>
    dplyr::mutate(old = paste0("-",index),
                  new = paste0("_", index)) |>
    dplyr::select(old, new) |>
    dplyr::filter(old != "-")

  # apply
  tbl |>
    # split on the agency name into 2 parts
    tidyr::separate(ad_variant_name, c('part1', 'part2'), "(?=XC|GRU|DTAS|GSP|Tierney|CANVAS)", remove = FALSE) |>
    # drop trailing hyphen/underscore
    dplyr::mutate(part1 = gsub("[[:punct:]]+$", "", part1),
                  # if a hyphen precedes a value from the LUT, put into a underscore
                  part1 = stringi::stri_replace_all_fixed(part1,
                                                          pattern = plut_tbl$old,
                                                          replacement = plut_tbl$new,
                                                          vectorize_all=FALSE),
                  # keep everything up until first dash
                  part1 = gsub("\\-.*", "", part1),
                  # if the string begins with a number, keep everything after ["number_"]
                  part1 = ifelse(stringr::str_detect(part1, "^[0-9]"), stringr::str_remove(part1, "^[^_]*_"), part1),
                  # if a hyphen in creative part, keep as spark creative note
                  spark_note = stringr::str_extract(part2, "(?<=-).*"),
                  # dropping trailing dash and hyphen - trying for better fuzzy match (DO I KEEP?)
                  ad_variant_name  = str_replace(ad_variant_name, "[-/][^-/]*$", "")) |>
    massive_cw() |>
    # TEMPORARY - there are some campaign names starting with DNU_ (for now, dropping from name)
    dplyr::mutate(paid_initiative_name = ifelse(stringr::str_detect(paid_initiative_name, "^DNU"),
                                    stringr::str_remove(paid_initiative_name, "^[^_]*_"),
                                    paid_initiative_name)) |>
    # now split out the campaign nomenclature
    tidyr::separate(paid_initiative_name, sep = "_", into = camp_taxonomy, remove = FALSE) |>
    dplyr::mutate(dplyr::across(c_client_code, ~clut$client_code[.]),
                  dplyr::across(c_billing_code, ~clut$billing_code[.]),
                  dplyr::across(c_target_code, ~clut$target_code[.]),
                  dplyr::across(c_campaign_type, ~clut$campaign_type[.]),
                  dplyr::across(c_year, ~clut$year[.]),
                  dplyr::across(c_quarter, ~clut$quarter[.]),
                  dplyr::across(c_campaign_objective, ~clut$campaign_objective[.]),
                  dplyr::across(c_mmm_category, ~clut$mmm_category[.])) |>
    data.frame()
}
