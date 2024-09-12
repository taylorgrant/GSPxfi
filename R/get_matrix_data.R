#' Read in the line items from Social Matrix sheets
#'
#' @param master_ids The name of the master matrix_ids dataframe in working memory
#' @param line The division or type of ad account
#' @param platform The platform of matrix data
#' @param email Your GSP gmail to authenticate and access the sheets
#'
#' @return Data frame with LOB, Platform, link to the Matrix, name of the sheet, and the matrix data
#' @export
#'
#' @examples
#' \dontrun{
#' test <- get_matrix_data(master_ids, line = "West",
#' platform = "Meta", email = "first_last@email.com")
#' }
get_matrix_data <- function(master_ids,
                            line = c("Central", "Internet/Product Diff", "NED",
                                     "Retargeting", "West", "Xfinity Mobile"),
                            platform = c("LinkedIn", "Meta", "Nextdoor", "Pinterest",
                                         "Reddit", "Snapchat", "TikTok", "Twitter"),
                            email){
  # authenticate googledrive & googlesheets
  options(gargle_oauth_email = email)

  if (line == "Retargeting" && !platform %in% c("Meta", "Pinterest")) {
    stop("\nFunction Stopped: Retargeting tactics are only used with Meta & Pinterest")
  }

  # filter the master IDs down to the link that we're interested in
  tmp_links <- master_ids |>
    dplyr::filter(dplyr::if_any(2, ~ . == platform) &
                    lob == line)

  # given a hyperlink, get the sheetnames associated
  get_sheet_names <- function(link) {
    link |>
      googlesheets4::gs4_get() |>
      # anonymous function to work with new pipe
      (\(x) x$sheets)() |>
      # drop these sheet names
      dplyr::filter(!name %in% c("Meta Template", "FBCPLISTS", "Glossary", "Social Ad Specs", # meta
                          "LinkedIn Template", "LICPLISTS", # linkedin
                          "NXTDCPLISTS", "Social Ad Specs ", "Nextdoor Template", # nextdoor
                          "Pinterest Template", "PINCPLISTS", # pinterest
                          "Reddit Template", "REDCPLISTS", # reddit
                          "Snapchat Template", "SCCPLISTS", # snapchat
                          "TikTok Template", "TIKTOKCPLISTS", # tiktok
                          "Twitter Template", "TWCPLISTS" # twitter
      )) |>
      dplyr::select(name)
  }

  # get all sheet names from each workbook and unnest
  tmpout <- tmp_links |>
    dplyr::group_by(lob) |>
    dplyr::mutate(sheet_names = purrr::map(hyperlink, get_sheet_names)) |>
    tidyr::unnest(cols = sheet_names)

  # function to get all placement data from each sheet
  get_sheet_data <- function(link, sheet) {
    if (platform == "Meta") {
      name_lookup <- c(placement_name = "placement_name_new",
                       placement_name = "new_placement_name",
                       post_text = "post_copy")

      googlesheets4::read_sheet(link, sheet = sheet) |>
        janitor::row_to_names(row_number = 2) |>
        janitor::clean_names() |>
        # rename any columns that don't have the same names
        dplyr::rename(dplyr::any_of(name_lookup)) |>
        dplyr::filter(!is.na(required_field_for_creative_launch)) |>
        dplyr::filter(!stringr::str_detect(required_field_for_creative_launch, "^Creative Friendly Name")) |>
        dplyr::mutate(dplyr::across(dplyr::matches("date"), ~lapply(., as.character))) |>
        tidyr::unnest(cols = c(flight_start_date, creative_launch_date,
                               creative_end_date,
                               flight_date_start_date, flight_date_end_date),
                      keep_empty = TRUE) |>
        dplyr::mutate(dplyr::across(dplyr::matches("date"), lubridate::ymd),
                      placement_name = stringr::str_replace_all(placement_name, " _", "_"),
                      placement_name = ifelse(stringr::str_count(placement_name, "_") == 0, NA, placement_name) # if there aren't underscores, NA
        ) |>
        # keep only lines with a placement name (or should it be prisma campaign name?)
        dplyr::filter(!is.na(placement_name)) |>
        # keeping certain columns
        dplyr::select(required_field_for_creative_launch, flight_date_start_date, flight_date_end_date,
                      creative_agency, mmm_category, ad_unit, price_point, cta,
                      post_text, post_headline, campaign_tactic, customer_type, geographic_target,
                      market, click_thru_url, prisma_io_campaign_name, placement_name)

    } else if (platform == "Snapchat") {
      name_lookup <- c(placement_name = "placement_name_new",
                       placement_name = "new_placement_name",
                       post_text = "post_copy")

      googlesheets4::read_sheet(link, sheet = sheet) |>
        janitor::row_to_names(row_number = 2) |>
        janitor::clean_names() |>
        # rename any columns that don't have the same names
        dplyr::rename(dplyr::any_of(name_lookup)) |>
        dplyr::filter(!is.na(required_field_for_creative_launch)) |>
        dplyr::filter(!stringr::str_detect(required_field_for_creative_launch, "^Creative Friendly Name")) |>
        dplyr::mutate(dplyr::across(dplyr::matches("date"), ~lapply(., as.character))) |>
        tidyr::unnest(cols = c(flight_start_date, creative_launch_date,
                               creative_end_date,
                               flight_date_start_date, flight_date_end_date),
                      keep_empty = TRUE) |>
        dplyr::mutate(dplyr::across(dplyr::matches("date"), lubridate::ymd),
                      placement_name = stringr::str_replace_all(placement_name, " _", "_"),
                      placement_name = ifelse(stringr::str_count(placement_name, "_") == 0, NA, placement_name) # if there aren't underscores, NA
        ) |>
        # keep only lines with a placement name (or should it be prisma campaign name?)
        dplyr::filter(!is.na(placement_name)) |>
        dplyr::select(required_field_for_creative_launch, flight_date_start_date, flight_date_end_date,
                      creative_agency, mmm_category, ad_unit, price_point, cta, creative_asset,
                      headline, discover_title_headline, campaign_tactic, customer_type, geographic_target,
                      market, click_thru_url, prisma_io_campaign_name, placement_name)

    } else if (platform == "Nextdoor") {
      name_lookup <- c(placement_name = "placement_name_new",
                       placement_name = "new_placement_name",
                       post_text = "post_copy")

      googlesheets4::read_sheet(link, sheet = sheet) |>
        janitor::row_to_names(row_number = 2) |>
        janitor::clean_names() |>
        # rename any columns that don't have the same names
        dplyr::rename(dplyr::any_of(name_lookup)) |>
        dplyr::filter(!is.na(required_field_for_creative_launch)) |>
        dplyr::filter(!stringr::str_detect(required_field_for_creative_launch, "^Creative Friendly Name")) |>
        dplyr::mutate(dplyr::across(dplyr::matches("date"), ~lapply(., as.character))) |>
        tidyr::unnest(cols = c(flight_start_date, creative_launch_date,
                               creative_end_date,
                               flight_date_start_date, flight_date_end_date),
                      keep_empty = TRUE) |>
        dplyr::mutate(dplyr::across(dplyr::matches("date"), lubridate::ymd),
                      placement_name = stringr::str_replace_all(placement_name, " _", "_"),
                      placement_name = ifelse(stringr::str_count(placement_name, "_") == 0, NA, placement_name) # if there aren't underscores, NA
        ) |>
        # keep only lines with a placement name (or should it be prisma campaign name?)
        dplyr::filter(!is.na(placement_name)) |>
        dplyr::select(required_field_for_creative_launch, flight_date_start_date, flight_date_end_date,
                      creative_agency, mmm_category, ad_unit, price_point, cta, creative_asset,
                      headline, body_copy, offer_text, campaign_tactic, customer_type, geographic_target,
                      market, click_thru_url, prisma_io_campaign_name, placement_name)

    } else if (platform == "Pinterest") {
      name_lookup <- c(placement_name = "placement_name_new",
                       placement_name = "new_placement_name",
                       post_text = "post_copy")

      googlesheets4::read_sheet(link, sheet = sheet) |>
        janitor::row_to_names(row_number = 2) |>
        janitor::clean_names() |>
        # rename any columns that don't have the same names
        dplyr::rename(dplyr::any_of(name_lookup)) |>
        dplyr::filter(!is.na(required_field_for_creative_launch)) |>
        dplyr::filter(!stringr::str_detect(required_field_for_creative_launch, "^Creative Friendly Name")) |>
        dplyr::mutate(dplyr::across(dplyr::matches("date"), ~lapply(., as.character)),
                      prisma_io_campaign_name = unlist(prisma_io_campaign_name),
                      prisma_io_campaign_name = as.character(prisma_io_campaign_name)) |>
        tidyr::unnest(cols = c(flight_start_date, creative_launch_date,
                               creative_end_date,
                               flight_date_start_date, flight_date_end_date),
                      keep_empty = TRUE) |>
        dplyr::mutate(dplyr::across(dplyr::matches("date"), lubridate::ymd),
                      placement_name = stringr::str_replace_all(placement_name, " _", "_"),
                      placement_name = ifelse(stringr::str_count(placement_name, "_") == 0, NA, placement_name) # if there aren't underscores, NA
        ) |>
        # keep only lines with a placement name (or should it be prisma campaign name?)
        dplyr::filter(!is.na(placement_name)) |>
        dplyr::select(required_field_for_creative_launch, flight_date_start_date, flight_date_end_date,
                      creative_agency, mmm_category, ad_unit, price_point, cta, creative_asset,
                      pin_description, pin_headline, board, campaign_tactic, customer_type, geographic_target,
                      market, click_thru_url, prisma_io_campaign_name, placement_name)

    } else if (platform == "Reddit") {
      name_lookup <- c(placement_name = "placement_name_new",
                       placement_name = "new_placement_name",
                       post_text = "post_copy")

      googlesheets4::read_sheet(link, sheet = sheet) |>
        janitor::row_to_names(row_number = 2) |>
        janitor::clean_names() |>
        # rename any columns that don't have the same names
        dplyr::rename(dplyr::any_of(name_lookup)) |>
        dplyr::filter(!is.na(required_field_for_creative_launch)) |>
        dplyr::filter(!stringr::str_detect(required_field_for_creative_launch, "^Creative Friendly Name")) |>
        dplyr::mutate(dplyr::across(dplyr::matches("date"), ~lapply(., as.character)),
                      prisma_io_campaign_name = unlist(prisma_io_campaign_name),
                      prisma_io_campaign_name = as.character(prisma_io_campaign_name)) |>
        tidyr::unnest(cols = c(flight_start_date, creative_launch_date,
                               creative_end_date,
                               flight_date_start_date, flight_date_end_date),
                      keep_empty = TRUE) |>
        dplyr::mutate(dplyr::across(dplyr::matches("date"), lubridate::ymd),
                      placement_name = stringr::str_replace_all(placement_name, " _", "_"),
                      placement_name = ifelse(stringr::str_count(placement_name, "_") == 0, NA, placement_name) # if there aren't underscores, NA
        ) |>
        # keep only lines with a placement name (or should it be prisma campaign name?)
        dplyr::filter(!is.na(placement_name)) |>
        dplyr::select(required_field_for_creative_launch, flight_date_start_date, flight_date_end_date,
                      creative_agency, mmm_category, ad_unit, price_point, cta, creative_asset,
                      post_headline, additional_text, campaign_tactic, customer_type, geographic_target,
                      market, click_thru_url, prisma_io_campaign_name, placement_name)

    } else if (platform == "TikTok") {
      name_lookup <- c(placement_name = "placement_name_new",
                       placement_name = "new_placement_name",
                       post_text = "post_copy")

      googlesheets4::read_sheet(link, sheet = sheet) |>
        janitor::row_to_names(row_number = 2) |>
        janitor::clean_names() |>
        # rename any columns that don't have the same names
        dplyr::rename(dplyr::any_of(name_lookup)) |>
        dplyr::filter(!is.na(required_field_for_creative_launch)) |>
        dplyr::filter(!stringr::str_detect(required_field_for_creative_launch, "^Creative Friendly Name")) |>
        dplyr::mutate(dplyr::across(dplyr::matches("date"), ~lapply(., as.character)),
                      prisma_io_campaign_name = unlist(prisma_io_campaign_name),
                      prisma_io_campaign_name = as.character(prisma_io_campaign_name)) |>
        tidyr::unnest(cols = c(flight_start_date, creative_launch_date,
                               creative_end_date,
                               flight_date_start_date, flight_date_end_date),
                      keep_empty = TRUE) |>
        dplyr::mutate(dplyr::across(dplyr::matches("date"), lubridate::ymd),
                      placement_name = stringr::str_replace_all(placement_name, " _", "_"),
                      placement_name = ifelse(stringr::str_count(placement_name, "_") == 0, NA, placement_name) # if there aren't underscores, NA
        ) |>
        # keep only lines with a placement name (or should it be prisma campaign name?)
        dplyr::filter(!is.na(placement_name)) |>
        dplyr::select(required_field_for_creative_launch, flight_date_start_date, flight_date_end_date,
                      creative_agency, mmm_category, ad_unit, price_point, cta, creative_asset,
                      post_text, campaign_tactic, customer_type, geographic_target,
                      market, click_thru_url, prisma_io_campaign_name, placement_name)
    }
  }
  out <- tmpout |>
    dplyr::mutate(matrix_data = purrr::map2(hyperlink, name, get_sheet_data)) |>
    dplyr::filter(purrr::map_int(matrix_data, nrow) > 0) # drop any rows with 0 data
}
