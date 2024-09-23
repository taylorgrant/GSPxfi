#' Summarise the merged data
#'
#' This function allows the user to specify what to group by and then summarises all metrics within the platform. It also calculates overall averages for both video and static for each grouping
#'
#' @param data Merged dataset
#' @param group_vars Variables to group by
#'
#' @return Dataframw with summarized media KPIs
#' @export
#'
#' @examples
#' \dontrun{
#' meta_summary <- dat_summarise(merged_meta,
#' group_vars = c("platform", "ad_media_type", "platform_position",
#' "ad_objective", "creative_detail"))
#' }
merged_summarise <- function(data, group_vars){

  avg_groups <- group_vars[group_vars != "creative_detail"]

  if (any(unique(data$platform) == "Facebook")) {
    summarise_meta <- function(data) {
      data |>
        dplyr::summarise(avg_duration = stats::weighted.mean(avg_duration, impressions),
                         dplyr::across(total_results:platform_leads, sum)) |>
        dplyr::mutate(click_rate = clicks/impressions,
               like_rate = likes/impressions,
               share_rate = shares/impressions,
               comment_rate = comments/impressions,
               engagement_rate = engagements/impressions,
               view_rate = video_views/impressions,
               view3_rate = video_views3/impressions,
               view25_rate = view25/impressions,
               view50_rate = view50/impressions,
               view75_rate = view75/impressions,
               vcr = video_completes/impressions,
               visit_rate = platform_site_visits/impressions,
               purchase_rate = platform_purchases/impressions,
               lead_rate = platform_leads/impressions,
               cpm = (spend/impressions)*1000,
               cplc = spend/clicks,
               cpe = spend/engagements,
               cpv3 = spend/video_views,
               cpvc = spend/video_completes,
               cp_visit = spend/platform_site_visits,
               cp_purchase = spend/platform_purchases,
               cp_lead = spend/platform_leads)
    }
    # video averages
    video_avg <- data |>
      dplyr::filter(video_views > 0 & spend > 0) |>
      dplyr::filter(!is.na(platform)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(avg_groups))) |>
      summarise_meta() |>
      dplyr::mutate(creative_detail = "Video Average")
    # static average
    static_avg <- data |>
      dplyr::filter(video_views == 0 & spend > 0) |>
      dplyr::filter(!is.na(platform)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(avg_groups))) |>
      summarise_meta() |>
      dplyr::mutate(creative_detail = "Static Average")

    # summarise data and bind averages
    data |>
      dplyr::filter(spend > 0) |>
      dplyr::filter(!is.na(platform)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      summarise_meta() |>
      dplyr::bind_rows(video_avg) |>
      dplyr::bind_rows(static_avg)

  } else if (any(unique(data$platform) == "Snapchat")) {
    summarise_snap <- function(data) {
      data |>
        dplyr::summarise(avg_duration = stats::weighted.mean(avg_duration, impressions),
                         dplyr::across(c(total_results:reach, clicks:story_completes), sum)) |>
        dplyr::mutate(click_rate = clicks/impressions,
               share_rate = shares/impressions,
               save_rate = saves/impressions,
               view_rate = video_views/impressions,
               view25_rate = view25/impressions,
               view50_rate = view50/impressions,
               view75_rate = view75/impressions,
               vcr = video_completes/impressions,
               purchase_rate = platform_purchases/impressions,
               story_open_rate = story_opens/impressions,
               story_complete_rate = story_completes/impressions,
               cpm = (spend/impressions)*1000,
               cplc = spend/clicks,
               cpv = spend/video_views,
               cpvc = spend/video_completes,
               cp_purchase = spend/platform_purchases)
    }
    # video averages
    video_avg <- data |>
      dplyr::filter(video_views > 0 & spend > 0) |>
      dplyr::filter(!is.na(platform)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(avg_groups))) |>
      summarise_snap() |>
      dplyr::mutate(creative_detail = "Video Average")
    # static average
    static_avg <- data |>
      dplyr::filter(video_views == 0 & spend > 0) |>
      dplyr::filter(!is.na(platform)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(avg_groups))) |>
      summarise_snap() |>
      dplyr::mutate(creative_detail = "Static Average")

    # summarise data and bind averages
    data |>
      dplyr::filter(spend > 0) |>
      dplyr::filter(!is.na(platform)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      summarise_snap() |>
      dplyr::bind_rows(video_avg) |>
      dplyr::bind_rows(static_avg)

  } else if (any(unique(data$platform) == "TikTok")) {
    summarise_tiktok <- function(data) {
      data |>
        dplyr::summarise(avg_duration = stats::weighted.mean(avg_duration, impressions),
                         dplyr::across(total_results:video_completes, sum)) |>
        dplyr::mutate(click_rate = clicks/impressions,
               like_rate = likes/impressions,
               comment_rate = comments/impressions,
               share_rate = shares/impressions,
               follow_rate = follows/impressions,
               profile_visit_rate = profile_visits/impressions,
               view_rate = video_views/impressions,
               view2_rate = video_views2/impressions,
               view6_rate = video_views6/impressions,
               view6fv_rate = video_views6fv/impressions,
               view15fv_rate = video_views15fv/impressions,
               view25_rate = view25/impressions,
               view50_rate = view50/impressions,
               view75_rate = view75/impressions,
               vcr = video_completes/impressions,
               cpm = (spend/impressions)*1000,
               cplc = spend/clicks,
               cpv = spend/video_views,
               cpvc = spend/video_completes)
    }
    # video averages
    video_avg <- data |>
      dplyr::filter(video_views > 0 & spend > 0) |>
      dplyr::filter(!is.na(platform)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(avg_groups))) |>
      summarise_tiktok() |>
      dplyr::mutate(creative_detail = "Video Average")

    # summarise data and bind averages
    data |>
      dplyr::filter(spend > 0) |>
      dplyr::filter(!is.na(platform)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      summarise_tiktok() |>
      dplyr::bind_rows(video_avg)

  } else if (any(unique(data$platform) == "Reddit")) {
    summarise_reddit <- function(data) {
      data |>
        dplyr::summarise(dplyr::across(total_results:video_full, sum)) |>
        dplyr::mutate(click_rate = clicks/impressions,
               video_start_rate = video_starts/impressions,
               view_rate = video_views/impressions,
               view3_rate = video_views3/impressions,
               view25_rate = view25/impressions,
               view50_rate = view50/impressions,
               view75_rate = view75/impressions,
               vcr = video_completes/impressions,
               view_full_rate = video_full/impressions,
               cpm = (spend/impressions)*1000,
               cplc = spend/clicks,
               cpv = spend/video_views,
               cpvc = spend/video_completes)
    }
    # video averages
    video_avg <- data |>
      dplyr::filter(video_views > 0 & spend > 0) |>
      dplyr::filter(!is.na(platform)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(avg_groups))) |>
      summarise_reddit() |>
      dplyr::mutate(creative_detail = "Video Average")
    # static average
    static_avg <- data |>
      dplyr::filter(video_views == 0 & spend > 0) |>
      dplyr::filter(!is.na(platform)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(avg_groups))) |>
      summarise_reddit() |>
      dplyr::mutate(creative_detail = "Static Average")

    # summarise data and bind averages
    data |>
      dplyr::filter(spend > 0) |>
      dplyr::filter(!is.na(platform)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      summarise_reddit() |>
      dplyr::bind_rows(video_avg) |>
      dplyr::bind_rows(static_avg)
  } else if (any(unique(data$platform) == "Pinterest")) {
    summarise_pinterest <- function(data) {
      data |>
        dplyr::summarise(avg_duration = stats::weighted.mean(avg_duration, impressions),
                         dplyr::across(total_results:video_completes, sum)) |>
        dplyr::mutate(click_rate = clicks/impressions,
               outbound_click_rate = outbound_clicks/impressions,
               engagement_rate = engagements/impressions,
               save_rate = saves/impressions,
               view_rate = video_views/impressions,
               view3_rate = video_views3/impressions,
               view25_rate = view25/impressions,
               view50_rate = view50/impressions,
               view75_rate = view75/impressions,
               vcr = video_completes/impressions,
               cpm = (spend/impressions)*1000,
               cplc = spend/clicks,
               cpolc = spend/outbound_clicks,
               cpv = spend/video_views,
               cpvc = spend/video_completes)
    }
    # video averages
    video_avg <- data |>
      dplyr::filter(video_views > 0 & spend > 0) |>
      dplyr::filter(!is.na(platform)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(avg_groups))) |>
      summarise_pinterest() |>
      dplyr::mutate(creative_detail = "Video Average")
    # static average
    static_avg <- data |>
      dplyr::filter(video_views == 0 & spend > 0) |>
      dplyr::filter(!is.na(platform)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(avg_groups))) |>
      summarise_pinterest() |>
      dplyr::mutate(creative_detail = "Static Average")

    # summarise data and bind averages
    data |>
      dplyr::filter(spend > 0) |>
      dplyr::filter(!is.na(platform)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      summarise_pinterest() |>
      dplyr::bind_rows(video_avg) |>
      dplyr::bind_rows(static_avg)
  }
}
