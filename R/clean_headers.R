#' Clean the column names of Sprinklr data
#'
#' Sprinklr has idiosyncratic column names that depend on the platform
#'
#' @param tbl Dataframe with Sprinklr names
#'
#' @return Dataframe with updated names
#' @export
#'
#' @examples
#' \dontrun{
#' sprinklr_data |> clean_headers()
#' }
clean_headers <- function(tbl){
  if (any(stringr::str_detect(names(tbl), "facebook"))) {
    tbl |>
      dplyr::rename(
        platform = facebook_publisher_platform,
        campaign_name = paid_initiative_name,
        device_type = facebook_device,
        platform_position = facebook_position,
        total_results = total_results_for_objective_sum,
        spend = spent_in_usd_sum,
        impressions = impressions_sum,
        clicks = facebook_link_clicks_sum,
        likes = facebook_post_likes_sum,
        shares = facebook_post_shares_sum,
        comments = facebook_post_comments_sum,
        engagements = facebook_post_engagement_sum,
        video_views = facebook_video_plays_sum,
        video_views3 = facebook_video_plays_3_sec_sum,
        view25 =facebook_video_plays_to_25_percent_sum,
        view50 = facebook_video_plays_to_50_percent_sum,
        view75 = facebook_video_plays_to_75_percent_sum,
        video_completes = facebook_video_plays_to_100_percent_sum,
        avg_duration = facebook_avg_duration_of_video_played_avg,
        platform_site_visits = facebook_landing_page_views_sum,
        platform_purchases = facebook_purchase_facebook_pixel_sum,
        platform_leads = facebook_lead_facebook_pixel_sum,
        creative_detail = required_field_for_creative_launch,
        prisma_campaign = prisma_io_campaign_name
      ) |>
      dplyr::mutate(platform_position = paste(platform,
                                       trimws(stringr::str_remove(platform_position,
                                                         "Facebook|Instagram")), sep = " ")) |>
      dplyr::select(-c(part1, spark_note)) |>
      dplyr::relocate(platform_site_visits:platform_leads, .before = avg_duration)

  } else if (any(stringr::str_detect(names(tbl), "snapchat"))) {

    tbl |>
      dplyr::rename(
        platform = channel,
        campaign_name = paid_initiative_name,
        total_results = total_results_for_objective_sum,
        spend = spent_in_usd_sum,
        impressions = impressions_sum,
        reach = snapchat_unique_reach_sum,
        daily_frequency = snapchat_daily_frequency_sum,
        clicks = snapchat_clicks_sum,
        shares = snapchat_shares_sum,
        saves = snapchat_saves_sum,
        video_views = snapchat_video_views_sum,
        view25 = snapchat_quartile_1_sum,
        view50 = snapchat_quartile_2_sum,
        view75 = snapchat_quartile_3_sum,
        video_completes = snapchat_view_completion_sum,
        avg_duration = snapchat_avg_view_time_avg,
        platform_purchases = snapchat_conversion_purchases_sum,
        story_opens = snapchat_story_opens_sum,
        story_completes = snapchat_story_completes_sum,
        avg_screen_time = snapchat_avg_screen_time_sum
      )
  } else if (any(stringr::str_detect(names(tbl), "pinterest"))) {
    tbl |>
      dplyr::rename(
        platform = channel,
        campaign_name = paid_initiative_name,
        platform_position = pinterest_placement,
        total_results = total_results_for_objective_sum,
        spend = spent_in_usd_sum,
        impressions = impressions_sum,
        clicks = clicks_sum,
        outbound_clicks = pinterest_outbound_clicks_sum,
        engagements = pinterest_engagement_sum,
        saves = pinterest_saves_sum,
        video_views = pinterest_total_mrc_video_views_sum,
        video_views3 = pinterest_total_3_sec_video_views_sum,
        view25 = pinterest_total_video_watches_at_25_percent_sum,
        view50 = pinterest_total_video_watches_at_50_percent_sum,
        view75 = pinterest_total_video_watches_at_75_percent_sum,
        video_completes = pinterest_total_video_watches_at_100_percent_sum,
        avg_duration = pinterest_paid_video_view_average_watch_time_in_seconds_avg,
        conversions = pinterest_total_conversions_sum,
        clickthru_checkout = pinterest_checkout_click_through_sum,
        viewthru_checkout = pinterest_checkout_view_through_sum
      )
  } else if (any(stringr::str_detect(names(tbl), "reddit"))) {
    tbl |>
      dplyr::rename(
        platform = channel,
        campaign_name = paid_initiative_name,
        total_results = total_results_for_objective_sum,
        spend = spent_in_usd_sum,
        impressions = impressions_sum,
        clicks = clicks_sum,
        video_starts = reddit_video_starts_sum,
        video_views = reddit_video_views_sum,
        video_views3 = reddit_3_second_views_sum,
        view25 = reddit_watches_at_25_percent_sum,
        view50 = reddit_watches_at_50_percent_sum,
        view75 = reddit_watches_at_75_percent_sum,
        video_completes = reddit_watches_at_100_percent_sum,
        video_full = reddit_full_video_views_sum
      )
  } else if (any(stringr::str_detect(names(tbl), "tik_tok"))) {
    tbl |>
      dplyr::rename(
        platform = channel,
        campaign_name = paid_initiative_name,
        total_results = total_results_for_objective_sum,
        spend = spent_in_usd_sum,
        impressions = impressions_sum,
        clicks = clicks_sum,
        likes = tik_tok_likes_sum,
        shares = tik_tok_shares_sum,
        comments = tik_tok_comments_sum,
        follows = tik_tok_follows_sum,
        profile_visits = tik_tok_profile_visits_sum,
        video_views = tik_tok_video_views_sum,
        video_views2 = tik_tok_2s_video_views_sum,
        video_views6 = tik_tok_6s_video_views_sum,
        video_views6fv = tik_tok_6_second_views_focused_view_sum,
        video_views15fv = tik_tok_15_second_views_focused_view_sum,
        view25 = tik_tok_video_views_at_25_percent_sum,
        view50 = tik_tok_video_views_at_50_percent_sum,
        view75 = tik_tok_video_views_at_75_percent_sum,
        video_completes = tik_tok_video_views_at_100_percent_sum,
        avg_duration = tik_tok_average_watch_time_per_video_view_avg,
        creative_detail = required_field_for_creative_launch,
        prisma_campaign = prisma_io_campaign_name
      )
  }
}
