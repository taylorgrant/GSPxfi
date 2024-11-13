#' Estimate decay rates for various DVs on a social platform
#'
#' Given platform data and a DV of interest, this will estimate the exponential and logarithmic decay of the DV for each piece of creative.
#' Table results provide the decay rate, half-life (only interpretable as such for exponential), whether regression is significant, RMSE,
#' r-squared, and AIC. An ID is also provided for easy plotting - see ?fit_decay_model to see how to plot
#'
#' @param data A merged dataset for a platform, normally built through `merge_all_lobs()` or `xfi_merge()`
#' @param group_vars Variables to group by, e.g. for TikTok c("platform", "ad_objective", "creative_detail", "date")
#' @param dv What is the DV that we're estimating the decay
#'
#' @return named list - plot_list is the daily data to be used for plotting; results table is the dataframe with results
#' @export
#'
#' @examples
#' \dontrun{
#' decay_tiktok <- decay_build(merged_tiktok,
#' group_vars = c("platform", "ad_objective", "creative_detail", "date"),
#' dv = "avg_duration")
#' decay_meta <- decay_build(subset(merged_meta, ad_media_type == "Video"),
#' group_vars = c("platform", "platform_position", "ad_objective", "creative_detail", "date"),
#' dv = "avg_duration")
#' }
decay_build <- function(data, group_vars, dv) {
  tmp <- merged_summarise(data, group_vars) |>
    dplyr::filter(!creative_detail %in% c("Video Average", "Static Average")) |>
    dplyr::mutate(day = dplyr::row_number())

  group_splits <- group_vars[group_vars != "date"]

  tmp_list <- split(tmp, tmp[group_splits]) |>
    purrr::keep(~nrow(.x) > 10) # keep only creative with more than 10 days of data

  # map actoss results
  results_list <- purrr::map2(tmp_list, seq_along(tmp_list), ~ {
    fit_decay_models(.x, dv, day = "day") %>%
      dplyr::mutate(id = .y)  # Here `.y` is now the numeric index
  })

  # distill to dataframe
  results_table <- data.table::rbindlist(results_list, idcol = "location") |>
    tidyr::separate(location, into = group_splits, sep = "\\.", extra = "merge")

  list(plot_list = tmp_list,
       results_table = results_table)
}



# Function to calculate RMSE
#'
#' This function is called within the `fit_decay models()` function; see below
#' @param actual Actual values of the DV
#' @param predicted Predicted values of the DV based on regression
#'
#' @return RMSE value
#' @export
#'
#' @examples
#' \dontrun{
#' calculate_rmse(data[[dv]], exp_predicted)
#' }
calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

#' Fit multiple decay models
#'
#' This is the main function to fit models and return results. If plot = TRUE, it will retrun a ggplot with the predicted vs actuals
#'
#' @param data Platform data of a single creative asset summarised to daily level along with other grouping variables
#' @param dv DV of interest
#' @param day Variable containing numeric count of days that creative ran
#' @param plot Logical, TRUE for plot
#'
#' @return Tibble of results, one row is exponential model results, other is logarithmic
#' @export
#'
#' @examples
#' \dontrun{
#' fit_decay_models(data = decay_meta$plot_list[[22]], dv = "avg_duration", day = "day", plot = TRUE)
#' }
fit_decay_models <- function(data, dv, day, plot = FALSE) {
  results <- list()  # To store results for each model

  # handle zeros in DV by adding small positive value (don't break the log)
  data_exp <- data
  data_exp[[dv]] <- ifelse(data_exp[[dv]] == 0, 0.001, data_exp[[dv]])

  # 1. Exponential Decay Model
  data_exp[[paste0("log_", dv)]] <- log(data_exp[[dv]])
  exp_formula <- as.formula(paste0("log_", dv, "~", day))
  exp_model <- lm(exp_formula, data = data_exp)

  # Check if the decay coefficient is negative and significant
  exp_summary <- summary(exp_model)
  exp_coef <- coef(exp_model)[[2]]
  exp_half_life <- log(2) / abs(exp_coef)
  exp_p_value <- exp_summary$coefficients[2, 4]
  exp_significant <- (exp_coef < 0) && (exp_p_value < 0.05)

  # Calculate RMSE
  exp_predicted <- exp(predict(exp_model, data))  # Back-transform predictions to original scale
  exp_rmse <- calculate_rmse(data[[dv]], exp_predicted)

  # Add exponential model results to the list
  results$exponential <- dplyr::tibble(
    model = "Exponential",
    decay_rate = exp_coef,
    half_life = exp_half_life,
    significant = exp_significant,
    rmse = exp_rmse,
    r_squared = exp_summary$adj.r.squared,
    aic = AIC(exp_model),
    n = nrow(data)
  )

  # 2. Logarithmic Decay Model
  log_formula <- as.formula(paste0(dv, "~ log(", day, ")"))
  log_model <- lm(log_formula, data = data)

  # Check if the decay coefficient is negative and significant
  log_summary <- summary(log_model)
  log_coef <- coef(log_model)[[2]]
  log_half_life <- log(2) / abs(log_coef)
  log_p_value <- log_summary$coefficients[2, 4]
  log_significant <- (log_coef < 0) && (log_p_value < 0.05)

  # Calculate RMSE
  log_predicted <- predict(log_model, data)
  log_rmse <- calculate_rmse(data[[dv]], log_predicted)

  # Add logarithmic model results to the list
  results$logarithmic <- dplyr::tibble(
    model = "Logarithmic",
    decay_rate = log_coef,
    half_life = log_half_life,
    significant = log_significant,
    rmse = log_rmse,
    r_squared = log_summary$adj.r.squared,
    aic = AIC(log_model),
    n = nrow(data)
  )

  # Combine results into a single tibble
  results_df <- dplyr::bind_rows(results$exponential, results$logarithmic)

  # Plot if plot = TRUE
  if (plot) {
    data <- data |>
      dplyr::mutate(exp_predicted = exp_predicted,
             log_predicted = log_predicted)
    print(plot_decay_models(data, dv, day, exp_predicted = exp_predicted, log_predicted = log_predicted))
  }

  return(results_df)
}

#' Plot predicted versus actuals for both models
#'
#' @param data Platform data of a single creative asset summarised to daily level along with other grouping variables
#' @param dv DV of interest
#' @param day Variable containing numeric count of days that creative ran
#' @param exp_predicted Predicted values of exponential decay model
#' @param log_predicted Predicted values of logarithmic decay model
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' p <- plot_decay_models(data, dv, day, exp_predicted = exp_predicted, log_predicted = log_predicted)
#' print(p)
#' }
plot_decay_models <- function(data, dv, day, exp_predicted, log_predicted) {

  if(stringr::str_detect(dv, "_")) {
    ylab <- stringr::str_to_title(stringr::str_replace(dv, "_", " "))
  } else {
    ylab = toupper(dv)
  }
  ggplot2::ggplot(data, ggplot2::aes(x = !!dplyr::sym(day), y = !!dplyr::sym(dv))) + #aes_string(x = day, y = dv)) +
    ggplot2::geom_point(color = "black", size = 1) +
    # geom_smooth(color = "black", linewidth = 1, se = FALSE) +
    ggplot2::geom_line(ggplot2::aes(y = exp_predicted), color = "blue", linetype = "dashed", linewidth = 1) +
    ggplot2::geom_line(ggplot2::aes(y = log_predicted), color = "red", linetype = "dotted", linewidth = 1) +
    ggplot2::labs(title = "Decay Model Comparison",
                  subtitle = "<span style='color:#3366FF;'>*Exponential*</span> vs. <span style='color:#FF0000;'>*Logarithmic*</span> Decay Predictions
         Compared to Actuals",
                  x = "Day",
                  y = ylab,
                  caption = glue::glue("Creative: {unique(data$creative_detail)}")) +
    theme_custom() +
    ggplot2::theme(
      plot.subtitle = ggtext::element_markdown())
}

#' Custom ggplot theme
#'
#' @param base_family Base family
#' @param base_size Base size
#' @param plot_title_family Title
#' @param plot_title_size Size
#' @param plot_title_face Title face
#' @param plot_title_margin Title margin
#' @param subtitle_family Subtitle family
#' @param subtitle_size Subtitle size
#' @param subtitle_face Subtitle face
#' @param subtitle_margin Subtitle margin
#' @param strip_text_family Strip text family
#' @param strip_text_size Strip text size
#' @param strip_text_face Strip text face
#' @param caption_family Caption family
#' @param caption_size Caption size
#' @param caption_face Caption face
#' @param caption_margin Caption margin
#' @param axis_title_family Axis title family
#' @param axis_title_size Axis title size
#' @param axis_title_face Axis title face
#' @param axis_title_just Axis title justification
#' @param plot_margin Plot margin
#' @param panel_spacing Panel spacing
#' @param grid_col Grid col
#' @param grid Grid
#' @param axis_col Axis col
#' @param axis Axis
#' @param ticks Ticks
#'
#' @return ggplot theme
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mpg, aes(displ, hwy)) +
#' geom_point() +
#' theme_custom()
#' }
theme_custom <- function(base_family="Open Sans", base_size = 11.5,
                     plot_title_family=base_family, plot_title_size = 16,
                     plot_title_face="plain", plot_title_margin = 4,
                     subtitle_family="Open Sans", subtitle_size = 12,
                     subtitle_face = "plain", subtitle_margin = 15,
                     strip_text_family = base_family, strip_text_size = 12,
                     strip_text_face = "plain",
                     caption_family = "Open Sans", caption_size = 9,
                     caption_face = "italic", caption_margin = 10,
                     axis_title_family = base_family, axis_title_size = 13,
                     axis_title_face = "plain", axis_title_just = "rt",
                     plot_margin = ggplot2::margin(10,10,10,10),
                     panel_spacing = ggplot2::unit(0.5, "lines"),
                     grid_col = "#cccccc", grid = TRUE,
                     axis_col = "#cccccc",axis = FALSE,
                     ticks = FALSE) {

  ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)

  ret <- ret + ggplot2::theme(legend.background=ggplot2::element_blank())
  ret <- ret + ggplot2::theme(legend.key=ggplot2::element_blank())

  if (inherits(grid, "character") | grid == TRUE) {

    ret <- ret + ggplot2::theme(panel.grid=ggplot2::element_line(color=grid_col, linewidth=0.10))
    ret <- ret + ggplot2::theme(panel.grid.major=ggplot2::element_line(color=grid_col, linewidth=0.1))
    ret <- ret + ggplot2::theme(panel.grid.minor=ggplot2::element_line(color=grid_col, linewidth=0.1))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.x=ggplot2::element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.y=ggplot2::element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.x=ggplot2::element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.y=ggplot2::element_blank())
    }

  } else {
    ret <- ret + ggplot2::theme(panel.grid=ggplot2::element_blank())
  }

  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + ggplot2::theme(axis.line=ggplot2::element_line(color="#2b2b2b", linewidth=0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.x=ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.x=ggplot2::element_line(color=axis_col, linewidth=0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.y=ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.y=ggplot2::element_line(color=axis_col, linewidth=0.15))
      }
    } else {
      ret <- ret + ggplot2::theme(axis.line.x=ggplot2::element_line(color=axis_col, linewidth=0.15))
      ret <- ret + ggplot2::theme(axis.line.y=ggplot2::element_line(color=axis_col, linewidth=0.15))
    }
  } else {
    ret <- ret + ggplot2::theme(axis.line=ggplot2::element_blank())
  }

  if (!ticks) {
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  } else {
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_line(linewidth=0.15))
    ret <- ret + ggplot2::theme(axis.ticks.x = ggplot2::element_line(linewidth=0.15))
    ret <- ret + ggplot2::theme(axis.ticks.y = ggplot2::element_line(linewidth=0.15))
    ret <- ret + ggplot2::theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)

  ret <- ret + ggplot2::theme(axis.text.x=ggplot2::element_text(margin=ggplot2::margin(t=0)))
  ret <- ret + ggplot2::theme(axis.text.y=ggplot2::element_text(margin=ggplot2::margin(r=0)))
  ret <- ret + ggplot2::theme(axis.title=ggplot2::element_text(size=axis_title_size, family=axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.x=ggplot2::element_text(hjust=xj, size=axis_title_size,
                                                                 family=axis_title_family, face=axis_title_face))
  ret <- ret + ggplot2::theme(axis.title.y=ggplot2::element_text(hjust=yj, size=axis_title_size,
                                                                 family=axis_title_family, face=axis_title_face))
  ret <- ret + ggplot2::theme(strip.text=ggplot2::element_text(hjust=0, size=strip_text_size,
                                                               face=strip_text_face, family=strip_text_family))
  ret <- ret + ggplot2::theme(panel.spacing.x=grid::unit(.5, "lines"))
  ret <- ret + ggplot2::theme(panel.spacing.y=grid::unit(.5, "lines"))
  ret <- ret + ggplot2::theme(plot.title=ggplot2::element_text(hjust=0, size=plot_title_size,
                                                               margin=ggplot2::margin(b=plot_title_margin),
                                                               family=plot_title_family, face=plot_title_face))
  ret <- ret + ggplot2::theme(plot.subtitle=ggplot2::element_text(hjust=0, size=subtitle_size,
                                                                  margin=ggplot2::margin(b=subtitle_margin),
                                                                  family=subtitle_family, face=subtitle_face))
  ret <- ret + ggplot2::theme(plot.caption=ggplot2::element_text(hjust=1, size=caption_size,
                                                                 margin=ggplot2::margin(t=caption_margin),
                                                                 family=caption_family, face=caption_face))
  ret <- ret + ggplot2::theme(plot.margin=plot_margin)

  ret <-  ret + ggplot2::theme(panel.spacing=panel_spacing)

  ret

}
