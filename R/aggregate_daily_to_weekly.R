#' Aggregate daily data to weekly data
#'
#' Counts weeks as beginning on Sunday and ending on Saturday. Drops observations
#' from the most recent week if not a full 7 days of data
#'
#' @param target_ts a `data.frame` of target data in a time series format
#'   (contains columns `time_index`, `location`, and `observation`) for a single
#'   location
#'
#' @return data.frame of time series data with the same set of input columns, with
#' weekly-aggregated data in `observation` column
aggregate_daily_to_weekly <- function(target_ts) {
  validate_target_ts(target_ts)
  most_recent_date <- max(target_ts$time_index)

  target_ts |>
    dplyr::mutate(
      sat_date = lubridate::ceiling_date(
        lubridate::ymd(.data[["time_index"]]),
        unit = "week"
      ) - 1,
      .before = "observation"
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("location", "sat_date")))) |>
    dplyr::filter(.data[["sat_date"]] <= most_recent_date) |>
    dplyr::summarize(observation = sum(.data[["observation"]], na.rm = FALSE)) |>
    dplyr::rename(time_index = "sat_date") |>
    dplyr::ungroup()
}
