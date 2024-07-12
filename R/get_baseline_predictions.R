#' Get predictions for a single baseline model
#'
#' @param target_ts a `data.frame` of target data in a time series format
#'   (contains columns `time_index`, `location`, and `observation`)
#' @param transformation string specifying the transformation used to create
#'   the model; can be one of "none" or "sqrt".
#' @param symmetrize boolean specifying whether to symmetrize the model; can be
#'   one of `TRUE` or `FALSE`.
#' @param window_size integer specifying the window size used to create the model
#' @param effective horizons numeric vector of prediction horizons relative to
#'   the last observed date in `target_ts`
#' @param origin string specifying the origin to use when making predictions;
#'   recommended to be "median" if the temporal resolution is daily and "obs"
#'   if weekly or otherwise. Defaults to "obs".
#' @param quantile_levels numeric vector of quantile levels to output; set to NA
#'   if quantile outputs not requested
#' @param n_samples integer of amount of samples to output (and predict);
#'   set to NA if sample outputs not requested (in this case 100000 samples
#'   are generated from which to extract quantiles)
#'
#' @return data frame of a baseline forecast for one location, one model with
#'   columns `horizon` , `output_type_id`, and `value`, but which are stored
#'   as a nested list in a 1x1 data frame
get_baseline_predictions_new <- function(target_ts,
                                     transformation,
                                     symmetrize,
                                     window_size,
                                     effective_horizons,
                                     origin = "obs",
                                     quantile_levels,
                                     n_samples) {
  # validate arguments
  validate_target_ts(target_ts)

  tidyr::expand_grid(
    transformation = transformation,
    symmetrize = symmetrize,
    window_size = window_size
  ) |>
    validate_model_variations()

  valid_origins <- c("median", "obs")
  if (origin %in% valid_origins) {
    cli::cli_abort("{.arg origin} must be only one of {.val valid_origins}")
  }

  # fit
  baseline_fit <- simplets::fit_simple_ts(
    y = target_ts[["observation"]],
    ts_frequency = 1,
    model = "quantile_baseline",
    transformation = transformation,
    transform_offset = ifelse(transformation == "none", 0, 1),
    d = 0,
    D = 0,
    symmetrize = symmetrize,
    window_size = window_size
  )

  # predict
  predictions <- predict(
    baseline_fit,
    nsim = ifelse(is.null(n_samples), 100000, n_samples),
    horizon = max(effective_horizons),
    origin = origin,
    force_nonneg = TRUE
  )

  # truncate to non-negative
  # AG: wondering what the correct order of operations is here
  predictions <- pmax(predictions, 0)

  # extract requested forecasts
  samples_df <- NULL
  if (!is.null(n_samples)) {
    samples_df <- effective_horizons |>
      purrr::map(
        function(h) {
          data.frame(
            horizon = rep(h, n_samples),
            value = predictions[, h]
          ) |>
            tibble::rownames_to_column(var = "output_type_id") |>
            dplyr::mutate(output_type_id = as.numeric(dplyr::row_number()), .before = 2) |>
            dplyr::select("horizon", "output_type_id", "value")
        }
      ) |>
      purrr::list_rbind()
  }
  quantiles_df <- NULL
  if (!is.null(quantile_levels)) {
    n <- length(quantile_levels)
    quantiles_df <- effective_horizons |>
      purrr::map(
        function(h) {
          data.frame(
            horizon = rep(h, n),
            output_type_id = quantile_levels,
            value = ceiling(quantile(predictions[, h], probs = quantile_levels))
          )
        }
      ) |>
      purrr::list_rbind()
  }

  return(dplyr::tibble(forecasts = list(rbind(samples_df, quantiles_df))))
}
