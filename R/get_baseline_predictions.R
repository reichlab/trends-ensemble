#' Get predictions for a single baseline model
#'
#' @param target_ts a `data.frame` of target data in a time series format
#'   (contains columns `time_index`, `location`, and `observation`) for a single
#'   location
#' @param transformation string specifying the transformation used on the
#'   distribution which determines its shape; can be one of "none" or "sqrt".
#' @param symmetrize boolean specifying whether to make the distribution symmetric;
#'   can be one of `TRUE` or `FALSE`.
#' @param window_size integer specifying how many previous observations in the
#'   target data should be used to inform the forecasts
#' @param effective_horizons numeric vector of prediction horizons relative to
#'   the last observed date in `target_ts`
#' @param origin string specifying the origin to use when making predictions;
#'   recommended to be "median" if the temporal resolution is daily and "obs"
#'   if weekly or otherwise. Defaults to "obs".
#' @param n_sim integer number of simulations to predict. Defaults to 100000.
#' @param quantile_levels numeric vector of quantile levels to output; set to NULL
#'   if quantile outputs not requested. Defaults to NULL.
#' @param n_samples integer number of samples to output, which are drawn from the
#'   simulated (sample) predictions (hence n_samples <= n_sim). Set to NULL
#'   if sample outputs not requested. Defaults to NULL.
#' @param round_predictions boolean specifying whether to round the output
#'   predictions to the nearest whole number. Defaults to FALSE
#' @param seed integer specifying a seed to set for reproducible results.
#'   Defaults to NULL, in which case no seed is set.
#'
#' @return data frame of a baseline forecast for one location, one model with
#'   columns `horizon` , `output_type`, `output_type_id`, and `value`,
#'   but which are stored as a nested list in a 1x1 data frame
#'
#' @importFrom rlang .data

get_baseline_predictions <- function(target_ts,
                                     transformation,
                                     symmetrize,
                                     window_size,
                                     effective_horizons,
                                     origin = "obs",
                                     n_sim = 100000,
                                     quantile_levels = NULL,
                                     n_samples = NULL,
                                     round_predictions = FALSE,
                                     seed = NULL) {
  # validate arguments
  validate_target_ts(target_ts)

  num_locs <- length(unique(target_ts[["location"]]))
  if (num_locs != 1) {
    cli::cli_abort("{.arg target_ts} contains {.val num_locs} but only one may be provided.")
  }

  validate_variation_inputs(transformation, symmetrize, window_size)

  valid_origins <- c("median", "obs")
  if (!origin %in% valid_origins) {
    cli::cli_abort("{.arg origin} must be only one of {.val valid_origins}")
  }

  if (!is.numeric(n_sim) || n_sim < 0 || n_sim != trunc(n_sim) || length(n_sim) != 1) {
    cli::cli_abort("{.arg n_sim} must be a single, non-negative integer value.")
  }

  if (any(quantile_levels > 1) || any(quantile_levels < 0)) {
    cli::cli_abort("{.arg quantile_levels} must only contain values between 0 and 1.")
  }

  if (!is.null(n_samples) &&
        (n_samples > n_sim || n_samples < 0 || n_samples != trunc(n_samples) || length(n_samples) != 1)) {
    cli::cli_abort("{.arg n_samples} must be a single, non-negative integer value.")
  }

  if (is.null(quantile_levels) && is.null(n_samples)) {
    cli::cli_abort("No forecasts requested: both `quantile_levels` and `n_samples` are NULL")
  }

  if (!is.null(seed)) set.seed(seed)

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
  predictions <- baseline_fit |>
    stats::predict(
      nsim = n_sim,
      horizon = max(effective_horizons),
      origin = origin,
      force_nonneg = TRUE
    )

  forecasts_df <- extract_predictions(predictions, effective_horizons, quantile_levels, n_samples)

  if (round_predictions) forecasts_df[["value"]] <- round(forecasts_df[["value"]], 0)
  return(dplyr::tibble(forecasts = list(forecasts_df)))
}


extract_predictions <- function(predictions,
                                effective_horizons,
                                quantile_levels = NULL,
                                n_samples = NULL) {
  samples_df <- NULL
  if (!is.null(n_samples)) {
    samples_df <- effective_horizons |>
      purrr::map(
        function(h) {
          data.frame(
            horizon = rep(h, n_samples),
            value = predictions[1:n_samples, h]
          ) |>
            tibble::rownames_to_column(var = "output_type_id") |>
            dplyr::mutate(
              output_type = "sample",
              output_type_id = as.numeric(dplyr::row_number()),
              .before = 2
            ) |>
            dplyr::select("horizon", "output_type", "output_type_id", "value")
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
            output_type = "quantile",
            output_type_id = quantile_levels,
            value = stats::quantile(predictions[, h], probs = quantile_levels)
          )
        }
      ) |>
      purrr::list_rbind()
  }

  combined_df <- rbind(samples_df, quantiles_df)
  return(combined_df)
}
