get_baseline_predictions <- function(location_data,
                                     response_var,
                                     transformation,
                                     symmetrize,
                                     window_size,
                                     horizons,
                                     temporal_resolution,
                                     h_adjust,
                                     taus) {
  # fit
  baseline_fit <- fit_simple_ts(
    y = location_data[[response_var]],
    ts_frequency = 1,
    model = 'quantile_baseline',
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
    nsim = 100000,
    horizon = horizons,
    origin = ifelse(
      temporal_resolution == "daily",
      "median",
      "obs"
    ),
    force_nonneg = TRUE
  )

  temporal_resolution <- match.arg(temporal_resolution, c("daily", "weekly"))
  if (temporal_resolution == "daily") {
    if (h_adjust < 0) { # if missing observations
      # augment with observed leading data
      # (dates of missing data filled with last observed value for forecasts)
      predictions <- cbind( #bind 100k identical samples using last observed value
        matrix(
          tail(location_data[[response_var]], abs(h_adjust)),
          nrow = 100000,
          ncol = abs(h_adjust),
          byrow = TRUE),
        predictions
      )
      h_adjust <- 0L
    } else if (h_adjust > 0) { # if there are observations starting on the forecast date
      # drop extra forecasts at the beginning
      predictions <- predictions[, -seq_len(h_adjust)]
    }
  }

  # truncate to non-negative
  # AG: wondering what the correct order of operations is here
  predictions <- pmax(predictions, 0)

  # aggregate to weekly if temporal_resolution is daily
  ## truncate to start at the first date of the first target week
  if (temporal_resolution == "daily") {
    predictions <-
    sapply(1:4, function(i)
      rowSums(predictions[, ((7 * (i - 1)) + 1):(7 * i)])
    )
  }
  # extract predictive quantiles, intervals, and medians
  quantiles_df <- get_quantiles_df(predictions, taus)

  return(tibble(quantiles_df = list(quantiles_df)))
}

