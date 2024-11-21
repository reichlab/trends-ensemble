#set up variations of baseline to fit
daily_variations <- tidyr::expand_grid(
  transformation = "none",
  symmetrize = TRUE,
  window_size = c(14 - 1, 7 - 1),
  temporal_resolution = "daily"
) |>
  dplyr::mutate(n_samples = ifelse(.data[["window_size"]] == 6, 12, 13), .before = 4)
weekly_variations <- tidyr::expand_grid(
  transformation = "none",
  symmetrize = TRUE,
  window_size = c(2, 1),
  temporal_resolution = "weekly"
) |>
  dplyr::mutate(n_samples = ifelse(.data[["window_size"]] == 1, 12, 13), .before = 4)

daily_ts <- expand.grid(
  stringsAsFactors = FALSE,
  location = c("ak", "al"),
  time_index = as.Date("2022-11-05") + 1:28,
  observation = NA
)
daily_ts$observation[daily_ts$location == "ak"] <-
  c(8, 9, 6, 7, 3, 6, 6, 5, 4, 11, 10, 3, 4, 3,
    3, 8, 6, 7, 10, 6, 4, 4, 6, 5, 2, 5, 6, 5)
daily_ts$observation[daily_ts$location == "al"] <-
  c(27, 19, 20, 16, 19, 22, 20, 21, 18, 25, 17, 25, 27, 22,
    15, 32, 21, 26, 18, 14, 14, 14, 24, 23, 16, 20, 14, 44)


test_that("missing or extraneous columns in component_variations throws an error", {
  daily_variations[, 1] |>
    create_trends_ensemble(daily_ts,
                           reference_date = "2022-12-10",
                           horizons = -6:21,
                           target = "inc hosp",
                           n_sim = 10000,
                           quantile_levels = c(.1, .5, .9),
                           return_baseline_predictions = FALSE) |>
    expect_error(regex = "`component_variations` is missing the column",
                 fixed = TRUE)

  daily_variations |>
    dplyr::mutate(horizons = 28) |>
    create_trends_ensemble(daily_ts,
                           reference_date = "2022-12-10",
                           horizons = -6:21,
                           target = "inc hosp",
                           n_sim = 10000,
                           quantile_levels = c(.1, .5, .9),
                           return_baseline_predictions = FALSE) |>
    expect_error(regex = "`component_variations` contains the extra column",
                 fixed = TRUE)
})

test_that("unsupported temporal_resolution values in component_variations throws an error", {
  daily_variations |>
    dplyr::mutate(temporal_resolution = "monthly") |>
    dplyr::bind_rows(daily_variations) |>
    create_trends_ensemble(daily_ts,
                           reference_date = "2022-12-10",
                           horizons = -6:21,
                           target = "inc hosp",
                           n_sim = 10000,
                           quantile_levels = c(.1, .5, .9),
                           return_baseline_predictions = FALSE) |>
    expect_error(regex = "`component_variations` must only include temporal resolution values",
                 fixed = TRUE)
})

test_that("multiple temporal_resolution values in component_variations throws an error", {
  daily_variations |>
    dplyr::bind_rows(weekly_variations) |>
    create_trends_ensemble(daily_ts,
                           reference_date = "2022-12-10",
                           horizons = -6:21,
                           target = "inc hosp",
                           n_sim = 10000,
                           quantile_levels = c(.1, .5, .9),
                           return_baseline_predictions = FALSE) |>
    expect_error(regex = "Currently `component_variations` may only contain one unique temporal resolution value",
                 fixed = TRUE)
})

test_that("providing target_ts that cannot be aggregated to match all requested temporal resolutions throws an error", {
  daily_variations |>
    create_trends_ensemble(aggregate_daily_to_weekly(daily_ts),
                           reference_date = "2022-12-10",
                           horizons = -6:21,
                           target = "inc hosp",
                           n_sim = 10000,
                           quantile_levels = c(.1, .5, .9),
                           return_baseline_predictions = FALSE) |>
    expect_error(regex = "Cannot match temporal resolution of provided `target_ts`",
                 fixed = TRUE)
})

test_that("output predictions is a model_out_tbl", {
  expect_warning(
    daily_variations |>
      create_trends_ensemble(daily_ts,
                             reference_date = "2022-12-10",
                             horizons = -6:21,
                             target = "inc hosp",
                             quantile_levels = c(.1, .5, .9),
                             return_baseline_predictions = FALSE) |>
      expect_s3_class("model_out_tbl"),
    regexp = "`model_out_tbl` contains the same number of samples from each component model",
    fixed = TRUE
  )
})

test_that("component outputs are correctly calculated", {
  daily_expected <- fit_baseline_models(
    daily_variations[, 1:4],
    daily_ts,
    reference_date = "2022-12-10",
    temporal_resolution = "daily",
    horizons = -6:21,
    target = "inc hosp",
    n_sim = 10000,
    quantile_levels = c(.1, .5, .9),
    seed = 1234
  )
  weekly_expected <- fit_baseline_models(
    weekly_variations[, 1:4],
    aggregate_daily_to_weekly(daily_ts),
    reference_date = "2022-12-10",
    temporal_resolution = "weekly",
    horizons = 0:3,
    target = "inc hosp",
    n_sim = 10000,
    quantile_levels = c(.1, .5, .9),
    seed = 1234
  )

  expect_warning(
    daily_actual <- daily_variations |>
      create_trends_ensemble(daily_ts,
                             reference_date = "2022-12-10",
                             horizons = -6:21,
                             target = "inc hosp",
                             n_sim = 10000,
                             quantile_levels = c(.1, .5, .9),
                             seed = 1234,
                             return_baseline_predictions = TRUE) |>
      purrr::pluck("baselines"),
    regexp = "`model_out_tbl` contains the same number of samples from each component model",
    fixed = TRUE
  )
  expect_warning(
    weekly_actual <- weekly_variations |>
      create_trends_ensemble(aggregate_daily_to_weekly(daily_ts),
                             reference_date = "2022-12-10",
                             horizons = 0:3,
                             target = "inc hosp",
                             n_sim = 10000,
                             quantile_levels = c(.1, .5, .9),
                             seed = 1234,
                             return_baseline_predictions = TRUE) |>
      purrr::pluck("baselines"),
    regexp = "`model_out_tbl` contains the same number of samples from each component model",
    fixed = TRUE
  )

  expect_equal(daily_actual, daily_expected, tolerance = 1e-3)
  expect_equal(weekly_actual, weekly_expected, tolerance = 1e-3)
})

test_that("ensemble is correctly calculated", {
  expect_warning(
    daily_outputs <- daily_variations |>
      create_trends_ensemble(daily_ts,
                             reference_date = "2022-12-10",
                             horizons = -6:21,
                             target = "inc hosp",
                             n_sim = 10000,
                             quantile_levels = c(.1, .5, .9),
                             return_baseline_predictions = TRUE),
    regexp = "`model_out_tbl` contains the same number of samples from each component model",
    fixed = TRUE
  )
  expect_warning(
    weekly_outputs <- weekly_variations |>
      create_trends_ensemble(daily_ts,
                             reference_date = "2022-12-10",
                             horizons = 0:3,
                             target = "inc hosp",
                             n_sim = 10000,
                             quantile_levels = c(.1, .5, .9),
                             return_baseline_predictions = TRUE),
    regexp = "`model_out_tbl` contains the same number of samples from each component model",
    fixed = TRUE
  )

  daily_quantile <- daily_outputs[["baselines"]] |>
    dplyr::filter(output_type == "quantile") |>
    hubEnsembles::simple_ensemble(
      agg_fun = median,
      model_id = "UMass-trends_ensemble"
    ) |>
    dplyr::mutate(
      reference_date = as.Date(reference_date),
      target_end_date = as.Date(target_end_date)
    )
  daily_sample <- daily_outputs[["baselines"]] |>
    dplyr::filter(output_type == "sample") |>
    dplyr::mutate(
      output_type_id = as.integer(factor(paste0(model_id, output_type_id))),
      model_id = "UMass-trends_ensemble",
      reference_date = as.Date(reference_date),
      target_end_date = as.Date(target_end_date)
    )
  dplyr::bind_rows(daily_quantile, daily_sample) |>
    expect_equal(daily_outputs[["ensemble"]])

  weekly_quantile <- weekly_outputs[["baselines"]] |>
    dplyr::filter(output_type == "quantile") |>
    hubEnsembles::simple_ensemble(
      agg_fun = median,
      model_id = "UMass-trends_ensemble"
    ) |>
    dplyr::mutate(
      reference_date = as.Date(reference_date),
    )
  weekly_sample <- weekly_outputs[["baselines"]] |>
    dplyr::filter(output_type == "sample") |>
    dplyr::mutate(
      output_type_id = as.integer(factor(paste0(model_id, output_type_id))),
      model_id = "UMass-trends_ensemble",
      reference_date = as.Date(reference_date),
      target_end_date = as.Date(target_end_date)
    )
  dplyr::bind_rows(weekly_quantile, weekly_sample) |>
    expect_equal(weekly_outputs[["ensemble"]])
})
