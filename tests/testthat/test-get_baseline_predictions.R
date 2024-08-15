# set up simple data for test cases
target_ts <- data.frame(stringsAsFactors = FALSE,
                        location = rep("ak", 10),
                        time_index = as.Date("2022-11-05") + seq(0, 63, 7),
                        observation = c(15, 14, 38, 69, 53, 73, 51, 43, 43, 32))


test_that("multiple locations in target data throws an error", {
  target_ts |>
    dplyr::mutate(location = "ak2") |>
    dplyr::bind_rows(target_ts) |>
    get_baseline_predictions(transformation = "none",
                             symmetrize = TRUE,
                             window_size = 3,
                             effective_horizons = 1:4,
                             origin = "obs",
                             quantile_levels = c(.1, .5, .9),
                             n_samples = NULL) |>
    expect_error(
      regexp = " but only one may be provided.", fixed = TRUE
    )
})

test_that("invalid origin value throws an error", {
  get_baseline_predictions(target_ts,
                           transformation = "none",
                           symmetrize = TRUE,
                           window_size = 3,
                           effective_horizons = 1:4,
                           origin = "x",
                           quantile_levels = c(.1, .5, .9),
                           n_samples = NULL) |>
    expect_error(
      regexp = "`origin` must be only one of ", fixed = TRUE
    )
})

test_that("invalid quantile_levels value throws an error", {
  get_baseline_predictions(target_ts,
                           transformation = "none",
                           symmetrize = TRUE,
                           window_size = 3,
                           effective_horizons = 1:4,
                           origin = "obs",
                           quantile_levels = c(1, 5, 9),
                           n_samples = NULL) |>
    expect_error(
      regexp = "`quantile_levels` must only contain values between 0 and 1.", fixed = TRUE
    )
})

test_that("not requesting any forecasts throws an error", {
  get_baseline_predictions(target_ts,
                           transformation = "none",
                           symmetrize = TRUE,
                           window_size = 3,
                           effective_horizons = 1:4,
                           origin = "obs",
                           quantile_levels = NULL,
                           n_samples = NULL) |>
    expect_error(
      regexp = "No forecasts requested: both `quantile_levels` and `n_samples` are NULL", fixed = TRUE
    )
})

# correct transformation/symmetrize/window_size applied

test_that("only non-negative forecast values are returned", {
  sample_predictions <- target_ts |>
    get_baseline_predictions(transformation = "none",
                             symmetrize = TRUE,
                             window_size = 3,
                             effective_horizons = 1:4,
                             origin = "obs",
                             quantile_levels = NULL,
                             n_samples = 10000,
                             round_predictions = FALSE,
                             seed = 1234) |>
    tidyr::unnest(cols = c("forecasts"))

  min_value <- min(sample_predictions$value)
  if (min_value != 0) {
    expect_gt(min(sample_predictions$value), 0)
  } else {
    expect_equal(min(sample_predictions$value), 0)
  }
})

test_that("the correct combination of horizon, output types, and output type IDs are returned", {
  sample_expected <-
    expand.grid(
      stringsAsFactors = FALSE,
      horizon = 1:4,
      output_type = "sample",
      output_type_id = 1:10000
    ) |>
    dplyr::tibble() |>
    dplyr::arrange(.data[["horizon"]])
  quantile_expected <-
    expand.grid(
      stringsAsFactors = FALSE,
      horizon = 1:4,
      output_type = "quantile",
      output_type_id = c(0.1, 0.5, 0.9)
    ) |>
    dplyr::tibble() |>
    dplyr::arrange(.data[["horizon"]])
  both_expected <- rbind(sample_expected, quantile_expected)

  attr(sample_expected, "out.attrs") <- NULL
  attr(quantile_expected, "out.attrs") <- NULL
  attr(both_expected, "out.attrs") <- NULL

  sample_actual <- target_ts |>
    get_baseline_predictions(transformation = "none",
                             symmetrize = TRUE,
                             window_size = 3,
                             effective_horizons = 1:4,
                             origin = "obs",
                             quantile_levels = NULL,
                             n_samples = 10000,
                             round_predictions = FALSE,
                             seed = 1234) |>
    tidyr::unnest(cols = c("forecasts")) |>
    dplyr::distinct(horizon, output_type, output_type_id)

  quantile_actual <- target_ts |>
    get_baseline_predictions(transformation = "none",
                             symmetrize = TRUE,
                             window_size = 3,
                             effective_horizons = 1:4,
                             origin = "obs",
                             quantile_levels = c(.1, .5, .9),
                             n_samples = NULL,
                             round_predictions = FALSE,
                             seed = 1234) |>
    tidyr::unnest(cols = c("forecasts")) |>
    dplyr::distinct(horizon, output_type, output_type_id)

  both_actual <- target_ts |>
    get_baseline_predictions(transformation = "none",
                             symmetrize = TRUE,
                             window_size = 3,
                             effective_horizons = 1:4,
                             origin = "obs",
                             quantile_levels = c(.1, .5, .9),
                             n_samples = 10000,
                             round_predictions = FALSE,
                             seed = 1234) |>
    tidyr::unnest(cols = c("forecasts")) |>
    dplyr::distinct(horizon, output_type, output_type_id)

  expect_equal(sample_actual, sample_expected)
  expect_equal(quantile_actual, quantile_expected)
  expect_equal(both_actual, both_expected)
})


