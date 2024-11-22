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

test_that("invalid n_sim value throws an error", {
  get_baseline_predictions(target_ts,
                           transformation = "none",
                           symmetrize = TRUE,
                           window_size = 3,
                           effective_horizons = 1:4,
                           origin = "obs",
                           n_sim = "test",
                           quantile_levels = NULL,
                           n_samples = 100) |>
    expect_error(
      regexp = "`n_sim` must be a single, non-negative integer value.", fixed = TRUE
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

test_that("invalid n_samples value throws an error", {
  get_baseline_predictions(target_ts,
                           transformation = "none",
                           symmetrize = TRUE,
                           window_size = 3,
                           effective_horizons = 1:4,
                           origin = "obs",
                           n_sim = 100000,
                           quantile_levels = NULL,
                           n_samples = 10.5) |>
    expect_error(
      regexp = "`n_samples` must be a single, non-negative integer value.", fixed = TRUE
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
                             n_sim = 10000,
                             quantile_levels = NULL,
                             n_samples = 10000,
                             round_predictions = FALSE,
                             seed = 1234) |>
    tidyr::unnest(cols = c("forecasts"))

  expect_gte(min(sample_predictions$value), 0)
})

test_that("results are reproducible", {
  sample1 <- target_ts |>
    get_baseline_predictions(transformation = "none",
                             symmetrize = TRUE,
                             window_size = 3,
                             effective_horizons = 1:4,
                             origin = "obs",
                             n_sim = 10000,
                             quantile_levels = NULL,
                             n_samples = 10000,
                             round_predictions = FALSE,
                             seed = 1234) |>
    tidyr::unnest(cols = c("forecasts"))
  sample2 <- target_ts |>
    get_baseline_predictions(transformation = "none",
                             symmetrize = TRUE,
                             window_size = 3,
                             effective_horizons = 1:4,
                             origin = "obs",
                             n_sim = 10000,
                             quantile_levels = NULL,
                             n_samples = 10000,
                             round_predictions = FALSE,
                             seed = 1234) |>
    tidyr::unnest(cols = c("forecasts"))

  quantile1 <- target_ts |>
    get_baseline_predictions(transformation = "none",
                             symmetrize = TRUE,
                             window_size = 3,
                             effective_horizons = 1:4,
                             origin = "obs",
                             n_sim = 10000,
                             quantile_levels = c(.1, .5, .9),
                             n_samples = NULL,
                             round_predictions = FALSE,
                             seed = 1234) |>
    tidyr::unnest(cols = c("forecasts"))
  quantile2 <- target_ts |>
    get_baseline_predictions(transformation = "none",
                             symmetrize = TRUE,
                             window_size = 3,
                             effective_horizons = 1:4,
                             origin = "obs",
                             n_sim = 10000,
                             quantile_levels = c(.1, .5, .9),
                             n_samples = NULL,
                             round_predictions = FALSE,
                             seed = 1234) |>
    tidyr::unnest(cols = c("forecasts"))

  both1 <- target_ts |>
    get_baseline_predictions(transformation = "none",
                             symmetrize = TRUE,
                             window_size = 3,
                             effective_horizons = 1:4,
                             origin = "obs",
                             n_sim = 10000,
                             quantile_levels = c(.1, .5, .9),
                             n_samples = 10000,
                             round_predictions = FALSE,
                             seed = 1234) |>
    tidyr::unnest(cols = c("forecasts"))
  both2 <- target_ts |>
    get_baseline_predictions(transformation = "none",
                             symmetrize = TRUE,
                             window_size = 3,
                             effective_horizons = 1:4,
                             origin = "obs",
                             n_sim = 10000,
                             quantile_levels = c(.1, .5, .9),
                             n_samples = 10000,
                             round_predictions = FALSE,
                             seed = 1234) |>
    tidyr::unnest(cols = c("forecasts"))

  expect_equal(sample1, sample2)
  expect_equal(quantile1, quantile2)
  expect_equal(both1, both2)
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
                             n_sim = 10000,
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
                             n_sim = 10000,
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
                             n_sim = 10000,
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

test_that("forecasts are correctly rounded when requested", {
  sample_expected <-
    expand.grid(
      stringsAsFactors = FALSE,
      horizon = 1:4,
      output_type = "sample",
      output_type_id = 1:10000,
      value = NA
    ) |>
    dplyr::tibble() |>
    dplyr::arrange(.data[["horizon"]])
  quantile_expected <-
    expand.grid(
      stringsAsFactors = FALSE,
      horizon = 1:4,
      output_type = "quantile",
      output_type_id = c(0.1, 0.5, 0.9),
      value = NA
    ) |>
    dplyr::tibble() |>
    dplyr::arrange(.data[["horizon"]])
  both_expected <- rbind(sample_expected, quantile_expected)

  both_actual <- target_ts |>
    get_baseline_predictions(transformation = "none",
                             symmetrize = TRUE,
                             window_size = 3,
                             effective_horizons = 1:4,
                             origin = "obs",
                             n_sim = 10000,
                             quantile_levels = c(0.1, 0.5, 0.9),
                             n_samples = 10000,
                             round_predictions = TRUE,
                             seed = 1234) |>
    tidyr::unnest(cols = c("forecasts"))

  both_expected$value <- round(both_actual$value, 0)
  attr(both_expected, "out.attrs") <- NULL

  expect_equal(both_actual, both_expected)
})

test_that("quantile forecasts are correctly calculated", {
  quantile_levels <- c(.1, .5, .9)
  quantile_expected <-
    expand.grid(
      stringsAsFactors = FALSE,
      horizon = 1:4,
      output_type = "quantile",
      output_type_id = quantile_levels,
      value = NA_real_
    ) |>
    dplyr::tibble() |>
    dplyr::arrange(.data[["horizon"]])

  sample_predictions <- target_ts |>
    get_baseline_predictions(transformation = "none",
                             symmetrize = TRUE,
                             window_size = 3,
                             effective_horizons = 1:4,
                             origin = "obs",
                             n_sim = 10000,
                             quantile_levels = NULL,
                             n_samples = 10000,
                             round_predictions = FALSE,
                             seed = 1234) |>
    tidyr::unnest(cols = c("forecasts"))

  sample_values <- sample_quantiles <- list()
  for (i in 1:4) {
    sample_values[[i]] <- sample_predictions$value[c(10000 * (i - 1) + 1 : 10000 * i)]
    sample_quantiles[[i]] <- sample_predictions[sample_predictions$horizon == i, ] |>
      dplyr::pull(4) |>
      stats::quantile(quantile_levels, names = FALSE)
  }
  quantile_expected$value <- unlist(sample_quantiles)
  attr(quantile_expected, "out.attrs") <- NULL

  quantile_actual <- target_ts |>
    get_baseline_predictions(transformation = "none",
                             symmetrize = TRUE,
                             window_size = 3,
                             effective_horizons = 1:4,
                             origin = "obs",
                             n_sim = 10000,
                             quantile_levels = quantile_levels,
                             n_samples = NULL,
                             round_predictions = FALSE,
                             seed = 1234) |>
    tidyr::unnest(cols = c("forecasts"))

  expect_equal(quantile_actual, quantile_expected, tolerance = 1.5e-1)
})


test_that("forecasts are correctly calculated regardless of target data ordering", {
  both_expected <- target_ts |>
    get_baseline_predictions(transformation = "none",
                             symmetrize = TRUE,
                             window_size = 3,
                             effective_horizons = 1:4,
                             origin = "obs",
                             n_sim = 10000,
                             quantile_levels = c(0.1, 0.5, 0.9),
                             n_samples = 10000,
                             round_predictions = TRUE,
                             seed = 1234) |>
    tidyr::unnest(cols = c("forecasts"))

  both_actual <- target_ts |>
    dplyr::arrange(.data[["observation"]]) |>
    get_baseline_predictions(transformation = "none",
                             symmetrize = TRUE,
                             window_size = 3,
                             effective_horizons = 1:4,
                             origin = "obs",
                             n_sim = 10000,
                             quantile_levels = c(0.1, 0.5, 0.9),
                             n_samples = 10000,
                             round_predictions = TRUE,
                             seed = 1234) |>
    tidyr::unnest(cols = c("forecasts"))

  expect_equal(both_actual, both_expected)
})

test_that("quantile forecasts do not include negative values for small target data values", {
  target_ts |>
    dplyr::mutate(observation = round(.data[["observation"]] / 7)) |>
    get_baseline_predictions(transformation = "none",
                             symmetrize = TRUE,
                             window_size = 3,
                             effective_horizons = 1:4,
                             origin = "obs",
                             n_sim = 10000,
                             quantile_levels = c(0.1, 0.5, 0.9),
                             n_samples = 10000,
                             round_predictions = TRUE,
                             seed = 1234) |>
    tidyr::unnest(cols = c("forecasts")) |>
    dplyr::pull(.data[["value"]]) |>
    min() |>
    expect_gte(0)
})
