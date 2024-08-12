# Tests for `validate_model_variations`
test_that("missing model_variations throws an error", {
  expect_error(
    validate_model_variations(NULL),
    regexp = "`model_variations` is missing", fixed = TRUE
  )
})

test_that("missing model_variations column throws an error", {
  data.frame(transformation = "log", symmetrize = TRUE) |>
    validate_model_variations() |>
    expect_error(
      regexp = "`model_variations` is missing the column", fixed = TRUE
    )
})

test_that("extra model_variations columns throws an error", {
  data.frame(id = 1, transformation = "sqrt", symmetrize = TRUE, window_size = 3) |>
    validate_model_variations() |>
    expect_error(
      regexp = "`model_variations` contains the extra column", fixed = TRUE
    )
})

test_that("duplicate rows in model_variations throws an error", {
  data.frame(transformation = c("sqrt", "sqrt"), symmetrize = c(TRUE, TRUE), window_size = c(3, 3)) |>
    validate_model_variations() |>
    expect_error(
      regexp = "`model_variations` contains duplicate rows", fixed = TRUE
    )
})

test_that("non-duplicate rows in model_variations do not throw an error", {
  data.frame(transformation = c("sqrt", "sqrt"), symmetrize = c(TRUE, TRUE), window_size = c(3, 4)) |>
    validate_model_variations() |>
    expect_no_error()
})

test_that("any argument with length > 1 throws an error", {
  validate_variation_inputs(transformation = "log", symmetrize = TRUE, window_size = c(3, 4)) |>
    expect_error(
      "`window_size` must be length 1", fixed = TRUE
    )
})

test_that("invalid transformation value throws an error", {
  validate_variation_inputs(transformation = "log", symmetrize = TRUE, window_size = 3) |>
    expect_error(
      "`transformation` must only contain values", fixed = TRUE
    )
})

test_that("invalid symmetrize value throws an error", {
  validate_variation_inputs(transformation = "none", symmetrize = 1, window_size = 3) |>
    expect_error(
      "`symmetrize` must only contain logical values", fixed = TRUE
    )
})

test_that("invalid window_size value throws an error", {
  validate_variation_inputs(transformation = "none", symmetrize = TRUE, window_size = 5.5) |>
    expect_error(
      "`window_size` must only contain non-negative integer values", fixed = TRUE
    )
})

test_that("correctly formatted model_variations throws no error", {
  data.frame(transformation = "none", symmetrize = TRUE, window_size = 5) |>
    validate_model_variations() |>
    expect_no_error()
})


# Tests for `validate_target_ts`
test_that("missing target_ts throws an error", {
  expect_error(
    validate_target_ts(NULL),
    regexp = "`target_ts` is missing", fixed = TRUE
  )
})

test_that("missing target_ts column throws an error", {
  data.frame(time_index = as.Date("2023-05-15"), location = "US") |>
    validate_target_ts() |>
    expect_error(
      regexp = "`target_ts` is missing the column", fixed = TRUE
    )
})

test_that("extra target_ts columns throws an error", {
  data.frame(time_index = as.Date("2023-05-15"), location = "US", horizon = 1, observation = 100) |>
    validate_target_ts() |>
    expect_error(
      regexp = "`target_ts` contains the extra column", fixed = TRUE
    )
})

test_that("correctly formatted target_ts throws no errors", {
  data.frame(time_index = as.Date("2023-05-15"), location = "US", observation = 100) |>
    validate_target_ts() |>
    expect_no_error()
})
