
# Tests for `validate_model_variations`
test_that("missing model_variations throws error", {
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

test_that("extra model_variations columns are dropped and throws a warning", {
  expected_variations <- data.frame(transformation = "sqrt", symmetrize = TRUE, window_size = 3)

  expect_warning(
    actual_variations <-
      data.frame(id = 1, transformation = "sqrt", symmetrize = TRUE, window_size = 3) |>
      validate_model_variations()
  )

  expect_equal(expected_variations, actual_variations)
})

test_that("invalid transformation value throws an error", {
  data.frame(transformation = "log", symmetrize = TRUE, window_size = 3) |>
    validate_model_variations() |>
    expect_error(
      "`transformation` must only contain values", fixed = TRUE
    )
})

test_that("invalid symmetrize value throws an error", {
  data.frame(transformation = "none", symmetrize = 1, window_size = 3) |>
    validate_model_variations() |>
    expect_error(
      "`symmetrize` must only contain logical values", fixed = TRUE
    )
})

test_that("invalid window_size value throws an error", {
  data.frame(transformation = "none", symmetrize = TRUE, window_size = 5.5) |>
    validate_model_variations() |>
    expect_error(
      "`window_size` must only contain integer values", fixed = TRUE
    )
})

test_that("valid case passes", {
  data.frame(transformation = "none", symmetrize = TRUE, window_size = 5) |>
    validate_model_variations() |>
    expect_no_error()
})

