# set up simple data for test cases
model_outputs <- expand.grid(stringsAsFactors = FALSE,
                             model_id = letters[1:4],
                             location = c("222", "888"),
                             reference_date = as.Date("2021-12-18"),
                             horizon = 1, #week
                             target = "inc death",
                             output_type = "quantile",
                             output_type_id = c(.1, .5, .9),
                             value = NA_real_)

v2_1 <- model_outputs$value[model_outputs$location == "222" &
                              model_outputs$output_type_id == .1] <-
  c(10, 30, 15, 20)
v2_5 <- model_outputs$value[model_outputs$location == "222" &
                              model_outputs$output_type_id == .5] <-
  c(40, 40, 45, 50)
v2_9 <- model_outputs$value[model_outputs$location == "222" &
                              model_outputs$output_type_id == .9] <-
  c(60, 70, 75, 80)
v8_1 <- model_outputs$value[model_outputs$location == "888" &
                              model_outputs$output_type_id == .1] <-
  c(100, 300, 400, 250)
v8_5 <- model_outputs$value[model_outputs$location == "888" &
                              model_outputs$output_type_id == .5] <-
  c(150, 325, 500, 300)
v8_9 <- model_outputs$value[model_outputs$location == "888" &
                              model_outputs$output_type_id == .9] <-
  c(250, 350, 500, 350)


test_that("Mismatched round_id_col throws an error", {
  model_outputs |>
    save_model_out_tbl(round_id_col = "forecast_date", extension = "csv") |>
    expect_error(regex = "No column named \"forecast_date\" in `model_out_tbl`",
                 fixed = TRUE)
})

test_that("Multiple round_id values in the provided model_out_tbl throws an error", {
  model_outputs |>
    dplyr::mutate(reference_date = reference_date - 7L) |>
    dplyr::bind_rows(model_outputs) |>
    save_model_out_tbl(round_id_col = "reference_date", extension = "csv") |>
    expect_error(
      regex = "Saving a `model_out_tbl` containing more than one unique \"reference_date\" value is not recommended",
      fixed = TRUE
    )
})

test_that("Invalid extension throws an error", {
  model_outputs |>
    save_model_out_tbl(round_id_col = "reference_date", extension = "pdf") |>
    expect_error(
      regex = "Please provide a valid `extension`, which may be one of",
      fixed = TRUE
    )
})

test_that("Capitalized valid extension does not throw an error", {
  model_outputs |>
    save_model_out_tbl(round_id_col = "reference_date", path = tempdir(), extension = "PARQUET") |>
    expect_no_error()
})

test_that("File names are as expected", {
  temp_dir <- withr::local_tempdir()
  save_model_out_tbl(model_outputs, round_id_col = "reference_date", path = temp_dir, extension = "csv")

  expected_paths <- fs::path(rep("x", 4))
  for (i in 1:4) {
    expected_paths[i] <- fs::path(paste0(temp_dir, "/", letters[i], "/2021-12-18-", letters[i], ".csv"))
  }
  actual_paths <- fs::dir_ls(temp_dir, recurse = TRUE, type = "file")
  expect_equal(unname(actual_paths), expected_paths)
})

test_that("Model outputs can be reconstructed", {
  temp_dir <- withr::local_tempdir()
  save_model_out_tbl(model_outputs, round_id_col = "reference_date", path = temp_dir, extension = "parquet")

  reconstituted_outputs <- letters[1:4] |>
    purrr::map(.f = function(model_id) {
      fs::path(paste0(temp_dir, "/", model_id, "/2021-12-18-", model_id, ".parquet")) |>
        arrow::read_parquet() |>
        dplyr::mutate(model_id = model_id, reference_date = as.Date("2021-12-18"))
    }) |>
    purrr::list_rbind() |>
    dplyr::arrange(output_type_id, location, model_id) |>
    hubUtils::as_model_out_tbl()
  expect_equal(reconstituted_outputs, hubUtils::as_model_out_tbl(model_outputs))
})
