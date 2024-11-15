#' Save the contents of a model_out_tbl as a CSV or parquet to a model-specific
#' directory inside the specified path.
#'
#' @param model_out_tbl an object of class `model_out_tbl` with component
#'   model outputs (e.g., predictions). If it contains multiple models, the results
#'   of each model will be saved to a separate file in individual directories.
#'   May only contain a single value in the round_id column, which is used to name
#'   the output file.
#' @param round_id_col a character string giving the name of the column that
#'   specifies the round ID
#' @param path a character string giving the path where the model-specific
#'   directories will be created, named for each model ID. Defaults to the current
#'   working directory.
#' @param extension a character string of the file type to save the model_out_tbl
#'   to. May be one of "csv", "pqt", or "parquet" (different capitalizations will be
#'   coerced). Defaults to "csv".
#'
#' @return NULL
#'
#' @export
save_model_out_tbl <- function(model_out_tbl, round_id_col = "reference_date", path = ".", extension = "csv") {
  if (!inherits(model_out_tbl, "model_out_tbl")) {
    model_out_tbl <- hubUtils::as_model_out_tbl(model_out_tbl)
  }

  if (!round_id_col %in% names(model_out_tbl)) {
    cli::cli_abort("No column named {.val {round_id_col}} in {.arg model_out_tbl}. 
                   Please provide a valid {.arg round_id_col}")
  }

  round_id_value <- unique(model_out_tbl[[round_id_col]])
  if (length(round_id_value) != 1) {
    cli::cli_abort("Saving a {.arg model_out_tbl} containing more than one unique 
                   {.val {round_id_col}} value is not recommended.")
  }
  model_names <- unique(model_out_tbl$model_id)

  valid_extensions <- c("csv", "parquet", "pqt")
  extension <- tolower(extension)
  if (!extension %in% valid_extensions || length(extension) != 1) {
    cli::cli_abort("Please provide a valid {.arg extension}, which may be one of {.val {valid_extensions}}")
  }

  # save all the component models in hub verse format
  model_names |>
    purrr::walk(.f = function(model_id) {
      model_folder <- file.path(path, model_id)
      if (!file.exists(model_folder)) dir.create(model_folder, recursive = TRUE)

      results_path <- file.path(
        model_folder,
        paste0(round_id_value, "-", model_id, ".", extension)
      )

      model_outputs <- model_out_tbl |>
        dplyr::filter(.data[["model_id"]] == model_id) |>
        dplyr::select(-"model_id")

      if (extension == "csv") {
        utils::write.csv(model_outputs, file = results_path, row.names = FALSE)
      } else if (extension == "parquet") {
        arrow::write_parquet(model_outputs, sink = results_path)
      }
    })

}
