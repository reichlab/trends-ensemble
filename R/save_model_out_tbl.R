#' Save the results of a model_out_tbl as a CSV to the specified path
#'
#' @param model_out_tbl an object of class `model_out_tbl` with component
#'   model outputs (e.g., predictions).
#' @param round_id_col a character string giving the name of the column that
#'   specifies the round ID
#' @param path a character string giving the path where the model outputs will
#'   be saved. Defaults to the current working directory.
#'
#' @return NULL
#'
#' @export
save_model_out_tbl <- function(model_out_tbl, round_id_col = "reference_date", path = ".") {
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

  # save all the component models in hub verse format
  model_names |>
    purrr::walk(.f = function(model_id) {
      model_folder <- file.path(path, model_id)
      if (!file.exists(model_folder)) dir.create(model_folder, recursive = TRUE)

      results_path <- file.path(
        model_folder,
        paste0(round_id_value, "-", model_id, ".csv")
      )

      model_out_tbl |>
        dplyr::filter(.data[["model_id"]] == model_id) |>
        dplyr::select(-"model_id") |>
        utils::write.csv(file = results_path, row.names = FALSE)
    })

}
