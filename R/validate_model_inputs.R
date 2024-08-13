#' Perform simple validations on the model variations dataframe used to define a
#' given baseline model
#'
#' @param model_variations a `data.frame` where each row specifies a set of
#'   hyperparameters to use for a single baseline model fit, with the following
#'   columns: `transformation`, `symmetrize`, and `window_size`. See the details
#'   for more information.
#' @details The types, and possible values for each of the columns in
#' `model_variations` are as follows:
#'   - transformation (character): "none" or "sqrt"
#'   - symmetric (boolean)
#'   - window_size (numeric): a non-negative integer
#' Additional validations that check each column for the correct type are performed
#' by `validate_variation_inputs()`, which will always be called following a call to
#' `validate_model_inputs()`.
#'
#' @return no return value
#'
#' @noRd

validate_model_variations <- function(model_variations) {
  variation_col <- c("transformation", "symmetrize", "window_size")
  actual_col <- colnames(model_variations)
  if (is.null(model_variations)) {
    cli::cli_abort("{.arg model_variations} is missing")
  } else if (!all(variation_col %in% actual_col)) {
    cli::cli_abort("{.arg model_variations} is missing the column{?s}: {.val {setdiff(variation_col, actual_col)}}.")
  } else if (!all(actual_col %in% variation_col) && all(variation_col %in% actual_col)) {
    cli::cli_abort(c(
      x = "{.arg model_variations} contains the extra column{?s}: {.val {setdiff(actual_col, variation_col)}}.",
      i = "Double check that your model variations table does not contain duplicate rows when removing columns."
    ))
  }

  if (nrow(dplyr::distinct(model_variations)) != nrow(model_variations)) {
    cli::cli_abort("{.arg model_variations} contains duplicate rows.")
  }
}


#' Perform simple validations on the individual variables defining a single
#' baseline model
#'
#' @param transformation string specifying the transformation used on the
#'   distribution which determines its shape; can be one of "none" or "sqrt".
#' @param symmetrize boolean specifying whether to make the distribution symmetric;
#'   can be one of `TRUE` or `FALSE`.
#' @param window_size integer specifying how many previous observations in the
#'   target data should be used to inform the forecasts
#'
#' @return no return value
#'
#' @noRd
validate_variation_inputs <- function(transformation, symmetrize, window_size) {
  # check variation inputs have length 1
  if (length(transformation) != 1) {
    cli::cli_abort("{.arg transformation} must be length 1")
  } else if (length(symmetrize) != 1) {
    cli::cli_abort("{.arg symmetrize} must be length 1")
  } else if (length(window_size) != 1) {
    cli::cli_abort("{.arg window_size} must be length 1")
  }

  valid_transformations <- c("none", "sqrt")
  if (!transformation %in% valid_transformations) {
    cli::cli_abort("{.arg transformation} must only contain values {.val {valid_transformations}}")
  } else if (!inherits(symmetrize, "logical")) {
    cli::cli_abort("{.arg symmetrize} must only contain logical values, e.g. TRUE or FALSE.")
  } else if (window_size != trunc(window_size) || window_size < 0) {
    cli::cli_abort("{.arg window_size} must only contain non-negative integer values.")
  }
}


#' Perform simple validations on the target data (time series) dataframe
#'
#' @param target_ts a `data.frame` of target data in a time series format
#'   (contains columns `time_index`, `location`, and `observation`)
#'
#' @return no return value
#'
#' @noRd
validate_target_ts <- function(target_ts) {
  target_col <- c("time_index", "location", "observation")
  actual_col <- colnames(target_ts)
  if (!all(target_col %in% actual_col)) {
    cli::cli_abort("{.arg target_ts} is missing the column{?s}: {.val {setdiff(target_col, actual_col)}}.")
  } else if (!all(actual_col %in% target_col) && all(target_col %in% actual_col)) {
    cli::cli_abort(c(
      x = "{.arg target_ts} contains the extra column{?s}: {.val {setdiff(actual_col, target_col)}}.",
      i = "Double check that your target data does not contain duplicate rows when removing columns."
    ))
  }

  if (nrow(dplyr::distinct(target_ts)) != nrow(target_ts)) {
    cli::cli_abort("{.arg target_ts} contains duplicate rows.")
  }
}
