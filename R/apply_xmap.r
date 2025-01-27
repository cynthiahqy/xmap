#' Apply Crossmap Transformation to Conformable Data
#'
#' This function applies crossmap transformation to a dataset, transforming
#' data based on specified mapping rules.
#'
#' @param .data The dataset to transform.
#' @param .xmap An `xmap_tbl` object.
#' @param ... (reserved)
#' @param values_from A `tidyselect` expression of columns in `.data`
#' with values to transform
#' @param keys_from A `tidyselect` expression specifies the column in `.data`
#' to match with `.xmap$from`
#' @return A tibble with transformed data.
#' @export
#' @rdname apply_xmap
apply_xmap <- function(
    .data, .xmap, ..., values_from,
    keys_from = names(.xmap$.from)) {
  ## TODO: verify .xmap is class xmap_tbl
  ## TODO: add ref column to check mass preservation (would catch modified weights)

  if (missing(keys_from)) {
    match_key <- keys_from
    msg <- c(
      "Matching keys in `.data${keys_from}` with
            `.xmap$.from${names(.xmap$.from)}`",
      "i" = "To silence, set `keys_from = {keys_from}`"
    )
    cli::cli_inform(msg)
  } else {
    match_key <- enquo(keys_from)
  }

  ## setup shared mass array (key_value pairs)
  key_id <- tidyselect::eval_select(
    match_key, .data
  )
  val_id <- tidyselect::eval_select(
    enquo(values_from), .data
  )
  key_val <- list(
    .key = .data[key_id],
    .value = .data[val_id]
  )
  ## coverage check
  if (!all(vec_in(key_val$.key, .xmap$.from))) {
    msg <- c(
      "x" = "One or more keys in `.data` do not have
                corresponding links in `.xmap`",
      "i" = "Add missing links to `.xmap` or
                subset `.data`"
    )
    cli::cli_abort(msg,
      class = "coverage_error"
    )
  }

  kv_tbl <- tibble::new_tibble(key_val)

  ## missing value arithmetic check
  na <- "error" # ifelse(missing(na), "error", arg_match(na))
  has_missing_values <- sapply(kv_tbl$.value, vec_any_missing)
  if (any(has_missing_values)) {
    miss_val_cols <- names(kv_tbl$.value)[has_missing_values]
    msg <- c(
      "x" = "Missing values not allowed in `.data` columns:
                {miss_val_cols}",
      "i" = "Remove or replace missing values."
    )
    if (na == "error") {
      cli::cli_abort(msg, class = "missing_mass_values")
    }
  }
  ## TODO: add diagnose function -- with nuance around one-to-one

  transformed_data <- dplyr::left_join(
    kv_tbl, .xmap,
    dplyr::join_by(.key == .from)
  ) |>
    dplyr::mutate(.value = .value * .weight_by[[1]]) |>
    dplyr::select(.to, .value) |>
    dplyr::group_by(.to) |>
    tidyr::unpack(.value) |>
    dplyr::summarise(.out = dplyr::across(
      dplyr::everything(),
      \(x) sum(x, na.rm = FALSE)
    ))

  transformed_data |>
    tidyr::unpack(dplyr::everything())
}

#' @export
#' @describeIn apply_xmap Returns messages for any diagnosed issues.
diagnose_apply_xmap <- function(
    .data, .xmap, ..., values_from,
    keys_from = NULL) {
  match_key <- enquo(keys_from) %||% names(.xmap$.from)
  ## setup shared mass array (key_value pairs)
  key_id <- tidyselect::eval_select(
    match_key, .data
  )
  val_id <- tidyselect::eval_select(
    enquo(values_from), .data
  )
  key_val <- list(
    .key = .data[key_id],
    .value = .data[val_id]
  )
  kv_tbl <- tibble::new_tibble(key_val)

  flags <- list()
  details <- list()

  key_in_from <- vec_in(key_val$.key, .xmap$.from)
  flags$not_covered <- !all(key_in_from)
  has_missing_values <- sapply(kv_tbl$.value, vec_any_missing)
  flags$missing_values <- any(has_missing_values)

  if (flags$not_covered) {
    n_uncovered <- sum(!key_in_from)
    uncovered_rows <-
      vec_slice(kv_tbl, !vec_in(key_val$.key, .xmap$.from))
    details$not_covered <- uncovered_rows

    msg <- c(
      "x" = "Found {n_uncovered} key{?s} in `.data`
                  without corresponding match in `.xmap$.from`",
      "See .$not_covered"
    )
    cli::cli_inform(msg, class = "not_covered")
  }
  if (flags$missing_values) {
    miss_val_cols <- names(kv_tbl$.value)[has_missing_values]
    details$miss_val_cols <- miss_val_cols

    msg <- c(
      "x" = "Missing values found in `.data` columns:
                    {miss_val_cols}",
      "See .$miss_val_cols"
    )
    cli::cli_inform(msg, class = "missing_mass_values")
  }

  if (any(simplify2array(flags))) {
    return(details)
  } else {
    msg <- c(
      "`.data` is conformable with `.xmap`.",
      "*" = "No missing values in `values_from`",
      "*" = "All `.data` keys can be matched with `.xmap$.from` keys"
    )
    cli::cli_inform(msg)
    invisible(.data)
  }
}
