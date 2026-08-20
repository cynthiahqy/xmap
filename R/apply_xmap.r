## shared checker (DO NOT EXPORT) ---------------------------------------------

#' Check whether matched keys/values are conformable with an `xmap_tbl`'s
#' `.from` set (internal)
#'
#' The single source of truth for the two data-conformability conditions
#' `apply_xmap()` requires before transforming, shared by `apply_xmap()`'s
#' abort gate, `diagnose_apply_xmap()`'s flags, and `validate_apply_xmap()`,
#' so the three don't independently re-implement (and risk drifting on) the
#' same checks.
#'
#' @param key A vector of matched keys from `.data` (`key_val$.key`).
#' @param value A data frame of matched value column(s) from `.data`
#' (`kv_tbl$.value`); checked column-by-column since more than one
#' `values_from` column may be selected.
#' @param from `.xmap$.from` to check `key`'s coverage against.
#' @return A list of two logicals: `covered` (every `key` has a matching
#' `from`) and `no_missing_values` (no `value` column has a missing value).
#' @keywords internal
check_conformable_xmap_data <- function(key, value, from) {
  list(
    covered = all(vctrs::vec_in(key, from)),
    no_missing_values = !any(sapply(value, vctrs::vec_any_missing))
  )
}

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
#' @examples
#' abc_xmap <- demo$abc_links |>
#'   as_xmap_tbl(from = "lower", to = "upper", weight_by = "share")
#' abc_data <- tibble::tibble(
#'   lower = unique(demo$abc_links$lower),
#'   count = runif(length(unique(demo$abc_links$lower)), min = 100, max = 500)
#' )
#' apply_xmap(
#'   .data = abc_data,
#'   .xmap = abc_xmap,
#'   values_from = count
#' )
apply_xmap <- function(
  .data,
  .xmap,
  values_from,
  keys_from = names(.xmap$.from),
  ...
) {
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
    match_key,
    .data
  )
  val_id <- tidyselect::eval_select(
    enquo(values_from),
    .data
  )
  key_val <- list(
    .key = .data[key_id],
    .value = .data[val_id]
  )
  kv_tbl <- tibble::new_tibble(key_val)

  checks <- check_conformable_xmap_data(key_val$.key, kv_tbl$.value, .xmap$.from)

  ## coverage check
  if (!checks$covered) {
    msg <- c(
      "x" = "One or more keys in {.arg .data} do not have
                corresponding links in {.arg .xmap}",
      "i" = "Add missing links to {.arg .xmap} or
                subset {.arg .data}",
      "i" = "Use {.fnc diagnose_apply_xmap} for further information"
    )
    cli::cli_abort(msg, class = "coverage_error")
  }

  ## missing value arithmetic check
  if (!checks$no_missing_values) {
    miss_val_cols <- names(kv_tbl$.value)[sapply(kv_tbl$.value, vec_any_missing)]
    msg <- c(
      "x" = "Missing values not allowed in {.arg .data} columns:
                {.val {miss_val_cols}}",
      "i" = "Remove or replace missing values",
      "i" = "Use {.fnc diagnose_apply_xmap} for further information"
    )
    cli::cli_abort(msg, class = "missing_mass_values")
  }
  ## TODO: add diagnose function -- with nuance around one-to-one

  transform_join <- dplyr::left_join(
    kv_tbl,
    .xmap,
    dplyr::join_by(!!sym(".key") == !!sym(".from"))
  )

  transformed_data <- transform_join |>
    dplyr::mutate(.value = .data$.value * .data$.weight_by[[1]]) |>
    dplyr::select(".to", ".value") |>
    # dplyr::select(dplyr::all_of(c(".to", ".value"))) |>
    dplyr::group_by(.data$.to) |>
    tidyr::unpack(tidyselect::all_of(".value")) |>
    dplyr::summarise(
      .out = dplyr::across(
        dplyr::everything(),
        \(x) sum(x, na.rm = FALSE)
      )
    )

  transformed_data |>
    tidyr::unpack(dplyr::everything())
}

#' @details
#' `diagnose_apply_xmap()` checks whether `.data` is conformable with
#' `.xmap` -- the same two conditions [apply_xmap()] checks -- and returns
#' detail on any offending rows, to help resolve the specific issue rather
#' than just knowing something's wrong. The returned `xmap_diagnosis`'s
#' `details` has one entry per condition (`NULL` where that check passed):
#'
#' - `not_covered`: rows of `.data` whose `keys_from` key has no matching
#'   link in `.xmap$.from`
#' - `missing_values`: rows of `.data` with a missing value in one or more
#'   `values_from` columns
#' @return `diagnose_apply_xmap()` returns an `xmap_diagnosis` object: a
#' list with `valid` (a scalar logical) and `details` (a named list of
#' tibbles of offending rows, one per check, `NULL` where that check
#' passed). Printing the result shows a readable pass/fail report; see
#' [new_xmap_diagnosis()].
#' @export
#' @describeIn apply_xmap Returns an `xmap_diagnosis` object diagnosing why
#' `.data` fails `apply_xmap()`'s conformability checks.
diagnose_apply_xmap <- function(
  .data,
  .xmap,
  values_from,
  keys_from = names(.xmap$.from),
  ...
) {
  match_key <- if (missing(keys_from)) names(.xmap$.from) else enquo(keys_from)
  ## setup shared mass array (key_value pairs)
  key_id <- tidyselect::eval_select(
    match_key,
    .data
  )
  val_id <- tidyselect::eval_select(
    enquo(values_from),
    .data
  )
  key_val <- list(
    .key = .data[key_id],
    .value = .data[val_id]
  )
  kv_tbl <- tibble::new_tibble(key_val)

  flags <- list()
  details <- list(
    not_covered = NULL,
    missing_values = NULL
  )

  checks <- check_conformable_xmap_data(key_val$.key, kv_tbl$.value, .xmap$.from)
  flags$not_covered <- !checks$covered
  flags$missing_values <- !checks$no_missing_values

  if (flags$not_covered) {
    key_in_from <- vec_in(key_val$.key, .xmap$.from)
    details$not_covered <- vec_slice(kv_tbl, !key_in_from)
  }
  if (flags$missing_values) {
    row_has_missing <- rowSums(is.na(kv_tbl$.value)) > 0
    details$missing_values <- vec_slice(kv_tbl, row_has_missing)
  }

  valid <- !any(simplify2array(flags))

  new_xmap_diagnosis(
    valid,
    details,
    labels = list(
      not_covered = c(
        pass = "All `.data` keys are covered by `.xmap$.from`",
        fail = "`.data` keys not covered by `.xmap$.from`"
      ),
      missing_values = c(
        pass = "No missing values in `.data`'s value columns",
        fail = "Missing values in `.data`'s value columns"
      )
    ),
    class = "xmap_diagnosis_apply",
    msg_valid = "{.field .data} is conformable with {.field .xmap}",
    msg_invalid = "{.field .data} is not conformable with {.field .xmap}"
  )
}

#' Cheaply check whether `.data` is conformable with an `xmap_tbl`
#'
#' `validate_apply_xmap()` checks the same two conditions `apply_xmap()`
#' requires before transforming `.data` -- every `keys_from` key has a
#' matching `.xmap$.from` link, and no `values_from` column has a missing
#' value -- and returns a single logical, without building the
#' offending-rows/columns detail objects that `diagnose_apply_xmap()` does.
#' It's the primitive to reach for when you only need a pass/fail answer --
#' e.g. checking many `.data`/`.xmap` group pairs with `dplyr::mutate()` or
#' `purrr::map2_lgl()` before applying any of them. Reach for
#' `diagnose_apply_xmap()` once `validate_apply_xmap()` says something
#' failed and you need to know why; `apply_xmap()` checks the same
#' conditions at transform time and aborts with a message pointing at the
#' offending condition.
#'
#' @inheritParams apply_xmap
#' @return A single logical.
#' @export
#' @examples
#' abc_xmap <- demo$abc_links |>
#'   as_xmap_tbl(from = "lower", to = "upper", weight_by = "share")
#' abc_data <- tibble::tibble(
#'   lower = unique(demo$abc_links$lower),
#'   count = runif(length(unique(demo$abc_links$lower)), min = 100, max = 500)
#' )
#' validate_apply_xmap(abc_data, abc_xmap, values_from = count)
validate_apply_xmap <- function(
  .data,
  .xmap,
  values_from,
  keys_from = names(.xmap$.from),
  ...
) {
  match_key <- if (missing(keys_from)) names(.xmap$.from) else enquo(keys_from)

  key_id <- tidyselect::eval_select(match_key, .data)
  val_id <- tidyselect::eval_select(enquo(values_from), .data)
  key_val <- list(
    .key = .data[key_id],
    .value = .data[val_id]
  )

  checks <- check_conformable_xmap_data(key_val$.key, key_val$.value, .xmap$.from)
  checks$covered && checks$no_missing_values
}
