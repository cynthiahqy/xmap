## generic -----------------------------------------------------------------

#' Cheaply check whether links form a valid crossmap
#'
#' `validate_as_xmap()` checks the same conditions as `diagnose_as_xmap_tbl()`
#' (no duplicate `.from`-`.to` pairs, no missing weights, outgoing weights
#' from each `.from` sum to one) but returns a single logical instead of a
#' detail object. It's the primitive to reach for when you only need a
#' pass/fail answer — e.g. inside `dplyr::filter()` or `dplyr::group_map()`
#' over many groups — since it never builds the offending-rows tibbles that
#' `diagnose_as_xmap_tbl()` does. Reach for `diagnose_as_xmap_tbl()` once
#' `validate_as_xmap()` says something failed and you need to know why.
#'
#' @param x An object with links to validate. Methods exist for `data.frame`.
#' @param ... Passed to methods.
#' @return A single logical.
#' @export
validate_as_xmap <- function(x, ...) {
  UseMethod("validate_as_xmap")
}

## data.frame method ---------------------------------------------------------

#' @param from The column in `x` that specifies the 'from' nodes.
#' @param to The column in `x` that specifies the 'to' nodes.
#' @param weight_by The column in `x` that specifies the weight of the links.
#' @inheritParams dplyr::near
#' @export
#' @rdname validate_as_xmap
#' @examples
#' demo$abc_links |>
#'   validate_as_xmap(from = lower, to = upper, weight_by = share)
validate_as_xmap.data.frame <- function(
    x, from, to, weight_by, ...,
    tol = .Machine$double.eps^0.5) {
  from_id <- tidyselect::eval_select(enquo(from), x)
  to_id <- tidyselect::eval_select(enquo(to), x)
  weight_by_id <- tidyselect::eval_select(enquo(weight_by), x)

  tbl_x <- tibble::tibble(
    .from = x[from_id],
    .to = x[to_id],
    .weight_by = x[weight_by_id]
  )

  no_dup_pairs <- anyDuplicated(tbl_x[c(".from", ".to")]) == 0
  no_missing_weights <- !vec_any_missing(tbl_x$.weight_by)

  from_sums <- tbl_x |>
    dplyr::group_by(.data$.from) |>
    dplyr::summarise(.sum.weight_by = sum(.data$.weight_by), .groups = "drop")
  weights_sum_to_one <- all(dplyr::near(from_sums$.sum.weight_by, 1L, tol = tol))

  no_dup_pairs && no_missing_weights && weights_sum_to_one
}
