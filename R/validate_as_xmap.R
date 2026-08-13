## generic -----------------------------------------------------------------

#' Cheaply check whether links form a valid crossmap
#'
#' `validate_as_xmap()` checks the same conditions as `diagnose_as_xmap_tbl()`
#' (no duplicate `.from`-`.to` pairs, no missing weights, outgoing weights
#' from each `.from` sum to one) but returns a single logical instead of a
#' detail object. The duplicate-pairs check is data-frame-specific; see the
#' `.matrix` method below for why it doesn't carry over. It's the primitive
#' to reach for when you only need a
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

## matrix method -------------------------------------------------------------

#' @section No duplicate-pairs check for matrices:
#' Unlike the `.data.frame` method, `validate_as_xmap.matrix()` does not
#' check for duplicate `.from`-`.to` pairs. A single cell can't encode a
#' duplicate pair (each is already a unique row x column intersection), but
#' base R places no uniqueness constraint on `dimnames()` — e.g.
#' `matrix(1:4, 2, 2, dimnames = list(c("a", "a"), c("x", "y")))` is a
#' valid matrix with a repeated row name. Repeated row names mean the same
#' `.from` key has more than one, independently-checked set of outgoing
#' weights (which one applies is ambiguous); repeated column names mean
#' weights for the same `.to` key are split across columns unnoticed,
#' since `rowSums()` doesn't care about column labels. Neither is
#' currently detected — `has_dimnames` only checks that
#' `rownames()`/`colnames()` are non-`NULL`, not that they're unique.
#' @export
#' @rdname validate_as_xmap
#' @examples
#' abc_matrix <- demo$abc_links |>
#'   tidyr::pivot_wider(names_from = upper, values_from = share, values_fill = 0) |>
#'   tibble::column_to_rownames("lower") |>
#'   as.matrix()
#' validate_as_xmap(abc_matrix)
validate_as_xmap.matrix <- function(x, ..., tol = .Machine$double.eps^0.5) {
  has_dimnames <- !is.null(rownames(x)) && !is.null(colnames(x))
  is_numeric <- is.numeric(x)
  no_missing_weights <- is_numeric && !anyNA(x)

  ## rows summing to exactly 0 (a .from with no outgoing links) fail here
  ## too, since 0 is never near enough to 1 — no special-casing needed.
  ## Only computed once is_numeric/no_missing_weights hold, so rowSums()
  ## never runs on non-numeric or NA-containing input.
  weights_sum_to_one <- no_missing_weights &&
    all(dplyr::near(rowSums(x), 1L, tol = tol))

  has_dimnames && is_numeric && no_missing_weights && weights_sum_to_one
}
