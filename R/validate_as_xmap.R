## generic -----------------------------------------------------------------

#' Cheaply check whether links form a valid crossmap
#'
#' A valid crossmap's links must satisfy three conditions:
#'
#' - every link has a non-missing `.from`, `.to`, and `.weight_by`
#' - no two links share the same `.from`-`.to` pair (data-frame
#'   representations only — see the `.matrix` method for why this doesn't
#'   carry over to a matrix representation)
#' - for each `.from`, the `.weight_by` values of its outgoing links sum to
#'   (approximately) one — this is what guarantees the total mass before
#'   and after a transformation stays the same
#'
#' `validate_as_xmap()` checks these conditions and returns a single
#' logical, without building the offending-rows detail objects that
#' `diagnose_as_xmap_tbl()` does. It's the primitive to reach for when you
#' only need a pass/fail answer — e.g. inside `dplyr::filter()` or
#' `dplyr::group_map()` over many groups. Reach for `diagnose_as_xmap_tbl()`
#' once `validate_as_xmap()` says something failed and you need to know
#' why; `xmap_tbl()`/`as_xmap_tbl()` check the same conditions at
#' construction time and abort with a message pointing at the offending
#' condition.
#'
#' @param x An object with links to validate. Methods exist for `data.frame`
#' and `matrix`.
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

  no_missing_from <- !vec_any_missing(tbl_x$.from)
  no_missing_to <- !vec_any_missing(tbl_x$.to)
  no_missing_weights <- !vec_any_missing(tbl_x$.weight_by)
  no_dup_pairs <- anyDuplicated(tbl_x[c(".from", ".to")]) == 0

  from_sums <- tbl_x |>
    dplyr::group_by(.data$.from) |>
    dplyr::summarise(.sum.weight_by = sum(.data$.weight_by), .groups = "drop")
  weights_sum_to_one <- all(dplyr::near(from_sums$.sum.weight_by, 1L, tol = tol))

  no_missing_from && no_missing_to && no_missing_weights &&
    no_dup_pairs && weights_sum_to_one
}

## matrix method -------------------------------------------------------------

#' @section Conditions for matrices:
#' A matrix represents `.from`/`.to` identity through `dimnames()` (rows =
#' `.from`, columns = `.to`) rather than per-link values, so the three
#' conditions above translate differently:
#'
#' - non-missing `.from`/`.to` becomes "`rownames()`/`colnames()` are
#'   non-`NULL`, with no repeated names"; non-missing `.weight_by` becomes
#'   "no `NA` cells"
#' - the no-duplicate-pairs check does not carry over as-is: a single cell
#'   can't encode a duplicate pair (each is already a unique row x column
#'   intersection). What can still happen — and is checked above as a
#'   `.from`/`.to`-identity condition, not a pairs condition — is repeated
#'   `dimnames()`: base R places no uniqueness constraint on them, e.g.
#'   `matrix(1:4, 2, 2, dimnames = list(c("a", "a"), c("x", "y")))` is a
#'   valid matrix with a repeated row name. A repeated row name would mean
#'   the same `.from` key has more than one, independently-checked set of
#'   outgoing weights; a repeated column name would mean weights for the
#'   same `.to` key are split across columns, invisible to `rowSums()`.
#'   Both are rejected by the row/column name uniqueness check
#' - weights summing to one becomes a row-sum check; a row summing to
#'   exactly zero (a `.from` with no outgoing links) fails here too, since
#'   0 is never near enough to 1
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
  no_dup_from <- has_dimnames && anyDuplicated(rownames(x)) == 0
  no_dup_to <- has_dimnames && anyDuplicated(colnames(x)) == 0
  is_numeric <- is.numeric(x)
  no_missing_weights <- is_numeric && !anyNA(x)

  ## Only computed once is_numeric/no_missing_weights hold, so rowSums()
  ## never runs on non-numeric or NA-containing input.
  weights_sum_to_one <- no_missing_weights &&
    all(dplyr::near(rowSums(x), 1L, tol = tol))

  has_dimnames && no_dup_from && no_dup_to &&
    is_numeric && no_missing_weights && weights_sum_to_one
}
