#' Boolean flags for properties of candidate and validated xmap links (internal)
#'
#' @description
#' `vhas_*()` functions check properties of xmap links and/or candidate links.
#' They are the shared primitives behind the three link-validity conditions
#' checked independently by [xmap_tbl()], [diagnose_as_xmap_tbl()], and
#' [validate_as_xmap()]'s `data.frame` method — every non-matrix check of
#' "is `x` a valid crossmap" should route through these rather than
#' reimplementing the underlying logic.
#' @param x a vector, or a single-column data frame (as used to store
#' `.from`/`.to`/`.weight_by` in `xmap_tbl`), to check for missing values
#' @param v_from,v_to,v_weights equal length vectors containing the source-target node pairs
#'
#' @return TRUE or FALSE
#' @keywords internal
#'
#' @name vhas
NULL

#' @describeIn vhas Returns TRUE if `x` has no missing values
#' @keywords internal
#'
vhas_no_missing <- function(x) {
  !vctrs::vec_any_missing(x)
}

#' @describeIn vhas Returns TRUE if xmap does not have
#' duplicate pairs of source-target nodes (irrespective of weights)
#' @keywords internal
#'
vhas_no_dup_pairs <- function(v_from, v_to) {
  stopifnot(identical(vctrs::vec_size(v_from), vctrs::vec_size(v_to)))
  links <- data.frame(v_from, v_to)
  dup_idx <- anyDuplicated(links)
  !as.logical(dup_idx)
}

#' @describeIn vhas Returns TRUE if all weights for a given `from` label
#' sum to (approximately) one. A `from` label with no outgoing weights, or
#' whose outgoing weights sum to zero, fails this check — a valid crossmap
#' has no dangling `.from` nodes. A missing weight also fails this check
#' (rather than propagating `NA`) — `vhas_no_missing()` is where a
#' missing-weight condition should be diagnosed on its own terms.
#' @keywords internal
#' @param tol numeric \eqn{\ge 0}. Ignore differences smaller than `tol`.
#' Passed through to the `tol` arg of `dplyr::near()`.
vhas_valid_weights <- function(v_from, v_weights, tol = .Machine$double.eps^0.5) {
  stopifnot(identical(length(v_from), length(v_weights)))
  sum_w <- tapply(
    X = v_weights,
    INDEX = v_from,
    FUN = sum,
    simplify = TRUE
  ) |> as.vector()
  isTRUE(all(dplyr::near(sum_w, 1L, tol = tol)))
}
