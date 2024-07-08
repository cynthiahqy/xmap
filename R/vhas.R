#' Boolean flags for properties of candidate and validated xmap links (internal)
#'
#' @description
#' `vhas_*()` functions check properties of xmap links and/or candidate links.
#' The functions only accepts equal length vector inputs to support multiple link formats,
#' but does not check if the inputs are from the same xmap.
#' @param v_from,v_to,v_weights equal length vectors containing the source-target node pairs
#'
#' @return TRUE or FALSE
#'
#' @name vhas
NULL

#' @describeIn vhas Returns TRUE if xmap does not have
#' duplicate pairs of source-target nodes (irrespective of weights)
#'
vhas_no_dup_pairs <- function(v_from, v_to) {
  stopifnot(identical(length(v_from), length(v_to)))
  links <- data.frame(v_from, v_to)
  dup_idx <- anyDuplicated(links)
  !as.logical(dup_idx)
}

# TODO: REPLACE WITH NICER FUNCTION
#' @describeIn vhas Returns TRUE if all weights for a given `from` label
#' sum to one (approximately)
#' @param tol numeric \eqn{\ge 0}. Ignore differences smaller than `tol`.
#' Passed through to the `tolerance` arg of `base::all.equal()`.
vhas_valid_weights <- function(v_from, v_weights, tol = .Machine$double.eps^0.5) {
  stopifnot(identical(length(v_from), length(v_weights)))
  sum_w <- tapply(
    X = v_weights,
    INDEX = v_from,
    FUN = sum,
    simplify = TRUE
  ) |> as.vector()
  sum_w <- sum_w[!sum_w == 0] ## allow for zero weight
  names(sum_w) <- NULL
  ones <- rep(1, length(sum_w))
  all(isTRUE(all.equal(sum_w, ones, tolerance = tol)))
}
