#' Compose Two Crossmaps Through a Shared Intermediate Classification
#'
#' Given `xmap1` (`A -> B`) and `xmap2` (`B -> C`) that share domain `B`
#' (`xmap1`'s `.to` and `xmap2`'s `.from`), `compose_xmap()` builds the
#' crossmap `A -> C` directly, without ever materialising `B`-level values.
#' Composed weights are standard weighted-path composition, summed over
#' every intermediate `b` connecting a given `a` to a given `c`:
#' `w(a, c) = sum_b w1(a, b) * w2(b, c)`.
#'
#' This is the right tool when an intermediate classification only exists
#' to chain two crossmaps together and its own values are never needed on
#' their own -- e.g. collapsing a source classification straight to a
#' coarser one through a shared but uninteresting mid-level classification
#' (`str_sub()`-style hierarchical aggregation, or any other deterministic
#' `B -> C` map), rather than computing and re-aggregating `B`-level
#' figures as an unnecessary intermediate step.
#'
#' @param xmap1 An `xmap_tbl`, `A -> B`.
#' @param xmap2 An `xmap_tbl`, `B -> C`. Every value in `xmap1`'s `.to`
#' must appear in `xmap2`'s `.from` -- `compose_xmap()` aborts rather than
#' silently drop uncovered mass.
#' @param ... (reserved)
#' @inheritParams dplyr::near
#' @return An `xmap_tbl`, `A -> C`.
#' @export
#' @examples
#' abc_xmap <- demo$abc_links |>
#'   as_xmap_tbl(from = lower, to = upper, weight_by = share)
#' region_xmap <- tibble::tibble(
#'   upper = c("AA", "BB", "CC", "DD", "EE"),
#'   region = c("N", "N", "S", "S", "S"),
#'   weight = 1
#' ) |>
#'   as_xmap_tbl(from = upper, to = region, weight_by = weight)
#' compose_xmap(abc_xmap, region_xmap)
compose_xmap <- function(xmap1, xmap2, ..., tol = .Machine$double.eps^0.5) {
  if (!inherits(xmap1, "xmap_tbl") || !inherits(xmap2, "xmap_tbl")) {
    cli::cli_abort(
      "{.arg xmap1} and {.arg xmap2} must both be {.cls xmap_tbl} objects.",
      class = "compose_xmap_bad_input"
    )
  }

  from_name <- get_name_from(xmap1)
  to_name <- get_name_to(xmap2)

  link1 <- tibble::tibble(
    .from = xmap1$.from[[1]],
    .mid = xmap1$.to[[1]],
    .w1 = xmap1$.weight_by[[1]]
  )
  link2 <- tibble::tibble(
    .mid = xmap2$.from[[1]],
    .to = xmap2$.to[[1]],
    .w2 = xmap2$.weight_by[[1]]
  )

  uncovered <- setdiff(unique(link1$.mid), unique(link2$.mid))
  if (length(uncovered) > 0) {
    msg <- c(
      "x" = "{.arg xmap2}'s `.from` does not cover every value in
                {.arg xmap1}'s `.to`",
      "i" = "Uncovered: {.val {uncovered}}"
    )
    cli::cli_abort(msg, class = "compose_xmap_uncovered")
  }

  composed <- dplyr::inner_join(
    link1, link2,
    by = ".mid", relationship = "many-to-many"
  ) |>
    dplyr::mutate(.w = .data$.w1 * .data$.w2) |>
    dplyr::group_by(.data$.from, .data$.to) |>
    dplyr::summarise(.w = sum(.data$.w), .groups = "drop")

  xmap_tbl(
    .from = tibble::tibble(!!from_name := composed$.from),
    .to = tibble::tibble(!!to_name := composed$.to),
    .weight_by = tibble::tibble(weight_by = composed$.w),
    tol = tol
  )
}
