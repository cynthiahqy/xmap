#' Compose Two Crossmaps Through a Shared Intermediate Classification
#'
#' Given `xmap1` (`S -> M`) and `xmap2` (`M -> T`) sharing intermediate
#' key set `M`, chains them into a single crossmap `S -> T` without
#' materialising `M`-level values. Composed weights sum, over every
#' shared `m`, the product of `xmap1`'s weight onto `m` and `xmap2`'s
#' weight from `m`:
#' \deqn{w(s, t) = \sum_{m \in M} w_1(s, m) \, w_2(m, t)}{
#'   w(s, t) = sum over m in M of  w1(s, m) * w2(m, t)}
#'
#' @details
#' Re-checks that both inputs are actually valid crossmaps, not just
#' correctly classed, and aborts otherwise.
#'
#' Only takes two crossmaps at a time. Matrix multiplication is
#' associative, so chain longer sequences with `Reduce()` instead of a
#' dedicated variadic interface -- see the example below. Grouped
#' composition (e.g. one `xmap1` per group, composed against a shared
#' `xmap2`) is likewise left to the caller via `dplyr::group_map()`.
#'
#' @param xmap1 An `xmap_tbl`, `S -> M`.
#' @param xmap2 An `xmap_tbl`, `M -> T`. Every value in `xmap1`'s `.to`
#' must appear in `xmap2`'s `.from`; the reverse isn't required -- `xmap2`
#' may hold `.from` values `xmap1` never uses.
#' @param ... (reserved)
#' @inheritParams dplyr::near
#' @return An `xmap_tbl`, `S -> T`.
#' @export
#' @examples
#' abc_xmap <- demo$abc_links |>
#'   as_xmap_tbl(from = lower, to = upper, weight_by = share)
#' top_xmap <- tibble::tibble(
#'   upper = c("AA", "BB", "CC", "DD", "EE"),
#'   top = c("AAA", "AAA", "BBB", "BBB", "BBB"),
#'   weight = 1
#' ) |>
#'   as_xmap_tbl(from = upper, to = top, weight_by = weight)
#' compose_xmap(abc_xmap, top_xmap)
#'
#' # chaining more than two crossmaps: reduce pairwise composition over a
#' # list, e.g. lower -> upper -> top -> region
#' region_xmap <- tibble::tibble(
#'   top = c("AAA", "BBB"),
#'   region = c("north", "south"),
#'   weight = 1
#' ) |>
#'   as_xmap_tbl(from = top, to = region, weight_by = weight)
#' Reduce(compose_xmap, list(abc_xmap, top_xmap, region_xmap))
compose_xmap <- function(xmap1, xmap2, ..., tol = .Machine$double.eps^0.5) {
  if (!inherits(xmap1, "xmap_tbl") || !inherits(xmap2, "xmap_tbl")) {
    cli::cli_abort(
      "{.arg xmap1} and {.arg xmap2} must both be {.cls xmap_tbl} objects.",
      class = "compose_xmap_bad_input"
    )
  }

  ## being classed xmap_tbl is not a guarantee of validity -- an xmap_tbl
  ## can be hand-assembled or mutated after construction without going
  ## back through xmap_tbl()'s validation gate, so check explicitly here
  ## rather than composing garbage in, garbage out
  if (!check_valid_xmap_df(xmap1, tol = tol)) {
    msg <- c(
      "x" = "{.arg xmap1} is not a valid crossmap",
      "i" = "Use {.fn diagnose_as_xmap_tbl} to see why"
    )
    cli::cli_abort(msg, class = "compose_xmap_invalid_input")
  }
  if (!check_valid_xmap_df(xmap2, tol = tol)) {
    msg <- c(
      "x" = "{.arg xmap2} is not a valid crossmap",
      "i" = "Use {.fn diagnose_as_xmap_tbl} to see why"
    )
    cli::cli_abort(msg, class = "compose_xmap_invalid_input")
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
