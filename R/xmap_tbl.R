## constructor ---------------------------------------------------------

#' @importFrom tibble new_tibble tibble
new_xmap_tbl <- function(x = list(
                           .from = tibble::tibble(source = character()),
                           .to = tibble::tibble(target = character()),
                           .weight_by = tibble::tibble(distr = double())
                         ),
                         tol = .Machine$double.eps^0.5, class = NULL) {
  if (!length(x) == 3) {
    abort("`x` must be a list of length 3.")
  }
  if (any(lapply(x, ncol) != 1)) {
    abort("`.from`, `.to`, `.weight_by` should only have one column each")
  }
  if (!is.numeric(x$.weight_by[[1]])) {
    abort("`x$`.weight_by`[[1]]` must be a numeric vector.")
  }
  if ((!is_double(tol) || length(tol) != 1)) {
    cli::cli_abort(
      "{.arg tol} must be a single double
            specifying the floating point tolerance."
    )
  }

  tibble::new_tibble(x,
    n = NULL, tol = tol,
    class = c(class, "xmap_tbl", "xmap")
  )
}

## Helpers -------------------------------------------------
# TODO: conditional error messages based on call???
# checks below share their underlying logic with validate_as_xmap()'s
# data.frame method and diagnose_as_xmap_tbl() via the vhas_*() helpers
# (R/vhas.R) -- see #19
xmap_tbl <- function(.from = tibble::tibble(source = character()),
                     .to = tibble::tibble(target = character()),
                     .weight_by = tibble::tibble(ones = 1L),
                     tol = .Machine$double.eps^0.5) {
  arg_names <- c(".from", ".to", ".weight_by")

  ## TODO: add message about recycling weights
  if (missing(.weight_by)) {
    message("Using unit `.weight_by = tibble::tibble(ones = 1L)`")
  }

  ## input classes checks
  class_pass <- sapply(
    list(.from, .to, .weight_by),
    \(x) "data.frame" %in% class(x)
  )
  if (!all(class_pass)) {
    msg <- c(
      "x" = "{.arg {arg_names[!class_pass]}} must be {?a/}
                    data frame-like object{?s}",
      "i" = "Try replacing `[[` or `$` selector with `[`,
                    or use {.fnc as_xmap_tbl()}"
    )
    cli::cli_abort(msg)
  }

  ## size checks
  if (vec_size(.from) != vec_size(.to)) {
    msg <- c(
      "x" = "{.arg {c('.from', '.to')}}
                    must be the same size"
    )
    cli::cli_abort(msg)
  }
  if (!any(
    vec_size(.weight_by) == 1,
    vec_size(.from) == vec_size(.weight_by)
  )) {
    msg <- c(
      "x" = "{.arg weight_by} must be the compatible size with
                    {.arg {c('from', 'to')}}"
    )
    cli::cli_abort(msg)
  }
  .weight_by <- vec_recycle(.weight_by, vec_size(.from))

  ## validate edge list and edges -- single shared check, see #19
  x_list <- list(.from, .to, .weight_by)
  names(x_list) <- arg_names
  tbl_x <- tibble::tibble(.from = .from, .to = .to, .weight_by = .weight_by)

  if (!check_valid_xmap_df(tbl_x, tol = tol)) {
    msg <- c(
      "x" = "{.arg {c('.from', '.to', '.weight_by')}} do not form a
                    valid crossmap",
      "i" = "Every link needs a non-missing `.from`, `.to`, `.weight_by`,
                    no two links may share a `.from`-`.to` pair, and each
                    `.from`'s outgoing `.weight_by` must sum to 1",
      "i" = "Use {.fnc diagnose_as_xmap_tbl} for further information"
    )
    cli::cli_abort(msg, class = "abort_invalid_xmap")
  }

  new_xmap_tbl(x = x_list, tol = tol)
}

## Coercion -------------------------
#' @export
#' @rdname as_xmap_tbl
as_xmap_tbl <- function(x, ...) {
  UseMethod("as_xmap_tbl")
}

#' Coercing data frames of links to crossmap tibbles
#'
#' This method takes a data.frame-like object and converts it into an `xmap_tbl`
#' based on specified columns for 'from', 'to', and 'weight'. Aborts with a
#' message pointing at the offending condition if the links aren't a valid
#' crossmap — the same conditions [validate_as_xmap()] checks, though
#' currently implemented independently rather than by calling it.
#'
#' @param x A data.frame or tibble to be converted in a crossmap tibble.
#' @param from The column in `x` that specifies the 'from' nodes.
#' @param to The column in `x` that specifies the 'to' nodes.
#' @param weight_by The column in `x` that specifies the weight of the links.
#' @param ... (reserved) Additional arguments passed to methods.
#' @inheritParams dplyr::near
#' @return Returns an xmap tibble object.
#' @export
#' @rdname as_xmap_tbl
#' @examples
#' demo$abc_links |>
#'   as_xmap_tbl(from = lower, to = upper, weight_by = share)
as_xmap_tbl.data.frame <- function(
    x, from, to, weight_by, ...,
    tol = .Machine$double.eps^0.5) {
  from_id <- tidyselect::eval_select(enquo(from), x)
  to_id <- tidyselect::eval_select(enquo(to), x)
  weight_by_id <- tidyselect::eval_select(enquo(weight_by), x)

  list_x <- list(
    .from = x[from_id],
    .to = x[to_id],
    .weight_by = x[weight_by_id]
  )

  ncol_fail <- (lapply(list_x, ncol) != 1)
  if (any(ncol_fail)) {
    msg <- c(
      x = "You can only select one column each for
                {.arg {names(list_x)}}",
      i = "You've selected more than one column for:
                {.arg {names(list_x)[ncol_fail]}}"
    )
    cli::cli_abort(c(msg))
  }

  xmap_tbl(
    .from = list_x$.from,
    .to = list_x$.to,
    .weight_by = list_x$.weight_by,
    tol = tol
  )
}

#' @details
#' `diagnose_as_xmap_tbl()` checks whether `x`'s links form a valid
#' crossmap — the same conditions [validate_as_xmap()] checks, though
#' currently implemented independently rather than by calling it — and
#' returns detail on any offending rows, to help resolve the specific
#' issue rather than just knowing something's wrong. The returned
#' `xmap_diagnosis`'s `details` has one entry per condition ('NULL' where
#' that check passed):
#'
#' - `bad_dups`: rows sharing a `.from`-`.to` pair with another row
#' - `miss_from`, `miss_to`, `miss_weight_by`: rows with a missing
#'   `.from`, `.to`, or `.weight_by` value, respectively
#' - `bad_froms`: for each `.from` whose outgoing weights don't sum to
#'   (near enough) one, that `.from` and its actual weight sum
#' @export
#' @rdname as_xmap_tbl
#' @return `diagnose_as_xmap_tbl()` returns an `xmap_diagnosis` object: a
#' list with `valid` (a scalar logical) and `details` (a named list of
#' tibbles of offending rows, one per check, `NULL` where that check
#' passed). Printing the result shows a readable pass/fail report; see
#' [new_xmap_diagnosis()].
diagnose_as_xmap_tbl <- function(
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

  flags <- list()
  details <- list(
    bad_dups = NULL,
    miss_from = NULL,
    miss_to = NULL,
    miss_weight_by = NULL,
    bad_froms = NULL
  )

  ## boolean flags share their logic with validate_as_xmap()'s data.frame
  ## method and xmap_tbl()'s construction gate via the vhas_*() helpers
  ## (R/vhas.R); only the offending-rows detail-building below is unique
  ## to diagnose_as_xmap_tbl() -- see #19
  flags$dup_pairs <- !vhas_no_dup_pairs(tbl_x$.from, tbl_x$.to)
  if (flags$dup_pairs) {
    details$bad_dups <- tbl_x |>
      dplyr::group_by(.data$.from, .data$.to) |>
      dplyr::summarise(.dup = dplyr::n(), .groups = "drop") |>
      dplyr::filter(.data$.dup != 1)
  }

  flags$miss_from <- !vhas_no_missing(tbl_x$.from)
  if (flags$miss_from) {
    details$miss_from <- tbl_x |>
      dplyr::filter(is.na(.data$.from[[1]]))
  }

  flags$miss_to <- !vhas_no_missing(tbl_x$.to)
  if (flags$miss_to) {
    details$miss_to <- tbl_x |>
      dplyr::filter(is.na(.data$.to[[1]]))
  }

  flags$miss_weight_by <- !vhas_no_missing(tbl_x$.weight_by)
  if (flags$miss_weight_by) {
    details$miss_weight_by <- tbl_x |>
      dplyr::filter(is.na(.data$.weight_by[[1]]))
  }

  flags$bad_froms <- !vhas_valid_weights(tbl_x$.from[[1]], tbl_x$.weight_by[[1]], tol = tol)
  if (flags$bad_froms) {
    details$bad_froms <- tbl_x |>
      dplyr::group_by(.data$.from) |>
      dplyr::summarise(.sum.weight_by = sum(.data$.weight_by), .groups = "drop") |>
      dplyr::mutate(.near = dplyr::near(.data$.sum.weight_by, 1L, tol = tol)) |>
      dplyr::filter(!.data$.near) |>
      dplyr::select(!dplyr::all_of(".near"))
  }

  valid <- !any(simplify2array(flags))

  new_xmap_diagnosis(
    valid, details,
    labels = list(
      bad_dups = c(
        pass = "No duplicate `.from`-`.to` pairs",
        fail = "Duplicate `.from`-`.to` pairs"
      ),
      miss_from = c(
        pass = "No missing values in `.from`",
        fail = "Missing values in `.from`"
      ),
      miss_to = c(
        pass = "No missing values in `.to`",
        fail = "Missing values in `.to`"
      ),
      miss_weight_by = c(
        pass = "No missing values in `.weight_by`",
        fail = "Missing values in `.weight_by`"
      ),
      bad_froms = c(
        pass = "Sum of `.weight_by` by `.from` are near enough to one",
        fail = "Sum of `.weight_by` by `.from` are not near enough to one"
      )
    ),
    class = "xmap_diagnosis_tbl"
  )
}

## metadata helpers (DO NOT EXPORT)
get_name_from <- function(x) names(x$.from)
get_name_to <- function(x) names(x$.to)
get_name_weight_by <- function(x) names(x$.weight_by)
get_from_set <- function(x) vec_unique(x$.from)
get_to_set <- function(x) vec_unique(x$.to)

## printing ---------------------------------------------

#' @export
tbl_sum.xmap_tbl <- function(x, ...) {
  default_header <- NextMethod()
  names(default_header) <- "A crossmap tibble"
  n_from_set <- vec_unique_count(x$.from)
  n_to_set <- vec_unique_count(x$.to)
  extra_info <- c("with unique keys" = sprintf(
    "[%s] %s -> [%s] %s",
    n_from_set, names(x$.from),
    n_to_set, names(x$.to)
  ))
  c(default_header, extra_info)
}

#' @export
tbl_format_footer.xmap_tbl <- function(x, setup, ...) {
  # TODO: modify footer to print total number of links
  # n_links <- vec_size(x)
  default_footer <- NextMethod()
  modified_footer <- gsub("rows", "links", default_footer)
  c(modified_footer)
}
