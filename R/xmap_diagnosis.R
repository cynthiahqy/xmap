## constructor -----------------------------------------------------------

#' Construct an `xmap_diagnosis` object
#'
#' `xmap_diagnosis` is the shared return contract for `diagnose_as_xmap_*()`
#' functions across crossmap representations (data frame, matrix, graph,
#' ...): a single object shape regardless of whether the diagnosis passed
#' or failed, so callers can inspect `$valid` programmatically, or print
#' the object for a human-readable report of what needs fixing.
#'
#' The set of checks, their labels, and what counts as an "offending
#' location" (rows, matrix cells, graph edges, ...) is specific to each
#' representation. Callers of `new_xmap_diagnosis()` supply `labels` for
#' their own checks, and are responsible for normalizing each check's
#' offending locations into a tibble before passing them in `details` —
#' `print.xmap_diagnosis()` stays representation-agnostic by only ever
#' printing tibbles.
#'
#' @param valid A single logical: did every check pass?
#' @param details A named list of tibbles (or `NULL`), one entry per check.
#' `NULL` means that check passed; a tibble holds the offending locations.
#' @param labels A named list, one entry per check (same names as
#' `details`), each entry a length-2 character vector with elements
#' `pass` and `fail` — the text to show when that check passed or
#' failed, respectively. A single check reads differently depending on
#' outcome (e.g. "No duplicate pairs" vs. "Duplicate pairs found").
#' @param class Additional subclass(es) to prepend, e.g. `"xmap_diagnosis_tbl"`,
#' for representation-specific methods beyond printing.
#' @return An `xmap_diagnosis` object.
#' @keywords internal
new_xmap_diagnosis <- function(valid, details, labels, class = character()) {
  stopifnot(
    is_bool(valid),
    is.list(details),
    !is.null(names(details)),
    is.list(labels),
    !is.null(names(labels)),
    identical(names(details), names(labels)),
    all(vapply(
      labels,
      \(l) is.character(l) && identical(names(l), c("pass", "fail")),
      logical(1)
    ))
  )
  structure(
    list(valid = valid, details = details, labels = labels),
    class = c(class, "xmap_diagnosis")
  )
}

## printing ----------------------------------------------------------------

#' @export
print.xmap_diagnosis <- function(x, ...) {
  cli::cli_div(theme = list(span.field = list(color = "blue")))
  if (x$valid) {
    cli::cli_alert_success("{.field xmap} is valid")
  } else {
    cli::cli_alert_danger("{.field xmap} is invalid")
  }

  for (check in names(x$labels)) {
    detail <- x$details[[check]]
    label <- x$labels[[check]]
    if (is.null(detail)) {
      cli::cli_bullets(c("v" = label[["pass"]]))
    } else {
      cli::cli_bullets(c("x" = "{label[['fail']]} ({nrow(detail)} row{?s})"))
      print(detail)
    }
  }

  invisible(x)
}
