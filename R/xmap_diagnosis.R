## constructor -----------------------------------------------------------

#' Construct an `xmap_diagnosis` object
#'
#' `xmap_diagnosis` is the return contract for `diagnose_as_xmap_tbl()`
#' (and, eventually, other `diagnose_*()` functions): a single object shape
#' regardless of whether the diagnosis passed or failed, so callers can
#' inspect `$valid` programmatically, or print the object for a
#' human-readable report of what needs fixing.
#'
#' @param valid A single logical: did every check pass?
#' @param details A named list of tibbles (or `NULL`), one entry per check.
#' `NULL` means that check passed; a tibble holds the offending rows.
#' @return An `xmap_diagnosis` object.
#' @keywords internal
new_xmap_diagnosis <- function(valid, details) {
  stopifnot(
    is_bool(valid),
    is.list(details),
    !is.null(names(details))
  )
  structure(
    list(valid = valid, details = details),
    class = "xmap_diagnosis"
  )
}

## printing ----------------------------------------------------------------

xmap_diagnosis_labels <- c(
  bad_dups = "No duplicate `.from`-`.to` pairs",
  miss_weight_by = "No missing values in `.weight_by`",
  bad_froms = "Sum of `.weight_by` by `.from` are near enough to one"
)

#' @export
print.xmap_diagnosis <- function(x, ...) {
  cli::cli_div(theme = list(span.field = list(color = "blue")))
  if (x$valid) {
    cli::cli_alert_success("{.field xmap} is valid")
  } else {
    cli::cli_alert_danger("{.field xmap} is invalid")
  }

  for (check in names(xmap_diagnosis_labels)) {
    detail <- x$details[[check]]
    label <- xmap_diagnosis_labels[[check]]
    if (is.null(detail)) {
      cli::cli_bullets(c("v" = label))
    } else {
      cli::cli_bullets(c("x" = "{label} ({nrow(detail)} row{?s})"))
      print(detail)
    }
  }

  invisible(x)
}
