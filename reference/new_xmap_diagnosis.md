# Construct an `xmap_diagnosis` object

`xmap_diagnosis` is the shared return contract for
`diagnose_as_xmap_*()` functions across crossmap representations (data
frame, matrix, graph, ...): a single object shape regardless of whether
the diagnosis passed or failed, so callers can inspect `$valid`
programmatically, or print the object for a human-readable report of
what needs fixing.

## Usage

``` r
new_xmap_diagnosis(valid, details, labels, class = character())
```

## Arguments

- valid:

  A single logical: did every check pass?

- details:

  A named list of tibbles (or `NULL`), one entry per check. `NULL` means
  that check passed; a tibble holds the offending locations.

- labels:

  A named list, one entry per check (same names as `details`), each
  entry a length-2 character vector with elements `pass` and `fail` —
  the text to show when that check passed or failed, respectively. A
  single check reads differently depending on outcome (e.g. "No
  duplicate pairs" vs. "Duplicate pairs found").

- class:

  Additional subclass(es) to prepend, e.g. `"xmap_diagnosis_tbl"`, for
  representation-specific methods beyond printing.

## Value

An `xmap_diagnosis` object.

## Details

The set of checks, their labels, and what counts as an "offending
location" (rows, matrix cells, graph edges, ...) is specific to each
representation. Callers of `new_xmap_diagnosis()` supply `labels` for
their own checks, and are responsible for normalizing each check's
offending locations into a tibble before passing them in `details` —
`print.xmap_diagnosis()` stays representation-agnostic by only ever
printing tibbles.
