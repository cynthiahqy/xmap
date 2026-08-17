# Changelog

## xmap (development version)

- Add overview of diagnostic validation functions to README & Getting
  Started Vignette
- Add
  [`validate_as_xmap()`](https://cynthiahqy.github.io/xmap/reference/validate_as_xmap.md),
  a generic with `data.frame` and `matrix` methods, for cheaply checking
  whether links (or a matrix) form a valid crossmap without building a
  detail object
- [`diagnose_as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md)
  now always returns a single `xmap_diagnosis` object
  (`$valid`/`$details`), replacing the previous inconsistent
  `TRUE`/`FALSE`/`invisible(x)`/bare
  [`list()`](https://rdrr.io/r/base/list.html) return contract; printing
  the result shows a readable pass/fail report
- `xmap_tbl()`,
  [`as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md),
  [`diagnose_as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md),
  and
  [`validate_as_xmap()`](https://cynthiahqy.github.io/xmap/reference/validate_as_xmap.md)
  now also check for missing `.from`/`.to` values (previously only
  `.weight_by` was checked)
- Add “Extracting Crossmaps from Existing Scripts” vignette, with
  `timor_occupn` and `indstat` (a list of
  `masked_sample`/`country_lookup`) datasets
- `xmap_tbl()`,
  [`diagnose_as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md),
  and
  [`validate_as_xmap()`](https://cynthiahqy.github.io/xmap/reference/validate_as_xmap.md)
  now share a single implementation of the three link-validity checks
  (previously each independently reimplemented the same logic); as part
  of this, a `.from` whose outgoing weights sum to zero is now correctly
  treated as invalid everywhere, matching
  [`validate_as_xmap.matrix()`](https://cynthiahqy.github.io/xmap/reference/validate_as_xmap.md)’s
  existing all-zero-row check.
  `xmap_tbl()`/[`as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md)
  now abort with a single `abort_invalid_xmap` condition instead of four
  separate condition classes, pointing users at
  [`diagnose_as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md)
  for detail
- [`validate_as_xmap()`](https://cynthiahqy.github.io/xmap/reference/validate_as_xmap.md)’s
  `tol` argument is now visible on the generic itself, not just its
  methods
- Internal validity-check helpers shared by
  `xmap_tbl()`/[`diagnose_as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md)/[`validate_as_xmap()`](https://cynthiahqy.github.io/xmap/reference/validate_as_xmap.md)
  now require `tol` to be passed explicitly (no internal default), so a
  future change that forgets to forward a user-supplied `tol` fails
  loudly instead of silently falling back to an undocumented default –
  no user-facing behavior change
- Add
  [`as_xmap_tbl.matrix()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md),
  a matrix constructor method for
  [`as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md)
  – an adjacency matrix can now be coerced directly into an `xmap_tbl`,
  without first reshaping it to long format by hand. It validates
  matrix-natively (via
  [`validate_as_xmap()`](https://cynthiahqy.github.io/xmap/reference/validate_as_xmap.md))
  before reshaping, so an all-zero row is correctly rejected rather than
  silently dropped. Resulting column names default to
  `names(dimnames(x))` when set, falling back to
  `"rowname"`/`"colname"`/`"cell"`, and can be overridden via the new
  `from`/`to`/`weight_by` string arguments
- Add
  [`compose_xmap()`](https://cynthiahqy.github.io/xmap/reference/compose_xmap.md),
  chaining two crossmaps sharing an intermediate classification
  (`S -> M`, `M -> T`) directly into one (`S -> T`), without
  materialising intermediate `M`-level values. Validates both inputs are
  actually valid crossmaps (not just correctly classed) and requires
  `xmap1`’s `.to` to be fully covered by `xmap2`’s `.from`, aborting
  rather than silently dropping mass; chain more than two crossmaps with
  `Reduce(compose_xmap, list(...))`

## xmap 0.1.0

CRAN release: 2025-01-31

- Initial CRAN submission.

The `xmap` package implements the Crossmaps framework for transforming
numeric data between statistical classifications.

Crossmap tibbles `xmap_tbl` encode instructions for transforming data
associated with source classification codes to data indexed by target
classification categories (e.g. agriculture (20%) -\> fisheries) as
`source`, `target` and `weight_by` links. There are two primary
functions:

1.  Creating crossmap tibbles from dataframes of links, and verifying
    they are valid transformations:

`as_xmap_tbl(from = "source_classification", to = "target_codes" , weight_by = "distribution_shares)`

2.  Using validated crossmaps to transform data:

`apply_xmap(.data = source_data, .xmap = source2target_crossmap, values_from = counts)`

This initial release also provides diagnostic functions to help debug
any verification errors:
[`diagnose_as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md)
and
[`diagnose_apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)
