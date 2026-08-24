# xmap 0.2.0

## New features

* `as_xmap_tbl()` gained a `matrix` method, so an adjacency matrix can be coerced directly into an `xmap_tbl` without first reshaping it to long format by hand. Column names default to `names(dimnames(x))`, falling back to `"rowname"`/`"colname"`/`"cell"`, and can be overridden via the new `from`/`to`/`weight_by` arguments (#32).
* `validate_as_xmap()` is a new generic (with `data.frame` and `matrix` methods) for cheaply checking whether links form a valid crossmap, without building a full diagnosis object.
* `validate_apply_xmap()` is the `apply_xmap()` equivalent of `validate_as_xmap()`: a cheap `TRUE`/`FALSE` check of whether data is conformable with an `xmap_tbl` (#45).
* `compose_xmap()` chains two crossmaps sharing an intermediate classification (`S -> M`, `M -> T`) directly into one (`S -> T`), without materialising intermediate values. Both inputs must already be valid crossmaps, and `xmap1`'s `.to` must be fully covered by `xmap2`'s `.from` (it aborts rather than silently dropping mass). Chain more than two crossmaps with `Reduce(compose_xmap, list(...))` (#32).
* New datasets:
  * `indstat` now covers 8 reporters (BRA, CHN, COL, DEU, JPN, ROU, USA, YEM; 17,365 rows, 1990-2013), chosen to illustrate both reconvergent splits (Colombia, imputed at 4 digits/exact at 3) and cross-boundary splits (Yemen, 95% crossing) once composed with `compose_xmap()` -- see `vignette("examine-compose-crossmaps")`.
  * `timor_occupn` is a ~1% sample (11,775 rows) of individual-level records from the Timor-Leste Population and Housing Census 2015, with original occupation codes (161 distinct values, ~67% missing) -- used in `vignette("extract-validate-existing")` to demonstrate recovering an implicit occupation-recoding script as an explicit crossmap.
* New vignettes:
  - `vignette("extract-validate-existing")`, on extracting and validating crossmaps from existing scripts, introducing the `timor_occupn` and `indstat` datasets.
  - `vignette("examine-compose-crossmaps")`, building on that to demonstrate grouped diagnostics and `compose_xmap()` across country/year groups (#32).
  - `vignette("applying-crossmaps")`, covering `apply_xmap()`'s coverage and missing-value guards (#43).

## Breaking changes

* `xmap_tbl()`, `as_xmap_tbl()`, `diagnose_as_xmap_tbl()`, and `validate_as_xmap()` now reject an individual `.weight_by` that is zero or negative, not just a `.from` whose weights fail to sum to one -- matching the crossmap definition's weight codomain of `(0, 1]` (#49).
* `xmap_tbl()`/`as_xmap_tbl()` now also check for missing `.from`/`.to` values (previously only `.weight_by` was checked), and abort with a single `abort_invalid_xmap` condition instead of four separate condition classes.
* `diagnose_as_xmap_tbl()` and `diagnose_apply_xmap()` now always return a single `xmap_diagnosis` object (`$valid`/`$details`), replacing inconsistent `TRUE`/`FALSE`/`invisible()`/side-effecting-message return contracts. Printing a diagnosis shows a readable pass/fail report.

## Minor improvements

* Documentation no longer points at bare GitHub issue numbers (e.g. `See #34.` in `?indstat`), which don't resolve for anyone reading the help pages on CRAN or pkgdown (#56).
* `compose_xmap()`'s docs note a known limitation: composed weights amplify floating-point drift relative to either input crossmap, so two individually-`tol`-valid crossmaps can compose into a result that fails that same `tol` (widen `tol` on the call if this happens).
* `validate_as_xmap()`'s `tol` argument is now visible on the generic itself, not just its methods.

# xmap 0.1.0

* Initial CRAN submission.

The `xmap` package implements the Crossmaps framework for transforming numeric data between statistical classifications.

Crossmap tibbles `xmap_tbl` encode instructions for transforming data associated with source classification codes to data indexed by target classification categories (e.g. agriculture (20%) -> fisheries) as `source`, `target` and `weight_by` links. There are two primary functions:

1. Creating crossmap tibbles from dataframes of links, and verifying they are valid transformations: 

`as_xmap_tbl(from = "source_classification", to = "target_codes" , weight_by = "distribution_shares)`

2. Using validated crossmaps to transform data:

`apply_xmap(.data = source_data, .xmap = source2target_crossmap, values_from = counts)`

This initial release also provides diagnostic functions to help debug any verification errors: `diagnose_as_xmap_tbl()` and `diagnose_apply_xmap()`
