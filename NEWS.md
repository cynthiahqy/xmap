# xmap (development version)

* Add overview of diagnostic validation functions to README & Getting Started Vignette
* Add `validate_as_xmap()`, a generic with `data.frame` and `matrix` methods, for cheaply checking whether links (or a matrix) form a valid crossmap without building a detail object
* `diagnose_as_xmap_tbl()` now always returns a single `xmap_diagnosis` object (`$valid`/`$details`), replacing the previous inconsistent `TRUE`/`FALSE`/`invisible(x)`/bare `list()` return contract; printing the result shows a readable pass/fail report
* `xmap_tbl()`, `as_xmap_tbl()`, `diagnose_as_xmap_tbl()`, and `validate_as_xmap()` now also check for missing `.from`/`.to` values (previously only `.weight_by` was checked)
* Add "Extracting Crossmaps from Existing Scripts" vignette, with `occupn_sample`, `indstat_masked`, and `indstat_country_lookup` datasets
* `xmap_tbl()`, `diagnose_as_xmap_tbl()`, and `validate_as_xmap()` now share a single implementation of the three link-validity checks (previously each independently reimplemented the same logic); as part of this, a `.from` whose outgoing weights sum to zero is now correctly treated as invalid everywhere, matching `validate_as_xmap.matrix()`'s existing all-zero-row check. `xmap_tbl()`/`as_xmap_tbl()` now abort with a single `abort_invalid_xmap` condition instead of four separate condition classes, pointing users at `diagnose_as_xmap_tbl()` for detail
* `validate_as_xmap()`'s `tol` argument is now visible on the generic itself, not just its methods
* Internal validity-check helpers shared by `xmap_tbl()`/`diagnose_as_xmap_tbl()`/`validate_as_xmap()` now require `tol` to be passed explicitly (no internal default), so a future change that forgets to forward a user-supplied `tol` fails loudly instead of silently falling back to an undocumented default -- no user-facing behavior change

# xmap 0.1.0

* Initial CRAN submission.

The `xmap` package implements the Crossmaps framework for transforming numeric data between statistical classifications.

Crossmap tibbles `xmap_tbl` encode instructions for transforming data associated with source classification codes to data indexed by target classification categories (e.g. agriculture (20%) -> fisheries) as `source`, `target` and `weight_by` links. There are two primary functions:

1. Creating crossmap tibbles from dataframes of links, and verifying they are valid transformations: 

`as_xmap_tbl(from = "source_classification", to = "target_codes" , weight_by = "distribution_shares)`

2. Using validated crossmaps to transform data:

`apply_xmap(.data = source_data, .xmap = source2target_crossmap, values_from = counts)`

This initial release also provides diagnostic functions to help debug any verification errors: `diagnose_as_xmap_tbl()` and `diagnose_apply_xmap()`
