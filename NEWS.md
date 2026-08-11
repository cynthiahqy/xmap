# xmap (development version)

* Add overview of diagnostic validation functions to README & Getting Started Vignette
* (draft, PR #4) Add "Extracting Crossmaps from Existing Scripts" vignette, with `occupn_sample`, `indstat_masked`, and `indstat_country_lookup` datasets; add `quiet`/`verbose` options to `diagnose_as_xmap_tbl()`

# xmap 0.1.0

* Initial CRAN submission.

The `xmap` package implements the Crossmaps framework for transforming numeric data between statistical classifications.

Crossmap tibbles `xmap_tbl` encode instructions for transforming data associated with source classification codes to data indexed by target classification categories (e.g. agriculture (20%) -> fisheries) as `source`, `target` and `weight_by` links. There are two primary functions:

1. Creating crossmap tibbles from dataframes of links, and verifying they are valid transformations: 

`as_xmap_tbl(from = "source_classification", to = "target_codes" , weight_by = "distribution_shares)`

2. Using validated crossmaps to transform data:

`apply_xmap(.data = source_data, .xmap = source2target_crossmap, values_from = counts)`

This initial release also provides diagnostic functions to help debug any verification errors: `diagnose_as_xmap_tbl()` and `diagnose_apply_xmap()`
