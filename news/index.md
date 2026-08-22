# Changelog

## xmap (development version)

- [`diagnose_apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)
  now returns an `xmap_diagnosis` object (`$valid`/`$details`, with a
  `not_covered`/`missing_values` offending-rows tibble per condition),
  replacing the previous side-effecting `cli_inform()` messages plus raw
  [`list()`](https://rdrr.io/r/base/list.html)/`invisible(.data)` return
  – matching
  [`diagnose_as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md)’s
  return contract.
  [`apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)’s
  `coverage_error`/`missing_mass_values` aborts now use the same `cli`
  markup (`{.arg}`/`{.val}`) as `xmap_tbl()`’s abort, and point at
  [`diagnose_apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)
  for detail, matching `xmap_tbl()`’s pointer to
  [`diagnose_as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md).
  [`new_xmap_diagnosis()`](https://cynthiahqy.github.io/xmap/reference/new_xmap_diagnosis.md)
  gained `msg_valid`/`msg_invalid` parameters (no default) so each
  caller states its own headline message rather than one hardcoded “xmap
  is valid/invalid” wording that only fit
  [`diagnose_as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md)’s
  use case

- Add
  [`validate_apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/validate_apply_xmap.md),
  a cheap `TRUE`/`FALSE` check of whether `.data` is conformable with an
  `xmap_tbl` (the same two conditions
  [`apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)
  requires – key coverage and no missing values – without
  [`diagnose_apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)’s
  detail-building), mirroring
  [`validate_as_xmap()`](https://cynthiahqy.github.io/xmap/reference/validate_as_xmap.md)’s
  role alongside
  `xmap_tbl()`/[`diagnose_as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md).
  [`apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)’s
  abort gate and
  [`diagnose_apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)’s
  flags now both call a single internal
  [`check_conformable_xmap_data()`](https://cynthiahqy.github.io/xmap/reference/check_conformable_xmap_data.md)
  helper for these two conditions, instead of each independently
  reimplementing them (same dedup shape as
  [\#19](https://github.com/cynthiahqy/xmap/issues19)’s
  [`check_valid_xmap_df()`](https://cynthiahqy.github.io/xmap/reference/check_valid_xmap_df.md)).
  Resolves [\#45](https://github.com/cynthiahqy/xmap/issues/45)

- Dev version bumped to `0.1.0.9004`; `forcats`, `RColorBrewer`, and
  `scales` added to `Suggests` (used by
  [`vignette("examine-compose-crossmaps")`](https://cynthiahqy.github.io/xmap/articles/examine-compose-crossmaps.md)
  but previously undeclared – `devtools::check()` now passes with 0
  errors where it previously failed to build that vignette in a clean
  check environment)

- [`compose_xmap()`](https://cynthiahqy.github.io/xmap/reference/compose_xmap.md)’s
  docs and
  [`vignette("examine-compose-crossmaps")`](https://cynthiahqy.github.io/xmap/articles/examine-compose-crossmaps.md)’s
  two-step comparison now note a known limitation: composed weights are
  sums of products of the input weights, which amplifies floating-point
  drift relative to either input alone, so two individually-`tol`-valid
  crossmaps can compose into a result that fails that same `tol`
  (widening `tol` on the call is the practical workaround). The vignette
  ties this directly to its own `max_abs_diff` output (`~1e-13`, not
  exactly `0`) as a concrete example of the drift. No behaviour change.

- `data-raw/indstat.R`’s `country` -\> `country_iso3c`/`country_name`
  join now errors on any `masked_sample$country` (UN M49 code) missing
  from `country_lookup`, instead of silently shipping `NA` ISO alpha-3
  labels; a regression test in `test-indstat.R` guards the shipped
  `indstat` object against the same failure mode

- [`vignette("examine-compose-crossmaps")`](https://cynthiahqy.github.io/xmap/articles/examine-compose-crossmaps.md)’s
  “Applying the collapsed transformation” section now actually runs –
  [`apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)
  only accepts a single `xmap_tbl`, so applying the per-`country`/`year`
  `isiccomb -> isic3` crossmap collection needs the same nest-then-map
  pattern already used later in the vignette (resolves the section’s
  `TODO: implement nested apply_xmap`). The two-step (`split_isiccomb()`
  then hand-aggregate to `isic3`) comparison used to validate this
  result now sits immediately after it; no change to the reported
  numbers

- `vignette("examine-crossmaps")` renamed to
  [`vignette("examine-compose-crossmaps")`](https://cynthiahqy.github.io/xmap/articles/examine-compose-crossmaps.md)
  (`examine-crossmaps.Rmd` -\> `examine-compose-crossmaps.Rmd`, title
  “Examining Collections of Crossmaps” -\> “Composing Crossmap
  Sequences”) to match its actual scope now that the
  reconvergent/crossing splits section has moved out (see below) – the
  vignette is about composing a chain of crossmaps, not general
  “examining collections”

- [`vignette("examine-compose-crossmaps")`](https://cynthiahqy.github.io/xmap/articles/examine-compose-crossmaps.md)’s
  “Reconvergent vs. crossing splits” section (the `crossing_by_country`
  summary table) has been pulled out pending a proper visualisation of
  the distinction, rather than merging a bare percentage table – tracked
  in [\#40](https://github.com/cynthiahqy/xmap/issues/40), which points
  at `crossmap-explorer`’s existing node-link diagrams as prior art

- [`vignette("examine-compose-crossmaps")`](https://cynthiahqy.github.io/xmap/articles/examine-compose-crossmaps.md)’s
  weight-type distribution and heatmap sections now derive `weight_type`
  from the validated `xmap_tbl` object built per `country`/`year` (kept
  from the `validate-crossmaps` chunk as `crossmaps$xmap`), rather than
  re-deriving it from the raw `split_links` tibble – resolves the
  section’s own TODO about examining the crossmap object directly. No
  change to the reported numbers.

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

- Add “Extracting Crossmaps from Existing Scripts” vignette
  (`extract-validate-existing.Rmd`), with `timor_occupn` and `indstat`
  (a list of `masked_sample`/`country_lookup`) datasets. Scoped to
  extraction and validation only; the collection-level analysis
  (weight-type distribution and `isiccomb` split overview plots) and the
  [`compose_xmap()`](https://cynthiahqy.github.io/xmap/reference/compose_xmap.md)/`isic3`
  reconvergence example move to the “Examining Collections of Crossmaps”
  vignette ([\#32](https://github.com/cynthiahqy/xmap/issues32))

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

- `indstat` covers 8 reporters (BRA, CHN, COL, DEU, JPN, ROU, USA, YEM;
  17,365 rows, years 1990-2013), regenerated from the upstream
  `xmap-example` export. Colombia, Romania and Yemen were chosen for
  structurally distinct splitting behaviour once their
  `isiccomb -> isic` splits are composed with the deterministic
  `isic -> isic.3` aggregation: splits that **reconverge** into a single
  3-digit parent (imputed at 4 digits, exact at 3 – Colombia, 0%
  crossing) versus those that **cross** 3-digit boundaries (real
  propagated allocation uncertainty – Yemen, 95% crossing); this
  composition is demonstrated in the “Examining Collections of
  Crossmaps” vignette
  ([\#32](https://github.com/cynthiahqy/xmap/issues32)).
  `indstat$country_lookup` is read from
  `data-raw/indstat-country-lookup.csv` rather than hardcoded in
  `data-raw/indstat.R`

- Add
  [`compose_xmap()`](https://cynthiahqy.github.io/xmap/reference/compose_xmap.md),
  chaining two crossmaps sharing an intermediate classification
  (`S -> M`, `M -> T`) directly into one (`S -> T`), without
  materialising intermediate `M`-level values. Validates both inputs are
  actually valid crossmaps (not just correctly classed) and requires
  `xmap1`’s `.to` to be fully covered by `xmap2`’s `.from`, aborting
  rather than silently dropping mass; chain more than two crossmaps with
  `Reduce(compose_xmap, list(...))`

- Add “Examining Collections of Crossmaps” vignette
  (`examine-crossmaps.Rmd`), building on
  `extract-validate-existing.Rmd`’s Case 2 `split_links`: grouped
  weight-distribution diagnostics across every `country`/`year`, and
  [`compose_xmap()`](https://cynthiahqy.github.io/xmap/reference/compose_xmap.md)
  applied per group (`isiccomb -> isic -> isic3`) to reach a coarser
  classification directly, including the reconvergent-vs-crossing split
  distinction called out in `indstat`’s docs
  ([\#32](https://github.com/cynthiahqy/xmap/issues32))

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
