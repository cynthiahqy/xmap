# Apply Crossmap Transformation to Conformable Data

This function applies crossmap transformation to a dataset, transforming
data based on specified mapping rules.

## Usage

``` r
apply_xmap(.data, .xmap, values_from, keys_from = names(.xmap$.from), ...)

diagnose_apply_xmap(
  .data,
  .xmap,
  values_from,
  keys_from = names(.xmap$.from),
  ...
)
```

## Arguments

- .data:

  The dataset to transform.

- .xmap:

  An `xmap_tbl` object.

- values_from:

  A `tidyselect` expression of columns in `.data` with values to
  transform

- keys_from:

  A `tidyselect` expression specifies the column in `.data` to match
  with `.xmap$from`

- ...:

  (reserved)

## Value

A tibble with transformed data.

`diagnose_apply_xmap()` returns an `xmap_diagnosis` object: a list with
`valid` (a scalar logical) and `details` (a named list of tibbles of
offending rows, one per check, `NULL` where that check passed). Printing
the result shows a readable pass/fail report; see
[`new_xmap_diagnosis()`](https://cynthiahqy.github.io/xmap/reference/new_xmap_diagnosis.md).

## Details

`diagnose_apply_xmap()` checks whether `.data` is conformable with
`.xmap` – the same two conditions `apply_xmap()` checks – and returns
detail on any offending rows, to help resolve the specific issue rather
than just knowing something's wrong. The returned `xmap_diagnosis`'s
`details` has one entry per condition (`NULL` where that check passed):

- `not_covered`: rows of `.data` whose `keys_from` key has no matching
  link in `.xmap$.from`

- `missing_values`: rows of `.data` with a missing value in one or more
  `values_from` columns

## Functions

- `diagnose_apply_xmap()`: Returns an `xmap_diagnosis` object diagnosing
  why `.data` fails `apply_xmap()`'s conformability checks.

## Examples

``` r
abc_xmap <- demo$abc_links |>
  as_xmap_tbl(from = "lower", to = "upper", weight_by = "share")
abc_data <- tibble::tibble(
  lower = unique(demo$abc_links$lower),
  count = runif(length(unique(demo$abc_links$lower)), min = 100, max = 500)
)
apply_xmap(
  .data = abc_data,
  .xmap = abc_xmap,
  values_from = count
)
#> Matching keys in `.data$lower` with `.xmap$.from$lower`
#> ℹ To silence, set `keys_from = lower`
#> # A tibble: 5 × 2
#>   upper count
#>   <chr> <dbl>
#> 1 AA    132. 
#> 2 BB    774. 
#> 3 CC     48.9
#> 4 DD     97.7
#> 5 EE     16.3
```
