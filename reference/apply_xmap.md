# Apply Crossmap Transformation to Conformable Data

This function applies crossmap transformation to a dataset, transforming
data based on specified mapping rules.

## Usage

``` r
apply_xmap(.data, .xmap, values_from, keys_from = names(.xmap$.from), ...)

diagnose_apply_xmap(.data, .xmap, values_from, keys_from = NULL, ...)
```

## Arguments

- .data:

  The dataset to transform.

- .xmap:

  An \`xmap_tbl\` object.

- values_from:

  A \`tidyselect\` expression of columns in \`.data\` with values to
  transform

- keys_from:

  A \`tidyselect\` expression specifies the column in \`.data\` to match
  with \`.xmap\$from\`

- ...:

  (reserved)

## Value

A tibble with transformed data.

## Functions

- `diagnose_apply_xmap()`: Returns messages for any diagnosed issues.

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
