# Coercing data frames of links to crossmap tibbles

This method takes a data.frame-like object and converts it into an
\`xmap_tbl\` based on specified columns for 'from', 'to', and 'weight'.

## Usage

``` r
as_xmap_tbl(x, ...)

# S3 method for class 'data.frame'
as_xmap_tbl(x, from, to, weight_by, ..., tol = .Machine$double.eps^0.5)

diagnose_as_xmap_tbl(
  x,
  from,
  to,
  weight_by,
  ...,
  tol = .Machine$double.eps^0.5
)
```

## Arguments

- x:

  A data.frame or tibble to be converted in a crossmap tibble.

- ...:

  (reserved) Additional arguments passed to methods.

- from:

  The column in \`x\` that specifies the 'from' nodes.

- to:

  The column in \`x\` that specifies the 'to' nodes.

- weight_by:

  The column in \`x\` that specifies the weight of the links.

- tol:

  Tolerance of comparison.

## Value

Returns an xmap tibble object.

## Examples

``` r
demo$abc_links |>
  as_xmap_tbl(from = lower, to = upper, weight_by = share)
#> # A crossmap tibble: 6 × 3
#> # with unique keys:  [4] lower -> [5] upper
#>   .from$lower .to$upper .weight_by$share
#>   <chr>       <chr>                <dbl>
#> 1 a           AA                     1  
#> 2 b           BB                     1  
#> 3 c           BB                     1  
#> 4 d           CC                     0.3
#> 5 d           DD                     0.6
#> 6 d           EE                     0.1
```
