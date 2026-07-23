# Handling Crossmap Tibbles

``` r

library(xmap)
library(dplyr)
```

## Nesting and Invalidation of Crossmap Tibbles

In most cases, crossmap tibbles (`xmap_tbl`) should behave just like
regular data frames or tibbles. However, we make use of nesting to
retain meaningful variable names whilst also attaching `.from`, `.to`
and `.weights` roles. Each column in an `xmap_tbl` actually contains a
one-column tibble:

``` r

abc_xmap <- demo$abc_links |>
  as_xmap_tbl(lower, upper, share)
str(abc_xmap)
#> xmap_tbl [6 × 3] (S3: xmap_tbl/xmap/tbl_df/tbl/data.frame)
#>  $ .from     : tibble [6 × 1] (S3: tbl_df/tbl/data.frame)
#>   ..$ lower: chr [1:6] "a" "b" "c" "d" ...
#>  $ .to       : tibble [6 × 1] (S3: tbl_df/tbl/data.frame)
#>   ..$ upper: chr [1:6] "AA" "BB" "BB" "CC" ...
#>  $ .weight_by: tibble [6 × 1] (S3: tbl_df/tbl/data.frame)
#>   ..$ share: num [1:6] 1 1 1 0.3 0.6 0.1
#>  - attr(*, "tol")= num 1.49e-08
```

This nested structure can lead to unexpected behaviour when manipulating
the `xmap_tbl` with standard `dplyr` verbs. This is somewhat intentional
as subsetting can (silently) invalidate a crossmap (especially weights
see `d -> CC` below):

``` r

abc_xmap[1:4, ]
#> # A crossmap tibble: 4 × 3
#> # with unique keys:  [4] lower -> [3] upper
#>   .from$lower .to$upper .weight_by$share
#>   <chr>       <chr>                <dbl>
#> 1 a           AA                     1  
#> 2 b           BB                     1  
#> 3 c           BB                     1  
#> 4 d           CC                     0.3
```

In most cases, we recommend flattening the crossmap tibble back to a
standard tibble, modifying and then coercing it again back to a
`xmap_tbl` to ensure weights are valid.

### Flattening and Exporting Crossmaps

There are a few ways to flatten or unpack a crossmap tibble. We
recommend using
[`tidyr::unpack()`](https://tidyr.tidyverse.org/reference/pack.html) or
`purrr:flatten_df()`, which both return tibbles:

``` r

abc_xmap |>
  tidyr::unpack(dplyr::everything()) ## or
#> # A tibble: 6 × 3
#>   lower upper share
#>   <chr> <chr> <dbl>
#> 1 a     AA      1  
#> 2 b     BB      1  
#> 3 c     BB      1  
#> 4 d     CC      0.3
#> 5 d     DD      0.6
#> 6 d     EE      0.1

abc_xmap |>
  purrr::flatten_df()
#> # A tibble: 6 × 3
#>   lower upper share
#>   <chr> <chr> <dbl>
#> 1 a     AA      1  
#> 2 b     BB      1  
#> 3 c     BB      1  
#> 4 d     CC      0.3
#> 5 d     DD      0.6
#> 6 d     EE      0.1
```

When saving or exporting `xmap_tbl` objects as flat files (e.g. to
`.csv`), you will need to first convert it into a standard tibble or
data.frame without nesting.

``` r

abc_xmap |>
  purrr::flatten_df() |>
  readr::write_csv("path/xmap.csv")
```

## Summarising Crossmaps

There are a number of features of crossmaps that might be of interest
for documenting data provenance or preprocessing steps. We include here
a selection of interesting properties and how to calculate them:

### Redistribution from Source Keys

If a crossmap involves any redistributions, `any(.xmap$.weight_by != 1)`
will be true. To find the links involved in redistribution:

``` r

abc_xmap |>
  dplyr::filter(.weight_by[[1]] != 1)
#> # A crossmap tibble: 3 × 3
#> # with unique keys:  [1] lower -> [3] upper
#>   .from$lower .to$upper .weight_by$share
#>   <chr>       <chr>                <dbl>
#> 1 d           CC                     0.3
#> 2 d           DD                     0.6
#> 3 d           EE                     0.1
```

### Composition of Target Keys

We can summarise which source keys contributed to each target:

``` r

abc_xmap |>
  dplyr::group_by(.to) |>
  dplyr::summarise(".from({names(abc_xmap$.from)})" := paste(.from[[1]], collapse = ", "))
#> # A tibble: 5 × 2
#>   .to$upper `.from(lower)`
#>   <chr>     <chr>         
#> 1 AA        a             
#> 2 BB        b, c          
#> 3 CC        d             
#> 4 DD        d             
#> 5 EE        d
```

## Visualisation

Crossmap tibbles are valid edge lists, and can be visualised as graphs
using packages such as [`ggraph`](https://ggraph.data-imaginist.com).
