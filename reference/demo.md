# Demo objects for the `xmap` package

A collection of small demo inputs for experimenting with functions in
the `xmap` package. `_pairs` objects are tibbles with just source-target
*pairs* (no weights), `_links` objects are tibbles with weighted
source-target *links*, and `_stats` objects are part-to-whole arrays of
key-value pairs to transform.

## Usage

``` r
demo
```

## Format

### `demo`

A list with:

- abc_links:

  tibble with 6 rows and 3 columns, specifying links `lower` -\> `upper`
  by `share`. Covers one-to-one, many-to-one and one-to-many relations.

- simple_links:

  tibble with 10 rows and 3 columns, specifying links `xcode` -\>
  `alphacode` by `weight`. The running example in
  [`vignette("applying-crossmaps")`](https://cynthiahqy.github.io/xmap/articles/applying-crossmaps.md).

- simple_stats:

  tibble with 7 rows and 2 columns: a `count` for each `xcode` source
  key in `simple_links`, totalling 2800. Conformable with a crossmap
  built from `simple_links`, so the two can be used together to
  demonstrate
  [`apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)
  end to end.

- aus_state_pairs:

  tibble with 8 rows and 2 columns, pairing `ctry` "AUS" with each
  Australian state and territory code.

- aus_state_pop_df:

  tibble containing 2022 population figures for Australia by state.
  Retrieved from:
  <https://www.abs.gov.au/statistics/people/population/national-state-and-territory-population/jun-2022>

## Examples

``` r
demo$abc_links
#> # A tibble: 6 × 3
#>   lower upper share
#>   <chr> <chr> <dbl>
#> 1 a     AA      1  
#> 2 b     BB      1  
#> 3 c     BB      1  
#> 4 d     CC      0.3
#> 5 d     DD      0.6
#> 6 d     EE      0.1
demo$simple_stats
#> # A tibble: 7 × 2
#>   xcode count
#>   <chr> <dbl>
#> 1 x1111   100
#> 2 x2222   200
#> 3 x3333   300
#> 4 x4444   400
#> 5 x5555   500
#> 6 x6666   600
#> 7 x7777   700
```
