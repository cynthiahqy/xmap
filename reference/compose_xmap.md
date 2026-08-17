# Compose Two Crossmaps Through a Shared Intermediate Classification

Given `xmap1` (`S -> M`) and `xmap2` (`M -> T`) sharing intermediate key
set `M`, chains them into a single crossmap `S -> T` without
materialising `M`-level values. Composed weights sum, over every shared
`m`, the product of `xmap1`'s weight onto `m` and `xmap2`'s weight from
`m`: \$\$w(s, t) = \sum\_{m \in M} w_1(s, m) \\ w_2(m, t)\$\$

## Usage

``` r
compose_xmap(xmap1, xmap2, ..., tol = .Machine$double.eps^0.5)
```

## Arguments

- xmap1:

  An `xmap_tbl`, `S -> M`.

- xmap2:

  An `xmap_tbl`, `M -> T`. Every value in `xmap1`'s `.to` must appear in
  `xmap2`'s `.from`; the reverse isn't required – `xmap2` may hold
  `.from` values `xmap1` never uses.

- ...:

  (reserved)

- tol:

  Tolerance of comparison.

## Value

An `xmap_tbl`, `S -> T`.

## Details

Re-checks that both inputs are actually valid crossmaps, not just
correctly classed, and aborts otherwise.

Only takes two crossmaps at a time. Matrix multiplication is
associative, so chain longer sequences with
[`Reduce()`](https://rdrr.io/r/base/funprog.html) instead of a dedicated
variadic interface – see the example below. Grouped composition (e.g.
one `xmap1` per group, composed against a shared `xmap2`) is likewise
left to the caller via
[`dplyr::group_map()`](https://dplyr.tidyverse.org/reference/group_map.html).

## Examples

``` r
abc_xmap <- demo$abc_links |>
  as_xmap_tbl(from = lower, to = upper, weight_by = share)
top_xmap <- tibble::tibble(
  upper = c("AA", "BB", "CC", "DD", "EE"),
  top = c("AAA", "AAA", "BBB", "BBB", "BBB"),
  weight = 1
) |>
  as_xmap_tbl(from = upper, to = top, weight_by = weight)
compose_xmap(abc_xmap, top_xmap)
#> # A crossmap tibble: 4 × 3
#> # with unique keys:  [4] lower -> [2] top
#>   .from$lower .to$top .weight_by$weight_by
#>   <chr>       <chr>                  <dbl>
#> 1 a           AAA                        1
#> 2 b           AAA                        1
#> 3 c           AAA                        1
#> 4 d           BBB                        1

# chaining more than two crossmaps: reduce pairwise composition over a
# list, e.g. lower -> upper -> top -> region
region_xmap <- tibble::tibble(
  top = c("AAA", "BBB"),
  region = c("north", "south"),
  weight = 1
) |>
  as_xmap_tbl(from = top, to = region, weight_by = weight)
Reduce(compose_xmap, list(abc_xmap, top_xmap, region_xmap))
#> # A crossmap tibble: 4 × 3
#> # with unique keys:  [4] lower -> [2] region
#>   .from$lower .to$region .weight_by$weight_by
#>   <chr>       <chr>                     <dbl>
#> 1 a           north                         1
#> 2 b           north                         1
#> 3 c           north                         1
#> 4 d           south                         1
```
