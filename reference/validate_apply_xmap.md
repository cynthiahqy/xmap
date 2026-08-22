# Cheaply check whether `.data` is conformable with an `xmap_tbl`

`validate_apply_xmap()` checks the same two conditions
[`apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)
requires before transforming `.data` – every `keys_from` key has a
matching `.xmap$.from` link, and no `values_from` column has a missing
value – and returns a single logical, without building the
offending-rows/columns detail objects that
[`diagnose_apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)
does. It's the primitive to reach for when you only need a pass/fail
answer – e.g. checking many `.data`/`.xmap` group pairs with
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
or
[`purrr::map2_lgl()`](https://purrr.tidyverse.org/reference/map2.html)
before applying any of them. Reach for
[`diagnose_apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)
once `validate_apply_xmap()` says something failed and you need to know
why;
[`apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)
checks the same conditions at transform time and aborts with a message
pointing at the offending condition.

## Usage

``` r
validate_apply_xmap(
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

A single logical.

## Examples

``` r
abc_xmap <- demo$abc_links |>
  as_xmap_tbl(from = "lower", to = "upper", weight_by = "share")
abc_data <- tibble::tibble(
  lower = unique(demo$abc_links$lower),
  count = runif(length(unique(demo$abc_links$lower)), min = 100, max = 500)
)
validate_apply_xmap(abc_data, abc_xmap, values_from = count)
#> [1] TRUE
```
