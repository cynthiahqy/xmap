# Cheaply check whether links form a valid crossmap

A valid crossmap's links must satisfy three conditions:

## Usage

``` r
validate_as_xmap(x, ..., tol = .Machine$double.eps^0.5)

# S3 method for class 'data.frame'
validate_as_xmap(x, from, to, weight_by, ..., tol = .Machine$double.eps^0.5)

# S3 method for class 'matrix'
validate_as_xmap(x, ..., tol = .Machine$double.eps^0.5)
```

## Arguments

- x:

  An object with links to validate. Methods exist for `data.frame` and
  `matrix`.

- ...:

  Passed to methods.

- tol:

  Tolerance of comparison.

- from:

  The column in `x` that specifies the 'from' nodes.

- to:

  The column in `x` that specifies the 'to' nodes.

- weight_by:

  The column in `x` that specifies the weight of the links.

## Value

A single logical.

## Details

- every link has a non-missing `.from`, `.to`, and `.weight_by`

- no two links share the same `.from`-`.to` pair (data-frame
  representations only — see the `.matrix` method for why this doesn't
  carry over to a matrix representation)

- for each `.from`, the `.weight_by` values of its outgoing links sum to
  (approximately) one — this is what guarantees the total mass before
  and after a transformation stays the same

`validate_as_xmap()` checks these conditions and returns a single
logical, without building the offending-rows detail objects that
[`diagnose_as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md)
does. It's the primitive to reach for when you only need a pass/fail
answer — e.g. inside
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
or
[`dplyr::group_map()`](https://dplyr.tidyverse.org/reference/group_map.html)
over many groups. Reach for
[`diagnose_as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md)
once `validate_as_xmap()` says something failed and you need to know
why;
`xmap_tbl()`/[`as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md)
check the same conditions at construction time and abort with a message
pointing at the offending condition.

## Conditions for matrices

A matrix represents `.from`/`.to` identity through
[`dimnames()`](https://rdrr.io/r/base/dimnames.html) (rows = `.from`,
columns = `.to`) rather than per-link values, so the three conditions
above translate differently:

- non-missing `.from`/`.to` becomes
  "[`rownames()`](https://rdrr.io/r/base/colnames.html)/[`colnames()`](https://rdrr.io/r/base/colnames.html)
  are non-`NULL`, with no repeated names"; non-missing `.weight_by`
  becomes "no `NA` cells"

- the no-duplicate-pairs check does not carry over as-is: a single cell
  can't encode a duplicate pair (each is already a unique row x column
  intersection). What can still happen — and is checked above as a
  `.from`/`.to`-identity condition, not a pairs condition — is repeated
  [`dimnames()`](https://rdrr.io/r/base/dimnames.html): base R places no
  uniqueness constraint on them, e.g.
  `matrix(1:4, 2, 2, dimnames = list(c("a", "a"), c("x", "y")))` is a
  valid matrix with a repeated row name. A repeated row name would mean
  the same `.from` key has more than one, independently-checked set of
  outgoing weights; a repeated column name would mean weights for the
  same `.to` key are split across columns, invisible to
  [`rowSums()`](https://rdrr.io/r/base/colSums.html). Both are rejected
  by the row/column name uniqueness check

- weights summing to one becomes a row-sum check; a row summing to
  exactly zero (a `.from` with no outgoing links) fails here too, since
  0 is never near enough to 1

## Examples

``` r
demo$abc_links |>
  validate_as_xmap(from = lower, to = upper, weight_by = share)
#> [1] TRUE
abc_matrix <- demo$abc_links |>
  tidyr::pivot_wider(names_from = upper, values_from = share, values_fill = 0) |>
  tibble::column_to_rownames("lower") |>
  as.matrix()
validate_as_xmap(abc_matrix)
#> [1] TRUE
```
