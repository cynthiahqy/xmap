# Coerce links into a crossmap tibble

Converts an object of links into an `xmap_tbl`. Methods exist for
`data.frame` and `matrix` — see their respective sections below for how
`from`/`to`/`weight_by` are interpreted by each. Aborts with a message
pointing at the offending condition if the links aren't a valid crossmap
— the same conditions
[`validate_as_xmap()`](https://cynthiahqy.github.io/xmap/reference/validate_as_xmap.md)
checks, though currently implemented independently rather than by
calling it (except for the `matrix` method, which does call
[`validate_as_xmap()`](https://cynthiahqy.github.io/xmap/reference/validate_as_xmap.md)
directly).

## Usage

``` r
as_xmap_tbl(x, ...)

# S3 method for class 'data.frame'
as_xmap_tbl(x, from, to, weight_by, ..., tol = .Machine$double.eps^0.5)

# S3 method for class 'matrix'
as_xmap_tbl(
  x,
  ...,
  from = NULL,
  to = NULL,
  weight_by = NULL,
  tol = .Machine$double.eps^0.5
)

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

  An object with links to coerce. Methods exist for `data.frame` and
  `matrix`.

- ...:

  (reserved) Additional arguments passed to methods.

- from:

  Identifies the 'from' nodes. For the `data.frame` method, the column
  in `x` that specifies them (tidyselect). For the `matrix` method, see
  the Matrix method section below.

- to:

  Identifies the 'to' nodes. For the `data.frame` method, the column in
  `x` that specifies them (tidyselect). For the `matrix` method, see the
  Matrix method section below.

- weight_by:

  Identifies the weight of the links. For the `data.frame` method, the
  column in `x` that specifies it (tidyselect). For the `matrix` method,
  see the Matrix method section below.

- tol:

  Tolerance of comparison.

## Value

Returns an xmap tibble object.

`diagnose_as_xmap_tbl()` returns an `xmap_diagnosis` object: a list with
`valid` (a scalar logical) and `details` (a named list of tibbles of
offending rows, one per check, `NULL` where that check passed). Printing
the result shows a readable pass/fail report; see
[`new_xmap_diagnosis()`](https://cynthiahqy.github.io/xmap/reference/new_xmap_diagnosis.md).

## Details

`diagnose_as_xmap_tbl()` checks whether `x`'s links form a valid
crossmap — the same conditions
[`validate_as_xmap()`](https://cynthiahqy.github.io/xmap/reference/validate_as_xmap.md)
checks, though currently implemented independently rather than by
calling it — and returns detail on any offending rows, to help resolve
the specific issue rather than just knowing something's wrong. The
returned `xmap_diagnosis`'s `details` has one entry per condition
('NULL' where that check passed):

- `bad_dups`: rows sharing a `.from`-`.to` pair with another row

- `miss_from`, `miss_to`, `miss_weight_by`: rows with a missing `.from`,
  `.to`, or `.weight_by` value, respectively

- `nonpositive_weights`: rows whose `.weight_by` is zero or negative

- `bad_froms`: for each `.from` whose outgoing weights don't sum to
  (near enough) one, that `.from` and its actual weight sum

## Data frame method

`as_xmap_tbl.data.frame()` takes a data.frame-like object and converts
it into an `xmap_tbl` based on specified columns for `from`, `to`, and
`weight_by`.

## Matrix method

`as_xmap_tbl.matrix()` takes an adjacency matrix (rows = `.from`,
columns = `.to`, cells = `.weight_by`, per
[`validate_as_xmap()`](https://cynthiahqy.github.io/xmap/reference/validate_as_xmap.md)'s
`.matrix` method) and reshapes it into an `xmap_tbl`, dropping
zero-weight cells (non-links). It checks matrix validity with
[`validate_as_xmap()`](https://cynthiahqy.github.io/xmap/reference/validate_as_xmap.md)
*before* reshaping — checking only after would let an all-zero row (a
`.from` with no outgoing links) disappear silently, since dropping its
only cells removes the row from the reshaped table before anything could
flag it.

`from`/`to`/`weight_by` here are optional strings naming the resulting
columns, since a matrix (unlike a data frame) has no columns to select
from — identity comes from
[`dimnames()`](https://rdrr.io/r/base/dimnames.html) instead. They
default to `names(dimnames(x))` when set, falling back to
`"rowname"`/`"colname"`/ `"cell"` (named after where each value is
actually pulled from) when `x` has no named dimnames.

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
abc_matrix <- demo$abc_links |>
  tidyr::pivot_wider(names_from = upper, values_from = share, values_fill = 0) |>
  tibble::column_to_rownames("lower") |>
  as.matrix()
as_xmap_tbl(abc_matrix)
#> # A crossmap tibble: 6 × 3
#> # with unique keys:  [4] rowname -> [5] colname
#>   .from$rowname .to$colname .weight_by$cell
#>   <chr>         <chr>                 <dbl>
#> 1 a             AA                      1  
#> 2 b             BB                      1  
#> 3 c             BB                      1  
#> 4 d             CC                      0.3
#> 5 d             DD                      0.6
#> 6 d             EE                      0.1
```
