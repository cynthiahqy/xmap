# Boolean flags for properties of candidate and validated xmap links (internal)

`vhas_*()` functions check properties of xmap links and/or candidate
links. They are the shared primitives behind the three link-validity
conditions checked independently by `xmap_tbl()`,
[`diagnose_as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md),
and
[`validate_as_xmap()`](https://cynthiahqy.github.io/xmap/reference/validate_as_xmap.md)'s
`data.frame` method — every non-matrix check of "is `x` a valid
crossmap" should route through these rather than reimplementing the
underlying logic.

## Usage

``` r
vhas_no_missing(x)

vhas_no_dup_pairs(v_from, v_to)

vhas_positive_weights(v_weights)

vhas_valid_weights(v_from, v_weights, tol)
```

## Arguments

- x:

  a vector, or a single-column data frame (as used to store
  `.from`/`.to`/`.weight_by` in `xmap_tbl`), to check for missing values

- v_from, v_to, v_weights:

  equal length vectors containing the source-target node pairs

- tol:

  numeric \>= 0. Ignore differences smaller than `tol`. Passed through
  to the `tol` arg of
  [`dplyr::near()`](https://dplyr.tidyverse.org/reference/near.html).
  Deliberately has no default – every caller must forward a `tol` value
  explicitly, so a caller that forgets to forward its own user-facing
  `tol` argument errors loudly instead of silently falling back to an
  unexposed, undocumented internal default.

## Value

TRUE or FALSE

## Functions

- `vhas_no_missing()`: Returns TRUE if `x` has no missing values

- `vhas_no_dup_pairs()`: Returns TRUE if xmap does not have duplicate
  pairs of source-target nodes (irrespective of weights)

- `vhas_positive_weights()`: Returns TRUE if every weight is strictly
  positive. A crossmap link's weight must lie in `(0, 1]` – a weight of
  exactly zero (or a negative weight) means the pair isn't a valid link
  at all, rather than a degenerate one, so it's checked separately from
  `vhas_valid_weights()`'s per-`.from` sum-to-one condition. A missing
  weight also fails this check (rather than propagating `NA`) –
  `vhas_no_missing()` is where a missing-weight condition should be
  diagnosed on its own terms.

- `vhas_valid_weights()`: Returns TRUE if all weights for a given `from`
  label sum to (approximately) one. A `from` label with no outgoing
  weights, or whose outgoing weights sum to zero, fails this check — a
  valid crossmap has no dangling `.from` nodes. A missing weight also
  fails this check (rather than propagating `NA`) — `vhas_no_missing()`
  is where a missing-weight condition should be diagnosed on its own
  terms.
