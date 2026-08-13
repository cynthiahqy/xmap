# Check whether already-split `.from`/`.to`/`.weight_by` columns form a valid crossmap (internal)

The single source of truth for the three link-validity conditions,
shared by
[`validate_as_xmap.data.frame()`](https://cynthiahqy.github.io/xmap/reference/validate_as_xmap.md)
and `xmap_tbl()`'s construction gate, so the two don't independently
re-implement (and risk drifting on) the same checks.

## Usage

``` r
check_valid_xmap_df(tbl_x, tol)
```

## Arguments

- tbl_x:

  A tibble/data frame with `.from`, `.to`, `.weight_by` columns (each
  may themselves be single-column data frames, as `xmap_tbl` stores
  them).

- tol:

  Deliberately has no default here, unlike the exported entry points
  that call this – forces every caller to explicitly forward its own
  user-facing `tol` rather than one silently drifting to an unexposed
  internal default if a future edit forgets to pass it through.

## Value

A single logical.
