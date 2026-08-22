# Check whether matched keys/values are conformable with an `xmap_tbl`'s `.from` set (internal)

The single source of truth for the two data-conformability conditions
[`apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)
requires before transforming, shared by
[`apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)'s
abort gate,
[`diagnose_apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)'s
flags, and
[`validate_apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/validate_apply_xmap.md),
so the three don't independently re-implement (and risk drifting on) the
same checks.

## Usage

``` r
check_conformable_xmap_data(key, value, from)
```

## Arguments

- key:

  A vector of matched keys from `.data` (`key_val$.key`).

- value:

  A data frame of matched value column(s) from `.data`
  (`kv_tbl$.value`); checked column-by-column since more than one
  `values_from` column may be selected.

- from:

  `.xmap$.from` to check `key`'s coverage against.

## Value

A list of two logicals: `covered` (every `key` has a matching `from`)
and `no_missing_values` (no `value` column has a missing value).
