# Package index

## Build and validate crossmaps

Coerce links (a data frame or matrix) into a validated `xmap_tbl`, and
check or diagnose validity without necessarily coercing.

- [`as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md)
  [`diagnose_as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md)
  : Coerce links into a crossmap tibble
- [`validate_as_xmap()`](https://cynthiahqy.github.io/xmap/reference/validate_as_xmap.md)
  : Cheaply check whether links form a valid crossmap

## Apply crossmaps to data

- [`apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)
  [`diagnose_apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)
  : Apply Crossmap Transformation to Conformable Data

- [`validate_apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/validate_apply_xmap.md)
  :

  Cheaply check whether `.data` is conformable with an `xmap_tbl`

## Compose crossmaps

Chain two crossmaps sharing an intermediate classification into one.

- [`compose_xmap()`](https://cynthiahqy.github.io/xmap/reference/compose_xmap.md)
  : Compose Two Crossmaps Through a Shared Intermediate Classification

## Example data

- [`demo`](https://cynthiahqy.github.io/xmap/reference/demo.md) :

  Demo objects for the `xmap` package

- [`timor_occupn`](https://cynthiahqy.github.io/xmap/reference/timor_occupn.md)
  : Timor-Leste census occupation codes

- [`indstat`](https://cynthiahqy.github.io/xmap/reference/indstat.md) :
  UNIDO INDSTAT4 industrial statistics (masked), with country lookup

## Internal

Internal helpers, not exported for direct use.

- [`new_xmap_diagnosis()`](https://cynthiahqy.github.io/xmap/reference/new_xmap_diagnosis.md)
  :

  Construct an `xmap_diagnosis` object

- [`check_valid_xmap_df()`](https://cynthiahqy.github.io/xmap/reference/check_valid_xmap_df.md)
  :

  Check whether already-split `.from`/`.to`/`.weight_by` columns form a
  valid crossmap (internal)

- [`vhas_no_missing()`](https://cynthiahqy.github.io/xmap/reference/vhas.md)
  [`vhas_no_dup_pairs()`](https://cynthiahqy.github.io/xmap/reference/vhas.md)
  [`vhas_valid_weights()`](https://cynthiahqy.github.io/xmap/reference/vhas.md)
  : Boolean flags for properties of candidate and validated xmap links
  (internal)

- [`xmap`](https://cynthiahqy.github.io/xmap/reference/xmap-package.md)
  [`xmap-package`](https://cynthiahqy.github.io/xmap/reference/xmap-package.md)
  : xmap: Transforming Data Between Statistical Classifications

- [`xmap-rlang`](https://cynthiahqy.github.io/xmap/reference/xmap-rlang.md)
  : Internal rlang methods

- [`xmap-vctrs`](https://cynthiahqy.github.io/xmap/reference/xmap-vctrs.md)
  : Internal vctrs methods
