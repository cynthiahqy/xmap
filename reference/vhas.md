# Boolean flags for properties of candidate and validated xmap links (internal)

\`vhas\_\*()\` functions check properties of xmap links and/or candidate
links. The functions only accepts equal length vector inputs to support
multiple link formats, but does not check if the inputs are from the
same xmap.

## Usage

``` r
vhas_no_dup_pairs(v_from, v_to)

vhas_valid_weights(v_from, v_weights, tol = .Machine$double.eps^0.5)
```

## Arguments

- v_from, v_to, v_weights:

  equal length vectors containing the source-target node pairs

- tol:

  numeric \\\ge 0\\. Ignore differences smaller than \`tol\`. Passed
  through to the \`tolerance\` arg of \`base::all.equal()\`.

## Value

TRUE or FALSE

## Functions

- `vhas_no_dup_pairs()`: Returns TRUE if xmap does not have duplicate
  pairs of source-target nodes (irrespective of weights)

- `vhas_valid_weights()`: Returns TRUE if all weights for a given
  \`from\` label sum to one (approximately)
