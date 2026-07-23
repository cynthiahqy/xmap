# xmap

``` r

library(xmap)
library(dplyr)
```

## The Crossmaps Framework and `{xmap}` workflow

This package is an implementation of the Crossmaps Framework for unified
*specification, verification, implementation and documentation* of
operations involved in transforming aggregate statistics between related
measurement instruments (e.g. classification codes).

The framework conceptualises the aggregation of redistribution of
numeric masses between related taxonomic structures as an operation
which applies a graph-based representation of mapping and redistribution
logic between source and target keys (the *crossmap*), to conformable
key-value pairs (*shared mass array*).

A *crossmap* specifies:

- related pairs of source and target key (e.g. states in country)
- weights between 0 and 1 for distributing numeric mass between each
  related pair of source and target keys (e.g. 25% of country-level GDP
  -\> state-A)

A *shared mass array* is a collection of key-value pairs, where the
values form a shared numeric and the keys are parts of a shared
conceptual whole (e.g. GDP by state -\> country)

The crossmaps framework is an alternative approach to data
transformation that removes the need for bespoke code to handle data
preparation involving many-to-one or one-to-many operations.

The framework gives rise to assertions on input *crossmap* and *shared
mass arrays* which ensure the transformations are valid, and implemented
exactly as specified. Valid and well-documented transformation workflows
should have the following properties:

- preservation of the shared total mass before and after transformation.
  For example, country level GDP should remain constant regardless of
  disaggregation method or granularity (e.g. state vs county)
- explicit handling of missing values, without any implicit missing
  value arithemtic (e.g. aggregating ‘missing’ state-level mass to
  country-level by treating the values as zeros via expressions like
  `sum(state, na.rm = TRUE)`)

See the related paper, [*A Unified Statistical And Computational
Framework For Ex-Post Harmonisation Of Aggregate
Statistics*](https://arxiv.org/abs/2406.14163), for further details on
the conditions which guarantee the above properties. This package
implements workflow warnings and errors to ensure relevant conditions
are met.

### Package Functions

This package allows you to create, validate and apply `xmap_tbl` objects
to perform valid and mass-preserving transformations of numeric
aggregates between statistical classifications. The crossmaps workflow
saves users from having to manually check code lines for implementation
errors by verifying crossmaps satisfy mathematically sufficient
conditions for valid transformation. It provides ‘guardrails’ for
transforming data between classifications.

| Step | Function | Purpose |
|----|----|----|
| Specify & Validate Crossmaps | [`as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md) | Coerce a data frame of `.from`/`.to`/`.weight_by` columns into a validated `xmap_tbl` |
|  | [`diagnose_as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md) | Diagnose why a data frame fails [`as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md) validation (invalid weights, missing weights, duplicate links) |
| Apply Transformations | [`apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md) | Apply a validated `xmap_tbl` to transform/aggregate/redistribute `.data` |
|  | [`diagnose_apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md) | Diagnose why [`apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md) fails (missing coverage, missing values) |

### Validation Functions

[`diagnose_as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md)
checks that:

- weights from a given `.from` key sum to one. This ensures that totals
  before and after transformation are the same.
- there are no missing weights
- there are no duplicated links

[`diagnose_apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)
checks that:

- all source categories in `.data` have matching instructions in
  `.xmap`. This ensures that data isn’t silently dropped in the
  transformation due to a missing category.
- there are no missing values in `.data` that could cause addition
  errors.

## Example: Country-State Mappings

Consider data transformations which reference relations between
hierarchical administrative regions.

In the following example, we use some basic data manipulation operations
from [dplyr](https://dplyr.tidyverse.org) to generate mapping weights
for transforming numeric mass (e.g. GDP):

- aggregating from state-level to country-level,
- redistributing from country-level to state-level

### Aggregation, Coverage, and Missing Value Checks

For aggregation, we use unit weights:

``` r

aus_state_agg_links <- demo$aus_state_pairs |>
  mutate(ones = 1L)
```

Links are validated when coercing them into crossmaps, and some
additional information about the transformation is computed (i.e. how
many unique keys are in the source and target taxonomies):

``` r

(agg_xmap <- aus_state_agg_links |>
  as_xmap_tbl(from = state, to = ctry, weight_by = ones)
)
#> # A crossmap tibble: 8 × 3
#> # with unique keys:  [8] state -> [1] ctry
#>   .from$state .to$ctry .weight_by$ones
#>   <chr>       <chr>              <int>
#> 1 AU-ACT      AUS                    1
#> 2 AU-NSW      AUS                    1
#> 3 AU-NT       AUS                    1
#> 4 AU-QLD      AUS                    1
#> 5 AU-SA       AUS                    1
#> 6 AU-TAS      AUS                    1
#> 7 AU-VIC      AUS                    1
#> 8 AU-WA       AUS                    1
```

The unit weights represent a “transfer” of 100% of the source values
indexed by `.from` keys to the target `.to` keys.

Let’s generate some dummy state-level data to apply our aggregation to:

``` r

set.seed(1395)
(aus_state_data <- demo$aus_state_pairs |>
  mutate(
    gdp = runif(n(), 100, 2000),
    ref = 100
  ))
#> # A tibble: 8 × 4
#>   ctry  state    gdp   ref
#>   <chr> <chr>  <dbl> <dbl>
#> 1 AUS   AU-ACT 1626.   100
#> 2 AUS   AU-NSW 1244.   100
#> 3 AUS   AU-NT   703.   100
#> 4 AUS   AU-QLD  239.   100
#> 5 AUS   AU-SA  1388.   100
#> 6 AUS   AU-TAS 1192.   100
#> 7 AUS   AU-VIC 1535.   100
#> 8 AUS   AU-WA   306.   100
```

Now to transform / aggregate our data:

``` r

(aus_ctry_data <- aus_state_data |>
  apply_xmap(
    .xmap = agg_xmap,
    values_from = c(gdp, ref),
    keys_from = state
  )
)
#> # A tibble: 1 × 3
#>   ctry    gdp   ref
#>   <chr> <dbl> <dbl>
#> 1 AUS   8233.   800
```

What happens if our crossmap was missing instructions for multiple
states?

``` r

## dropping links
agg_xmap[1:3, ]
#> # A crossmap tibble: 3 × 3
#> # with unique keys:  [3] state -> [1] ctry
#>   .from$state .to$ctry .weight_by$ones
#>   <chr>       <chr>              <int>
#> 1 AU-ACT      AUS                    1
#> 2 AU-NSW      AUS                    1
#> 3 AU-NT       AUS                    1

## will lead to an error!
apply_xmap(
  .data = aus_state_data,
  .xmap = agg_xmap[1:3, ],
  values_from = c(gdp, ref),
  keys_from = state
)
#> Error in `apply_xmap()`:
#> ✖ One or more keys in `.data` do not have corresponding links in `.xmap`
#> ℹ Add missing links to `.xmap` or subset `.data`
```

This error prevents the accidental dropping of observations by
incomplete specification of transformation instruction.

To inspect and remedy this issue, we can use
[`diagnose_apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)
to find out which keys in `.data` are not covered by the `.xmap`:

``` r

diagnose_apply_xmap(
  .data = aus_state_data,
  .xmap = agg_xmap[1:3, ],
  values_from = c(gdp, ref)
)
#> ✖ Found 8 keys in `.data` without corresponding match in `.xmap$.from`
#> See .$not_covered
#> $not_covered
#> # A tibble: 8 × 2
#>   .key         .value$gdp  $ref
#>   <tibble[,0]>      <dbl> <dbl>
#> 1                   1626.   100
#> 2                   1244.   100
#> 3                    703.   100
#> 4                    239.   100
#> 5                   1388.   100
#> 6                   1192.   100
#> 7                   1535.   100
#> 8                    306.   100
```

Missing values will also be flagged to encourage explicit handling of
missing values before the
[`apply_xmap()`](https://cynthiahqy.github.io/xmap/reference/apply_xmap.md)
mapping transformation:

``` r

# add some `NA`
aus_state_data_na <- aus_state_data
aus_state_data_na[c(1, 3, 5), "gdp"] <- NA

apply_xmap(
  .data = aus_state_data_na,
  .xmap = agg_xmap,
  values_from = gdp,
  keys_from = state
)
#> Error in `apply_xmap()`:
#> ✖ Missing values not allowed in `.data` columns: gdp
#> ℹ Remove or replace missing values.
```

### Redistribution, valid weights and preserving totals

For redistributing, we can choose any weights as long as the sum of
weights on outgoing links from each source key totals one (or
[`dplyr::near()`](https://dplyr.tidyverse.org/reference/near.html)
enough). This ensures that we only split source values into percentage
parts that sum to 100%.

A common naive strategy is to distribute equally amongst related target
keys:

``` r

demo$aus_state_pairs |>
  group_by(ctry) |>
  mutate(equal = 1 / n_distinct(state)) |>
  ungroup() |>
  as_xmap_tbl(from = ctry, to = state, weight_by = equal)
#> # A crossmap tibble: 8 × 3
#> # with unique keys:  [1] ctry -> [8] state
#>   .from$ctry .to$state .weight_by$equal
#>   <chr>      <chr>                <dbl>
#> 1 AUS        AU-ACT               0.125
#> 2 AUS        AU-NSW               0.125
#> 3 AUS        AU-NT                0.125
#> 4 AUS        AU-QLD               0.125
#> 5 AUS        AU-SA                0.125
#> 6 AUS        AU-TAS               0.125
#> 7 AUS        AU-VIC               0.125
#> 8 AUS        AU-WA                0.125
```

If we use invalid weights, such as unit weights,
[`as_xmap_tbl()`](https://cynthiahqy.github.io/xmap/reference/as_xmap_tbl.md)
will error:

``` r

demo$aus_state_pairs |>
  mutate(ones = 1) |>
  as_xmap_tbl(from = ctry, to = state, weight_by = ones)
#> Error in `xmap_tbl()`:
#> ! Invalid `.weight_by` found for some links
#> ✖ The total outgoing `.weight_by` for some `.from` nodes are not near enough to
#>   1
#> ℹ Modify `.weight_by` or adjust `tol` and try again.
#> ℹ Use `diagnose_xmap_tbl() for more information.
```

Except in the case of one-to-one mappings, crossmaps are generally
lateral (one-way), and have different weights in each direction.

A more sophisticated strategy for generating weights is to use reference
information. For example, we can use population shares to redistribute
GDP between states:

``` r

(split_xmap_pop <- demo$aus_state_pop_df |>
  group_by(ctry) |>
  mutate(pop_share = pop / sum(pop)) |>
  ungroup() |>
  as_xmap_tbl(
    from = ctry, to = state, weight_by = pop_share
  ))
#> # A crossmap tibble: 8 × 3
#> # with unique keys:  [1] ctry -> [8] state
#>   .from$ctry .to$state .weight_by$pop_share
#>   <chr>      <chr>                    <dbl>
#> 1 AUS        AU-ACT                 0.0176 
#> 2 AUS        AU-NSW                 0.314  
#> 3 AUS        AU-NT                  0.00965
#> 4 AUS        AU-QLD                 0.205  
#> 5 AUS        AU-SA                  0.0701 
#> 6 AUS        AU-TAS                 0.0220 
#> 7 AUS        AU-VIC                 0.255  
#> 8 AUS        AU-WA                  0.107
```

Let’s redistribute the country level data we aggregated above back to
state level using our calcuted population weights:

``` r

aus_state_data2 <- aus_ctry_data |>
  mutate(ref = 10000) |>
  apply_xmap(split_xmap_pop,
    values_from = c(gdp, ref),
    keys_from = ctry
  )
```

Note: that the values in the transformed `ref` column do not exactly
match the float values in `.weight_by$pop_share` used as transformation
weights. This is due to floating point inaccuracies. Over larger
transformations with more keys, this may result in slight mismatches
between the total numeric mass before and after transformation.

    #> # A tibble: 8 × 5
    #>   .from$ctry state     gdp    ref .weight_by$pop_share
    #>   <chr>      <chr>   <dbl>  <dbl>                <dbl>
    #> 1 AUS        AU-ACT  145.   176.               0.0176 
    #> 2 AUS        AU-NSW 2584.  3139.               0.314  
    #> 3 AUS        AU-NT    79.4   96.5              0.00965
    #> 4 AUS        AU-QLD 1687.  2049.               0.205  
    #> 5 AUS        AU-SA   577.   701.               0.0701 
    #> 6 AUS        AU-TAS  181.   220.               0.0220 
    #> 7 AUS        AU-VIC 2096.  2546.               0.255  
    #> 8 AUS        AU-WA   883.  1072.               0.107
