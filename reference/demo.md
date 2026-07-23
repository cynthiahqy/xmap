# Demo objects for the \`xmap\` package

A collection of demo inputs for experimenting with functions in the
\`xmap\` package. \`\_pairs\` objects are tibbles with just
source-target \*pairs\* (no weights) \`\_links\` objects are tibbles
with weighted source-target \*links\*.

## Usage

``` r
demo
```

## Format

\## \`demo\` A list with:

- ctr_iso3c_pairs:

  named vector with 249 elements. Names are ISO-3 country codes, values
  are ISO English country names. Retrieved from \`countrycode\` package:
  <https://github.com/vincentarelbundock/countrycode>

- anzsco22_isco8_crosswalk:

  tibble with 10 rows and 5 columns. Subset of crosswalk between
  ANZSCO22 and ISCO8 Occupation Code Standards published by The
  AUstralian Bureau of Statistics

- anzsco22_stats:

  tibble with 10 rows and 2 columns. Stylised Occupation Counts

- simple_links:

  tibble with 10 rows and 3 columns. specifying links
  \`xcode\`-\>\`alphacode\` by \`weight\`

- abc_links:

  tibble with 6 rows and 3 columns, specifying links
  \`lower\`-\>\`upper\` by \`share\`

- aus_state_pairs:

  named list with 1 element named "AUS" containing codes for the
  Australian states

- aus_state_pop_df:

  tibble containing 2022 population figures for Australia by state.
  Retrieved from:
  <https://www.abs.gov.au/statistics/people/population/national-state-and-territory-population/jun-2022>

## Examples

``` r
demo$abc_links
#> # A tibble: 6 × 3
#>   lower upper share
#>   <chr> <chr> <dbl>
#> 1 a     AA      1  
#> 2 b     BB      1  
#> 3 c     BB      1  
#> 4 d     CC      0.3
#> 5 d     DD      0.6
#> 6 d     EE      0.1
```
