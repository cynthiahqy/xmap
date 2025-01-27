
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xmap <a href="https://cynthiahqy.github.io/xmap/"><img src="man/figures/logo.png" align="right" height="138" alt="xmap website" /></a>

The `{xmap}` package provides support for transformations of numeric
aggregates between statistical classifications (e.g. occupation or
industry categorisations) using the Crossmaps framework. Implements
classes for representing transformations between a source and target
classification as graph structures (i.e. Crossmaps), and methods for
validating and applying crossmaps to transform data collected under the
source classification into data indexed using the target classification
codes.

## Overview

Crossmaps consist of links (`to`, `from`, and `weight_by`) that
associate source and target classification codes with weights for
redistributing numeric mass attached to each code in a source
classification. For example, a given link could specify encode that 10%
of people in a source classification are employed in a particular target
occupation. A collection of links between two classifications forms a
crossmap graph structure, which when represented as an table of links
(i.e. an edge list table) can be easily verified against conditions that
are required for a valid transformation of data between the specified
classifications.

Using a valid crossmap guarantees that the total mass before and after
the transformation remains the same. For example, if we reclassify
counts of workers by occupation, we would expect that the total number
of workers across all occupation categories remains unchanged after
reclassification. However, comparing totals is not always sufficient to
identify mistakes in data transformation as there can be multiple ways
to redistribute mass between source and target classifications while
maintaining the same total. The crossmaps workflow saves users from
having to manually check code lines for implementation errors by
verifying crossmaps satisfy mathematically sufficient conditions for
valid transformation.

For details on how these guarantees arise from graph properities of
crossmaps, see the related paper, [*A Unified Statistical And
Computational Framework For Ex-Post Harmonisation Of Aggregate
Statistics*](https://arxiv.org/abs/2406.14163). This package allows you
to create, validate and apply `xmap_tbl` objects to perform valid and
mass-preserving transformations of numeric aggregates between
statistical classifications.

## Installation

To install the latest CRAN release of `xmap`:

``` r
install.packages("xmap")
```

To install the latest development version of `xmap`:

``` r
remotes::install_github("cynthiahqy/xmap")
```

## Usage

### Creating crossmaps

The easiest way to create a crossmap is to coerce a dataframe
(e.g. `xmap::demo$abc_links`) containing source codes, target codes and
weights between them:

``` r
library(xmap)
demo$abc_links |>
  as_xmap_tbl(from = "lower", to = "upper", weight_by = "share")
```

    ## # A crossmap tibble: 6 × 3
    ## # with unique keys:  [4] lower -> [5] upper
    ##   .from$lower .to$upper .weight_by$share
    ##   <chr>       <chr>                <dbl>
    ## 1 a           AA                     1  
    ## 2 b           BB                     1  
    ## 3 c           BB                     1  
    ## 4 d           CC                     0.3
    ## 5 d           DD                     0.6
    ## 6 d           EE                     0.1

If the coercion fails, you can use `diagnose_as_xmap()` to identify
issues:

``` r
bad_links <- demo$abc_links
bad_links[4, "share"] <- 5

diagnose_as_xmap_tbl(bad_links, from = "lower", to = "upper", weight_by = "share")
```

    ## Warning: The sum of weights on outgoing links for some source nodes are not near 1
    ## ℹ Fix weights or adjust `tol=`
    ## ℹ See `.$bad_froms` for more details

    ## $bad_dups
    ## NULL
    ## 
    ## $miss_weight_by
    ## NULL
    ## 
    ## $bad_froms
    ## # A tibble: 1 × 2
    ##   .from$lower .sum.weight_by
    ##   <chr>                <dbl>
    ## 1 d                      5.7

### Applying crossmaps

When using a crossmap to transform data, you want to make sure that the
crossmap covers all the codes present in your data. For example, if your
data contained a count for the category “teacher”, but your crossmap
doesn’t have any links with “teacher”, then you risk silently losing
data in the transformation. Even if you wanted to remove the count for
“teacher”, this should be done in the original dataset explicitly
(e.g. via filtering and removing rows), rather than implictly in the
transformation.

To use a suitable crossmap to transform data, you can use
`apply_xmap()`:

``` r
abc_xmap <- demo$abc_links |>
  as_xmap_tbl(from = "lower", to = "upper", weight_by = "share")
abc_data <- tibble::tibble(
  lower = unique(demo$abc_links$lower),
  count = runif(length(unique(demo$abc_links$lower)), min = 100, max = 500)
)
transformed_data <- apply_xmap(
  .data = abc_data,
  .xmap = abc_xmap,
  values_from = count
)
```

    ## Matching keys in `.data$lower` with `.xmap$.from$lower`
    ## ℹ To silence, set `keys_from = lower`

``` r
## totals still match!
sum(abc_data$count) == sum(transformed_data$count)
```

    ## [1] TRUE
