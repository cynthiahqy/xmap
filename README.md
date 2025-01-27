
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
