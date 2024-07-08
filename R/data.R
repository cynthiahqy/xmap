#' Mock input objects for the `xmap` package
#'
#' A collection of mock inputs for experimenting with functions
#' in the `xmap` package.
#' `_pairs` objects are tibbles with just source-target *pairs* (no weights)
#' `_links` objects are tibbles with weighted source-target *links*.
#'
#' @format ## `mock`
#' A list with:
#' \describe{
#'  \item{ctr_iso3c_pairs}{named vector with 249 elements. Names are ISO-3 country codes, values are ISO English country names. Retrieved from `countrycode` package:
#'    \url{https://github.com/vincentarelbundock/countrycode}}
#'  \item{anzsco21_pairs}{tibble with 51 rows and 4 columns. Contains major and submajor occupation codes and descriptions for ANZSCO21. Retrieved from `strayr::anzsco2021` via:
#'    \url{https://github.com/runapp-aus/strayr}}
#'  \item{abc_links}{tibble with 6 rows and 3 columns, specifying links `lower`-``}
#'  \item{aus_state_pairs}{named list with 1 element named "AUS" containing codes for the Australian states}
#'  \item{aus_state_pop_df}{tibble containing 2022 population figures for Australia by state. Retrieved from:
#'    \url{https://www.abs.gov.au/statistics/people/population/national-state-and-territory-population/jun-2022}}
#'  }
#' @examples
#' mock$abc_links
"mock"