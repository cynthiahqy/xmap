#' Demo objects for the `xmap` package
#'
#' A collection of demo inputs for experimenting with functions
#' in the `xmap` package.
#' `_pairs` objects are tibbles with just source-target *pairs* (no weights)
#' `_links` objects are tibbles with weighted source-target *links*.
#'
#' @format ## `demo`
#' A list with:
#' \describe{
#'  \item{ctr_iso3c_pairs}{named vector with 249 elements. Names are ISO-3 country codes, values are ISO English country names. Retrieved from `countrycode` package:
#'    \url{https://github.com/vincentarelbundock/countrycode}}
#'  \item{anzsco22_isco8_crosswalk}{tibble with 10 rows and 5 columns. Subset of crosswalk between ANZSCO22 and ISCO8 Occupation Code Standards published by The AUstralian Bureau of Statistics}
#'  \item{anzsco22_stats}{tibble with 6 rows and 2 columns. Stylised Occupation Counts}
#'  \item{soc2018_isco8_crosswalk}{tibble with 7 rows and 5 columns. Illustrative subset of a correspondence between SOC 2018 and ISCO8 Occupation Code Standards, hand-authored to mirror `anzsco22_isco8_crosswalk` for the same ISCO-08 targets -- NOT sourced from BLS's published crosswalk, see `vignette("creating-crossmap-weights")`}
#'  \item{soc2018_stats}{tibble with 5 rows and 2 columns. Stylised Occupation Counts}
#'  \item{simple_links}{tibble with 10 rows and 3 columns. specifying links `xcode`->`alphacode` by `weight`}
#'  \item{abc_links}{tibble with 6 rows and 3 columns, specifying links `lower`->`upper` by `share`}
#'  \item{aus_state_pairs}{named list with 1 element named "AUS" containing codes for the Australian states}
#'  \item{aus_state_pop_df}{tibble containing 2022 population figures for Australia by state. Retrieved from:
#'    \url{https://www.abs.gov.au/statistics/people/population/national-state-and-territory-population/jun-2022}}
#'  }
#' @examples
#' demo$abc_links
"demo"

#' Timor-Leste census occupation codes
#'
#' A ~1% sample of individual-level records from the Timor-Leste
#' Population and Housing Census 2015, prepared for the
#' occupation-categorisation analysis in Mata Dalan Institute (2020) --
#' see `@source` below. Used in
#' `vignette("extracting-crossmaps-from-scripts")` (Case 1) to
#' demonstrate recovering an implicit occupation-recoding script as an
#' explicit crossmap.
#'
#' @format A tibble with 11,775 rows and 5 columns:
#' \describe{
#'  \item{houseid}{household identifier (5,508 distinct households)}
#'  \item{pno}{person number within the household}
#'  \item{p3p3_sex}{sex of the individual: `"1. Male"` or `"2. Female"`}
#'  \item{p3p4_age}{age in years (0-98)}
#'  \item{occupn}{original occupation code (161 distinct non-missing
#'  values, ranging 110-9999). `NA` where no occupation code was
#'  recorded -- these rows skew toward younger ages (median 12 vs. 39.5
#'  for rows with a code) but the two groups overlap, so age alone
#'  doesn't fully explain which rows are missing}
#' }
#' @source Individual-level extract of the Timor-Leste Population and
#' Housing Census 2015 microdata, prepared for the occupation-category
#' analysis (Figures 1-2) in: Mata Dalan Institute (2020), "The Informal
#' Sector in Timor-Leste in the Midst of COVID-19", August 2020, with
#' support from Oxfam and Professor Brett Inder (Monash University).
#' \url{https://oi-files-cng-v2-prod.s3.eu-west-2.amazonaws.com/asia.oxfam.org/s3fs-public/file_attachments/MDI_COVID-19_Informal\%20sector\%20Research_Aug\%2020_Final_English.pdf}
#'
#' `timor_occupn` is a ~1% sample of the full 1,179,654-row individual-level
#' census extract, grouped by `occupn` and sampled with dplyr -- so the set
#' of occupation codes present is closer to fully represented than a plain
#' random sample of individuals would give. See `data-raw/occupation.R`.
"timor_occupn"

#' UNIDO INDSTAT4 industrial statistics (masked), with country lookup
#'
#' A subset of UNIDO's INDSTAT4 industrial-statistics database, with the
#' reported output `value` masked to a constant, bundled together with a
#' small country-code lookup table since the two are relationally paired
#' (`indstat$masked_sample$country` joins onto
#' `indstat$country_lookup$code`). Used in
#' `vignette("extracting-crossmaps-from-scripts")` (Case 2) to
#' demonstrate grouped crossmap validation across `country`/`year`. Some
#' `isic` industry codes are reported only in combination (`isiccomb`),
#' with a single `value` covering several `isic` codes at once -- the
#' vignette splits these back out.
#'
#' @format ## `indstat`
#' A list with:
#' \describe{
#'  \item{masked_sample}{tibble with 10,117 rows and 11 columns:
#'  \describe{
#'   \item{ctable}{table code; `14` (the only value in this subset)
#'   denotes the OUTPUT dimension of INDSTAT4}
#'   \item{country}{three-digit UN M49 country code (joins onto
#'   `country_lookup$code`); 133 distinct countries in the full
#'   INDSTAT4 Rev.3 dataset, 5 in this subset}
#'   \item{year}{observation year (1991-2011)}
#'   \item{isic}{4-digit ISIC industry code}
#'   \item{isiccomb}{ISIC code as originally reported -- either the
#'   same as `isic`, or a combined code (containing a letter, e.g.
#'   `"151A"`) covering several `isic` codes at once}
#'   \item{value}{reported output value in USD, masked to `1000` in
#'   this dataset (real values are not shipped); `NA` for `isic`
#'   codes with no directly reported value (i.e. covered only by
#'   another row's `isiccomb`)}
#'   \item{utable}{output valuation methodology, consistent within a
#'   country/year but variable across countries: `12` = factor
#'   prices, `13` = producers' prices, `14` = valuation not defined
#'   (`11` = basic prices does not appear in this subset)}
#'   \item{source}{reporting-status flag (`0`/`1` in this subset;
#'   `0`-`3` in the full dataset) -- exact code meanings are
#'   undocumented upstream, not just unconfirmed here (see reference
#'   below)}
#'   \item{unit}{value unit; always `"$"` (USD) in INDSTAT4, no
#'   national-currency variants}
#'   \item{country_iso3c}{ISO-3c country code, joined from
#'   `country_lookup`}
#'   \item{country_name}{country name, joined from `country_lookup`}
#'  }}
#'  \item{country_lookup}{tibble with 5 rows and 4 columns, a small
#'  lookup table of the 5 countries included in `masked_sample`, used to
#'  join ISO-3c codes and country names onto it:
#'  \describe{
#'   \item{code}{three-digit UN M49 country code, joins onto
#'   `masked_sample$country`}
#'   \item{name}{country name}
#'   \item{iso3c}{ISO-3c country code}
#'   \item{income_group}{World Bank income group classification, 2006
#'   vintage: `"H"` = high income, `"UM"` = upper-middle income,
#'   `"LM"` = lower-middle income (`"L"` = low income does not appear
#'   in this subset)}
#'  }}
#' }
#' @source `masked_sample`: downloaded and parsed from the UNIDO
#' INDSTAT4 website (Rev.3, 2019 vintage). See
#' \url{https://cynthiahqy.github.io/indstat-TPP/001-clean_INDSTAT.html}
#' for the cleaning pipeline this subset was derived from.
#'
#' `country_lookup`: `data-raw/indstat.R` -- hand-built for the 5
#' countries in `masked_sample`. `income_group` is the `2006` column of
#' the World Bank's historical income classification workbook ("Country
#' Analytical History" sheet of `OGHIST.xlsx`), confirmed by
#' cross-referencing current published values for these 5 countries;
#' current download at
#' \url{https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups}
"indstat"
