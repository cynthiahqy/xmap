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
#' `vignette("extract-validate-existing")` (Case 1) to
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
#' `vignette("extract-validate-existing")` (Case 2) to
#' demonstrate grouped crossmap validation across `country`/`year`. Some
#' `isic` industry codes are reported only in combination (`isiccomb`),
#' with a single `value` covering several `isic` codes at once -- the
#' vignette splits these back out.
#'
#' The 8 reporters are five large economies (BRA, CHN, DEU, JPN, USA)
#' plus three chosen for structurally distinct splitting behaviour once
#' the split is re-aggregated to 3-digit ISIC: Colombia (splits are
#' entirely reconvergent -- imputed at 4 digits, exact at 3), Romania
#' (the deepest sustained convergence in the source extract) and Yemen
#' (~95% of `isic` values sit in splits that cross a 3-digit boundary).
#'
#' @format ## `indstat`
#' A list with:
#' \describe{
#'  \item{masked_sample}{tibble with 17,365 rows and 11 columns:
#'  \describe{
#'   \item{ctable}{table code; `14` (the only value in this subset)
#'   denotes the OUTPUT dimension of INDSTAT4}
#'   \item{country}{three-digit UN M49 country code (joins onto
#'   `country_lookup$code`); 133 distinct countries in the full
#'   INDSTAT4 Rev.3 dataset, 8 in this subset}
#'   \item{year}{observation year (1990-2013)}
#'   \item{isic}{3- or 4-digit ISIC Rev.3 industry code; every 4-digit
#'   code nests inside the 3-digit code given by its first three
#'   digits}
#'   \item{isiccomb}{ISIC code as originally reported -- either the
#'   same as `isic`, or a combined code (containing a letter, e.g.
#'   `"151A"`) covering several `isic` codes at once}
#'   \item{value}{reported output value in USD, masked to `1000` in
#'   this dataset (real values are not shipped); `NA` for `isic`
#'   codes with no directly reported value (i.e. covered only by
#'   another row's `isiccomb`)}
#'   \item{utable}{output valuation methodology, consistent within a
#'   country/year but variable across countries: `11` = basic prices,
#'   `12` = factor prices, `13` = producers' prices, `14` = valuation
#'   not defined}
#'   \item{source}{reporting-status flag (`0`-`3`) -- exact code meanings are
#'   undocumented upstream, not just unconfirmed here (see reference
#'   below)}
#'   \item{unit}{value unit; always `"$"` (USD) in INDSTAT4, no
#'   national-currency variants}
#'   \item{country_iso3c}{ISO-3c country code, joined from
#'   `country_lookup`}
#'   \item{country_name}{country name, joined from `country_lookup`}
#'  }}
#'  \item{country_lookup}{tibble with 8 rows and 4 columns, a small
#'  lookup table of the 8 countries included in `masked_sample`, used to
#'  join ISO-3c codes and country names onto it:
#'  \describe{
#'   \item{code}{three-digit UN M49 country code, joins onto
#'   `masked_sample$country`}
#'   \item{name}{country name}
#'   \item{iso3c}{ISO-3c country code}
#'   \item{income_group}{World Bank income group classification, 2006
#'   vintage: `"H"` = high income, `"UM"` = upper-middle income,
#'   `"LM"` = lower-middle income, `"L"` = low income. All four groups
#'   are represented in this subset}
#'  }}
#' }
#' @source `masked_sample`: downloaded and parsed from the UNIDO
#' INDSTAT4 website (Rev.3, 2019 vintage). See
#' \url{https://cynthiahqy.github.io/indstat-TPP/001-clean_INDSTAT.html}
#' for the cleaning pipeline this subset was derived from.
#'
#' `country_lookup`: read by `data-raw/indstat.R` from
#' `data-raw/indstat-country-lookup.csv`, exported alongside
#' `masked_sample` by the same upstream script. `income_group` is the
#' `2006` column of the World Bank's historical income classification
#' workbook ("Country Analytical History" sheet of `OGHIST.xlsx`);
#' current download at
#' \url{https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups}
"indstat"

#' AUS/USA occupation classification crosswalks to ISCO-08
#'
#' Real, published national-classification-to-ISCO-08 correspondence data
#' for ISCO-08 Sub-Major Group 11 ("Chief Executives, Senior Officials and
#' Legislators"), covering Australia (ANZSCO) and the USA (SOC 2010). Used
#' in `vignette("creating-crossmap-weights")` (Case 1) to illustrate
#' turning an unweighted, many-to-many published correspondence into a
#' crossmap by supplying weights via committee judgment, since the source
#' correspondences carry no split of their own.
#'
#' Note the SOC vintage: this uses **SOC 2010**, not SOC 2018 -- SOC 2010
#' was BLS's operational classification through 2018 (SOC 2018 was first
#' used for the May 2019 OEWS release), so it is the correct vintage for
#' the ~2016 reference period the vignette illustrates.
#'
#' Retrieved and subsetted in
#' \url{https://github.com/cynthiahqy/example_aus-us-occupn} (see that
#' repo's README "Download provenance" table for full retrieval URLs and
#' dates); see `data-raw/aus_usa_occupn.R` for how the subset was
#' assembled into this package.
#'
#' @format ## `aus_usa_occupn`
#' A list with:
#' \describe{
#'  \item{anzsco_isco8_crosswalk}{tibble with 11 rows and 5 columns: real
#'  subset of ABS's ANZSCO 2013 v1.2 -> ISCO-08 v2 correspondence, 6
#'  ANZSCO codes mapping to 5 ISCO-08 codes. One row has `anzsco22 = NA`
#'  (ISCO-08 `1113` "Traditional chiefs and heads of villages" has no
#'  ANZSCO correspondent at all in ABS's table -- an intentional
#'  no-match, not a missing value)}
#'  \item{soc2010_isco8_crosswalk}{tibble with 11 rows and 6 columns: real
#'  subset of BLS's published ISCO-08 x SOC 2010 crosswalk, 6 SOC codes
#'  mapping to 5 ISCO-08 codes}
#'  \item{anzsco_definitions}{tibble with 6 rows and 16 columns:
#'  ANZSCO unit-group definitions and hierarchy (title, definition,
#'  specialisations, skill level, minor/submajor/major group) for the
#'  ANZSCO codes above -- hand-transcribed from individual ABS "Unit
#'  Group" web pages (ABS does not publish these as a structured
#'  download); see each row's `source_url`}
#'  \item{soc2010_definitions}{tibble with 6 rows and 3 columns: BLS's
#'  SOC 2010 detailed-occupation titles and definitions for the SOC
#'  codes above}
#'  \item{isco8_definitions}{tibble with 5 rows and 7 columns: ILO's
#'  ISCO-08 unit-group titles, definitions, and included/excluded
#'  occupations for the ISCO-08 codes above}
#'  \item{anzsco_stats}{tibble with 6 rows and 2 columns. Stylised
#'  occupation counts for the (non-`NA`) ANZSCO codes above -- illustrative
#'  round numbers, NOT real published ABS statistics. Real 2016 Census
#'  counts at this 6-digit ANZSCO level exist (ABS Census TableBuilder),
#'  but can't be shipped here: TableBuilder output is governed by ABS's
#'  Conditions of Use, not the CC BY 4.0 licence covering the crosswalk
#'  data above, and redistributing derived output in an open-source
#'  package isn't clearly permitted without separate ABS confirmation --
#'  see \url{https://github.com/cynthiahqy/example_aus-us-occupn/issues/4}}
#'  \item{soc2010_stats}{tibble with 6 rows and 2 columns. Stylised
#'  occupation counts for the SOC codes above -- illustrative round
#'  numbers, NOT real published BLS statistics}
#' }
#' @source `anzsco_isco8_crosswalk`, `anzsco_definitions`: Australian
#' Bureau of Statistics, ANZSCO 2013 Version 1.2, correspondence to
#' ISCO-08 v2 and unit-group definitions;
#' \url{https://www.abs.gov.au/statistics/classifications/anzsco-australian-and-new-zealand-standard-classification-occupations}
#'
#' `soc2010_isco8_crosswalk`, `soc2010_definitions`: US Bureau of Labor
#' Statistics, 2010 SOC Crosswalks (ISCO-08 x SOC 2010);
#' \url{https://www.bls.gov/soc/soccrosswalks.htm}
#'
#' `isco8_definitions`: International Labour Organization, ISCO-08;
#' \url{https://isco-ilo.netlify.app}
#'
#' Licence: ABS content is published under Creative Commons Attribution
#' 4.0 International. BLS content is a US federal government work,
#' presumptively public domain under 17 U.S.C. section 105. ISCO-08
#' (ILO) is "available online free of charge and can be used without
#' prior authorization" -- confirmed at
#' \url{https://isco-ilo.netlify.app/en/faq/}, no formal named licence.
"aus_usa_occupn"
