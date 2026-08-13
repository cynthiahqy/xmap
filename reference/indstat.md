# UNIDO INDSTAT4 industrial statistics (masked), with country lookup

A subset of UNIDO's INDSTAT4 industrial-statistics database, with the
reported output `value` masked to a constant, bundled together with a
small country-code lookup table since the two are relationally paired
(`indstat$masked_sample$country` joins onto
`indstat$country_lookup$code`). Used in
[`vignette("extracting-crossmaps-from-scripts")`](https://cynthiahqy.github.io/xmap/articles/extracting-crossmaps-from-scripts.md)
(Case 2) to demonstrate grouped crossmap validation across
`country`/`year`. Some `isic` industry codes are reported only in
combination (`isiccomb`), with a single `value` covering several `isic`
codes at once – the vignette splits these back out.

## Usage

``` r
indstat
```

## Format

### `indstat`

A list with:

- masked_sample:

  tibble with 10,117 rows and 11 columns:

  ctable

  :   table code; `14` (the only value in this subset) denotes the
      OUTPUT dimension of INDSTAT4

  country

  :   three-digit UN M49 country code (joins onto
      `country_lookup$code`); 133 distinct countries in the full
      INDSTAT4 Rev.3 dataset, 5 in this subset

  year

  :   observation year (1991-2011)

  isic

  :   4-digit ISIC industry code

  isiccomb

  :   ISIC code as originally reported – either the same as `isic`, or a
      combined code (containing a letter, e.g. `"151A"`) covering
      several `isic` codes at once

  value

  :   reported output value in USD, masked to `1000` in this dataset
      (real values are not shipped); `NA` for `isic` codes with no
      directly reported value (i.e. covered only by another row's
      `isiccomb`)

  utable

  :   output valuation methodology, consistent within a country/year but
      variable across countries: `12` = factor prices, `13` = producers'
      prices, `14` = valuation not defined (`11` = basic prices does not
      appear in this subset)

  source

  :   reporting-status flag (`0`/`1` in this subset; `0`-`3` in the full
      dataset) – exact code meanings are undocumented upstream, not just
      unconfirmed here (see reference below)

  unit

  :   value unit; always `"$"` (USD) in INDSTAT4, no national-currency
      variants

  country_iso3c

  :   ISO-3c country code, joined from `country_lookup`

  country_name

  :   country name, joined from `country_lookup`

- country_lookup:

  tibble with 5 rows and 4 columns, a small lookup table of the 5
  countries included in `masked_sample`, used to join ISO-3c codes and
  country names onto it:

  code

  :   three-digit UN M49 country code, joins onto
      `masked_sample$country`

  name

  :   country name

  iso3c

  :   ISO-3c country code

  income_group

  :   World Bank income group classification, 2006 vintage: `"H"` = high
      income, `"UM"` = upper-middle income, `"LM"` = lower-middle income
      (`"L"` = low income does not appear in this subset)

## Source

`masked_sample`: downloaded and parsed from the UNIDO INDSTAT4 website
(Rev.3, 2019 vintage). See
<https://cynthiahqy.github.io/indstat-TPP/001-clean_INDSTAT.html> for
the cleaning pipeline this subset was derived from.

`country_lookup`: `data-raw/indstat.R` – hand-built for the 5 countries
in `masked_sample`. `income_group` is the `2006` column of the World
Bank's historical income classification workbook ("Country Analytical
History" sheet of `OGHIST.xlsx`), confirmed by cross-referencing current
published values for these 5 countries; current download at
<https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups>
