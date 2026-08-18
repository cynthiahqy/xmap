# UNIDO INDSTAT4 industrial statistics (masked), with country lookup

A subset of UNIDO's INDSTAT4 industrial-statistics database, with the
reported output `value` masked to a constant, bundled together with a
small country-code lookup table since the two are relationally paired
(`indstat$masked_sample$country` joins onto
`indstat$country_lookup$code`). Used in
[`vignette("extract-validate-existing")`](https://cynthiahqy.github.io/xmap/articles/extract-validate-existing.md)
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

  tibble with 17,365 rows and 11 columns:

  ctable

  :   table code; `14` (the only value in this subset) denotes the
      OUTPUT dimension of INDSTAT4

  country

  :   three-digit UN M49 country code (joins onto
      `country_lookup$code`); 133 distinct countries in the full
      INDSTAT4 Rev.3 dataset, 8 in this subset

  year

  :   observation year (1990-2013)

  isic

  :   3- or 4-digit ISIC Rev.3 industry code; every 4-digit code nests
      inside the 3-digit code given by its first three digits

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
      variable across countries: `11` = basic prices, `12` = factor
      prices, `13` = producers' prices, `14` = valuation not defined

  source

  :   reporting-status flag (`0`-`3`) – exact code meanings are
      undocumented upstream, not just unconfirmed here (see reference
      below)

  unit

  :   value unit; always `"$"` (USD) in INDSTAT4, no national-currency
      variants

  country_iso3c

  :   ISO-3c country code, joined from `country_lookup`

  country_name

  :   country name, joined from `country_lookup`

- country_lookup:

  tibble with 8 rows and 4 columns, a small lookup table of the 8
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
      income, `"UM"` = upper-middle income, `"LM"` = lower-middle
      income, `"L"` = low income. All four groups are represented in
      this subset

- isic_rev3_lookup:

  tibble with 529 rows and 4 columns, the full ISIC Rev. 3
  classification hierarchy, giving a label for every `isic`/`isiccomb`
  code number used in `masked_sample`:

  code

  :   ISIC Rev. 3 code: a single letter for `"section"` (17 rows),
      otherwise 2/3/4 digits for `"division"`/`"group"`/`"class"`

  level

  :   one of `"section"`, `"division"`, `"group"`, `"class"`, determined
      by the length of `code`

  label

  :   English description of the code

  parent_code

  :   code of the immediate numeric parent – a `"class"` code's first
      three digits (its `"group"`), or a `"group"` code's first two
      digits (its `"division"`). `NA` for `"section"` and `"division"`,
      since sections cover ranges of divisions rather than sharing a
      numeric prefix with them

## Source

`masked_sample`: downloaded and parsed from the UNIDO INDSTAT4 website
(Rev.3, 2019 vintage). See
<https://cynthiahqy.github.io/indstat-TPP/001-clean_INDSTAT.html> for
the cleaning pipeline this subset was derived from.

`country_lookup`: read by `data-raw/indstat.R` from
`data-raw/indstat-country-lookup.csv`, exported alongside
`masked_sample` by the same upstream script. `income_group` is the
`2006` column of the World Bank's historical income classification
workbook ("Country Analytical History" sheet of `OGHIST.xlsx`); current
download at
<https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups>

`isic_rev3_lookup`: UN Statistics Division classifications registry,
ISIC Rev. 3 English structure file, downloaded from
<https://unstats.un.org/unsd/classifications/Econ/Download/In%20Text/ISIC_Rev_3_english_structure.Txt>
into `data-raw/isic_rev3_structure.txt`. See \#34.

## Details

The 8 reporters are five large economies (BRA, CHN, DEU, JPN, USA) plus
three chosen for structurally distinct splitting behaviour once the
split is re-aggregated to 3-digit ISIC: Colombia (splits are entirely
reconvergent – imputed at 4 digits, exact at 3), Romania (the deepest
sustained convergence in the source extract) and Yemen (~95% of `isic`
values sit in splits that cross a 3-digit boundary).
