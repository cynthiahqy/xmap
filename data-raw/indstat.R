## Both source CSVs are exported by `xmap-example/xmap-example.R` in
## https://github.com/cynthiahqy/conformr-indstat -- the country lookup is now
## read from `indstat-country-lookup.csv` rather than hardcoded here, so the
## two stay in sync when the sample changes.

indstat_country_lookup <-
  readr::read_csv(
    "data-raw/indstat-country-lookup.csv",
    col_types = readr::cols(.default = readr::col_character())
  )

indstat_masked_sample <-
  readr::read_csv("data-raw/indstat_rev3_masked_subset.csv") |>
  dplyr::inner_join(
    indstat_country_lookup[c("code", "iso3c", "name")],
    by = dplyr::join_by(country == code),
    unmatched = c("error", "drop")
  ) |>
  dplyr::rename(country_name = name, country_iso3c = iso3c)

## ISIC Rev. 3 classification hierarchy, giving a label for every
## isic/isiccomb code number used in masked_sample. Source: UN Statistics
## Division classifications registry, ISIC Rev. 3 English structure file,
## downloaded from
## https://unstats.un.org/unsd/classifications/Econ/Download/In%20Text/ISIC_Rev_3_english_structure.Txt
## into data-raw/isic_rev3_structure.txt (fixed-width: code padded to 14
## characters, then description; CRLF line endings). See #34.
isic_rev3_raw <-
  readr::read_fwf(
    "data-raw/isic_rev3_structure.txt",
    col_positions = readr::fwf_widths(c(14, NA), col_names = c("code", "label")),
    col_types = readr::cols(.default = readr::col_character()),
    skip = 1
  ) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), stringr::str_trim))

## ISIC Rev. 3 hierarchy: 1-letter sections, then 2/3/4-digit
## divisions/groups/classes. Level and parent code follow directly from
## code length/prefix, matching the `isic`/`isic3` composition used in
## vignette("examine-compose-crossmaps").
isic_rev3_lookup <- isic_rev3_raw |>
  dplyr::mutate(
    level = dplyr::case_when(
      stringr::str_length(code) == 1 ~ "section",
      stringr::str_length(code) == 2 ~ "division",
      stringr::str_length(code) == 3 ~ "group",
      stringr::str_length(code) == 4 ~ "class"
    ),
    parent_code = dplyr::case_when(
      level == "section" ~ NA_character_,
      level == "division" ~ NA_character_, # sections aren't numeric-prefixed
      level == "group" ~ stringr::str_sub(code, 1, 2),
      level == "class" ~ stringr::str_sub(code, 1, 3)
    )
  ) |>
  dplyr::select(code, level, label, parent_code)

indstat <- list(
  masked_sample = indstat_masked_sample,
  country_lookup = indstat_country_lookup,
  isic_rev3_lookup = isic_rev3_lookup
)

usethis::use_data(indstat, overwrite = TRUE)
