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
  dplyr::left_join(
    indstat_country_lookup[c("code", "iso3c", "name")],
    by = dplyr::join_by(country == code)
  ) |>
  dplyr::rename(country_name = name, country_iso3c = iso3c)

indstat <- list(
  masked_sample = indstat_masked_sample,
  country_lookup = indstat_country_lookup
)

usethis::use_data(indstat, overwrite = TRUE)
