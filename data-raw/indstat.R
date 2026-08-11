country_lookup <- 
  structure(list(code = c("076", "156", "276", "392", "840"), name = c("Brazil", 
"China", "Germany", "Japan", "United States of America"), iso3c = c("BRA", 
"CHN", "DEU", "JPN", "USA"), income_group = c("UM", "LM", "H", 
"H", "H")), row.names = c(NA, -5L), class = "data.frame") |> tibble::as_tibble()

indstat_masked <- 
  readr::read_csv("data-raw/indstat_rev3_masked_subset.csv") |>
  dplyr::left_join(country_lookup[c("code", "iso3c", "name")], by = join_by(country == code)) |>
  dplyr::rename(country_name = name, country_iso3c = iso3c)

usethis::use_data(indstat_masked)
