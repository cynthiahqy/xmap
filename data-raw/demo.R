demo <- list()

demo$ctr_iso3c_pairs <- countrycode::codelist |>
  dplyr::select(iso3c, iso.name.en) |>
  tidyr::drop_na()

# demo_named$collapse_list <- list(MAMM = c("elephant", "whale", "monkey"),
#                       REPT = c("lizard", "turtle"),
#                       CRUS = c("crab"))

#'  \item{anzsco21_pairs}{tibble with 51 rows and 4 columns. Contains major and submajor occupation codes and descriptions for ANZSCO21. Retrieved from `strayr::anzsco2021` via:
#'    \url{https://github.com/runapp-aus/strayr}}
# demo$anzsco21_pairs <- strayr::anzsco2021 |>
#   dplyr::select(tidyselect::starts_with(
#     c("anzsco_major", "anzsco_submajor")
#   )) |>
#   dplyr::distinct() |>
#   dplyr::select(tidyselect::ends_with("_code"), tidyselect::everything())

demo$anzsco22_isco8_crosswalk <- tibble::tribble(
  ~anzsco22, ~anzsco22_descr, ~isco8, ~partial, ~isco8_descr,
  "111111", "Chief Executive or Managing Director", "1112", "p", "Senior government officials",
  "111111", "Chief Executive or Managing Director", "1114", "p", "Senior officials of special-interest organizations",
  "111111", "Chief Executive or Managing Director", "1120", "p", "Managing directors and chief executives",
  "111211", "Corporate General Manager", "1112", "p", "Senior government officials",
  "111211", "Corporate General Manager", "1114", "p", "Senior officials of special-interest organizations",
  "111211", "Corporate General Manager", "1120", "p", "Managing directors and chief executives",
  "111212", "Defence Force Senior Officer", "0110", "p", "Commissioned armed forces officers",
  "111311", "Local Government Legislator", "1111", "p", "Legislators",
  "111312", "Member of Parliament", "1111", "p", "Legislators",
  "111399", "Legislators nec", "1111", "p", "Legislators"
)

demo$anzsco22_stats <-
  tibble::tribble(
    ~anzsco22, ~count,
    111111, 1000,
    111211, 500,
    111212, 40,
    111311, 300,
    111312, 150,
    111399, 10
  ) |>
  dplyr::mutate(anzsco22 = as.character(anzsco22))

demo$abc_links <- tibble::tribble(
  ~lower, ~upper, ~share,
  "a", "AA", 1, # one-to-one
  "b", "BB", 1, # one-FROM-many
  "c", "BB", 1,
  "d", "CC", 0.3, # one-to-many
  "d", "DD", 0.6,
  "d", "EE", 0.1
)

demo$simple_links <- tibble::tribble(
  ~xcode, ~alphacode, ~weight,
  "x1111", "A1", 1,
  "x2222", "B2", 0.5,
  "x2222", "B3", 0.5,
  "x3333", "C5", 1,
  "x4444", "C5", 1,
  "x5555", "D6", 0.4,
  "x5555", "D7", 0.6,
  "x6666", "D6", 0.3,
  "x6666", "D7", 0.7,
  "x7777", "D6", 1
)

#'  \item{abc_xmap_tbl}{xmap_tbl: lower -> upper BY share.
#'  demo crossmap with 6 links including one-to-one, one-to-many and many-to-one relations.}
# demo$abc_xmap_tbl <-
#   xmap::as_xmap_tbl(from = lower, to = upper, weights = share)

demo$aus_state_pairs <-
  list(AUS = c(
    "AU-NSW", "AU-QLD", "AU-SA", "AU-TAS",
    "AU-VIC", "AU-WA", "AU-ACT", "AU-NT"
  )) |>
  tibble::enframe(name = "ctry", value = "state") |>
  tidyr::unnest_longer(col = tidyr::all_of("state")) |>
  dplyr::arrange(state)

demo$aus_state_pop_df <- tibble::tribble(
  ~state_name, ~state, ~pop,
  "New South Wales", "AU-NSW", 8153600,
  "Victoria", "AU-VIC", 6613700,
  "Queensland", "AU-QLD", 5322100,
  "South Australia", "AU-SA", 1820500,
  "Western Australia", "AU-WA", 2785300,
  "Tasmania", "AU-TAS", 571500,
  "Northern Territory", "AU-NT", 250600,
  "Australian Capital Territory", "AU-ACT", 456700
) |>
  dplyr::mutate(ctry = "AUS", .after = state_name) |>
  dplyr::arrange(state)

usethis::use_data(demo, overwrite = TRUE)
