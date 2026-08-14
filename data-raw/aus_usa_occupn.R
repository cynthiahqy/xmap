## Real ANZSCO/SOC2010 <-> ISCO-08 occupation crosswalk data for the
## "creating-crossmap-weights" vignette (Case 1, #14/#26). Source files are
## the finalised ISCO-08 Sub-Major Group 11 subset assembled in
## cynthiahqy/example_aus-us-occupn (see that repo's data/subset/ and
## README "Download provenance" table for full retrieval details).
##
## Note the vintage fix from #14's original discussion: BLS's operational
## classification for the vignette's 2016 reference year is SOC **2010**,
## not SOC 2018.
##
## Crosswalk columns are renamed from source (soc2010_code/isco08_code/
## partial_match/...) to a short form (soc2010/anzsco22/isco8/partial/...)
## so both crosswalks share a naming convention and the vignette's helper
## code (equal_split(), as_xmap_tbl(from = ..., to = ...)) reads cleanly.
## Definitions tables keep their source column names.

anzsco_isco8_crosswalk <-
  readr::read_csv("data-raw/aus_usa_occupn_anzsco_to_isco08_crosswalk.csv") |>
  dplyr::rename(
    anzsco22 = anzsco_code,
    anzsco22_descr = anzsco_title,
    isco8 = isco08_code,
    partial = partial_match,
    isco8_descr = isco08_title
  ) |>
  dplyr::mutate(anzsco22 = as.character(anzsco22), isco8 = as.character(isco8))

soc2010_isco8_crosswalk <-
  readr::read_csv("data-raw/aus_usa_occupn_soc2010_to_isco08_crosswalk.csv") |>
  dplyr::rename(
    soc2010 = soc2010_code,
    soc2010_descr = soc2010_title,
    isco8 = isco08_code,
    partial = partial_match,
    isco8_descr = isco08_title
  ) |>
  dplyr::mutate(isco8 = as.character(isco8))

anzsco_definitions <-
  readr::read_csv("data-raw/aus_usa_occupn_anzsco1.2_definitions.csv")

soc2010_definitions <-
  readr::read_csv("data-raw/aus_usa_occupn_soc2010_definitions.csv")

isco8_definitions <-
  readr::read_csv("data-raw/aus_usa_occupn_isco08_definitions.csv")

## Stylised occupation counts, re-keyed to the SOC 2010 codes that actually
## appear in soc2010_isco8_crosswalk above (the codes inherited from
## demo$soc2018_stats -- "11-9151", "55-1019" -- don't exist in the real
## crosswalk and would silently drop out of apply_xmap()'s join).
## Illustrative numbers, NOT real BLS counts -- real count data is a
## separate, not-yet-sourced item, see the #14 checklist.
soc2010_stats <- tibble::tribble(
  ~soc2010, ~count,
  "11-1011", 4000,
  "11-1021", 12000,
  "11-1031", 200,
  "11-9161", 150,
  "11-2031", 900,
  "11-9199", 300
)

## Stylised occupation counts (moved from demo$anzsco22_stats, re-keyed to
## the real anzsco_isco8_crosswalk's source codes -- "111212" from the old
## placeholder doesn't exist in the real crosswalk; "139999" does).
## Illustrative numbers, NOT real ABS counts.
anzsco_stats <- tibble::tribble(
  ~anzsco22, ~count,
  "111111", 1000,
  "111211", 500,
  "111311", 300,
  "111312", 150,
  "111399", 10,
  "139999", 60
)

aus_usa_occupn <- list(
  anzsco_isco8_crosswalk = anzsco_isco8_crosswalk,
  soc2010_isco8_crosswalk = soc2010_isco8_crosswalk,
  anzsco_definitions = anzsco_definitions,
  soc2010_definitions = soc2010_definitions,
  isco8_definitions = isco8_definitions,
  anzsco_stats = anzsco_stats,
  soc2010_stats = soc2010_stats
)

usethis::use_data(aus_usa_occupn, overwrite = TRUE)
