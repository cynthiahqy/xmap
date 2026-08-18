## Source: UN Statistics Division classifications registry, ISIC Rev. 3
## English structure file (code + description), downloaded from
## https://unstats.un.org/unsd/classifications/Econ/Download/In%20Text/ISIC_Rev_3_english_structure.Txt
## into data-raw/isic_rev3_structure.txt (fixed-width: code padded to 14
## characters, then description; CRLF line endings).
##
## See #34 for context: this gives `isic`/`isiccomb` code numbers used
## throughout the indstat sample and examine-crossmaps vignette (#32) a
## human-readable label.

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
## code length/prefix, matching the `isic`/`isic3` composition already used
## in vignette("examine-crossmaps").
isic_rev3 <- isic_rev3_raw |>
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

usethis::use_data(isic_rev3, overwrite = TRUE)
