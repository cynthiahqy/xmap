library(dplyr)
library(readr)

## timor_occupn is a ~1% sample of the full 1,179,654-row individual-level
## census extract, grouped by `occupn` and sampled with dplyr, so the set of
## occupation codes present is closer to fully represented than a plain
## random sample of individuals would give -- see data-raw/occupn-data-sample.csv
timor_occupn <- readr::read_csv("data-raw/occupn-data-sample.csv")
occupn_vector <- timor_occupn |>
  distinct(occupn)

usethis::use_data(timor_occupn, overwrite = TRUE)
