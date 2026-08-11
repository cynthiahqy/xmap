library(dplyr)
library(readr)

occupn_sample <- readr::read_csv("data-raw/occupn-data-sample.csv")
occupn_vector <- occupn_sample |>
  distinct(occupn) 

usethis::use_data(occupn_sample, overwrite = TRUE)
