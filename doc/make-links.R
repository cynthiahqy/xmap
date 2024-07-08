## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(xmap)
library(dplyr)

## ----example-aus-agg----------------------------------------------------------
aus_state_agg_links <- mock$aus_state_pairs |>
  mutate(ones = 1L)

## -----------------------------------------------------------------------------
aus_state_split_equal <- mock$aus_state_pairs |>
  group_by(ctry) |>
  mutate(equal = 1 / n_distinct(state)) |>
  ungroup()
aus_state_split_by_pop <- mock$aus_state_pop_df |>
  group_by(ctry) |>
  mutate(pop_share = pop / sum(pop)) |>
  ungroup()

