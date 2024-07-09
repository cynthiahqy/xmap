## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(xmap)
library(dplyr)

## ----example-aus-agg-01-weights-----------------------------------------------
aus_state_agg_links <- mock$aus_state_pairs |>
  mutate(ones = 1L)

## ----example-aus-agg-02-as-xmap-----------------------------------------------
(agg_xmap <- aus_state_agg_links |>
  as_xmap_tbl(from = state, to = ctry, weight_by = ones)
)

## ----example-aus-agg-03-state-data--------------------------------------------
set.seed(1395)
(aus_state_data <- mock$aus_state_pairs |>
  mutate(
    gdp = runif(n(), 100, 2000),
    ref = 100
  ))

## ----example-aus-agg-04-apply-xmap--------------------------------------------
(aus_ctry_data <- aus_state_data |>
  apply_xmap(
    .xmap = agg_xmap,
    values_from = c(gdp, ref),
    keys_from = state
  )
)

## ----example-aus-agg-05-no-coverage, error=TRUE-------------------------------
## dropping links
agg_xmap[1:3, ]

## will lead to an error!
apply_xmap(
  .data = aus_state_data,
  .xmap = agg_xmap[1:3, ],
  values_from = c(gdp, ref),
  keys_from = state
)

## -----------------------------------------------------------------------------
diagnose_apply_xmap(
  .data = aus_state_data,
  .xmap = agg_xmap[1:3, ],
  values_from = c(gdp, ref)
)

## ---- error=TRUE--------------------------------------------------------------
# add some `NA`
aus_state_data_na <- aus_state_data
aus_state_data_na[c(1, 3, 5), "gdp"] <- NA

apply_xmap(
  .data = aus_state_data_na,
  .xmap = agg_xmap,
  values_from = gdp,
  keys_from = state
)

## ----example-weights-01-equal-------------------------------------------------
mock$aus_state_pairs |>
  group_by(ctry) |>
  mutate(equal = 1 / n_distinct(state)) |>
  ungroup() |>
  as_xmap_tbl(from = ctry, to = state, weight_by = equal)

## ----example-weights-02-invalid, error = TRUE---------------------------------
mock$aus_state_pairs |>
  mutate(ones = 1) |>
  as_xmap_tbl(from = ctry, to = state, weight_by = ones)

## ----example-weights-02-ref---------------------------------------------------
(split_xmap_pop <- mock$aus_state_pop_df |>
  group_by(ctry) |>
  mutate(pop_share = pop / sum(pop)) |>
  ungroup() |>
  as_xmap_tbl(
    from = ctry, to = state, weight_by = pop_share
  ))

## ----example-weights-03-apply-pop---------------------------------------------
aus_state_data2 <- aus_ctry_data |>
  mutate(ref = 10000) |>
  apply_xmap(split_xmap_pop,
    values_from = c(gdp, ref),
    keys_from = ctry
  )

## ----echo=FALSE---------------------------------------------------------------
left_join(
  aus_state_data2,
  tidyr::unpack(split_xmap_pop, .to),
  join_by(state)
) |>
  select(.from, everything())

