demo <- list()

demo$abc_links <- tibble::tribble(
  ~lower , ~upper , ~share ,
  "a"    , "AA"   , 1      , # one-to-one
  "b"    , "BB"   , 1      , # one-FROM-many
  "c"    , "BB"   , 1      ,
  "d"    , "CC"   , 0.3    , # one-to-many
  "d"    , "DD"   , 0.6    ,
  "d"    , "EE"   , 0.1
)

demo$simple_links <- tibble::tribble(
  ~xcode  , ~alphacode , ~weight ,
  "x1111" , "A1"       , 1       ,
  "x2222" , "B2"       , 0.5     ,
  "x2222" , "B3"       , 0.5     ,
  "x3333" , "C5"       , 1       ,
  "x4444" , "C5"       , 1       ,
  "x5555" , "D6"       , 0.4     ,
  "x5555" , "D7"       , 0.6     ,
  "x6666" , "D6"       , 0.3     ,
  "x6666" , "D7"       , 0.7     ,
  "x7777" , "D6"       , 1
)

## Part-to-whole array over simple_links' source keys. Values are varied
## (100-700) rather than flat: with flat values, swapping the weight vectors
## between x5555 and x6666 would leave the output unchanged, so the example
## could not show that which source key carries which weights matters.
demo$simple_stats <- tibble::tribble(
  ~xcode  , ~count ,
  "x1111" ,   100  ,
  "x2222" ,   200  ,
  "x3333" ,   300  ,
  "x4444" ,   400  ,
  "x5555" ,   500  ,
  "x6666" ,   600  ,
  "x7777" ,   700
)

#'  \item{abc_xmap_tbl}{xmap_tbl: lower -> upper BY share.
#'  demo crossmap with 6 links including one-to-one, one-to-many and many-to-one relations.}
# demo$abc_xmap_tbl <-
#   xmap::as_xmap_tbl(from = lower, to = upper, weights = share)

demo$aus_state_pairs <-
  list(
    AUS = c(
      "AU-NSW",
      "AU-QLD",
      "AU-SA",
      "AU-TAS",
      "AU-VIC",
      "AU-WA",
      "AU-ACT",
      "AU-NT"
    )
  ) |>
  tibble::enframe(name = "ctry", value = "state") |>
  tidyr::unnest_longer(col = tidyr::all_of("state")) |>
  dplyr::arrange(state)

demo$aus_state_pop_df <- tibble::tribble(
  ~state_name                    , ~state   , ~pop    ,
  "New South Wales"              , "AU-NSW" , 8153600 ,
  "Victoria"                     , "AU-VIC" , 6613700 ,
  "Queensland"                   , "AU-QLD" , 5322100 ,
  "South Australia"              , "AU-SA"  , 1820500 ,
  "Western Australia"            , "AU-WA"  , 2785300 ,
  "Tasmania"                     , "AU-TAS" ,  571500 ,
  "Northern Territory"           , "AU-NT"  ,  250600 ,
  "Australian Capital Territory" , "AU-ACT" ,  456700
) |>
  dplyr::mutate(ctry = "AUS", .after = state_name) |>
  dplyr::arrange(state)

usethis::use_data(demo, overwrite = TRUE)
