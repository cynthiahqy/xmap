test_that("indstat$masked_sample has no unmapped country -> iso3c/name joins", {
    expect_false(anyNA(indstat$masked_sample$country_iso3c))
    expect_false(anyNA(indstat$masked_sample$country_name))
})

test_that("every masked_sample$country appears in country_lookup$code", {
    expect_true(all(indstat$masked_sample$country %in% indstat$country_lookup$code))
})
