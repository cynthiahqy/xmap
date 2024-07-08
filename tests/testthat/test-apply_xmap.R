test_that("Tests can access test data", {
    expect_s3_class(simple_links, "data.frame")
    expect_s3_class(simple_data, "data.frame")
})

test_that("apply_xmap() works for single value column", {
    expect_s3_class(apply_xmap(
        .data = simple_data,
        .xmap = simple_xmap,
        values_from = xcode_mass,
        keys_from = xcode
    ), "tbl_df")
})

test_that("coverage check in apply_xmap() works", {
    expect_error(
        apply_xmap(
            .data = simple_data,
            .xmap = simple_xmap[1:3, ],
            values_from = xcode_mass,
            keys_from = xcode
        ),
        class = "coverage_error"
    )
})

test_that("apply_xmap() works for multiple value columns", {
    mod_data <- simple_data |>
        dplyr::mutate(
            xcode_rmass =
                trunc(runif(dplyr::n(), 100, 1000))
        )
    out_obj <- apply_xmap(
        .data = mod_data,
        .xmap = simple_xmap,
        values_from = c(xcode_mass, xcode_rmass),
        keys_from = xcode
    )
    expect_s3_class(
        out_obj,
        class = "data.frame"
    )
    # expect_equal(names(out_obj), c(".to", ".out"))
    expect_equal(
        ncol(mod_data),
        ncol(out_obj)
    )
})

test_that("apply_xmap() detects NAs in value columns", {
    na_data <- simple_data
    na_data$xcode_mass[[3]] <- NA
    browser()
    expect_error(
        apply_xmap(
            .xmap = simple_xmap,
            .data = na_data,
            values_from = xcode_mass,
            keys_from = xcode
        ),
        class = "missing_mass_values"
    )
    expect_warning(
        diagnose_apply_xmap(
            .xmap = simple_xmap,
            .data = na_data,
            values_from = xcode_mass
        ),
        class = "missing_mass_values"
    )
})
