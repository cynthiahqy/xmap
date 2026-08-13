test_that("Tests can access test data", {
    expect_s3_class(simple_links, "data.frame")
})

test_that("new_xmap_tbl() works", {
    expect_s3_class(
        new_xmap_tbl(list(
            .from = simple_links["xcode"],
            .to = simple_links["alphacode"],
            .weight_by = simple_links["weight"]
        )),
        "xmap_tbl"
    )
})

test_that("xmap_tbl() works", {
    expect_s3_class(
        xmap_tbl(
            simple_links["xcode"],
            simple_links["alphacode"],
            simple_links["weight"]
        ),
        "xmap_tbl"
    )
})

test_that("xmap_tbl() works without weight_by", {
    expect_error(
        xmap_tbl(
            simple_links["xcode"],
            simple_links["alphacode"]
        ),
        class = "abort_bad_weight_by"
    )
})

test_that("as_xmap_tbl() works", {
    expect_s3_class(
        as_xmap_tbl(
            simple_links,
            xcode, alphacode, weight
        ),
        "xmap_tbl"
    )
})

test_that("Abort if selecting multiple columns in as_xmap_tbl()", {
    expect_error(
        as_xmap_tbl(simple_links, c(xcode, alphacode), alphacode, weight)
    )
})

test_that("xmap_tbl() rejects non-data-frame inputs", {
    expect_error(
        xmap_tbl(simple_links$xcode, simple_links$alphacode)
    )
})

test_that("xmap_tbl() picks up bad weight_by", {
    expect_error(
        xmap_tbl(
            simple_links["alphacode"],
            simple_links["xcode"],
            simple_links["weight"]
        ),
        class = "abort_bad_weight_by"
    )
})

test_that("xmap_tbl() and diagnose_as_xmap_tbl() pick up duplicate links", {
    tfrom <- tibble::tibble(source = c("A1", "A1", "A2"))
    tto <- tibble::tibble(target = c("x1", "x1", "x2"))
    twgts <- tibble::tibble(weight_by = c(1L, 1L, 1L))
    links <- tibble::tibble(.from = tfrom, .to = tto, .weight_by = twgts)
    expect_error(
        xmap_tbl(links$.from, links$.to),
        class = "abort_dup_pairs"
    )

    diagnostics <- diagnose_as_xmap_tbl(links, .from, .to, .weight_by)
    expect_s3_class(diagnostics, "xmap_diagnosis")
    expect_false(diagnostics$valid)
    expect_equal(nrow(diagnostics$details$bad_dups), 1)
    expect_null(diagnostics$details$miss_weight_by)
})

test_that("xmap_tbl() pick up missing weight_by", {
    links <- tibble::tibble(
        source = c("A1", "A2", "A3"),
        target = c("x1", "x2", "x3"),
        weight_by = c(1L, NA, 1L)
    )
    expect_error(
        as_xmap_tbl(links, source, target, weight_by),
        class = "missing_weight_by"
    )

    diagnostics <- diagnose_as_xmap_tbl(links, source, target, weight_by)
    expect_s3_class(diagnostics, "xmap_diagnosis")
    expect_false(diagnostics$valid)
    expect_equal(nrow(diagnostics$details$miss_weight_by), 1)
    expect_null(diagnostics$details$bad_dups)
})

test_that("diagnose_as_xmap_tbl() reports a valid xmap as passing", {
    diagnostics <- diagnose_as_xmap_tbl(simple_links, xcode, alphacode, weight)
    expect_s3_class(diagnostics, "xmap_diagnosis")
    expect_true(diagnostics$valid)
    expect_true(all(vapply(diagnostics$details, is.null, logical(1))))
})

test_that("diagnose_grouped_xmap_tbl() requires a grouped data frame", {
    expect_error(
        diagnose_grouped_xmap_tbl(simple_links, xcode, alphacode, weight)
    )
})

test_that("diagnose_grouped_xmap_tbl() diagnoses each group separately", {
    stacked_links <- dplyr::bind_rows(
        dplyr::mutate(simple_links, set = "valid"),
        dplyr::mutate(simple_links, set = "invalid")
    )
    stacked_links$weight[
        stacked_links$set == "invalid" & stacked_links$xcode == "x1111"
    ] <- 0.5

    out <- stacked_links |>
        dplyr::group_by(set) |>
        diagnose_grouped_xmap_tbl(from = xcode, to = alphacode, weight_by = weight)

    expect_s3_class(out, "tbl_df")
    expect_setequal(names(out), c("set", "data", "valid", "diagnosis"))
    expect_equal(nrow(out), 2)

    valid_row <- out[out$set == "valid", ]
    expect_true(valid_row$valid)
    expect_s3_class(valid_row$diagnosis[[1]], "xmap_diagnosis")
    expect_true(valid_row$diagnosis[[1]]$valid)

    invalid_row <- out[out$set == "invalid", ]
    expect_false(invalid_row$valid)
    expect_false(invalid_row$diagnosis[[1]]$valid)
    expect_equal(nrow(invalid_row$diagnosis[[1]]$details$bad_froms), 1)
})

if (FALSE) {
    read.csv("test.csv", stringsAsFactors = TRUE) |>
        as_xmap_tbl(xcode, alphacode, weight)
}
