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

test_that("xmap_tbl() and diagnose_xmap_links() pick up duplicate links", {
    tfrom <- tibble::tibble(source = c("A1", "A1", "A2"))
    tto <- tibble::tibble(target = c("x1", "x1", "x2"))
    twgts <- tibble::tibble(weight_by = c(1L, 1L, 1L))
    links <- tibble::tibble(.from = tfrom, .to = tto, .weight_by = twgts)
    expect_error(
        xmap_tbl(links$.from, links$.to),
        class = "abort_dup_pairs"
    )
    expect_warning(
        diagnose_as_xmap_tbl(links, .from, .to, .weight_by)
    )
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
    expect_warning(
        diagnose_as_xmap_tbl(links, source, target, weight_by),
        class = "missing_weight_by"
    )
})

if (FALSE) {
    read.csv("test.csv", stringsAsFactors = TRUE) |>
        as_xmap_tbl(xcode, alphacode, weight)
}
