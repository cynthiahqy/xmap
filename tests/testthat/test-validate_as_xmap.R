test_that("validate_as_xmap() is a generic with a data.frame method", {
    expect_true(is.function(validate_as_xmap))
    expect_true(exists("validate_as_xmap.data.frame"))
})

test_that("validate_as_xmap.data.frame() returns TRUE for valid links", {
    result <- validate_as_xmap(simple_links, xcode, alphacode, weight)
    expect_type(result, "logical")
    expect_length(result, 1)
    expect_true(result)
})

test_that("validate_as_xmap.data.frame() returns FALSE for duplicate pairs", {
    tfrom <- tibble::tibble(source = c("A1", "A1", "A2"))
    tto <- tibble::tibble(target = c("x1", "x1", "x2"))
    twgts <- tibble::tibble(weight_by = c(1L, 1L, 1L))
    links <- tibble::tibble(.from = tfrom, .to = tto, .weight_by = twgts)

    expect_false(validate_as_xmap(links, .from, .to, .weight_by))
})

test_that("validate_as_xmap.data.frame() returns FALSE for missing weights", {
    links <- tibble::tibble(
        source = c("A1", "A2", "A3"),
        target = c("x1", "x2", "x3"),
        weight_by = c(1L, NA, 1L)
    )

    expect_false(validate_as_xmap(links, source, target, weight_by))
})

test_that("validate_as_xmap.data.frame() returns FALSE when weights don't sum to one", {
    links <- tibble::tibble(
        source = c("A1", "A2"),
        target = c("x1", "x2"),
        weight_by = c(0.5, 1)
    )

    expect_false(validate_as_xmap(links, source, target, weight_by))
})
