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

test_that("check_valid_xmap_df() has no internal tol default -- errors if tol is omitted", {
    tbl_x <- tibble::tibble(.from = "a", .to = "b", .weight_by = 1)
    expect_error(
        check_valid_xmap_df(tbl_x),
        regexp = "tol"
    )
})

test_that("a caller that forgets to forward its own tol to check_valid_xmap_df() errors loudly, rather than silently using an unexposed internal default", {
    tbl_x <- tibble::tibble(.from = "a", .to = "b", .weight_by = 1)

    # mocked "outer" user-facing function: has its own tol argument, but
    # forgets to forward it to the shared internal checker
    outer_forgetful <- function(x, tol = .Machine$double.eps^0.5) {
        check_valid_xmap_df(x)
    }
    expect_error(
        outer_forgetful(tbl_x, tol = 0.5),
        regexp = "tol"
    )

    # the correctly-written counterpart succeeds and actually respects the
    # caller-supplied tol
    outer_correct <- function(x, tol = .Machine$double.eps^0.5) {
        check_valid_xmap_df(x, tol = tol)
    }
    expect_true(outer_correct(tbl_x))
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

valid_matrix <- matrix(
    c(1, 0, 0.5, 0.5),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("A1", "A2"), c("x1", "x2"))
)

test_that("validate_as_xmap() is a generic with a matrix method", {
    expect_true(exists("validate_as_xmap.matrix"))
})

test_that("validate_as_xmap.matrix() returns TRUE for a valid matrix", {
    result <- validate_as_xmap(valid_matrix)
    expect_type(result, "logical")
    expect_length(result, 1)
    expect_true(result)
})

test_that("validate_as_xmap.matrix() returns FALSE without dimnames", {
    no_rownames <- valid_matrix
    rownames(no_rownames) <- NULL
    expect_false(validate_as_xmap(no_rownames))

    no_colnames <- valid_matrix
    colnames(no_colnames) <- NULL
    expect_false(validate_as_xmap(no_colnames))
})

test_that("validate_as_xmap.matrix() returns FALSE for non-numeric input", {
    chr_matrix <- valid_matrix
    storage.mode(chr_matrix) <- "character"
    expect_false(validate_as_xmap(chr_matrix))
})

test_that("validate_as_xmap.matrix() returns FALSE for missing cell values", {
    na_matrix <- valid_matrix
    na_matrix[1, 1] <- NA
    expect_false(validate_as_xmap(na_matrix))
})

test_that("validate_as_xmap.matrix() returns FALSE for rows not summing to one", {
    bad_matrix <- valid_matrix
    bad_matrix["A1", ] <- c(0.5, 0.6)
    expect_false(validate_as_xmap(bad_matrix))
})

test_that("validate_as_xmap.matrix() returns FALSE for an all-zero row", {
    zero_row_matrix <- valid_matrix
    zero_row_matrix["A1", ] <- c(0, 0)
    expect_false(validate_as_xmap(zero_row_matrix))
})

test_that("validate_as_xmap.matrix() returns FALSE for duplicate row names", {
    # base R places no uniqueness constraint on dimnames, so this matrix
    # has two independently-valid-looking rows both claiming to be "A1"'s
    # outgoing weights.
    dup_rownames <- matrix(
        c(1, 0, 0.5, 0.5),
        nrow = 2, byrow = TRUE,
        dimnames = list(c("A1", "A1"), c("x1", "x2"))
    )
    expect_false(validate_as_xmap(dup_rownames))
})

test_that("validate_as_xmap.matrix() returns FALSE for duplicate column names", {
    dup_colnames <- matrix(
        c(1, 0, 0, 1),
        nrow = 2, byrow = TRUE,
        dimnames = list(c("A1", "A2"), c("x1", "x1"))
    )
    expect_false(validate_as_xmap(dup_colnames))
})
