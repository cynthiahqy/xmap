test_that("new_xmap_diagnosis() works", {
    diag <- new_xmap_diagnosis(
        valid = TRUE,
        details = list(bad_dups = NULL, miss_weight_by = NULL, bad_froms = NULL)
    )
    expect_s3_class(diag, "xmap_diagnosis")
    expect_true(diag$valid)
})

test_that("new_xmap_diagnosis() validates its inputs", {
    expect_error(new_xmap_diagnosis(valid = "yes", details = list()))
    expect_error(new_xmap_diagnosis(valid = TRUE, details = unname(list(1))))
})

test_that("print.xmap_diagnosis() reports a passing diagnosis", {
    diag <- new_xmap_diagnosis(
        valid = TRUE,
        details = list(bad_dups = NULL, miss_weight_by = NULL, bad_froms = NULL)
    )
    expect_message(print(diag), "is valid")
    expect_invisible(print(diag))
})

test_that("print.xmap_diagnosis() reports a failing diagnosis with details", {
    diag <- new_xmap_diagnosis(
        valid = FALSE,
        details = list(
            bad_dups = tibble::tibble(.from = "A1", .to = "x1", .dup = 2L),
            miss_weight_by = NULL,
            bad_froms = NULL
        )
    )
    expect_message(print(diag), "is invalid")
    expect_message(print(diag), "\\(1 row\\)")
    expect_output(print(diag), "A1")
})
