diagnosis_labels <- list(
    bad_dups = c(pass = "No duplicate `.from`-`.to` pairs", fail = "Duplicate `.from`-`.to` pairs"),
    miss_weight_by = c(pass = "No missing values in `.weight_by`", fail = "Missing values in `.weight_by`"),
    bad_froms = c(
        pass = "Sum of `.weight_by` by `.from` are near enough to one",
        fail = "Sum of `.weight_by` by `.from` are not near enough to one"
    )
)

test_that("new_xmap_diagnosis() works", {
    diagnostics <- new_xmap_diagnosis(
        valid = TRUE,
        details = list(bad_dups = NULL, miss_weight_by = NULL, bad_froms = NULL),
        labels = diagnosis_labels
    )
    expect_s3_class(diagnostics, "xmap_diagnosis")
    expect_true(diagnostics$valid)
})

test_that("new_xmap_diagnosis() validates its inputs", {
    expect_error(new_xmap_diagnosis(valid = "yes", details = list(), labels = list()))
    expect_error(new_xmap_diagnosis(
        valid = TRUE,
        details = unname(list(1)),
        labels = list(a = c(pass = "A", fail = "B"))
    ))
    expect_error(new_xmap_diagnosis(
        valid = TRUE,
        details = list(a = NULL, b = NULL),
        labels = list(a = c(pass = "A", fail = "B"))
    ))
    # labels must be pass/fail pairs, not bare strings
    expect_error(new_xmap_diagnosis(
        valid = TRUE,
        details = list(a = NULL),
        labels = list(a = "A")
    ))
})

test_that("new_xmap_diagnosis() supports representation-specific subclasses", {
    diagnostics <- new_xmap_diagnosis(
        valid = TRUE,
        details = list(bad_dups = NULL, miss_weight_by = NULL, bad_froms = NULL),
        labels = diagnosis_labels,
        class = "xmap_diagnosis_tbl"
    )
    expect_s3_class(diagnostics, c("xmap_diagnosis_tbl", "xmap_diagnosis"))
})

test_that("print.xmap_diagnosis() reports a passing diagnosis", {
    diagnostics <- new_xmap_diagnosis(
        valid = TRUE,
        details = list(bad_dups = NULL, miss_weight_by = NULL, bad_froms = NULL),
        labels = diagnosis_labels
    )
    expect_message(print(diagnostics), "is valid")
    expect_message(print(diagnostics), "No duplicate")
    expect_invisible(print(diagnostics))
})

test_that("print.xmap_diagnosis() reports a failing diagnosis with details", {
    diagnostics <- new_xmap_diagnosis(
        valid = FALSE,
        details = list(
            bad_dups = tibble::tibble(.from = "A1", .to = "x1", .dup = 2L),
            miss_weight_by = NULL,
            bad_froms = NULL
        ),
        labels = diagnosis_labels
    )
    expect_message(print(diagnostics), "is invalid")
    expect_message(print(diagnostics), "Duplicate `.from`-`.to` pairs \\(1 row\\)")
    expect_message(print(diagnostics), "No missing values")
    expect_output(print(diagnostics), "A1")
})
