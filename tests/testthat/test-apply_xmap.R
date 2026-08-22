test_that("Tests can access test data", {
  expect_s3_class(simple_links, "data.frame")
  expect_s3_class(simple_data, "data.frame")
})

test_that("apply_xmap() works for single value column", {
  expect_s3_class(
    apply_xmap(
      .data = simple_data,
      .xmap = simple_xmap,
      values_from = xcode_mass,
      keys_from = xcode
    ),
    "tbl_df"
  )
})

test_that("diagnose_apply_xmap() reports conformable data as passing", {
  diagnostics <- diagnose_apply_xmap(
    .data = simple_data,
    .xmap = simple_xmap,
    values_from = xcode_mass,
    keys_from = xcode
  )
  expect_s3_class(diagnostics, "xmap_diagnosis")
  expect_true(diagnostics$valid)
  expect_true(all(vapply(diagnostics$details, is.null, logical(1))))
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
      xcode_rmass = trunc(runif(dplyr::n(), 100, 1000))
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
  expect_error(
    apply_xmap(
      .xmap = simple_xmap,
      .data = na_data,
      values_from = xcode_mass,
      keys_from = xcode
    ),
    class = "missing_mass_values"
  )
  diagnostics <- diagnose_apply_xmap(
    .xmap = simple_xmap,
    .data = na_data,
    values_from = xcode_mass,
    keys_from = xcode
  )
  expect_s3_class(diagnostics, "xmap_diagnosis")
  expect_false(diagnostics$valid)
  expect_equal(nrow(diagnostics$details$missing_values), 1)
  expect_null(diagnostics$details$not_covered)
})

test_that("validate_apply_xmap() returns TRUE for conformable data", {
  expect_true(
    validate_apply_xmap(
      .data = simple_data,
      .xmap = simple_xmap,
      values_from = xcode_mass,
      keys_from = xcode
    )
  )
})

test_that("validate_apply_xmap() returns FALSE for uncovered keys", {
  expect_false(
    validate_apply_xmap(
      .data = simple_data,
      .xmap = simple_xmap[1:3, ],
      values_from = xcode_mass,
      keys_from = xcode
    )
  )
})

test_that("validate_apply_xmap() returns FALSE for missing values", {
  na_data <- simple_data
  na_data$xcode_mass[[3]] <- NA
  expect_false(
    validate_apply_xmap(
      .data = na_data,
      .xmap = simple_xmap,
      values_from = xcode_mass,
      keys_from = xcode
    )
  )
})

test_that("diagnose_apply_xmap() detects not covered keys", {
  diagnostics <- diagnose_apply_xmap(
    .data = simple_data,
    .xmap = simple_xmap[1:3, ],
    values_from = xcode_mass,
    keys_from = xcode
  )
  expect_s3_class(diagnostics, "xmap_diagnosis")
  expect_false(diagnostics$valid)
  expect_equal(nrow(diagnostics$details$not_covered), 5)
  expect_null(diagnostics$details$missing_values)
})
