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
    class = "abort_invalid_xmap"
  )
})

test_that("as_xmap_tbl() works", {
  expect_s3_class(
    as_xmap_tbl(
      simple_links,
      xcode,
      alphacode,
      weight
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
    class = "abort_invalid_xmap"
  )
})

test_that("xmap_tbl() rejects a `.from` whose weights sum to zero", {
  # a `.from` with no valid outgoing weight is not a valid crossmap node,
  # matching validate_as_xmap.matrix()'s all-zero-row check -- see #19
  tfrom <- tibble::tibble(source = c("A1", "A2"))
  tto <- tibble::tibble(target = c("x1", "x2"))
  twgts <- tibble::tibble(weight_by = c(0, 1))
  links <- tibble::tibble(.from = tfrom, .to = tto, .weight_by = twgts)
  expect_error(
    xmap_tbl(links$.from, links$.to, links$.weight_by),
    class = "abort_invalid_xmap"
  )
})

test_that("xmap_tbl() and diagnose_as_xmap_tbl() pick up an individual zero weight, distinctly from bad_froms", {
  # A1's weights still sum to one (0 + 1), so this must be caught by the
  # dedicated nonpositive_weights check, not bad_froms -- #49
  links <- tibble::tibble(
    source = c("A1", "A1"),
    target = c("x1", "x2"),
    weight_by = c(0, 1)
  )

  expect_error(
    as_xmap_tbl(links, source, target, weight_by),
    class = "abort_invalid_xmap"
  )

  diagnostics <- diagnose_as_xmap_tbl(links, source, target, weight_by)
  expect_s3_class(diagnostics, "xmap_diagnosis")
  expect_false(diagnostics$valid)
  expect_equal(nrow(diagnostics$details$nonpositive_weights), 1)
  expect_null(diagnostics$details$bad_froms)
})

test_that("xmap_tbl() and diagnose_as_xmap_tbl() pick up a negative weight", {
  links <- tibble::tibble(
    source = c("A1", "A1"),
    target = c("x1", "x2"),
    weight_by = c(-0.5, 1.5)
  )

  expect_error(
    as_xmap_tbl(links, source, target, weight_by),
    class = "abort_invalid_xmap"
  )

  diagnostics <- diagnose_as_xmap_tbl(links, source, target, weight_by)
  expect_s3_class(diagnostics, "xmap_diagnosis")
  expect_false(diagnostics$valid)
  expect_equal(nrow(diagnostics$details$nonpositive_weights), 1)
})

test_that("xmap_tbl() and diagnose_as_xmap_tbl() pick up duplicate links", {
  tfrom <- tibble::tibble(source = c("A1", "A1", "A2"))
  tto <- tibble::tibble(target = c("x1", "x1", "x2"))
  twgts <- tibble::tibble(weight_by = c(1L, 1L, 1L))
  links <- tibble::tibble(.from = tfrom, .to = tto, .weight_by = twgts)
  expect_error(
    xmap_tbl(links$.from, links$.to),
    class = "abort_invalid_xmap"
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
    class = "abort_invalid_xmap"
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

test_that("xmap_tbl() and diagnose_as_xmap_tbl() pick up missing from", {
  links <- tibble::tibble(
    source = c("A1", NA, "A3"),
    target = c("x1", "x2", "x3"),
    weight_by = c(1L, 1L, 1L)
  )
  expect_error(
    as_xmap_tbl(links, source, target, weight_by),
    class = "abort_invalid_xmap"
  )

  diagnostics <- diagnose_as_xmap_tbl(links, source, target, weight_by)
  expect_s3_class(diagnostics, "xmap_diagnosis")
  expect_false(diagnostics$valid)
  expect_equal(nrow(diagnostics$details$miss_from), 1)
  expect_null(diagnostics$details$miss_to)
})

test_that("xmap_tbl() and diagnose_as_xmap_tbl() pick up missing to", {
  links <- tibble::tibble(
    source = c("A1", "A2", "A3"),
    target = c("x1", NA, "x3"),
    weight_by = c(1L, 1L, 1L)
  )
  expect_error(
    as_xmap_tbl(links, source, target, weight_by),
    class = "abort_invalid_xmap"
  )

  diagnostics <- diagnose_as_xmap_tbl(links, source, target, weight_by)
  expect_s3_class(diagnostics, "xmap_diagnosis")
  expect_false(diagnostics$valid)
  expect_equal(nrow(diagnostics$details$miss_to), 1)
  expect_null(diagnostics$details$miss_from)
})

## as_xmap_tbl.matrix() -----------------------------------------------------

valid_xmap_matrix <- matrix(
  c(1, 0, 0.5, 0.5),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(c("A1", "A2"), c("x1", "x2"))
)

test_that("as_xmap_tbl() is a generic with a matrix method", {
  expect_true(exists("as_xmap_tbl.matrix"))
})

test_that("as_xmap_tbl.matrix() works and drops zero-weight cells", {
  result <- as_xmap_tbl(valid_xmap_matrix)
  expect_s3_class(result, "xmap_tbl")
  expect_equal(nrow(result), 3) # A1-x1, A2-x1, A2-x2; A1-x2 is a zero cell
})

test_that("as_xmap_tbl.matrix() falls back to rowname/colname/cell without dimnames names", {
  result <- as_xmap_tbl(valid_xmap_matrix)
  expect_equal(get_name_from(result), "rowname")
  expect_equal(get_name_to(result), "colname")
})

test_that("as_xmap_tbl.matrix() uses dimnames() names when set", {
  named_matrix <- valid_xmap_matrix
  dimnames(named_matrix) <- list(
    source = rownames(named_matrix),
    target = colnames(named_matrix)
  )

  result <- as_xmap_tbl(named_matrix)
  expect_equal(get_name_from(result), "source")
  expect_equal(get_name_to(result), "target")
})

test_that("as_xmap_tbl.matrix() lets from/to/weight_by override the column names", {
  result <- as_xmap_tbl(
    valid_xmap_matrix,
    from = "src",
    to = "tgt",
    weight_by = "w"
  )
  expect_equal(get_name_from(result), "src")
  expect_equal(get_name_to(result), "tgt")
  expect_equal(get_name_weight_by(result), "w")
})

test_that("as_xmap_tbl.matrix() rejects an all-zero row instead of silently dropping it", {
  # A naive pivot_longer() + filter(weight != 0) port would drop A1's row
  # entirely (all its cells are 0) before any check could see it, so the
  # bad `.from` key would just vanish instead of raising an error --
  # regression test for the bug flagged while scoping #21.
  zero_row_matrix <- valid_xmap_matrix
  zero_row_matrix["A1", ] <- c(0, 0)

  expect_error(
    as_xmap_tbl(zero_row_matrix),
    class = "abort_invalid_xmap"
  )
})

test_that("as_xmap_tbl.matrix() rejects an invalid matrix (rows not summing to one)", {
  bad_matrix <- valid_xmap_matrix
  bad_matrix["A1", ] <- c(0.5, 0.6)

  expect_error(
    as_xmap_tbl(bad_matrix),
    class = "abort_invalid_xmap"
  )
})

if (FALSE) {
  read.csv("test.csv", stringsAsFactors = TRUE) |>
    as_xmap_tbl(xcode, alphacode, weight)
}
