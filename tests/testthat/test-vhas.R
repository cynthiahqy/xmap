test_that("vhas_no_missing() works", {
  expect_true(vhas_no_missing(c("a", "b")))
  expect_false(vhas_no_missing(c("a", NA)))
})

test_that("vhas_no_dup_pairs() works", {
  expect_true(vhas_no_dup_pairs(c("a", "b"), c("x", "y")))
  expect_false(vhas_no_dup_pairs(c("a", "a"), c("x", "x")))
})

test_that("vhas_positive_weights() works", {
  expect_true(vhas_positive_weights(c(0.5, 1, 0.001)))
  expect_false(vhas_positive_weights(c(0.5, 0)))
  expect_false(vhas_positive_weights(c(0.5, -0.5)))
})

test_that("vhas_positive_weights() returns FALSE (not NA) for missing weights", {
  # NA > 0 is NA; isTRUE(all(...)) must resolve that to FALSE rather than
  # letting NA propagate into a caller's if()/&&
  expect_false(vhas_positive_weights(c(0.5, NA)))
})

test_that("vhas_valid_weights() works when tol is supplied", {
  expect_true(vhas_valid_weights(
    c("a", "a", "b"),
    c(0.5, 0.5, 1),
    tol = .Machine$double.eps^0.5
  ))
  expect_false(vhas_valid_weights(
    c("a", "b"),
    c(0.5, 1),
    tol = .Machine$double.eps^0.5
  ))
})

test_that("vhas_valid_weights() has no internal tol default -- errors if tol is omitted", {
  expect_error(
    vhas_valid_weights(c("a", "a"), c(0.5, 0.5)),
    regexp = "tol"
  )
})

test_that("a caller that forgets to forward its own tol to vhas_valid_weights() errors loudly, rather than silently using an unexposed internal default", {
  # mocked "outer" user-facing function: has its own tol argument, but
  # forgets to forward it to the internal helper
  outer_forgetful <- function(from, weights, tol = .Machine$double.eps^0.5) {
    vhas_valid_weights(from, weights)
  }
  expect_error(
    outer_forgetful(c("a", "a"), c(0.5, 0.5), tol = 0.5),
    regexp = "tol"
  )

  # the correctly-written counterpart succeeds and actually respects the
  # caller-supplied tol
  outer_correct <- function(from, weights, tol = .Machine$double.eps^0.5) {
    vhas_valid_weights(from, weights, tol = tol)
  }
  expect_false(outer_correct(c("a", "a"), c(0.5, 0.4))) # sum = 0.9, default tol: fails
  expect_true(outer_correct(c("a", "a"), c(0.5, 0.4), tol = 0.5)) # loose tol: passes
})
