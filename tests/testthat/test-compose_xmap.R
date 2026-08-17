group_xmap <- tibble::tibble(
    alphacode = c("A1", "B2", "B3", "C5", "D6", "D7"),
    group = c("AB", "AB", "AB", "C", "D", "D"),
    weight = 1
) |>
    as_xmap_tbl(alphacode, group, weight_by = weight)

test_that("compose_xmap() works and returns an xmap_tbl", {
    composed <- compose_xmap(simple_xmap, group_xmap)
    expect_s3_class(composed, "xmap_tbl")
    expect_equal(get_name_from(composed), "xcode")
    expect_equal(get_name_to(composed), "group")
})

test_that("compose_xmap() weights are a valid crossmap (sum to 1 per .from)", {
    composed <- compose_xmap(simple_xmap, group_xmap)
    expect_true(diagnose_as_xmap_tbl(composed, .from, .to, .weight_by)$valid)
})

test_that("compose_xmap() sums weights over multiple shared intermediates", {
    # x2222 splits B2 (0.5) / B3 (0.5), both of which collapse into "AB" --
    # composing should recombine them into a single AB link with weight 1
    composed <- compose_xmap(simple_xmap, group_xmap)
    x2222_row <- composed[composed$.from[[1]] == "x2222", ]
    expect_equal(nrow(x2222_row), 1)
    expect_equal(x2222_row$.weight_by[[1]], 1)
})

test_that("compose_xmap() respects an explicit `tol`", {
    composed <- compose_xmap(simple_xmap, group_xmap, tol = 1e-3)
    expect_s3_class(composed, "xmap_tbl")
})

test_that("compose_xmap() aborts if xmap1 and xmap2 are not xmap_tbl", {
    expect_error(
        compose_xmap(simple_links, group_xmap),
        class = "compose_xmap_bad_input"
    )
})

test_that("compose_xmap() aborts if xmap1 is classed xmap_tbl but not actually valid", {
    # hand-assembled: classed xmap_tbl without going through xmap_tbl()'s
    # validation gate -- weights sum to 1.8, not 1
    fake_xmap <- tibble::tibble(
        .from = tibble::tibble(a = c("x", "x")),
        .to = tibble::tibble(b = c("y", "z")),
        .weight_by = tibble::tibble(w = c(0.9, 0.9))
    )
    class(fake_xmap) <- c("xmap_tbl", "xmap", class(fake_xmap))

    expect_error(
        compose_xmap(fake_xmap, group_xmap),
        class = "compose_xmap_invalid_input"
    )
})

test_that("compose_xmap() aborts if xmap2 is classed xmap_tbl but not actually valid", {
    fake_xmap <- tibble::tibble(
        .from = tibble::tibble(a = c("x", "x")),
        .to = tibble::tibble(b = c("y", "z")),
        .weight_by = tibble::tibble(w = c(0.9, 0.9))
    )
    class(fake_xmap) <- c("xmap_tbl", "xmap", class(fake_xmap))

    expect_error(
        compose_xmap(simple_xmap, fake_xmap),
        class = "compose_xmap_invalid_input"
    )
})

test_that("compose_xmap() allows xmap2 to have unused `.from` entries", {
    # mirrors apply_xmap()'s asymmetric coverage rule: `.xmap` (here xmap2)
    # can have more instructions than xmap1 ever uses -- only xmap1's `.to`
    # must be fully covered by xmap2's `.from`, not the other way round
    extra_group_xmap <- tibble::tibble(
        alphacode = c("A1", "B2", "B3", "C5", "D6", "D7", "Z9"),
        group = c("AB", "AB", "AB", "C", "D", "D", "unused"),
        weight = 1
    ) |>
        as_xmap_tbl(alphacode, group, weight_by = weight)

    composed <- compose_xmap(simple_xmap, extra_group_xmap)
    expect_s3_class(composed, "xmap_tbl")
    expect_false("unused" %in% composed$.to[[1]])
})

test_that("compose_xmap() aborts if xmap2 doesn't cover xmap1's `.to`", {
    partial_group_xmap <- tibble::tibble(
        alphacode = c("A1", "B2", "B3"),
        group = c("AB", "AB", "AB"),
        weight = 1
    ) |>
        as_xmap_tbl(alphacode, group, weight_by = weight)

    expect_error(
        compose_xmap(simple_xmap, partial_group_xmap),
        class = "compose_xmap_uncovered"
    )
})
