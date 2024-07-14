test_that("autoplot() works for simple xmap", {
    expect_s3_class(
        autoplot(simple_xmap),
        "ggraph"
    )
})

test_that("autoplot() works for multicol xmap", {
    expect_s3_class(
        autoplot(multicol_xmap),
        "ggraph"
    )
})

if (FALSE) {
    library(ggplot2)

    ## matrix style plots
    multicol_xmap |>
        xmap_collapse_multicol() |>
        ggplot(aes(x = .from, y = .to, label = .weight_by)) +
        geom_text() +
        labs(x = xmap_get_col_names(multicol_xmap)$.from)

    multicol_xmap |>
        xmap_collapse_multicol() |>
        ggplot(aes(x = .from, y = .to, label = .weight_by)) +
        geom_tile(fill = "purple") +
        geom_text() +
        labs(x = xmap_get_col_names(multicol_xmap)$.from)

    multicol_xmap |>
        xmap_collapse_multicol() |>
        ggplot(aes(x = .from, y = .to, label = .weight_by)) +
        # geom_tile_matrix() +
        # theme_matrix()
        labs(x = xmap_get_col_names(multicol_xmap)$.from)

    ## graph style plots
}
