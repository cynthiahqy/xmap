if (FALSE) {
    library(ggplot2)

    simple_links |>
        tidyr::separate_wider_position(
            xcode,
            c(letter = 1, numbers = 2)
        )

    simple_xmap |>
        dplyr::mutate(
            cat.from =
                purrr::map_chr(.from, \(x) paste(x, "1", collapse = "x"))
        )
    simple_xmap |>
        purrr::flatten_df() |>
        ggplot(aes(x = alphacode, y = xcode)) +
        geom_point()
}
