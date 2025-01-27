simple_links <- demo$simple_links
simple_data <- simple_links |>
    dplyr::distinct(xcode) |>
    dplyr::mutate(xcode_mass = 100)
simple_xmap <- as_xmap_tbl(
    simple_links,
    xcode, alphacode, weight
)

multicol_links <- simple_links |>
    tidyr::separate_wider_position(
        xcode,
        c(x_letter = 1, x_numbers = 4)
    )
multicol_xmap <- xmap:::new_xmap_tbl(
    list(
        .from = multicol_links[c("x_letter", "x_numbers")],
        .to = multicol_links["alphacode"],
        .weight_by = multicol_links["weight"]
    )
)
