simple_links <- demo$simple_links
simple_data <- simple_links |>
    dplyr::distinct(xcode) |>
    dplyr::mutate(xcode_mass = 100)
simple_xmap <- as_xmap_tbl(
    simple_links,
    xcode, alphacode, weight
)
