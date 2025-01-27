#' @export
as_xmap_matrix <- function(x, ...) {
    UseMethod("as_xmap_matrix")
}

#' @export
as_xmap_matrix.xmap_tbl <- function(x, ...) {
    .xmap_tbl <- x

    xmap_complete <- .xmap_tbl |>
        xmap:::xmap_collapse_multicol() |>
        tidyr::complete(.from, .to) |>
        dplyr::arrange(.from)
    row_names <- vec_unique(xmap_complete$.from)
    col_names <- vec_unique(xmap_complete$.to)

    xmap_matrix <- new_xmap_matrix(
        data = xmap_complete$.weight_by,
        nrow = vec_unique_count(xmap_complete$.from),
        byrow = TRUE,
        dimnames = list(
            row_names,
            col_names
        )
    )

    return(xmap_matrix)
}

## TODO: as_xmap_matrix.matrix()

## constructor
new_xmap_matrix <- function(data, nrow, dimnames, byrow = FALSE) {
    xmap_mtx <- matrix(
        data = data,
        nrow = nrow,
        byrow = TRUE,
        dimnames = dimnames
    )
    class(xmap_mtx) <- c("xmap_matrix", class(xmap_mtx))
    return(xmap_mtx)
}
