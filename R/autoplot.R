## Helpers -----=

xmap_get_col_names <- function(.xmap,
                               collapse_chr = c(.from = "*", .to = "*")) {
    col_names <- setNames(lapply(.xmap, names), names(.xmap))
    col_names$.from <- paste(col_names$.from,
        collapse = collapse_chr[".from"]
    )
    col_names$.to <- paste(col_names$.to,
        collapse = collapse_chr[".to"]
    )
    return(col_names)
}

xmap_collapse_multicol <- function(.xmap,
                                   collapse_chr =
                                       c(.from = "*", .to = "*")) {
    .xmap |>
        dplyr::rowwise() |>
        dplyr::mutate(
            .from = paste(c_across(.from),
                collapse = collapse_chr[".from"]
            ),
            .to = paste(c_across(.to),
                collapse = collapse_chr[".to"]
            ),
            .weight_by = .weight_by[[1]]
        )
}

#' Autoplot function for xmap objects
#'
#' This function generates a plot of an xmap object using the ggraph and ggplot2
#' packages. It visualizes the relationships between nodes and edges in the xmap_df
#' object, with different styles for unit weight and fractional weight links, and
#' prints fractional weights on edges.
#'
#' @param object An xmap object.
#' @param ... Additional arguments (currently unused).
#'
#' @importFrom ggplot2 autoplot aes
#' @importFrom dplyr c_across
#' @importFrom rlang sym
#' @importFrom ggraph circle
#'
#' @return ggplot2 object
#' @name autoplot.xmap
#'
#' @examples
#' library(ggplot2)
#' library(ggraph)
#' library(tidygraph)
#' library(xmap)
#' df <- data.frame(
#'     from = c("A", "A", "B", "B", "B"),
#'     to = c("X", "Y", "X", "Y", "Z"),
#'     weights = c(0.6, 0.4, 0.2, 0.7, 0.1)
#' )
#' xmap <- as_xmap_tbl(df, from, to, weights)
#' autoplot(xmap)
autoplot.xmap <- function(object, ..., plot_type = c("bigraph")) {
    if (plot_type == "bigraph") {
        xmap:::ggxmap_as_bigraph(.xmap = object)
    }
}

#' @rdname autoplot.xmap
#' @export
autoplot.xmap_tbl <- function(object, ...) {
    NextMethod()
}

ggxmap_as_bigraph <- function(.xmap, ...) {
    if (!requireNamespace("ggraph", quietly = TRUE)) {
        cli::cli_abort('Please `install.package("ggraph")`')
    }

    # object <- as_xmap_tbl(object)
    # x_attrs <- attributes(object)

    tidygraph_data <- object |> # object
        xmap:::xmap_collapse_multicol() |>
        tidygraph::as_tbl_graph() |>
        ## calculating edge properties
        tidygraph::activate(edges) |>
        tidygraph::mutate(frac_weight = ifelse(.weight_by < 1, TRUE, FALSE)) |>
        tidygraph::mutate(edge_linetype = ifelse(frac_weight, "dashed", "solid")) |>
        tidygraph::mutate(edge_label_pos = ifelse(frac_weight, 0.8, 0.2)) |>
        ## calculating node properties
        tidygraph::activate(nodes) |>
        tidygraph::mutate(
            n_from = tidygraph::centrality_degree(mode = "in"),
            in_from = n_from == 0,
            collapse = n_from > 1,
            comp_group = tidygraph::group_components()
        )

    tidygraph_data |>
        ## now we plot...
        ggraph::ggraph(layout = "sugiyama") +
        ## unit weight links,
        ggraph::geom_edge_diagonal(
            aes(
                edge_linetype = I(edge_linetype),
                edge_alpha = .weight_by,
                filter = !frac_weight
            ),
            end_cap = circle(6, "mm"),
            show.legend = FALSE
        ) +
        ## frac weight links,
        ggraph::geom_edge_diagonal(
            aes(
                edge_linetype = I(edge_linetype),
                edge_alpha = .weight_by,
                filter = frac_weight,
                # label = round(.weight_by, digits = 3),
                # label_pos = edge_label_pos,
            ),
            end_cap = circle(6, "mm"),
            show.legend = FALSE,
            angle_calc = "along",
            label_dodge = grid::unit(2, "mm")
        ) +
        ## from nodes,
        ggraph::geom_node_label(aes(
            label = name,
            filter = in_from,
            fill = as.factor(comp_group),
        ), ) +
        ## to nodes,
        ggraph::geom_node_label(
            aes(
                label = name,
                fill = as.factor(comp_group),
                filter = !in_from,
                alpha = n_from
            ),
            show.legend = FALSE,
        ) +
        # ggplot2::scale_fill_continuous() +
        ## and finally modify coordinates, scale and theme
        ggplot2::coord_flip() +
        ggplot2::scale_y_reverse() +
        ggplot2::theme_minimal() +
        ggraph::th_no_axes() +
        ggplot2::guides(fill = "none")
}
