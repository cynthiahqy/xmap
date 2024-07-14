#' @rdname autoplot.xmap
#' @export
autoplot.xmap <- function(object, ...) {
    if (!requireNamespace("ggraph", quietly = TRUE)) {
        cli::cli_abort('Please `install.package("ggraph")`')
    }

    object <- as_xmap_tbl(object)

    x_attrs <- attributes(object)

    tidygraph_data <-
        tidygraph::as_tbl_graph(object) |>
        ## calculating edge properties
        tidygraph::activate(edges) |>
        tidygraph::mutate(frac_weight = ifelse(!!sym(x_attrs$col_weights) < 1, TRUE, FALSE)) |>
        tidygraph::mutate(edge_linetype = ifelse(frac_weight, "dashed", "solid")) |>
        tidygraph::mutate(edge_label_pos = ifelse(frac_weight, 0.8, 0.2)) |>
        ## calculating node properties
        tidygraph::activate(nodes) |>
        tidygraph::mutate(
            n_from = tidygraph::centrality_degree(mode = "in"),
            in_from = n_from == 0,
            collapse = n_from > 1
        )

    tidygraph_data |>
        ## now we plot...
        ggraph::ggraph(layout = "sugiyama") +
        ## unit weight links,
        ggraph::geom_edge_diagonal(
            aes(
                edge_linetype = I(edge_linetype),
                edge_alpha = !!sym(x_attrs$col_weights),
                filter = !frac_weight
            ),
            end_cap = circle(6, "mm"),
            show.legend = FALSE
        ) +
        ## frac weight links,
        ggraph::geom_edge_diagonal(
            aes(
                edge_linetype = I(edge_linetype),
                edge_alpha = !!sym(x_attrs$col_weights),
                filter = frac_weight,
                label = round(!!sym(x_attrs$col_weights), digits = 3),
                label_pos = edge_label_pos,
            ),
            end_cap = circle(6, "mm"),
            show.legend = FALSE,
            angle_calc = "along",
            label_dodge = grid::unit(2, "mm")
        ) +
        ## from nodes,
        ggraph::geom_node_label(aes(
            label = name,
            filter = in_from
        ), ) +
        ## to nodes,
        ggraph::geom_node_label(
            aes(
                label = name,
                fill = collapse,
                filter = !in_from
            ),
            show.legend = FALSE,
        ) +
        ggplot2::scale_fill_brewer(palette = "Greys") +
        ## and finally modify coordinates, scale and theme
        ggplot2::coord_flip() +
        ggplot2::scale_y_reverse() +
        ggplot2::theme_minimal() +
        ggraph::th_no_axes()
}
