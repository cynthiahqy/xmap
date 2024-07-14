## Helpers -----=
xmap_collapse_col_names <- function(.xmap,
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
#' packages.
#' The default mode is `bigraph`,
#' with different styles for unit weight and fractional weight links, and
#' (optionally, prints fractional weights on edges.)
#' The second mode is `matrix`, which as the name suggests,
#' plots the adjacency matrix of the crossmap.
#'
#' @param object An xmap object.
#' @param ... Additional arguments (currently unused).
#' @param plot_type What visual representation of the crossmap to produce
#'
#' @importFrom ggplot2 autoplot aes
#' @importFrom dplyr c_across
#' @importFrom rlang sym
#' @importFrom ggraph circle
#'
#' @return ggplot2 object
#' @name autoplot.xmap
#' @export
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
#' autoplot(xmap, plot_type = "bigraph")
#' demo$simple_links |>
#'     as_xmap_tbl(xcode, alphacode, weight) |>
#'     autoplot(plot_type = "matrix")
autoplot.xmap <- function(object, ..., plot_type = c("bigraph", "matrix")) {
    plot_type <- arg_match(plot_type)
    object <- object |> xmap::as_xmap_tbl()
    if (plot_type == "bigraph") {
        xmap:::ggxmap_as_bigraph(object)
    } else if (plot_type == "matrix") {
        xmap:::ggxmap_as_matrix(object)
    }
}

#' @rdname autoplot.xmap
#' @export
autoplot.xmap_tbl <- function(object, ...) {
    NextMethod()
}

ggxmap_as_bigraph <- function(.xmap_tbl, ...) {
    if (!requireNamespace("ggraph", quietly = TRUE)) {
        cli::cli_abort('Please `install.package("ggraph")`')
    }

    # object <- as_xmap_tbl(object)
    # x_attrs <- attributes(object)

    tidygraph_data <- .xmap_tbl |> # object
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

#' @importFrom ggplot2 element_blank element_text
ggxmap_as_matrix <- function(.xmap_tbl, ...) {
    ## data helpers
    case_when_outgoing <- function(weights) {
        dplyr::case_when(
            weights == 1 ~ "unit",
            weights < 1 ~ "fractional",
            weights == 0 ~ NA,
            is.na(weights) ~ NA,
            .default = "invalid weight!"
        )
    }
    ## data helper
    matrix_long <- .xmap_tbl |>
        xmap:::xmap_collapse_multicol() |>
        tidyr::complete(.from, .to) |>
        dplyr::mutate(weight_type = case_when_outgoing(.weight_by))

    # TODO: colour doesn't match components
    # TODO: should matrix reflect components?

    ## plot
    matrix_long |>
        ggplot2::ggplot(aes(
            y = .from,
            x = .to
        )) +
        ggplot2::geom_tile(aes(fill = weight_type), col = "grey") +
        ggplot2::geom_text(aes(label = .weight_by),
            data = tidyr::drop_na(matrix_long, .weight_by)
        ) +
        ggplot2::scale_y_discrete(limits = rev) +
        ggplot2::scale_fill_brewer() +
        ggplot2::coord_fixed() +
        # ggplot2::labs(x = element_blank(), y = element_blank()) +
        ggplot2::labs(
            y = xmap_collapse_col_names(.xmap_tbl)$.from,
            x = xmap_collapse_col_names(.xmap_tbl)$.to
        ) +
        ggplot2::scale_x_discrete(position = "top") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        xmap:::theme_ggxmap_matrix()
}

#' @importFrom ggplot2 %+replace% theme theme_minimal element_rect margin unit
theme_ggxmap_matrix <- function(base_size = 11,
                                base_family = "",
                                base_line_size = base_size / 22,
                                base_rect_size = base_size / 22) {
    theme_minimal(
        base_size = base_size,
        base_family = base_family,
        base_line_size = base_line_size,
        base_rect_size = base_rect_size
    ) %+replace%
        theme(
            plot.margin = margin(0, 0, 0, 0),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.spacing.y = unit(0, "lines"),
            panel.border = element_blank(),
            axis.text = element_text(),
            axis.ticks = element_blank(),
            strip.background = element_rect(fill = "grey95"),
            # strip.text = element_text(hjust = 0),
            strip.placement = "outside",
            complete = TRUE
        )
}
