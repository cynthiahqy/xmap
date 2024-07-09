## constructor ---------------------------------------------------------

#' @importFrom tibble new_tibble tibble
new_xmap_tbl <- function(x = list(
                             .from = tibble::tibble(source = character()),
                             .to = tibble::tibble(target = character()),
                             .weight_by = tibble::tibble(distr = double())
                         ),
                         tol = .Machine$double.eps^0.5, class = NULL) {
    if (!length(x) == 3) {
        abort("`x` must be a list of length 3.")
    }
    if (any(lapply(x, ncol) != 1)) {
        abort("`.from`, `.to`, `.weight_by` should only have one column each")
    }
    if (!is.numeric(x$.weight_by[[1]])) {
        abort("`x$`.weight_by`[[1]]` must be a numeric vector.")
    }
    if ((!is_double(tol) || length(tol) != 1)) {
        cli::cli_abort(
            "{.arg tol} must be a single double
            specifying the floating point tolerance."
        )
    }

    tibble::new_tibble(x,
        n = NULL, tol = tol,
        class = c(class, "xmap_tbl", "xmap")
    )
}

## Helpers -------------------------------------------------
xmap_tbl <- function(.from = tibble::tibble(source = character()),
                     .to = tibble::tibble(target = character()),
                     .weight_by = tibble::tibble(ones = 1L),
                     tol = .Machine$double.eps^0.5) {
    arg_names <- c(".from", ".to", ".weight_by")

    ## TODO: add message about recycling weights
    if (missing(.weight_by)) {
        message("Using unit `.weight_by = tibble::tibble(ones = 1L)`")
    }

    ## input classes checks
    class_pass <- sapply(
        list(.from, .to, .weight_by),
        \(x) "data.frame" %in% class(x)
    )
    if (!all(class_pass)) {
        msg <- c(
            "x" = "{.arg {arg_names[!class_pass]}} must be {?a/}
                    data frame-like object{?s}",
            "i" = "Try replacing `[[` or `$` selector with `[`,
                    or use {.fnc as_xmap_tbl()}",
        )
        cli::cli_abort(msg)
    }

    ## size checks
    if (vec_size(.from) != vec_size(.to)) {
        msg <- c(
            "x" = "{.arg {c('.from', '.to')}}
                    must be the same size"
        )
        cli::cli_abort(msg)
    }
    if (!any(
        vec_size(.weight_by) == 1,
        vec_size(.from) == vec_size(.weight_by)
    )) {
        msg <- c(
            "x" = "{.arg weight_by} must be the compatible size with
                    {.arg {c('from', 'to')}}"
        )
        cli::cli_abort(msg)
    }
    .weight_by <- vec_recycle(.weight_by, vec_size(.from))

    ## validate edge list and edges
    # TODO: add unique checks:
    ## vec_unique_count(tibble::tibble(.from, .to)) != vec_size(.from)
    if (anyDuplicated(data.frame(.from, .to))) {
        msg <- c(
            "x" = "There should only be one edge for each unique
                `.from`-`.to` pair",
            "i" = "Remove or collapse duplicated `.from`-`.to` pairs",
            "i" = "Use diagnose_xmap_tbl() for further information"
        )
        cli::cli_abort(msg, class = "abort_dup_pairs")
    }

    if (vec_any_missing(.weight_by)) {
        msg <- c(
            "x" = "Missing values not allowed in `.weight_by`."
        )
        cli::cli_abort(msg, class = "missing_weight_by")
    }

    if (!vhas_valid_weights(.from[[1]], .weight_by[[1]], tol = tol)) {
        ## TODO: consider retrieving all matches
        # tibble::tibble(.from = .from, .to = .to) -> edges
        # vec_locate_matches(bad_froms$.from, edges$.from) -> matches
        # vec_slice(edges, matches$haystack) |> purrr::flatten_df()

        msg <- c("Invalid `.weight_by` found for some links",
            "x" = "The total outgoing `.weight_by` for some `.from` nodes
                    are not near enough to 1",
            "i" = "Modify `.weight_by` or adjust `tol` and try again.",
            "i" = "Use `diagnose_xmap_tbl() for more information."
        )
        cli::cli_abort(
            message = c(msg),
            class = "abort_bad_weight_by"
        )
    }

    x_list <- list(.from, .to, .weight_by)
    names(x_list) <- arg_names

    new_xmap_tbl(x = x_list, tol = tol)
}

## Coercion -------------------------
#' @export
#' @rdname as_xmap_tbl
as_xmap_tbl <- function(x, ...) {
    UseMethod("as_xmap_tbl")
}

#' Coercing data frames of links to crossmap tibbles
#'
#' This method takes a data.frame-like object and converts it into an `xmap_tbl`
#' based on specified columns for 'from', 'to', and 'weight'.
#'
#' @param x A data.frame or tibble to be converted in a crossmap tibble.
#' @param from The column in `x` that specifies the 'from' nodes.
#' @param to The column in `x` that specifies the 'to' nodes.
#' @param weight_by The column in `x` that specifies the weight of the links.
#' @param ... (reserved) Additional arguments passed to methods.
#' @inheritParams dplyr::near
#' @return Returns an xmap tibble object.
#' @export
#' @rdname as_xmap_tbl
#' @examples
#' demo$abc_links |>
#'     as_xmap_tbl(from = lower, to = upper, weight_by = share)
as_xmap_tbl.data.frame <- function(
    x, from, to, weight_by, ...,
    tol = .Machine$double.eps^0.5) {
    from_id <- tidyselect::eval_select(enquo(from), x)
    to_id <- tidyselect::eval_select(enquo(to), x)
    weight_by_id <- tidyselect::eval_select(enquo(weight_by), x)

    list_x <- list(
        .from = x[from_id],
        .to = x[to_id],
        .weight_by = x[weight_by_id]
    )

    ncol_fail <- (lapply(list_x, ncol) != 1)
    if (any(ncol_fail)) {
        msg <- c(
            x = "You can only select one column each for
                {.arg {names(list_x)}}",
            i = "You've selected more than one column for:
                {.arg {names(list_x)[ncol_fail]}}"
        )
        cli::cli_abort(c(msg))
    }

    xmap_tbl(
        .from = list_x$.from,
        .to = list_x$.to,
        .weight_by = list_x$.weight_by,
        tol = tol
    )
}

#' @export
#' @rdname as_xmap_tbl
diagnose_as_xmap_tbl <- function(
    x, from, to, weight_by, ...,
    tol = .Machine$double.eps^0.5) {
    from_id <- tidyselect::eval_select(enquo(from), x)
    to_id <- tidyselect::eval_select(enquo(to), x)
    weight_by_id <- tidyselect::eval_select(enquo(weight_by), x)

    tbl_x <- tibble::tibble(
        .from = x[from_id],
        .to = x[to_id],
        .weight_by = x[weight_by_id]
    )

    flags <- list()
    details <- list(
        bad_dups = NULL,
        miss_weight_by = NULL,
        bad_froms = NULL
    )

    flags$dup_pairs <- anyDuplicated(tbl_x[c(".from", ".to")])

    if (flags$dup_pairs) {
        msg <- c("Duplicate `.from`-`.to` links detected.",
            "i" = "See `.$bad_dups` for more details"
        )
        cli::cli_warn(msg, "dup_pairs")
        details$bad_dups <- tbl_x |>
            dplyr::group_by(.from, .to) |>
            dplyr::summarise(.dup = dplyr::n()) |>
            dplyr::filter(.dup != 1)
    }

    ## DONE: add missing weights diagnosis
    flags$miss_weight_by <- vec_any_missing(tbl_x$.weight_by)
    if (flags$miss_weight_by) {
        col_names <- names(tbl_x$.weight_by)
        msg <- c(
            "x" = "Missing values not allowed in  `weight_by`.",
            "i" = "Replace or remove missing values from column{?s}
            {.col {col_names}}",
            "i" = "See `.$miss_weight_by` for more details"
        )
        cli::cli_warn(msg, class = "missing_weight_by")
        details$miss_weight_by <- tbl_x |>
            dplyr::filter(is.na(.weight_by[[1]]))
    }

    ## TODO: implement a cheaper check
    bad_froms <- tbl_x |>
        dplyr::group_by(.from) |>
        dplyr::summarise(.sum.weight_by = sum(.weight_by)) |>
        dplyr::mutate(.near = dplyr::near(.sum.weight_by, 1L, tol = tol)) |>
        dplyr::filter(!.near) |>
        dplyr::select(-.near)

    flags$bad_froms <- (nrow(bad_froms) != 0)

    if (flags$bad_froms) {
        msg <- c("The sum of weights on outgoing links for some source nodes
            are not near 1",
            "i" = "Fix weights or adjust `tol=`",
            "i" = "See `.$bad_froms` for more details"
        )
        cli::cli_warn(msg)
        details$bad_froms <- bad_froms
    }

    if (any(simplify2array(flags))) {
        return(details)
    } else {
        msg <- c(
            "Provided `.from`-`.to` links and `.weight_by` are valid",
            "*" = "No duplicate `.from`-`.to` pairs found",
            "*" = "No missing values in `.weight_by`",
            "*" = "Sum of `.weight_by` by `.from` are near enough to one"
        )
        cli::cli_inform(msg)
        invisible(x)
    }
}

## metadata helpers (DO NOT EXPORT)
get_name_from <- function(x) names(x$.from)
get_name_to <- function(x) names(x$.to)
get_name_weight_by <- function(x) names(x$.weight_by)
get_from_set <- function(x) vec_unique(x$.from)
get_to_set <- function(x) vec_unique(x$.to)

## printing ---------------------------------------------

#' @export
tbl_sum.xmap_tbl <- function(x, ...) {
    default_header <- NextMethod()
    names(default_header) <- "A crossmap tibble"
    n_from_set <- vec_unique_count(x$.from)
    n_to_set <- vec_unique_count(x$.to)
    extra_info <- c("with unique keys" = sprintf(
        "[%s] %s -> [%s] %s",
        n_from_set, names(x$.from),
        n_to_set, names(x$.to)
    ))
    c(default_header, extra_info)
}

#' @export
tbl_format_footer.xmap_tbl <- function(x, setup, ...) {
    # TODO: modify footer to print total number of links
    # n_links <- vec_size(x)
    default_footer <- NextMethod()
    modified_footer <- gsub("rows", "links", default_footer)
    c(modified_footer)
}
