box::use(
    stats[na.omit],
    stringr[str_extract]
)

box::use(
    R / logic[...],
    R / api[...]
)

#' @export
pipeline_talent_tree_ids <- function(client) {
    res <- lapply(
        get_talent_tree_ids(client),
        function(x) {
            tryCatch(
                expr = {
                    list(
                        tree_id = x$tree_id,
                        spec_id = spec_to_id(x$spec),
                        spec = x$spec
                    )
                }, error = function(e) {
                    log_error(e, context = "Pipeline talent tree ids")
                    NULL
                }
            )
        }
    )
    unique(res)
}

#' @export
pipeline_talent_trees <- function(talent_tree_ids, client) {
    lapply(
        talent_tree_ids,
        function(x) {
            tryCatch(
                {
                    list(
                        id = x$spec_id,
                        talents = get_talent_data(
                            tree_id = x$tree_id,
                            spec_id = x$spec_id,
                            client = client
                        )
                    )
                },
                error = function(e) {
                    log_error(e, context = "Pipeline talent trees")
                    NULL
                }
            )
        }
    )
}

#' @export
pipeline_capstone <- function(talent_trees) {
    get_maximal_row <- function(lst) {
        possible_rows <- unlist(lapply(lst, function(talent) {
            row <- talent[["row"]]
            if (!is.null(row) && is.numeric(row) && length(row) > 0L) {
                return(row)
            } else {
                return(-Inf)
            }
        }))
        max(possible_rows, na.rm = TRUE)
    }

    filter_by_row <- function(lst, max_row) {
        Filter(function(talent) talent[["row"]] >= max_row, lst)
    }

    lapply(talent_trees, function(x) {
        list(
            id = x$id,
            class = {
                max_row <- get_maximal_row(x[["talents"]][["class"]])
                filter_by_row(x[["talents"]][["class"]], max_row)
            },
            specialisation = {
                max_row <- get_maximal_row(x[["talents"]][["specialisation"]])
                filter_by_row(x[["talents"]][["specialisation"]], max_row)
            }
        )
    })
}


get_talent_data <- function(tree_id, spec_id, client) {
    get_node_ids <- function(lst) {
        lapply(
            lst,
            function(item) {
                list(
                    row = safely_reduce(item, "display_row"),
                    spell_id = safely_reduce(
                        item,
                        "ranks",
                        1L,
                        "tooltip",
                        "spell_tooltip",
                        "spell",
                        "id"
                    ),
                    talent_id = safely_reduce(
                        item,
                        "ranks",
                        1L,
                        "tooltip",
                        "talent",
                        "id"
                    )
                )
            }
        )
    }

    resp <- safe_request(talent_tree_data_request(tree_id, spec_id, client))
    x <- safely_reduce(resp, "class_talent_nodes")
    y <- safely_reduce(resp, "spec_talent_nodes")

    list(
        class = get_node_ids(x),
        specialisation = get_node_ids(y)
    )
}

get_talent_tree_ids <- function(...) {
    resp <- safe_request(talent_tree_request(...))
    x <- safely_reduce(resp, "spec_talent_trees")

    mapply(
        \(key, name) {
            list(
                tree_id = str_extract(
                    key,
                    pattern = "(?<=(talent-tree\\/))[0-9]*(?=\\/)"
                ),
                spec = name
            )
        },
        key = x$key,
        name = x$name,
        SIMPLIFY = FALSE
    )
}
