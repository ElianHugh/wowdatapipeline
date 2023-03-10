box::use(stats[na.omit])

box::use(
    R / logic[...],
    . / utils[...],
    . / helpers[...]
)

#' @export
pipeline_talent_tree_ids <- function(client) {
    res <- lapply(
        get_talent_tree_ids(client),
        function(x) {
            tryCatch(
                expr = {
                    list(
                        tree_id = safely_reduce(x, "tree_id", 1),
                        spec_id = spec_to_id(safely_reduce(x, "spec", 1)),
                        spec = safely_reduce(x, "spec", 1)
                    )
                }, error = function(e) {
                    log_error(e)
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
                        log_error(e)
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
            if (!is.null(row) && is.numeric(row)) {
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
    getNodeIDs <- function(lst) {
        lapply(
            lst,
            function(item) {
                list(
                    row = safely_reduce(item, "display_row"),
                    spell_id = safely_reduce(
                        item,
                        "ranks",
                        1,
                        "tooltip",
                        "spell_tooltip",
                        "spell",
                        "id"
                    ),
                    talent_id = safely_reduce(
                        item,
                        "ranks",
                        1,
                        "tooltip",
                        "talent",
                        "id"
                    )
                )
            }
        )
    }

    resp <- safe_request(talent_tree_data_request(tree_id, spec_id, client))
    x <- safely_reduce(
        resp,
        "class_talent_nodes"
    )
    y <- safely_reduce(
        resp,
        "spec_talent_nodes"
    )

    list(
        class = getNodeIDs(x),
        specialisation = getNodeIDs(y)
    )
}

get_talent_tree_ids <- function(...) {
    box::use(stringr[str_extract])
    resp <- safe_request(talent_tree_request(...))
    x <- safely_reduce(resp, "spec_talent_trees")
    lapply(
        x,
        function(tree) {
            list(
                tree_id = str_extract(
                    safely_reduce(tree, "key", "href", 1),
                    pattern = "(?<=(talent-tree\\/))[0-9]*(?=\\/)"
                ),
                spec = safely_reduce(tree, "name", 1)
            )
        }
    )
}
