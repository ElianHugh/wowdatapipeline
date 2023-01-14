box::use(stats[na.omit])

box::use(
    R / logic[...],
    . / utils[...],
    . / helpers[...]
)

#' @export
pipeline_talent_tree_ids <- function(client) {
    lapply(
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
}

#' @export
pipeline_talent_trees <- function(talent_tree_ids, client) {
    unlist(
        lapply(
            talent_tree_ids,
            function(x) {
                tryCatch(
                    {
                        res <- list(get_talent_data(
                            tree_id = x$tree_id,
                            spec_id = x$spec_id,
                            client = client
                        ))
                        if (length(res) > 0L) {
                            names(res) <- x$spec_id
                        }
                        res
                    },
                    error = function(e) {
                        log_error(e)
                        NULL
                    }
                )
            }
        ),
        recursive = FALSE
    )
}

#' @export
pipeline_capstone <- function(talent_trees) {
    lapply(
        talent_trees,
        function(x) {
            tryCatch(
                expr = {
                    capstone_row <- max(
                        na.omit(unlist(lapply(x, function(talent) talent[["row"]])))
                    )
                    if (!is.null(capstone_row)) {
                        Filter(function(talent) talent[["row"]] >= 10, x)
                    }
                }, error = function(e) {
                    log_error(e)
                    NULL
                }
            )
        }
    )
}


get_talent_data <- function(tree_id, spec_id, client) {
    resp <- safe_request(talent_tree_data_request(tree_id, spec_id, client))
    x <- safely_reduce(
        resp,
        "class_talent_nodes"
    )

    lapply(
        x,
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
                )
            )
        }
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
