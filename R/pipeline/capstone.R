box::use(
    purrr[map, pluck]
)

box::use(
    R / logic[...],
    R / pipeline[...]
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
                tryCatch({
                    res <- list(get_talent_data(
                        tree_id = x$tree_id,
                        spec_id = x$spec_id,
                        client = client
                    ))
                    if (length(res) > 0L) {
                        names(res) <- x$spec_id
                    }
                    res
                }, error = function(e) {
                    NULL
                })
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
                        na.omit(unlist(lapply(x, function(talent) talent$row)))
                    )
                    if (!is.null(capstone_row)) {
                        Filter(function(talent) talent$row >= 10, x)
                    }
                }, error = function(e) {
                    NULL
                }
            )
        }
    )
}