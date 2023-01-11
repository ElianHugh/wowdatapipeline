#' @export
is_safe <- function(x) {
    !is.null(x) && !inherits(x, "error")
}

#' @export
batch_list <- function(lst, group_size = 100L) {
    structure(
        split(lst, ceiling(seq_along(lst) / group_size)),
        names = NULL
    )
}