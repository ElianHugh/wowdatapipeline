#' @export
safely_reduce <- function(init, ...) {
    tryCatch(
        {
            Reduce(`[[`, x = list(...), init = init)
        },
        error = function(e) {
            list(NULL)
        }
    )
}

