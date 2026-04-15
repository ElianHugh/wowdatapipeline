box::use(
    httr2[req_perform, req_retry, resp_is_error, resp_body_string],
    utils[URLencode],
    yyjsonr[...]
)

#' @export
get_locale <- function(x) {
    reg <- list(
        us = "en_US",
        eu = "en_GB"
    )
    reg[[x]]
}

#' @export
encode_string <- function(x) {
    tolower(URLencode(tolower(x), reserved = TRUE))
}

#' @export
resp_body_yyjson <- function(x) {
    if (!inherits(x, "httr2_response")) {
        stop("`resp` must be an HTTP response object")
    }

    text <- resp_body_string(x, "UTF-8")
    read_json_str(
        str = text,
        opts = opts_read_json(
            # arr_of_objs_to_df = FALSE
        )
    )
}

#' @export
is_safe <- function(x) {
    !is.null(x) && !inherits(x, "error")
}

#' @export
batch_list <- function(lst, group_size = 50L) {
    structure(
        split(lst, ceiling(seq_along(lst) / group_size)),
        names = NULL
    )
}

#' @export
batch_df <- function(df, group_size = 50L) {
    structure(
        split(df, ceiling(seq_len(nrow(df)) / group_size)),
        names = NULL
    )
}
