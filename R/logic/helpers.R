box::use(
    httr2[req_perform, req_retry, resp_is_error, resp_body_json],
    utils[URLencode]
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
