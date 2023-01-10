box::use(
    httr2[req_perform, req_retry, resp_is_error, resp_body_json],
    purrr[pluck],
    utils[URLencode]
)

#' @export
safe_request <- function(req_call) {
    tryCatch({
        req <- req_perform(req_call)
        if (resp_is_error(req)) {
            NULL
        } else {
            resp_body_json(req)
        }
    }, error = function(e) {
        # cat(
        #     "{",
        #     sprintf('"msg": "%s"', gsub('"', "'", e[["message"]])),
        #     sprintf(', "call": "%s"', gsub('"', "'", deparse(e[["call"]]))),
        #     sprintf(', "url": "%s"', req_call$url),
        #     "}\n",
        #     sep = "",
        #     append = TRUE,
        #     file = "output/log.ndjson"
        # )
        # NULL
    })
}

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
try_pluck <- function(x, ...) {
    tryCatch(
        {
            pluck(x, ...)
        },
        error = function(e) {
            list(NULL)
        }
    )
}
