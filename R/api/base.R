box::use(
    httr2[...]
)

#' @export
get_region <- function() {
    "us"
}

#' @export
with_bnet_defaults <- function(req) {
    req_throttle(req, 100L) %>%
    req_url_query(locale = "en_US") %>%
    req_headers(`Accept-Encoding` = "gzip, deflate")
}