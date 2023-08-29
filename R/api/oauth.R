box::use(
    httr2[...]
)

#' @export
new_client <- function() {
    oauth_client(
        id = Sys.getenv("BLIZZAUTH_ID"),
        secret = Sys.getenv("BLIZZAUTH_SECRET"),
        token_url = "https://us.oauth.battle.net/oauth/token",
        name = "blizzard-oauth",
        auth = "body"
    )
}

#' @export
with_oauth <- function(req, client) {
    req_oauth_client_credentials(req, client = client) %>%
        req_headers(`Accept-Encoding` = "gzip, deflate") %>%
        req_headers("Accept" = "application/json")
}
