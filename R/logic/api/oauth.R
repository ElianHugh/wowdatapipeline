box::use(
    httr2[...]
)

#' @export
new_client <- function() {
    oauth_client(
        id = "464cbbb24c7543568f03fd7e2528cfed",
        secret = "aGrkTTFLvv6G0GFmmykzFRGL33XJdiqy",
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
