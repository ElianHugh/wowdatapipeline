box::use(httr2[...])

box::use(
    . / namespaces[...],
    .. / endpoints / data[...],
    .. / oauth[...],
    .. / base[...]
)

#' @export
pvp_season_request <- function(client) {
    api <- pvp_season()
    request(api) %>%
        with_dynamic_namespace() %>%
        with_oauth(client) %>%
        with_bnet_defaults()
}

#' @export
pvp_leaderboard_request <- function(season, bracket, client) {
    api <- pvp_leaderboard(season, bracket)
    request(api) %>%
        with_dynamic_namespace() %>%
        with_oauth(client) %>%
        with_bnet_defaults()
}

#' @export
talent_tree_request <- function(client) {
    api <- talent_tree()
    request(api) %>%
        with_static_namespace() %>%
        with_oauth(client) %>%
        with_bnet_defaults()
}

#' @export
talent_tree_data_request <- function(tree_id, spec_id, client) {
    api <- talent_tree_data(tree_id, spec_id)
    request(api) %>%
        with_static_namespace() %>%
        with_oauth(client) %>%
        with_bnet_defaults()
}