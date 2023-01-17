box::use(httr2[...])

box::use(
    . / namespaces[...],
    .. / endpoints / profile[...],
    .. / oauth[...],
    .. / base[...]
)

#' @export
character_profile_request <- function(realmslug, name, client) {
    api <- character_profile(realmslug, name)
    request(api) %>%
        with_profile_namespace() %>%
        with_oauth(client) %>%
        with_bnet_defaults()
}

#' @export
character_media_request <- function(realmslug, name, client) {
    api <- character_media(realmslug, name)
    request(api) %>%
        with_profile_namespace() %>%
        with_oauth(client) %>%
        with_bnet_defaults()
}

#' @export
character_equipment_request <- function(realmslug, name, client) {
    api <- character_equipment(realmslug, name)
    request(api) %>%
        with_profile_namespace() %>%
        with_oauth(client) %>%
        with_bnet_defaults()
}

#' @export
character_statistics_request <- function(realmslug, name, client) {
    api <- character_statistics(realmslug, name)
    request(api) %>%
        with_profile_namespace() %>%
        with_oauth(client) %>%
        with_bnet_defaults()
}

#' @export
character_talents_request <- function(realmslug, name, client) {
    api <- character_talents(realmslug, name)
    request(api) %>%
        with_profile_namespace() %>%
        with_oauth(client) %>%
        with_bnet_defaults()
}