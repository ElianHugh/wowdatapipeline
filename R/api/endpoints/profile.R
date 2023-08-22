box::use(
    glue[glue]
)
box::use(
    . / base[...],
    .. / base[...]
)

#' @export
character_profile <- function(realmslug, name) {
    region <- get_region()
    endpoint <- glue("profile/wow/character/{realmslug}/{name}")
    glue(base_url)
}

#' @export
character_media <- function(realmslug, name) {
    region <- get_region()
    endpoint <- glue("profile/wow/character/{realmslug}/{name}/character-media")
    glue(base_url)
}

#' @export
character_equipment <- function(realmslug, name) {
 region <- get_region()
 endpoint <- glue("profile/wow/character/{realmslug}/{name}/equipment")
 glue(base_url)
}

#' @export
character_statistics <- function(realmslug, name) {
    region <- get_region()
    endpoint <- glue("profile/wow/character/{realmslug}/{name}/statistics")
    glue(base_url)
}

#' @export
character_talents <- function(realmslug, name) {
    region <- get_region()
    endpoint <- glue("profile/wow/character/{realmslug}/{name}/specializations")
    glue(base_url)
}