box::use(
    glue[glue]
)
box::use(
    . / base[...],
    .. / base[...]
)

#' @export
pvp_season <- function() {
    region <- get_region()
    endpoint <- "data/wow/pvp-season/index"
    glue(base_url)
}

#' @export
pvp_leaderboard <- function(season, bracket) {
    region <- get_region()
    endpoint <- glue("data/wow/pvp-season/{season}/pvp-leaderboard/{bracket}")
    glue(base_url)
}

#' @export
talent_tree <- function() {
    region <- get_region()
    endpoint <- glue("data/wow/talent-tree/index")
    glue(base_url)
}

#' @export
talent_tree_data <- function(tree_id, spec_id) {
    region <- get_region()
    endpoint <- glue("data/wow/talent-tree/{tree_id}/playable-specialization/{spec_id}")
    glue(base_url)
}
