box::use(.. / base[...])
box::use(httr2[...], glue[glue])

#' @export
with_static_namespace <- function(req) {
 req_headers(
     req,
     "Battlenet-Namespace" = glue("static-{get_region()}")
 )
}

#' @export
with_dynamic_namespace <- function(req) {
 req_headers(
     req,
     "Battlenet-Namespace" = glue("dynamic-{get_region()}")
 )
}

#' @export
with_profile_namespace <- function(req) {
 req_headers(
     req,
     "Battlenet-Namespace" = glue("profile-{get_region()}")
 )
}