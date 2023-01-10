box::use(
    purrr[map, pluck],
    httr2[multi_req_perform, resp_body_json]
)

box::use(
    R / logic[...],
    R / pipeline[...]
)

#' @export
pipeline_construct_requests <- function(batched_data, type, client) {
    map(seq_len(length(batched_data)), function(i) {
        row <- batched_data[[i]]
        map(row, function(x) {
            tryCatch(
                expr = {
                    name <- tolower(x$name)
                    realm <- tolower(x$realm)
                    request_fn <- switch(type,
                        profile = character_profile_request,
                        media = character_media_request,
                        equipment = character_equipment_request,
                        statistics = character_statistics_request,
                        talents = character_talents_request
                    )
                    request_fn(realm, name, client)
                }, error = function(e) {
                    NULL
                }
            )
        })
    })
}

#' @export
pipeline_perform_requests <- function(requests) {
    map(
        requests,
        multi_req_perform
    )
}

#' @export
pipeline_get_request_body <- function(performed_requests) {
    reqs <- unlist(performed_requests, recursive = FALSE)
    map(reqs, function(x) {
        tryCatch(
            expr = {
                if (inherits(x, "httr2_response")) {
                    resp_body_json(x)
                }
            }, error = function(e) {
                NULL
            }
        )
    })
}


#' @export
pipeline_profile_data <- function(profile_resp) {
    map(
        profile_resp,
        function(resp) {
            id <- try_pluck(resp, "id")
            if (!is.null(id)) {
                list(
                    id      = try_pluck(resp, "id"),
                    name    = try_pluck(resp, "name"),
                    gender  = try_pluck(resp, "gender", "type"),
                    faction = try_pluck(resp, "faction", "type"),
                    race    = try_pluck(resp, "race", "id"),
                    class   = try_pluck(resp, "character_class", "id"),
                    spec    = try_pluck(resp, "active_spec", "id"),
                    realm   = try_pluck(resp, "realm", "slug")
                )
            }
        }
    )
}

#' @export
pipeline_media_data <- function(media_resp) {
    map(media_resp, function(resp) {
        id <- try_pluck(resp, "character", "id")
        if (!is.null(id)) {
            list(
                id = id,
                avatar = try_pluck(resp, "assets", 1, "value"),
                inset = try_pluck(resp, "assets", 2, "value"),
                main = try_pluck(resp, "assets", 3, "value"),
                main_raw = try_pluck(resp, "assets", 4, "value")
            )
        }
    })
}

#' @export
pipeline_equipment_data <- function(equipment_resp) {
    map(equipment_resp, function(resp) {
        id <- try_pluck(resp, "character", "id")
        if (!is.null(id)) {
            list(
                id = id,
                items = map(
                    try_pluck(resp, "equipped_items"),
                    function(item) {
                        list(
                            id = try_pluck(item, "item", "id"),
                            slot = try_pluck(item, "slot", "type")
                        )
                    }
                )
            )
        }
    })
}

#' @export
pipeline_statistics_data <- function(statistics_resp) {
    map(statistics_resp, function(resp) {
        id <- try_pluck(resp, "character", "id")
        if (!is.null(id)) {
            list(
                id = id,
                mastery = try_pluck(resp, "mastery", "value"),
                versatility = try_pluck(resp, "versatility"),
                melee_haste = try_pluck(resp, "melee_haste", "value"),
                ranged_haste = try_pluck(resp, "ranged_haste", "value"),
                spell_haste = try_pluck(resp, "spell_haste", "value"),
                melee_crit = try_pluck(resp, "melee_crit", "value"),
                ranged_crit = try_pluck(resp, "ranged_crit", "value"),
                spell_crit = try_pluck(resp, "spell_crit", "value")
            )
        }
    })
}

#' @export
pipeline_talents_data <- function(talents_resp) {
    map(talents_resp, function(resp) {
        specs <- try_pluck(resp, "specializations")
        map(
            specs,
            function(spec) {
                list(
                    specialization = try_pluck(spec, "specialization", "id"),
                    pvp_talents = map(
                        try_pluck(spec, "pvp_talent_slots"),
                        function(slot) {
                            try_pluck(slot, "selected", "talent", "id")
                        }
                    ),
                    loadouts = map(
                        try_pluck(spec, "loadouts"),
                        function(loadout) {
                            list(
                                active = try_pluck(loadout, "is_active"),
                                code = try_pluck(loadout, "talent_loadout_code"),
                                class_talents = map(
                                    try_pluck(loadout, "selected_class_talents"),
                                    function(talent) {
                                        try_pluck(talent, "id")
                                    }
                                ),
                                spec_talents = map(
                                    try_pluck(loadout, "selected_spec_talents"),
                                    function(talent) {
                                        try_pluck(talent, "id")
                                    }
                                )
                            )
                        }
                    )
                )
            }
        )
    })
}
