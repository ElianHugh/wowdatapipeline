box::use(
    httr2[multi_req_perform, resp_body_json]
)

box::use(
    R / logic[...],
    R / pipeline[...]
)

#' @export
pipeline_season_data <- function(client) {
    resp <- safe_request(pvp_season_request(client))
    res <- safely_reduce(
        resp,
        "current_season",
        "id",
        1
    )
    if (is.null(res)) {
        log_error(
            list(
                call = "pipeline_season_data",
                message = "Season returned NULL"
            )
        )
    }
    res
}

#' @export
pipeline_leaderboard_data <- function(season, bracket, client) {
    resp <- safe_request(pvp_leaderboard_request(season, bracket, client))
    entries <- safely_reduce(resp, "entries")
    lapply(
        entries,
        function(x) {
            char <- safely_reduce(x, "character")
            list(
                id = safely_reduce(char, "id", 1),
                name = safely_reduce(char, "name", 1),
                realm = safely_reduce(char, "realm", "slug", 1),
                faction = safely_reduce(x, "faction", "type", 1),
                rank = safely_reduce(x, "rank", 1),
                rating = safely_reduce(x, "rating", 1),
                played = safely_reduce(x, "season_match_statistics", "played", 1),
                won = safely_reduce(x, "season_match_statistics", "won", 1),
                lost = safely_reduce(x, "season_match_statistics", "lost", 1)
            )
        }
    )[1:60]
}

#' @export
pipeline_player_list <- function(ladder_data) {
    lapply(ladder_data, function(x) {
        id <- safely_reduce(x, "id", 1)
        if (!is.null(id)) {
            list(
                id = id,
                name = safely_reduce(x, "name", 1),
                realm = safely_reduce(x, "realm", 1)
            )
        }
    })
}

#' @export
pipeline_construct_requests <- function(batched_data, type, client) {
    lapply(seq_len(length(batched_data)), function(i) {
        row <- batched_data[[i]]
        lapply(row, function(x) {
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
        })
    })
}

#' @export
pipeline_perform_requests <- function(requests) {
    lapply(
        requests,
        multi_req_perform
    )
}

#' @export
pipeline_get_request_body <- function(performed_requests) {
    reqs <- unlist(performed_requests, recursive = FALSE)
    lapply(reqs, function(x) {
        if (inherits(x, "httr2_response")) {
            resp_body_json(x)
        }
    })
}

#' @export
pipeline_extract_data <- function(resp, type) {
    switch(type,
        "profile" = pipeline_profile_data,
        "media" = pipeline_media_data,
        "equipment" = pipeline_equipment_data,
        "statistics" = pipeline_statistics_data,
        "talents" = pipeline_talents_data
    )(resp)
}

pipeline_profile_data <- function(profile_resp) {
    lapply(
        profile_resp,
        function(resp) {
            id <- safely_reduce(resp, "id")
            if (!is.null(id)) {
                list(
                    id      = id,
                    name    = safely_reduce(resp, "name"),
                    gender  = safely_reduce(resp, "gender", "type"),
                    faction = safely_reduce(resp, "faction", "type"),
                    race    = safely_reduce(resp, "race", "id"),
                    class   = safely_reduce(resp, "character_class", "id"),
                    spec    = safely_reduce(resp, "active_spec", "id"),
                    realm   = safely_reduce(resp, "realm", "slug")
                )
            }
        }
    )
}

pipeline_media_data <- function(media_resp) {
    lapply(media_resp, function(resp) {
        id <- safely_reduce(resp, "character", "id")
        if (!is.null(id)) {
            list(
                id = id,
                avatar = safely_reduce(resp, "assets", 1, "value"),
                inset = safely_reduce(resp, "assets", 2, "value"),
                main = safely_reduce(resp, "assets", 3, "value"),
                main_raw = safely_reduce(resp, "assets", 4, "value")
            )
        }
    })
}

pipeline_equipment_data <- function(equipment_resp) {
    lapply(equipment_resp, function(resp) {
        id <- safely_reduce(resp, "character", "id")
        if (!is.null(id)) {
            list(
                id = id,
                items = lapply(
                    safely_reduce(resp, "equipped_items"),
                    function(item) {
                        list(
                            id = safely_reduce(item, "item", "id", 1),
                            slot = safely_reduce(item, "slot", "type", 1)
                        )
                    }
                )
            )
        }
    })
}

pipeline_statistics_data <- function(statistics_resp) {
    lapply(statistics_resp, function(resp) {
        id <- safely_reduce(resp, "character", "id")
        if (!is.null(id)) {
            list(
                id = id,
                mastery = safely_reduce(resp, "mastery", "rating", 1),
                versatility = safely_reduce(resp, "versatility", 1),
                melee_haste = safely_reduce(resp, "melee_haste", "rating", 1),
                ranged_haste = safely_reduce(resp, "ranged_haste", "rating", 1),
                spell_haste = safely_reduce(resp, "spell_haste", "rating", 1),
                melee_crit = safely_reduce(resp, "melee_crit", "rating", 1),
                ranged_crit = safely_reduce(resp, "ranged_crit", "rating", 1),
                spell_crit = safely_reduce(resp, "spell_crit", "rating", 1)
            )
        }
    })
}

pipeline_talents_data <- function(talents_resp) {
    lapply(talents_resp, function(resp) {
        specs <- safely_reduce(resp, "specializations")
        id <- safely_reduce(resp, "character", "id", 1)
        if (!is.null(id)) {
            list(
                id = id,
                specializations = lapply(
                    specs,
                    function(spec) {
                        list(
                            specialization = safely_reduce(spec, "specialization", "id"),
                            pvp_talents = lapply(
                                safely_reduce(spec, "pvp_talent_slots"),
                                function(slot) {
                                    safely_reduce(slot, "selected", "talent", "id")
                                }
                            ),
                            loadouts = lapply(
                                safely_reduce(spec, "loadouts"),
                                function(loadout) {
                                    list(
                                        active = safely_reduce(loadout, "is_active"),
                                        code = safely_reduce(loadout, "talent_loadout_code"),
                                        class_talents = lapply(
                                            safely_reduce(loadout, "selected_class_talents"),
                                            function(talent) {
                                                safely_reduce(talent, "tooltip", "spell_tooltip", "spell", "id")
                                            }
                                        ),
                                        spec_talents = lapply(
                                            safely_reduce(loadout, "selected_spec_talents"),
                                            function(talent) {
                                                list(
                                                    spell_id = safely_reduce(talent, "tooltip", "spell_tooltip", "spell", "id"),
                                                    talent_id = safely_reduce(talent, "tooltip", "talent", "id")
                                                )

                                            }
                                        )
                                    )
                                }
                            )
                        )
                    }
                )
            )
        } else {
            NULL
        }
    })
}
