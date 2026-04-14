box::use(
    httr2[req_perform_parallel, resp_body_json],
    dplyr[...],
    tidyr[...]
)

box::use(
    R / logic[...],
    R / api[...],
    R / pipeline[...]
)

#' @export
pipeline_season_data <- function(client) {
    res <- pvp_season_request(client) |>
        safe_request() |>
        safely_reduce("current_season", "id", 1L)
    if (is.null(res)) {
        err <- list(
            call = "pipeline_season_data",
            message = "Season returned NULL"
        )
        log_error(err)
    }
    res
}

#' @export
pipeline_leaderboard_data <- function(season, bracket, client) {
    entries <- pvp_leaderboard_request(season, bracket, client) |>
        safe_request() |>
        safely_reduce("entries") |>
        hoist(faction, "type") |>
        unnest_wider(season_match_statistics) |>
        unnest_wider(character) |>
        hoist(realm, "slug") |>
        select(
            id,
            name,
            realm = slug,
            faction = type,
            rank,
            rating,
            played,
            won,
            lost
        )
    entries[1L:1000L, ]
}

#' @export
pipeline_player_list <- function(ladder_data) {
    ladder_data |>
        select(id, name, realm) |>
        filter(!is.null(id))
}

#' @export
pipeline_construct_requests <- function(batched_data, type, client) {
    lapply(batched_data, function(x) {
        mapply(
            \(id, realm, name) {
                name <- tolower(name)
                realm <- tolower(realm)
                if (!is.null(name) && !is.null(realm)) {
                    request_fn <- switch(type,
                        profile = character_profile_request,
                        media = character_media_request,
                        equipment = character_equipment_request,
                        statistics = character_statistics_request,
                        talents = character_talents_request
                    )
                    request_fn(realm, name, client)
                }
            },
            id = x$id,
            realm = x$realm,
            name = x$name,
            SIMPLIFY = FALSE
        )
    })
}

#' @export
pipeline_perform_requests <- function(requests) {
    lapply(
        requests,
        \(req) req_perform_parallel(req, on_error = "continue")
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
pipeline_extract_data <- function(batched_data, type, client) {
    resp <- pipeline_construct_requests(batched_data, type, client) |>
        pipeline_perform_requests() |>
        pipeline_get_request_body()

    switch(type,
        "profile" = pipeline_profile_data,
        "media" = pipeline_media_data,
        "equipment" = pipeline_equipment_data,
        "statistics" = pipeline_statistics_data,
        "talents" = pipeline_talents_data
    )(resp)
}

pipeline_profile_data <- function(profile_resp) {
    lapply(profile_resp, extract_profile_data)
}

pipeline_media_data <- function(media_resp) {
    lapply(media_resp, extract_media_data)
}

pipeline_equipment_data <- function(equipment_resp) {
    lapply(equipment_resp, extract_equipment_data)
}


pipeline_statistics_data <- function(statistics_resp) {
    lapply(statistics_resp, extract_statistics_data)
}

pipeline_talents_data <- function(talents_resp) {
    lapply(talents_resp, extract_talents_data)
}

extract_leaderboard_data <- function(x) {
    char <- safely_reduce(x, "character")
    list(
        id = safely_reduce(char, "id", 1L),
        name = safely_reduce(char, "name", 1L),
        realm = safely_reduce(char, "realm", "slug", 1L),
        faction =safely_reduce(x, "faction", "type", 1L) |>
            tolower(),
        rank = safely_reduce(x, "rank", 1L),
        rating = safely_reduce(x, "rating", 1L),
        played = safely_reduce(x, "season_match_statistics", "played", 1L),
        won = safely_reduce(x, "season_match_statistics", "won", 1L),
        lost = safely_reduce(x, "season_match_statistics", "lost", 1L)
    )
}

extract_profile_data <- function(resp) {
    id <- safely_reduce(resp, "id")
    if (!is.null(id)) {
        list(
            id      = id,
            name    = safely_reduce(resp, "name"),
            body_type  = switch(
                safely_reduce(resp, "gender", "type"),
                "MALE" = 0L,
                "FEMALE" = 1L,
                -1
            ),
            faction = safely_reduce(resp, "faction", "type") |>
                tolower(),
            race    = safely_reduce(resp, "race", "id"),
            class   = safely_reduce(resp, "character_class", "id"),
            spec    = safely_reduce(resp, "active_spec", "id"),
            realm   = safely_reduce(resp, "realm", "slug")
        )
    }
}

extract_media_data <- function(resp) {
    id <- safely_reduce(resp, "character", "id")
    if (!is.null(id)) {
        list(
            id = id,
            avatar = safely_reduce(resp, "assets", 1L, "value"),
            inset = safely_reduce(resp, "assets", 2L, "value"),
            main = safely_reduce(resp, "assets", 3L, "value")
        )
    }
}

extract_equipment_data <- function(resp) {
    id <- safely_reduce(resp, "character", "id")
    if (!is.null(id)) {
        list(
            id = id,
            items = lapply(
                safely_reduce(resp, "equipped_items"),
                function(item) {
                    list(
                        id = safely_reduce(item, "item", "id", 1L),
                        slot = safely_reduce(item, "slot", "type", 1L) |>
                            tolower()
                    )
                }
            )
        )
    }
}

extract_statistics_data <- function(resp) {
    id <- safely_reduce(resp, "character", "id")
    if (!is.null(id)) {
        list(
            id = id,
            mastery = safely_reduce(resp, "mastery", "rating_normalized", 1L),
            versatility = safely_reduce(resp, "versatility", 1L),
            melee_haste = safely_reduce(resp, "melee_haste", "rating_normalized", 1L),
            ranged_haste = safely_reduce(resp, "ranged_haste", "rating_normalized", 1L),
            spell_haste = safely_reduce(resp, "spell_haste", "rating_normalized", 1L),
            melee_crit = safely_reduce(resp, "melee_crit", "rating_normalized", 1L),
            ranged_crit = safely_reduce(resp, "ranged_crit", "rating_normalized", 1L),
            spell_crit = safely_reduce(resp, "spell_crit", "rating_normalized", 1L)
        )
    }
}

extract_talents_data <- function(resp) {
    specs <- safely_reduce(resp, "specializations")
    id <- safely_reduce(resp, "character", "id", 1L)
    active_hero <- safely_reduce(resp, "active_hero_talent_tree", "id")

    get_spell_id <- function(talent) {
    safely_reduce(talent, c("tooltip", "spell_tooltip", "spell", "id")) %||%
        safely_reduce(talent, c("tooltip", "talent", "id")) %||%
        NA_integer_
    }


    if (!is.null(id)) {
        list(
            id = id,
            hero = active_hero,
            specializations = lapply(
                specs,
                function(spec) {
                    # TODO, ignore the active, we need to find
                    # the spec that matches the player's spec
                    # according to the **ladder**
                    active_loadout <- NULL
                    loadouts <- safely_reduce(spec, "loadouts")

                    if (!is.null(loadouts)) {
                        active_idx <- which(vapply(
                            loadouts,
                            function(x) isTRUE(x$is_active),
                            logical(1)
                        ))
                        if (length(active_idx) > 0) {
                            active_loadout <- loadouts[[active_idx[1]]]
                        }
                    }

                    list(
                        loadout = safely_reduce(active_loadout, "talent_loadout_code"),
                        specialization = safely_reduce(
                            spec,
                            "specialization",
                            "id"
                        ),
                        pvp_talents = lapply(
                            safely_reduce(spec, "pvp_talent_slots"),
                            function(slot) {
                                list(
                                    spell_id = safely_reduce(
                                        slot,
                                        "selected",
                                        "spell_tooltip",
                                        "spell",
                                        "id"
                                    )
                                )
                            }
                        ),
                        class_talents = vapply(
                            safely_reduce(active_loadout, "selected_class_talents") %||% list(),
                            get_spell_id,
                            integer(1)
                        ),

                        spec_talents = vapply(
                            safely_reduce(active_loadout, "selected_spec_talents") %||% list(),
                            get_spell_id,
                            integer(1)
                        )

                        # loadouts = lapply(
                        #     {
                        #         loadouts <- safely_reduce(spec, "loadouts")
                        #         active_loadout <- Filter(\(x) isTRUE(safely_reduce(x, "is_active")), loadouts)
                        #         active_loadout <- active_loadout[[1]] %||% NULL
                        #     },
                        #     function(loadout) {
                        #         list(
                        #             active = safely_reduce(loadout, "is_active"),
                        #             code = safely_reduce(loadout, "talent_loadout_code"),
                        #             class_talents = lapply(
                        #                 safely_reduce(loadout, "selected_class_talents"),
                        #                 function(talent) {
                        #                     list(
                        #                         spell_id = safely_reduce(talent, "tooltip", "spell_tooltip", "spell", "id"),
                        #                         talent_id = safely_reduce(talent, "tooltip", "talent", "id")
                        #                     )
                        #                 }
                        #             ),
                        #             spec_talents = lapply(
                        #                 safely_reduce(loadout, "selected_spec_talents"),
                        #                 function(talent) {
                        #                     list(
                        #                         spell_id = safely_reduce(talent, "tooltip", "spell_tooltip", "spell", "id"),
                        #                         talent_id = safely_reduce(talent, "tooltip", "talent", "id")
                        #                     )
                        #                 }
                        #             )
                        #         )
                        #     }
                        # )
                    )
                }
            )
        )
    } else {
        NULL
    }
}




