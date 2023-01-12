box::use(
    targets[tar_target],
    tarchetypes[tar_map, tar_age],
    tibble[tibble]
)

box::use(
    . / ladder[...],
    . / capstone[...],
    . / formats[tar_json],
    . / utils[...],
    . / helpers[...]
)

#' @export
capstone_targets <- list(
    tar_target(
        spec_talent_tree_ids,
        pipeline_talent_tree_ids(client)
    ),
    tar_target(
        spec_talent_trees,
        pipeline_talent_trees(spec_talent_tree_ids, client)
    ),
    tar_target(
        spec_capstones,
        pipeline_capstone(spec_talent_trees),
        format = tar_json
    )
)

#' @export
season_targets <- list(
    tar_target(
        season,
        pipeline_season_data(client)
    )
)

#' @export
ladder_targets <- list(
    tar_map(
        values = tibble(
            bracket = c(
                "2v2",
                "3v3",
                "rbg"
            )
        ),
        tar_age(
            ladder_data,
            pipeline_leaderboard_data(season, bracket, client),
            age = as.difftime(1, units = "days"),
            format = tar_json
        ),
        tar_target(
            player_list,
            pipeline_player_list(ladder_data)
        )
    )
)

#' @export
profile_targets <- list(
    tar_target(
        batched_ladder_data,
        batch_list(master_player_list)
    ),
    tar_map(
        values = tibble(
            type = c(
                "profile",
                "media",
                "equipment",
                "statistics",
                "talents"
            )
        ),
        tar_target(
            batched_requests,
            pipeline_construct_requests(batched_ladder_data, type, client)
        ),
        tar_target(
            performed_requests,
            pipeline_perform_requests(batched_requests)
        ),
        tar_target(
            resp,
            pipeline_get_request_body(performed_requests)
        )
    ),
    tar_target(
        profile_data,
        pipeline_profile_data(resp_profile),
        format = tar_json
    ),
    tar_target(
        media_data,
        pipeline_media_data(resp_media),
        format = tar_json
    ),
    tar_target(
        equipment_data,
        pipeline_equipment_data(resp_equipment),
        format = tar_json
    ),
    tar_target(
        statistics_data,
        pipeline_statistics_data(resp_statistics),
        format = tar_json
    ),
    tar_target(
        talents_data,
        pipeline_talents_data(resp_talents),
        format = tar_json
    )
)