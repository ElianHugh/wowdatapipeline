box::use(
    targets[tar_target, tar_cue],
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
        data_capstones,
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
            data_ladder,
            pipeline_leaderboard_data(season, bracket, client),
            age = as.difftime(1, units = "days"),
            format = tar_json,
            cue = tar_cue(
                file = FALSE,
                seed = FALSE
            )
        ),
        tar_target(
            player_list,
            pipeline_player_list(data_ladder)
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
                "statistics"
                # "talents"
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
            responses,
            pipeline_get_request_body(performed_requests)
        ),
        tar_target(
            data,
            pipeline_extract_data(responses, type),
            format = tar_json,
            cue = tar_cue(
                file = FALSE,
                seed = FALSE
            )
        )
    )
)