box::use(
    targets[tar_target, tar_cue, tar_option_set],
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

tar_option_set(
    format = "qs",
    memory = "transient",
    garbage_collection = TRUE
)

#' @export
capstone_targets <- list(
    tar_target(
        input_talent_tree_ids,
        pipeline_talent_tree_ids(client)
    ),
    tar_target(
        input_talent_trees,
        pipeline_talent_trees(input_talent_tree_ids, client)
    ),
    tar_target(
        data_capstones,
        pipeline_capstone(input_talent_trees),
        format = tar_json
    )
)

#' @export
season_targets <- list(
    tar_target(
        input_season,
        pipeline_season_data(client),
        cue = tar_cue(
            mode = "always"
        )
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
            data,
            pipeline_leaderboard_data(input_season, bracket, client),
            age = as.difftime(1L, units = "days"),
            format = tar_json,
            cue = tar_cue(
                file = FALSE,
                seed = FALSE
            )
        ),
        tar_target(
            input_player_list,
            pipeline_player_list(data)
        )
    )
)

#' @export
profile_targets <- list(
    tar_target(
        input_batched_ladder_data,
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
            input_batched_requests,
            pipeline_construct_requests(input_batched_ladder_data, type, client)
        ),
        tar_target(
            input_performed_requests,
            pipeline_perform_requests(input_batched_requests)
        ),
        tar_target(
            input_responses,
            pipeline_get_request_body(input_performed_requests)
        ),
        tar_target(
            data,
            pipeline_extract_data(input_responses, type),
            format = tar_json,
            cue = tar_cue(
                file = FALSE,
                seed = FALSE
            )
        )
    )
)
