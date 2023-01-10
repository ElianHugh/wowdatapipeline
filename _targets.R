options(box.path = getwd())

box::use(
    targets[tar_target, tar_option_set],
    tarchetypes[tar_age, tar_map],
    purrr[pluck, map_df, map],
    tibble[tibble],
    jsonlite[write_json],
    httr2[multi_req_perform, resp_is_error, resp_body_json]
)

box::use(
    R / logic[...],
    R / pipeline[...]
)

tar_option_set(
    format = "qs",
    memory = "transient",
    garbage_collection = TRUE
)

list(
    tar_target(
        client,
        new_client(),
    ),
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
    ),
    tar_target(
        season,
        get_season(client)
    ),
    tar_map(
        values = tibble(
            bracket = c(
                "2v2"
                # "3v3",
                # "rbg"
            )
        ),
        tar_age(
            ladder_data,
            get_leaderboard(season, bracket, client),
            age = as.difftime(1, units = "days"),
            format = tar_json
        ),
        tar_target(
            batched_ladder_data,
            batch_list(ladder_data)
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
)
