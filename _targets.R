options(box.path = getwd())

box::use(
    targets[tar_target, tar_option_set, tar_cue],
    tarchetypes[tar_age, tar_map, tar_combine],
    tibble[tibble],
    jsonlite[write_json],
    httr2[multi_req_perform, resp_is_error, resp_body_json],
    dplyr[bind_rows]
)

box::use(
    R / logic[...],
    R / api[...],
    R / pipeline[...]
)

tar_option_set(
    format = "qs",
    memory = "transient",
    garbage_collection = TRUE
)

clear_logs()

list(
    tar_target(
        client,
        new_client(),
        cue = tar_cue(
            mode = "always"
        )
    ),
    # capstone_targets, ## todo
    season_targets,
    ladder_targets,
    tar_combine(
        master_player_list,
        safely_reduce(ladder_targets, 1L, 2L),
        command = bind_rows(!!!.x)
    ),
    profile_targets
)
