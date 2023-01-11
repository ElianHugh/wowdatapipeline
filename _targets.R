options(box.path = getwd())

box::use(
    targets[tar_target, tar_option_set],
    tarchetypes[tar_age, tar_map, tar_combine],
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
    capstone_targets,
    season_targets,
    ladder_targets,
    tar_combine(
        master_player_list,
        safely_reduce(ladder_targets, 1, 2),
        use_names = FALSE,
        command = c(!!!.x)[!duplicated(c(!!!.x))]
    ),
    profile_targets
)
