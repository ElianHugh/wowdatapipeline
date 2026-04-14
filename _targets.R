options(box.path = getwd())

box::use(
    targets[tar_option_set],
    tarchetypes[tar_combine],
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

client <- new_client()

list(
    season_targets,
    ladder_targets,
    tar_combine(
        master_player_list,
        safely_reduce(ladder_targets, 1L, 2L),
        command = bind_rows(!!!.x) |>
            dplyr::distinct(id, .keep_all = TRUE)
    ),
    profile_targets,
    capstone_targets,
    db_targets
)
