box::use(
    targets[tar_target, tar_cue, tar_option_set],
    tarchetypes[tar_map, tar_age],
    tibble[tibble]
)

box::use(
    R / logic[...],
    . / ladder[...],
    . / capstone[...]
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
        pipeline_talent_trees(input_talent_tree_ids, client),
    ),
    tar_target(
        data_trees,
        purrr::map_dfr(input_talent_trees, \(tree) {
            dplyr::bind_rows(
                class = tree$talents$class,
                specialisation = tree$talents$specialisation,
                .id = "tree_type"
            )
        }),
        format = "qs"
    )
)

#' @export
season_targets <- list(
    tar_age(
        input_season,
        pipeline_season_data(client),
        age = as.difftime(1L, units = "days")
    )
)

#' @export
ladder_targets <- list(
    tar_map(
        values = tibble(
            bracket = c("2v2", "3v3")
        ),
        tar_age(
            data,
            pipeline_leaderboard_data(input_season, bracket, client),
            age = as.difftime(1L, units = "days")
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
        batch_df(master_player_list, group_size = 50L)
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
            data,
            pipeline_extract_data(input_batched_ladder_data, type, client),
            format = "qs"
        )
    ),
    tar_target(profile_tbl, dplyr::bind_rows(data_profile)),
    tar_target(media_tbl, dplyr::bind_rows(data_media)),
    tar_target(equipment_tbl, {
        dplyr::bind_rows(data_equipment) |>
            tidyr::unnest_wider(items, names_sep = "_") |>
            tidyr::pivot_wider(
                id_cols = id,
                names_from = items_slot,
                values_from = items_id,
                values_fn = \(x) x[[1]]
            )
    }),
    tar_target(statistics_tbl, dplyr::bind_rows(data_statistics)),
    tar_target(talents_tbl, {
        dplyr::bind_rows(data_talents) |>
            tidyr::unnest_wider(specializations, names_sep = "_") |>
            dplyr::rename(
                spec = specializations_specialization,
                pvp_talents = specializations_pvp_talents,
                spec_talents = specializations_spec_talents,
                class_talents = specializations_class_talents,
                loadout = specializations_loadout
            ) |>
            dplyr::mutate(
                pvp_talents = purrr::map(
                    pvp_talents,
                    \(talents) purrr::map_int(talents, "spell_id")
                )
            ) |>
            dplyr::select(-spec) |>
            dplyr::distinct(id, .keep_all = TRUE)
    }),
    tar_target(leaderboard_2v2, build_leaderboard_rows(data_2v2, profile_tbl, talents_tbl)),
    tar_target(leaderboard_3v3, build_leaderboard_rows(data_3v3, profile_tbl, talents_tbl))
)

#' @export
build_leaderboard_rows <- function(bracket_tbl, profile_tbl, talents_tbl) {
    bracket_tbl |>
        dplyr::left_join(
            profile_tbl |>
                dplyr::select(
                    id,
                    race,
                    class,
                    spec,
                    body_type
                ),
            by = "id"
        ) |>
        dplyr::left_join(
            talents_tbl |>
                dplyr::select(id, hero),
            by = "id"
        ) |>
        dplyr::distinct(id, .keep_all = TRUE)
}

#' @export
write_db_tables <- function(
    twos,
    threes,
    media,
    equipment,
    statistics,
    talents,
    path
) {
    tmp_path <- paste0(path, ".next")
    if (file.exists(tmp_path)) file.remove(tmp_path)

    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = tmp_path)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

    DBI::dbWriteTable(con, "bracket_2v2", twos, overwrite = TRUE)
    DBI::dbWriteTable(con, "bracket_3v3", threes, overwrite = TRUE)
    DBI::dbWriteTable(con, "player_media", media, overwrite = TRUE)
    DBI::dbWriteTable(con, "player_equipment", equipment, overwrite = TRUE)
    DBI::dbWriteTable(con, "player_statistics", statistics, overwrite = TRUE)
    DBI::dbWriteTable(con, "player_talents", talents, overwrite = TRUE)
    file.rename(tmp_path, path)
    normalizePath(path, mustWork = FALSE)
}

#' @export
db_targets <- list(
    tar_target(
        duckdb_tables,
        write_db_tables(
            leaderboard_2v2,
            leaderboard_3v3,
            media_tbl,
            equipment_tbl,
            statistics_tbl,
            talents_tbl,
            "_targets/user/players.duckdb"
        ),
        format = "file"
    )
)