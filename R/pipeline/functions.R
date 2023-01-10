box::use(
    purrr[map, pluck],
    stringr[str_extract]
)

box::use(
    R / logic[...]
)

#' @export
get_season <- function(...) {
    resp <- safe_request(pvp_season_request(...))
    safely_reduce(
        resp,
        "current_season",
        "id",
        1
    )
}

#' @export
get_leaderboard <- function(...) {
    resp <- safe_request(pvp_leaderboard_request(...))
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
    )
}

#' @export
get_media <- function(...) {
    resp <- safe_request(character_media_request(...))
    list(
        avatar = safely_reduce(media, "assets", 1, "value", 1),
        inset = safely_reduce(media, "assets", 2, "value", 1),
        main = safely_reduce(media, "assets", 3, "value", 1),
        main_raw = safely_reduce(media, "assets", 4, "value", 1)
    )
}

#' @export
get_profile <- function(...) {
    resp <- safe_request(character_profile_request(...))
    list(
        id      = safely_reduce(resp, "id", 1),
        name    = safely_reduce(resp, "name", 1),
        gender  = safely_reduce(resp, "gender", "type", 1),
        faction = safely_reduce(resp, "faction", "type", 1),
        race    = safely_reduce(resp, "race", "id", 1),
        class   = safely_reduce(resp, "character_class", "id", 1),
        spec    = safely_reduce(resp, "active_spec", "id", 1),
        realm   = safely_reduce(resp, "realm", "slug", 1)
    )
}

#' @export
get_talents <- function(...) {
    resp <- safe_request(character_talents_request(...))
    specs <- safely_reduce(resp, "specializations", 1)
    lapply(
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
                                    safely_reduce(talent, "id")
                                }
                            ),
                            spec_talents = lapply(
                                safely_reduce(loadout, "selected_spec_talents"),
                                function(talent) {
                                    safely_reduce(talent, "id")
                                }
                            )
                        )
                    }
                )
            )
        }
    )
}

#' @export
get_stats <- function(...) {
    resp <- safe_request(character_statistics_request(...))
    list(
        id = safely_reduce(resp, "character", "id"),
        mastery = safely_reduce(resp, "mastery", "value"),
        versatility = safely_reduce(resp, "versatility"),
        melee_haste = safely_reduce(resp, "melee_haste", "value"),
        ranged_haste = safely_reduce(resp, "ranged_haste", "value"),
        spell_haste = safely_reduce(resp, "spell_haste", "value"),
        melee_crit = safely_reduce(resp, "melee_crit", "value"),
        ranged_crit = safely_reduce(resp, "ranged_crit", "value"),
        spell_crit = safely_reduce(resp, "spell_crit", "value")
    )
}


#' @export
get_equip <- function(...) {
    resp <- safe_request(character_equipment_request(...))
    list(
        id = safely_reduce(resp, "character", "id"),
        items = lapply(
            safely_reduce(resp, "equipped_items"),
            function(item) {
                list(
                    id = safely_reduce(item, "item", "id"),
                    slot = safely_reduce(item, "slot", "type")
                )
            }
        )
    )
}

#' @export
get_talent_tree_ids <- function(...) {
    resp <- safe_request(talent_tree_request(...))
    x <- safely_reduce(resp, "spec_talent_trees")
    lapply(
        x,
        function(tree) {
            list(
                tree_id = str_extract(
                    pluck(tree, "key", "href"),
                    pattern = "(?<=(talent-tree\\/))[0-9]*(?=\\/)"
                ),
                spec = safely_reduce(tree, "name")
            )
        }
    )
}

#' @export
get_talent_data <- function(...) {
    resp <- safe_request(talent_tree_data_request(...))
    x <- safely_reduce(
        resp,
        "class_talent_nodes"
    )

    lapply(
        x,
        function(item) {
            tryCatch(
                expr = {
                    list(
                        row = safely_reduce(item, "display_row"),
                        spell_id = safely_reduce(
                            item,
                            "ranks",
                            1,
                            "tooltip",
                            "spell_tooltip",
                            "spell",
                            "id"
                        )
                    )
                }, error = function(e) {
                    NULL
                }
            )
        }
    )
}

#' @export
get_player_info <- function(...) {
    list(
        media = get_media(...),
        profile = get_profile(...),
        talents = get_talents(...),
        stats = get_stats(...),
        equip = get_equip(...)
    )
}


#' @export
spec_to_id <- function(spec_string) {
    list(
      # Mage
      Arcane = 62,
      Fire = 63,
      Frost = 64,
      # Paladin
      Holy = 65,
      Protection = 66,
      Retribution = 70,
      # Warrior
      Arms = 71,
      Fury = 72,
      Protection = 73,
      # Druid
      Balance = 102,
      Feral = 103,
      Guardian = 104,
      Restoration = 105,
      # DK
      Blood = 250,
      Frost = 251,
      Unholy = 252,
      # Hunter
      "Beast Mastery" = 253,
      Marksmanship = 254,
      Survival = 255,
      # Priest
      Discipline = 256,
      Holy = 257,
      Shadow = 258,
      # Rogue
      Assassination = 259,
      Outlaw = 260,
      Subtlety = 261,
      # Shaman
      Elemental = 262,
      Enhancement = 263,
      Restoration = 264,
      # Warlock
      Affliction = 265,
      Demonology = 266,
      Destruction = 267,
      # Monk
      Brewmaster = 268,
      Windwalker = 269,
      Mistweaver = 270,
      # DH
      Havoc = 577,
      Vengeance = 581,
      # Evoker
      Preservation = 1467,
      Devastation = 1468
    )[[spec_string]]
}
