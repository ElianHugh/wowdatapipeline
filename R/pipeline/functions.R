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
    try_pluck(
        resp,
        "current_season",
        "id"
    )
}

#' @export
get_leaderboard <- function(...) {
    resp <- safe_request(pvp_leaderboard_request(...))
    entries <- try_pluck(resp, "entries")
    map(
        entries,
        function(x) {
            char <- try_pluck(x, "character")
            list(
                id = try_pluck(char, "id"),
                name = try_pluck(char, "name"),
                realm = try_pluck(char, "realm", "slug"),
                faction = try_pluck(x, "faction", "type"),
                rank = try_pluck(x, "rank"),
                rating = try_pluck(x, "rating"),
                played = try_pluck(x, "season_match_statistics", "played"),
                won = try_pluck(x, "season_match_statistics", "won"),
                lost = try_pluck(x, "season_match_statistics", "lost")
            )
        }
    )
}

#' @export
get_media <- function(...) {
    resp <- safe_request(character_media_request(...))
    list(
        avatar = try_pluck(media, "assets", 1, "value"),
        inset = try_pluck(media, "assets", 2, "value"),
        main = try_pluck(media, "assets", 3, "value"),
        main_raw = try_pluck(media, "assets", 4, "value")
    )
}

#' @export
get_profile <- function(...) {
    resp <- safe_request(character_profile_request(...))
    list(
        id      = try_pluck(resp, "id"),
        name    = try_pluck(resp, "name"),
        gender  = try_pluck(resp, "gender", "type"),
        faction = try_pluck(resp, "faction", "type"),
        race    = try_pluck(resp, "race", "id"),
        class   = try_pluck(resp, "character_class", "id"),
        spec    = try_pluck(resp, "active_spec", "id"),
        realm   = try_pluck(resp, "realm", "slug")
    )
}

#' @export
get_talents <- function(...) {
    resp <- safe_request(character_talents_request(...))
    specs <- try_pluck(resp, "specializations")
    map(
        specs,
        function(spec) {
            list(
                specialization = try_pluck(spec, "specialization", "id"),
                pvp_talents = map(
                    try_pluck(spec, "pvp_talent_slots"),
                    function(slot) {
                        try_pluck(slot, "selected", "talent", "id")
                    }
                ),
                loadouts = map(
                    try_pluck(spec, "loadouts"),
                    function(loadout) {
                        list(
                            active = try_pluck(loadout, "is_active"),
                            code = try_pluck(loadout, "talent_loadout_code"),
                            class_talents = map(
                                try_pluck(loadout, "selected_class_talents"),
                                function(talent) {
                                    try_pluck(talent, "id")
                                }
                            ),
                            spec_talents = map(
                                try_pluck(loadout, "selected_spec_talents"),
                                function(talent) {
                                    try_pluck(talent, "id")
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
        id = try_pluck(resp, "character", "id"),
        mastery = try_pluck(resp, "mastery", "value"),
        versatility = try_pluck(resp, "versatility"),
        melee_haste = try_pluck(resp, "melee_haste", "value"),
        ranged_haste = try_pluck(resp, "ranged_haste", "value"),
        spell_haste = try_pluck(resp, "spell_haste", "value"),
        melee_crit = try_pluck(resp, "melee_crit", "value"),
        ranged_crit = try_pluck(resp, "ranged_crit", "value"),
        spell_crit = try_pluck(resp, "spell_crit", "value")
    )
}


#' @export
get_equip <- function(...) {
    resp <- safe_request(character_equipment_request(...))
    list(
        id = try_pluck(resp, "character", "id"),
        items = map(
            try_pluck(resp, "equipped_items"),
            function(item) {
                list(
                    id = try_pluck(item, "item", "id"),
                    slot = try_pluck(item, "slot", "type")
                )
            }
        )
    )
}

#' @export
get_talent_tree_ids <- function(...) {
    resp <- safe_request(talent_tree_request(...))
    x <- try_pluck(resp, "spec_talent_trees")
    map(
        x,
        function(tree) {
            list(
                tree_id = str_extract(
                    pluck(tree, "key", "href"),
                    pattern = "(?<=(talent-tree\\/))[0-9]*(?=\\/)"
                ),
                spec = try_pluck(tree, "name")
            )
        }
    )
}

#' @export
get_talent_data <- function(...) {
    resp <- safe_request(talent_tree_data_request(...))
    x <- try_pluck(
        resp,
        "class_talent_nodes"
    )

    map(
        x,
        function(item) {
            tryCatch(
                expr = {
                    list(
                        row = try_pluck(item, "display_row"),
                        spell_id = try_pluck(
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
