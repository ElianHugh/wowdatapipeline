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
      Devastation = 1468,
      Augmentation = 1473
    )[[spec_string]]
}
