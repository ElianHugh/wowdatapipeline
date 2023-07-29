#' @export
spec_to_id <- function(spec_string) {
  list(
    # Mage
    Arcane = 62L,
    Fire = 63L,
    Frost = 64L,
    # Paladin
    Holy = 65L,
    Protection = 66L,
    Retribution = 70L,
    # Warrior
    Arms = 71L,
    Fury = 72L,
    Protection = 73L,
    # Druid
    Balance = 102L,
    Feral = 103L,
    Guardian = 104L,
    Restoration = 105L,
    # DK
    Blood = 250L,
    Frost = 251L,
    Unholy = 252L,
    # Hunter
    "Beast Mastery" = 253L,
    Marksmanship = 254L,
    Survival = 255L,
    # Priest
    Discipline = 256L,
    Holy = 257L,
    Shadow = 258L,
    # Rogue
    Assassination = 259L,
    Outlaw = 260L,
    Subtlety = 261L,
    # Shaman
    Elemental = 262L,
    Enhancement = 263L,
    Restoration = 264L,
    # Warlock
    Affliction = 265L,
    Demonology = 266L,
    Destruction = 267L,
    # Monk
    Brewmaster = 268L,
    Windwalker = 269L,
    Mistweaver = 270L,
    # DH
    Havoc = 577L,
    Vengeance = 581L,
    # Evoker
    Preservation = 1467L,
    Devastation = 1468L,
    Augmentation = 1473L
  )[[spec_string]]
}
