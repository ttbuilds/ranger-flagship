library(tidyverse)
source("../utils/utils.R")
source("../utils/weapon_data.R")

feats             <- c("CBE", "SS", "ResCon", "Alert", "Lucky")
magic_weapon_at   <- 5
magic_weapon_plus <- 1

build_table <- tibble(
  level         = 1:20,
  class_level   = c(
    paste("Ranger",   1:5), 
    paste("Cleric",   1),
    paste("Fighter",  1:4),
    paste("Sorcerer", 1),
    paste("Warlock",  1:3),
    paste("Rogue",    1:3),
    paste("Warlock",  4:5),    
    paste("Rogue",    4))) %>%
  mutate(
    current_class       = gsub("([A-Za-z]+) ([0-9]+)", "\\1", class_level),
    current_class_level = gsub("([A-Za-z]+) ([0-9]+)", "\\2", class_level),
    ranger_level  = cumsum(current_class == "Ranger"),
    cleric_level  = cumsum(current_class == "Cleric"),
    fighter_level = cumsum(current_class == "Fighter"),
    sorc_level    = cumsum(current_class == "Sorcerer"),
    warlock_level = cumsum(current_class == "Warlock"),
    rogue_level   = cumsum(current_class == "Rogue"),
    asi_level     = level == 1 | current_class_level %in% c(4,8,12,16,19),
    asi_feat      = "",
    prof          = get_proficiency(level),
    archery       = pmax(ranger_level, fighter_level) >= 2,
    extra_attack  = pmax(ranger_level, fighter_level) >= 5,
    action_surge  = fighter_level >= 2,
    dread_ambusher= ranger_level  >= 3
  )

total_asis = with(build_table, sum(asi_level))
build_table[build_table$asi_level, "asi_feat"] <- feats[1:total_asis]

dpr_table <- build_table %>% mutate(
  dex          = 16 + 2 * cumsum(asi_feat == "+2 dex"),
  cbe          = cumsum(asi_feat == "CBE") > 0,
  ss           = cumsum(asi_feat == "SS") > 0,
  abi          = mod(dex),
  magic_weapon = ifelse(level >= magic_weapon_at, magic_weapon_plus, 0),
  hbc          = warlock_level >= 1,
  weapon       = ifelse(hbc, "heavy_xbow", "hand_xbow"),
  weapon_dice  = weapon_dice[weapon],
  atk_mod      = prof + abi + 2 * archery - 5 * ss + magic_weapon,
  enemy_ac     = baseline_ac(level)) %>%
  rowwise() %>%
  mutate(
    base_tohit   = hit_chance(atk_mod, AC = enemy_ac),
    base_tocrit  = crit_chance(min_to_crit = 20 - hbc),
    adv_tohit    = hit_chance(atk_mod, AC = enemy_ac, adv = 1),
    adv_tocrit   = crit_chance(min_to_crit = 20 - hbc, adv = 1),
    dmg_formula  = paste(weapon_dice,"+",abi+10*ss+magic_weapon+hbc*prof),
    dread_dph    = "1d8",
    atks_per_action = 1 + extra_attack,
    ba_atk       = cbe * (weapon == "hand_xbow"),
    surge_atks   = action_surge * (atks_per_action + dread_ambusher),
    base_atks    = atks_per_action + ba_atk,
    nova_atks_nosurge = base_atks + dread_ambusher,
    nova_atks_surge   = nova_atks_nosurge + surge_atks,
    atk_base_dmg = dpa(base_tohit, base_tocrit, dmg_formula),
    atk_adv_dmg  = dpa(adv_tohit,  adv_tocrit, dmg_formula),
    dread_dpa    = dpa(base_tohit, base_tocrit, dread_dph),
    dread_adv_dpa  = dpa(adv_tohit,  adv_tocrit,  dread_dph),
    base_dpr       = base_atks * atk_base_dmg,
    base_adv_dpr   = base_atks * atk_adv_dmg,    
    nova_dpr_nosurge = nova_atks_nosurge * atk_base_dmg + dread_dpa,
    nova_adv_nosurge = nova_atks_nosurge * atk_adv_dmg + dread_adv_dpa,    
    surge_dpr        = surge_atks * atk_base_dmg + action_surge * dread_dpa,
    surge_adv_dpr    = surge_atks * atk_adv_dmg + action_surge * dread_adv_dpa,
    nova_dpr_surge   = nova_dpr_nosurge + surge_dpr,
    nova_adv_surge   = nova_adv_nosurge + surge_adv_dpr,
    has_smite        = warlock_level >= 5,
    smite_formula    = "4d8",
    smite_base_dmg       = has_smite * bonus_dpt(base_tohit, base_tocrit, nova_atks_nosurge, smite_formula),
    smite_surge_dmg      = has_smite * bonus_dpt(base_tohit, base_tocrit, nova_atks_surge, smite_formula),
    smite_adv_dmg        = has_smite * bonus_dpt(adv_tohit, adv_tocrit, nova_atks_nosurge, smite_formula),    
    smite_adv_surge_dmg  = has_smite * bonus_dpt(adv_tohit, adv_tocrit, nova_atks_surge, smite_formula),        
    smite_turn_dmg       = nova_dpr_nosurge + smite_base_dmg,
    smite_adv_turn_dmg   = nova_adv_nosurge + smite_adv_dmg,
    smite_surge_turn_dmg = nova_dpr_surge + smite_surge_dmg,    
    smite_adv_surge_turn_dmg = nova_adv_surge + smite_adv_surge_dmg,
    sneak_dice    = ceiling(rogue_level / 2),
    sneak_expr    = paste0(sneak_dice, "d6"),
    sneak_dpr     = bonus_dpt(base_tohit, base_tocrit, base_atks, sneak_expr),
    sneak_adv_dpr = bonus_dpt(adv_tohit,  adv_tocrit,  base_atks, sneak_expr),
    sneak_nova_nosurge_dpr = bonus_dpt(base_tohit, base_tocrit, nova_atks_nosurge, sneak_expr),
    sneak_nova_adv_nosurge_dpr = bonus_dpt(adv_tohit, adv_tocrit, nova_atks_nosurge, sneak_expr),    
    sneak_nova_surge_dpr = bonus_dpt(base_tohit, base_tocrit, nova_atks_surge, sneak_expr),        
    sneak_nova_adv_surge_dpr = bonus_dpt(adv_tohit, adv_tocrit, nova_atks_surge, sneak_expr),
    sneak_autocrit_nosurge_dmg = bonus_dpt(adv_tohit,  adv_tohit,   nova_atks_nosurge, sneak_expr),
    sneak_autocrit_surge_dmg = bonus_dpt(adv_tohit,  adv_tohit,   nova_atks_surge, sneak_expr),    
    atk_base_autocrit_dmg = dpa(adv_tohit, adv_tohit, dmg_formula),
    dread_autocrit_dmg = dpa(adv_tohit, adv_tohit, dread_dph),
    autocrit_nosurge_dpr = nova_atks_nosurge * atk_base_autocrit_dmg + dread_autocrit_dmg + sneak_autocrit_nosurge_dmg,
    autocrit_surge_dpr   = nova_atks_surge   * atk_base_autocrit_dmg + (1 + action_surge) * dread_autocrit_dmg + sneak_autocrit_surge_dmg
)

base_total_dpr     <- base_nova_dpr + sneak_dpr
base_adv_dpr       <- adv_nova_dpr  + sneak_adv_dpr
surprise_total_dpr <- surprise_base_dpr + surprise_dread_dpr + sneak_surprise_dpr
surprise_smite_dpr <- surprise_total_dpr + smite_autocrit_dpr

hex5_avg_nova <- function(adv_pct, smites_per_turn){
  (1 - adv_pct) * (1 - smites_per_turn) * nonsmite_turn_dmg +
  adv_pct       * (1 - smites_per_turn) * nonsmite_turn_dmg_adv +
  (1 - adv_pct) * smites_per_turn       * smite_turn_dmg +
  adv_pct       * smites_per_turn       * smite_adv_turn_dmg
}

ass_avg_nova <- function(adv_pct, surprise_chance) {
  (1 - adv_pct)   * (1 - surprise_chance) * base_total_dpr +
  adv_pct         * (1 - surprise_chance) * base_adv_dpr   +
  surprise_chance * surprise_total_dpr
}

result_table <- tibble(
  adv_pct       = rep(seq(0, 1, length = 11), times = 11),
  surprise_pct  = rep(seq(0, 0.50, length = 11), each  = 11)) %>%
  mutate(
    ass_nova_dmg  = ass_avg_nova(adv_pct, surprise_pct),
    hex5_nova_dmg = hex5_avg_nova(adv_pct, 0.5),
    hex5_edge     = hex5_nova_dmg - ass_nova_dmg)

result_table %>%
  ggplot(aes(x = adv_pct, y = surprise_pct, fill = hex5_edge, color = hex5_edge)) +
  geom_point() +
  scale_color_distiller(type = "div")