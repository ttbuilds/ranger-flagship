library(tidyverse)
source("../utils/utils.R")
source("../utils/weapon_data.R")

feats_ass         <- c("CBE", "SS", "ResCon", "Alert", "Lucky")
feats_hex         <- c("CBE", "SS", "ResCon", "Alert", "Lucky")
feats_bugass     <- c("Bugbear", "SS", "ResCon", "Alert", "Lucky")
feat_at_first     <- TRUE
magic_weapon_at   <- 5
magic_weapon_plus <- 1
starting_stats <- c(
  str = 9,
  dex = 14+2,
  con = 15,
  int = 8,
  wis = 13,
  cha = 13
)

enemy_init_mod<- 2
advantage_pct <- 0.50
surprise_pct  <- 1.00
round1_weight <- 0.50
surge_pct     <- 1.00
hbc_pct       <- 1.00

hex5_progression <- c(
    paste("Ranger",   1:5), 
    paste("Cleric",   1),
    paste("Fighter",  1:4),
    paste("Sorcerer", 1),
    paste("Warlock",  1:5),
    paste("Rogue",    1:4))
    # paste("Sorcerer", 2:3),
    # paste("Ranger",   6:7))

ass_progression <- c(
    paste("Ranger",   1:5), 
    paste("Cleric",   1),
    paste("Fighter",  1:4),
    paste("Sorcerer", 1),
    paste("Warlock",  1:3),
    paste("Rogue",    1:3),
    paste("Warlock",  4:5),    
    paste("Rogue",    4))

flag_ranger_build_table <- function(build_table)
{
  fr_build_table <- build_table %>% 
    mutate(
      archery         = ranger_level >= 2 | fighter_level >= 1,
      dread_ambusher  = ranger_level >= 3,
      assassinate     = rogue_level >= 3,
      precision_atk   = fighter_level >= 3,
      hbc             = warlock_level >= 1,
      eldritch_smite  = warlock_level >= 5,
      double_smite    = eldritch_smite * (sorc_level >= 3),
      dex          = starting_stats["dex"] + 2 * cumsum(asi_feat == "+2 dex"),
      abi          = mod(dex),
      bugbear      = cumsum(asi_feat == "Bugbear") > 0,
      cbe          = cumsum(asi_feat == "CBE") > 0,
      ss           = cumsum(asi_feat == "SS") > 0,
      alert        = cumsum(asi_feat == "Alert") > 0,
      init_mod     = mod(dex) + 5 * alert,
      pct_win_init = mapply(win_contest_pct, bonus = init_mod, opposing = enemy_init_mod),      
      weapon       = ifelse(bugbear, "longbow", ifelse(hbc, "heavy_xbow", "hand_xbow")),
      weapon_dice  = weapon_dice[weapon],    
      magic_weapon = ifelse(level >= magic_weapon_at, magic_weapon_plus, 0),    
      smite_dice   = paste0(eldritch_smite * (1 + ceiling(warlock_level / 2)), "d8"),    
      dread_dice   = paste0(as.numeric(dread_ambusher), "d8"),    
      sneak_dice   = ceiling(rogue_level / 2) %>% paste0("d6"),    
      atk_mod      = prof + abi + 2 * archery + magic_weapon,
      atks_per_action   = 1 + extra_attack,    
      ba_atks           = cbe * (weapon == "hand_xbow")) %>%
    mutate(
      across(c(
        "extra_attack", "action_surge", "archery", "cbe", "ss", 
        "dread_ambusher", "precision_atk", "assassinate", "hbc", "eldritch_smite"),
        as.numeric)) %>%
    select(
      build_label,
      level, class_level, asi_feat, prof, dex, abi, init_mod, pct_win_init,
      default_ac, extra_attack,
      action_surge, archery, 
      bugbear, cbe, ss, 
      dread_ambusher, precision_atk, assassinate, hbc, eldritch_smite, double_smite,
      weapon, weapon_dice, magic_weapon,
      dread_dice, smite_dice, sneak_dice,
      atks_per_action, ba_atks,
      atk_mod
    )
  return(fr_build_table)
}

calculate_dpr <- function(build_table, situation_table) {
  dpr_table <- build_table %>%
    crossing(situation_table) %>%
    mutate(
      ac_adjustment  = relative_ac,
      target_ac      = default_ac + relative_ac,
      power_atk      = power_atk,
      advantage      = adv,
      surprise       = surprise,
      surging        = surge,
      cursed_target  = curse
    ) %>%
    rowwise() %>%
    mutate(
      to_hit         = hit_chance(
        atk = atk_mod - 5 * ss * power_atk + (4-0.5*adv)*precision_atk, 
        AC  = target_ac, 
        adv = adv),
      to_hit_adv     = hit_chance(
        atk_mod - 5 * ss * power_atk + 3.5 * precision_atk, AC = target_ac, adv = 1),
      to_hit_r1      = 
        (assassinate * pct_win_init) * to_hit_adv + 
        (1 - assassinate * round1 * pct_win_init) * to_hit,
      to_crit        = crit_chance(min_to_crit = 20 - hbc*curse, adv = adv),
      to_crit_adv    = crit_chance(min_to_crit = 20 - hbc*curse, adv = adv),      
      to_crit_r1     = (1 - assassinate*pct_win_init) * to_crit +
        assassinate * pct_win_init * (surprise*to_hit_adv + (1-surprise)*to_crit_adv), 
      dmg_formula    = paste(weapon_dice,"+",abi + 10*ss*power_atk + magic_weapon + cursed_target*prof),
      atks_per_round = 
        (1 + extra_attack + dread_ambusher * round1) * 
        (1 + action_surge * surge) + 
        ba_atks,
      atks_from_r2  = round1 * surprise * (atks_per_action+ba_atks),
      base_dpa      = 
        dpa(
          to_hit      = round1 * to_hit_r1 + (1-round1) * to_hit,
          to_crit     = round1 * to_crit_r1 + (1-round1) * to_crit, 
          dmg_formula = dmg_formula),
      dread_dpa = 
        dpa(
          to_hit      = to_hit_r1, 
          to_crit     = to_crit_r1, 
          dmg_formula = dread_dice),
      base_dmg    = atks_per_round * base_dpa,
      base_dmg_r2 = atks_from_r2 * dpa(to_hit, to_crit, dmg_formula),
      dread_dmg = round1 * (1 + action_surge * surge) * dread_dpa,
      sneak_dmg = 
        bonus_dpt(
          to_hit      = round1 * to_hit_r1 + (1 - round1) * to_hit, 
          to_crit     = round1 * to_crit_r1 + (1 - round1) * to_crit, 
          n_atks      = atks_per_round, 
          dmg_formula = sneak_dice) + 
        pct_win_init * bonus_dpt(to_hit, to_crit, atks_from_r2, sneak_dice),
      smite_dmg = 
        bonus_dpt(
          to_hit      = round1 * to_hit_r1 + (1-round1) * to_hit, 
          to_crit     = round1 * to_crit_r1 + (1-round1) * to_crit, 
          n_atks      = atks_per_round,
          dmg_formula = smite_dice) +
        pct_win_init * double_smite * bonus_dpt(to_hit, to_crit, atks_from_r2, smite_dice),
      bugbear_dmg = 
        surprise * round1 * bugbear * bonus_dpt(to_hit_r1, to_crit_r1, atks_per_round, "2d6"),
      total_dmg = 
        base_dmg + pct_win_init * base_dmg_r2 + dread_dmg + sneak_dmg + smite_dmg + bugbear_dmg,
      scenario_weight = 
        dbinom(advantage, size = 1, prob = advantage_pct) *
        dbinom(surprise,  size = 1, prob = surprise_pct) *
        dbinom(surging,   size = 1, prob = surge_pct) *
        dbinom(cursed_target, size = 1, prob = hbc_pct)
      ) %>%
    select(
      build_label, level, ac_adjustment, target_ac, 
      round1, power_atk, advantage, surprise, surging, cursed_target,
      to_hit, to_crit, dmg_formula, atks_per_round, atks_from_r2,
      base_dpa, dread_dpa, 
      base_dmg, dread_dmg, sneak_dmg, smite_dmg, total_dmg, scenario_weight) %>%
    pivot_wider(
      names_from  = power_atk, 
      values_from = c(
        "to_hit", "to_crit", 
        "dmg_formula", "base_dpa", "dread_dpa", 
        "base_dmg", "dread_dmg", "sneak_dmg", "smite_dmg", "total_dmg")) %>% 
    mutate(use_pwr_atk = ifelse(total_dmg_0 < total_dmg_1, 1, 0)) %>% 
    pivot_longer(
      cols          = matches("([a-z]+_[a-z]+)_([01])"), 
      names_to      = c(".value", "pwr_atk"), 
      names_pattern = "([a-z]+_[a-z]+)_([01])") %>%
    filter(pwr_atk == use_pwr_atk) %>%
    select(-pwr_atk)
  return(dpr_table)
}

hex5_build_table <- 
  hex5_progression %>%
  make_build_frame("Hex5", feats_hex, feat_at_first) %>%
  flag_ranger_build_table()

ass_build_table <-  
  ass_progression %>%
  make_build_frame("Assassin3", feats_ass, feat_at_first) %>%
  flag_ranger_build_table()

bugass_build_table <-  
  ass_progression %>%
  make_build_frame("Bugbear Assassin3", feats_bugass, feat_at_first) %>%
  flag_ranger_build_table()

situation_table <- 
  expand_grid(
    relative_ac = 0,
    round1      = 1,
    power_atk   = c(0,1),
    adv         = c(0,1),
    surprise    = c(0,1),
    surge       = c(0,1),
    curse       = c(0,1)
  )

hex5_dmg <- hex5_build_table %>% calculate_dpr(situation_table)
hex5_avg <- hex5_dmg %>%
  group_by(build_label, level, advantage) %>%
  summarize(
    total_dmg = sum(total_dmg * (scenario_weight / advantage_pct))
  )

ass_dmg <- ass_build_table %>% calculate_dpr(situation_table)
ass_avg <- ass_dmg %>%
  group_by(build_label, level, advantage) %>%
  summarize(
    total_dmg = sum(total_dmg * (scenario_weight/ advantage_pct))
  )

bugass_dmg <- bugass_build_table %>% calculate_dpr(situation_table)
bugass_avg <- bugass_dmg %>%
  group_by(build_label, level, advantage) %>%
  summarize(
    total_dmg = sum(total_dmg * (scenario_weight/ advantage_pct))
  )

master_build_sequence <- hex5_build_table %>%
  bind_rows(ass_build_table)
write_csv(master_build_sequence, "deranger-builds.csv")

master_data <- hex5_dmg %>%
  bind_rows(ass_dmg)

write_csv(master_data, file = "deranger.csv")

plotty <- ass_avg %>%
  bind_rows(hex5_avg) %>%
  #bind_rows(bugass_avg) %>%
  # filter(surging & cursed_target & surprise) %>%
  ggplot(aes(x = level, y = total_dmg, color = build_label, linetype = factor(advantage))) +
  geom_line() +
  scale_x_continuous(name = "Level", breaks = 1:20) +
  scale_y_continuous(name = "Damage Before Enemy's First Turn", breaks = seq(0,300,10)) +
  scale_color_discrete(name = "Build Version") +
  scale_linetype_manual(
    name = "Advantage?", 
    values = c(`0` = 2, `1` = 1),
    labels = c(`0` = "No", `1` = "Yes"))

ggsave("~/tmp/tmp.png", plotty)