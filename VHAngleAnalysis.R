# ============================================================================
# VHAngleAnalysis.R
# ============================================================================
#
# PURPOSE
# -------
# Describes the distribution of vertical angle mismatch across swings and
# players, establishes its relationship to contact rate, and produces
# player-level profiles for use in downstream modeling and validation.
#
# This file intentionally contains no models. Swing-level logistic modeling
# is in VHAngleContactModel.R. Cross-year validation is in VHAngleValidation.R.
#
# CORE METRIC
# -----------
# signed_diff = attack_angle - vaa_contact
#
#   Positive: bat ascending more than pitch descending → lift and contact
#   Near zero: bat matches descending pitch plane → topped grounders
#   Negative: bat above pitch plane → weak topped contact, rarely whiffs
#   Large positive: bat passes under the ball → whiffs
#
# The relationship with contact rate is asymmetric and nonlinear. A GAM is
# fitted to swing-level contact outcomes to characterize the curve, and the
# productive window is derived empirically as the range where predicted
# contact probability is within CONTACT_WINDOW_THRESHOLD pp of its peak.
#
# OUTPUTS
# -------
#   swing_data       — full pitch-level data (needed by VHAngleContactModel.R)
#   swings           — all swings with signed_diff
#   player_overall   — overall player profile table (primary output)
#   player_zone      — in-zone vs chase splits
#   player_pitch     — pitch-type splits (long format)
#   player_profile() — single-player summary
#   contact_curve    — GAM predictions across signed_diff range
#   productive_window — list(low, high, peak) derived from contact_curve
# ============================================================================

library(tidyverse)
library(mgcv)
library(ggplot2)
library(gridExtra)
library(viridis)
library(lmtest)
library(ggrepel)

source("VHAngle_utils.R")


# ============================================================================
# CONFIGURATION
# ============================================================================

MIN_SWINGS_PLAYER  <- 200   # minimum swings to include a player
MIN_SWINGS_PITCH   <-  15   # minimum swings vs a pitch type for that split
MIN_SWINGS_ZONE    <-  30   # minimum swings in/out of zone for zone splits

# Productive window is derived from the contact GAM, not set manually.
# This threshold controls how close to peak a value needs to be to qualify.
CONTACT_WINDOW_THRESHOLD <- 2   # pp below peak contact rate


# ============================================================================
# 1. LOAD AND PREPROCESS
# ============================================================================

cat("Loading data...\n")
swing_data <- read_csv("savantData2025.csv", show_col_types = FALSE) %>%
  preprocess_statcast()

swings <- build_swings(swing_data) %>%
  mutate(signed_diff = vert_angle_diff_contact)

cat(sprintf("Pitches: %s | Swings: %s | Contact rate: %.1f%%\n\n",
            scales::comma(nrow(swing_data)),
            scales::comma(nrow(swings)),
            mean(swings$contact, na.rm = TRUE) * 100))


# ============================================================================
# 2. SIGNED MISMATCH DISTRIBUTION
# ============================================================================
#
# Shows the shape of the signed_diff distribution and how contact rate
# changes across the mismatch spectrum. Bins with n < 500 are flagged
# as unreliable — extreme negative values in particular are dominated
# by bunts and check swings, not representative full swings.

# ============================================================================
# 2. CONTACT CURVE — GAM FIT
# ============================================================================
#
# Fits a GAM (thin-plate spline) to swing-level contact outcomes as a function
# of signed_diff. This gives a smooth, continuous characterization of the
# contact rate curve without arbitrary binning choices.
#
# The productive window is then derived as the range of signed_diff where the
# predicted contact probability is within CONTACT_WINDOW_THRESHOLD pp of its
# peak, restricted to the reliable data range (signed_diff -20° to +40°,
# based on sample sizes from the binned exploration).

cat("=== CONTACT CURVE (GAM) ===\n\n")

gam_data <- swings %>%
  filter(!is.na(signed_diff), !is.na(contact),
         between(signed_diff, -20, 45))

contact_gam <- gam(contact ~ s(signed_diff, k = 15),
                   family = binomial,
                   data   = gam_data)

# Predict across the reliable range at 0.5° resolution
contact_curve <- tibble(signed_diff = seq(-20, 45, by = 0.5)) %>%
  mutate(
    pred_logit    = predict(contact_gam, newdata = .),
    pred_contact  = plogis(pred_logit) * 100
  )

# Derive productive window from the curve
peak_contact  <- max(contact_curve$pred_contact)
peak_diff     <- contact_curve$signed_diff[which.max(contact_curve$pred_contact)]
window_data   <- contact_curve %>% filter(pred_contact >= peak_contact - CONTACT_WINDOW_THRESHOLD)

productive_window <- list(
  low  = min(window_data$signed_diff),
  high = max(window_data$signed_diff),
  peak = peak_diff,
  peak_contact = round(peak_contact, 1)
)

cat(sprintf("GAM contact peak:    %+.1f°  (predicted contact: %.1f%%)\n",
            productive_window$peak, productive_window$peak_contact))
cat(sprintf("Productive window:   %+.1f° to %+.1f°  (within %d pp of peak)\n\n",
            productive_window$low, productive_window$high, CONTACT_WINDOW_THRESHOLD))

cat("Contact rate at key reference points:\n")
contact_curve %>%
  filter(signed_diff %in% c(-10, 0, 10, 16, 20, 26, 32, 40)) %>%
  mutate(pred_contact = round(pred_contact, 1)) %>%
  select(signed_diff, pred_contact) %>%
  print()
cat("\n")


# ============================================================================
# 3. PLAYER PROFILES
# ============================================================================
#
# One row per player. Includes:
#   - Volume and outcomes (n_swings, contact_rate, whiff_rate)
# Three separate player metric tables, each focused on one dimension.
# player_profile() brings them together for a single player.

cat("=== PLAYER PROFILES ===\n\n")

# Eligible players — must have minimum swings overall
eligible_players <- swings %>%
  filter(!is.na(signed_diff), pitch_group != "Other") %>%
  count(player_name) %>%
  filter(n >= MIN_SWINGS_PLAYER) %>%
  pull(player_name)

cat(sprintf("Players with >= %d swings: %d\n\n", MIN_SWINGS_PLAYER, length(eligible_players)))

swings_eligible <- swings %>%
  filter(player_name %in% eligible_players,
         !is.na(signed_diff),
         pitch_group != "Other") %>%
  mutate(
    # Per-swing GAM-predicted contact probability.
    # Used to compute expected_contact_rate per player — a better summary
    # of swing plane quality than mean + SD because it respects the
    # nonlinear, asymmetric shape of the contact curve.
    gam_pred_contact = plogis(predict(
      contact_gam,
      newdata = data.frame(signed_diff = signed_diff)
    )) * 100
  )


# ---- Table 1: Overall ----
# One row per player. Volume, outcomes, overall signed diff, and swing plane
# classification. The starting point for any player comparison.

player_overall <- swings_eligible %>%
  group_by(player_name) %>%
  summarise(
    n_swings             = n(),
    contact_rate         = mean(contact,          na.rm = TRUE) * 100,
    whiff_rate           = mean(whiff,            na.rm = TRUE) * 100,
    avg_bat_speed        = mean(bat_speed,        na.rm = TRUE),
    avg_signed_diff      = mean(signed_diff,      na.rm = TRUE),
    sd_signed_diff       = sd(signed_diff,        na.rm = TRUE),
    
    # Expected contact: average of per-swing GAM predictions.
    # Captures the full shape of the player's mismatch distribution —
    # not just the mean but the cost of variance given the curve's asymmetry.
    expected_contact_rate = mean(gam_pred_contact, na.rm = TRUE),
    
    # Window distribution: where do this player's swings actually fall?
    pct_below_window     = mean(signed_diff <  productive_window$low,  na.rm = TRUE) * 100,
    pct_in_window        = mean(between(signed_diff,
                                        productive_window$low,
                                        productive_window$high),       na.rm = TRUE) * 100,
    pct_above_window     = mean(signed_diff >  productive_window$high, na.rm = TRUE) * 100,
    
    .groups = "drop"
  ) %>%
  mutate(
    # How much does the player over/underperform their swing plane distribution?
    # Positive = better actual contact than the GAM predicts from their angles alone.
    contact_over_expected = contact_rate - expected_contact_rate,
    
    swing_plane_type = case_when(
      avg_signed_diff > productive_window$high          ~ "Steep",
      avg_signed_diff >= productive_window$low          ~ "Optimal",
      avg_signed_diff >= productive_window$low / 2      ~ "Moderate",
      avg_signed_diff >= 0                              ~ "Level",
      TRUE                                              ~ "Downward"
    ),
    swing_plane_type = factor(swing_plane_type,
                              levels = c("Downward", "Level", "Moderate",
                                         "Optimal", "Steep"))
  ) %>%
  arrange(desc(avg_signed_diff))

cat("=== player_overall ===\n")
cat("Columns: n_swings | contact_rate | whiff_rate | avg_bat_speed |")
cat(" avg_signed_diff | sd_signed_diff | pct_in_window | swing_plane_type\n\n")

cat("Swing plane type breakdown:\n")
player_overall %>%
  group_by(swing_plane_type) %>%
  summarise(
    n            = n(),
    pct          = round(n() / nrow(player_overall) * 100, 1),
    avg_contact  = round(mean(contact_rate), 1),
    avg_diff     = round(mean(avg_signed_diff), 1),
    .groups = "drop"
  ) %>%
  print()
cat("\n")

cat("Correlations with contact rate (overall):\n")
tribble(
  ~Metric,                          ~r,
  "Expected contact rate (GAM)",
  cor(player_overall$expected_contact_rate,  player_overall$contact_rate, use = "complete.obs"),
  "Avg signed diff",
  cor(player_overall$avg_signed_diff,        player_overall$contact_rate, use = "complete.obs"),
  "% in productive window",
  cor(player_overall$pct_in_window,          player_overall$contact_rate, use = "complete.obs"),
  "% below window",
  cor(player_overall$pct_below_window,       player_overall$contact_rate, use = "complete.obs"),
  "% above window",
  cor(player_overall$pct_above_window,       player_overall$contact_rate, use = "complete.obs"),
  "SD signed diff (consistency)",
  cor(player_overall$sd_signed_diff,         player_overall$contact_rate, use = "complete.obs"),
  "Avg bat speed",
  cor(player_overall$avg_bat_speed,          player_overall$contact_rate, use = "complete.obs")
) %>%
  mutate(r = round(r, 3)) %>%
  arrange(r) %>%
  print()
cat("\n")

cat("Contact over expected — most positive (outperforming swing plane):\n")
player_overall %>%
  select(player_name, contact_rate, expected_contact_rate, contact_over_expected) %>%
  arrange(desc(contact_over_expected)) %>%
  head(10) %>%
  mutate(across(where(is.numeric), ~ round(., 1))) %>%
  print()

cat("\nContact over expected — most negative (underperforming swing plane):\n")
player_overall %>%
  select(player_name, contact_rate, expected_contact_rate, contact_over_expected) %>%
  arrange(contact_over_expected) %>%
  head(10) %>%
  mutate(across(where(is.numeric), ~ round(., 1))) %>%
  print()
cat("\n")


# ---- Table 2: Zone splits ----
# In-zone vs chase, each with their own signed diff and contact rate.
# Tells you whether a player's swing plane changes when they expand outside
# the zone, and whether that change costs them contact.

player_zone <- swings_eligible %>%
  filter(!is.na(in_zone)) %>%
  group_by(player_name, in_zone) %>%
  summarise(
    n_swings        = n(),
    contact_rate    = mean(contact,     na.rm = TRUE) * 100,
    whiff_rate      = mean(whiff,       na.rm = TRUE) * 100,
    avg_signed_diff = mean(signed_diff, na.rm = TRUE),
    sd_signed_diff  = sd(signed_diff,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(zone_label = if_else(in_zone, "in_zone", "chase")) %>%
  select(-in_zone) %>%
  pivot_wider(
    names_from  = zone_label,
    values_from = c(n_swings, contact_rate, whiff_rate,
                    avg_signed_diff, sd_signed_diff)
  ) %>%
  # Suppress splits below minimum sample
  mutate(
    across(contains("_in_zone"),
           ~ if_else(n_swings_in_zone >= MIN_SWINGS_ZONE, .x, NA_real_)),
    across(contains("_chase"),
           ~ if_else(n_swings_chase   >= MIN_SWINGS_ZONE, .x, NA_real_))
  ) %>%
  # Zone-chase contact rate differential (how much more contact in-zone?)
  mutate(
    contact_rate_diff   = contact_rate_in_zone - contact_rate_chase,
    signed_diff_shift   = avg_signed_diff_chase - avg_signed_diff_in_zone
  ) %>%
  arrange(desc(signed_diff_shift))

cat("=== player_zone ===\n")
cat("Columns: n/contact_rate/whiff_rate/avg_signed_diff/sd_signed_diff × [in_zone, chase]\n")
cat("         contact_rate_diff | signed_diff_shift (how much swing plane changes on chases)\n\n")

# League averages for zone splits — used for relative comparisons
zone_lg_avg <- list(
  contact_rate_in_zone  = mean(player_zone$contact_rate_in_zone,  na.rm = TRUE),
  contact_rate_chase    = mean(player_zone$contact_rate_chase,     na.rm = TRUE),
  whiff_rate_in_zone    = mean(player_zone$whiff_rate_in_zone,     na.rm = TRUE),
  whiff_rate_chase      = mean(player_zone$whiff_rate_chase,       na.rm = TRUE),
  avg_signed_diff_in_zone = mean(player_zone$avg_signed_diff_in_zone, na.rm = TRUE),
  avg_signed_diff_chase   = mean(player_zone$avg_signed_diff_chase,   na.rm = TRUE),
  contact_rate_diff     = mean(player_zone$contact_rate_diff,      na.rm = TRUE),
  signed_diff_shift     = mean(player_zone$signed_diff_shift,      na.rm = TRUE)
)

# Add relative-to-average columns (player value minus league average)
player_zone <- player_zone %>%
  mutate(
    rel_contact_rate_in_zone    = contact_rate_in_zone    - zone_lg_avg$contact_rate_in_zone,
    rel_contact_rate_chase      = contact_rate_chase      - zone_lg_avg$contact_rate_chase,
    rel_whiff_rate_in_zone      = whiff_rate_in_zone      - zone_lg_avg$whiff_rate_in_zone,
    rel_whiff_rate_chase        = whiff_rate_chase        - zone_lg_avg$whiff_rate_chase,
    rel_signed_diff_in_zone     = avg_signed_diff_in_zone - zone_lg_avg$avg_signed_diff_in_zone,
    rel_signed_diff_chase       = avg_signed_diff_chase   - zone_lg_avg$avg_signed_diff_chase,
    rel_contact_rate_diff       = contact_rate_diff       - zone_lg_avg$contact_rate_diff,
    rel_signed_diff_shift       = signed_diff_shift       - zone_lg_avg$signed_diff_shift
  )

cat("League averages (zone splits):\n")
tibble(
  Metric = c("Contact rate — in zone", "Contact rate — chase",
             "Whiff rate — in zone",   "Whiff rate — chase",
             "Avg signed diff — in zone", "Avg signed diff — chase",
             "Contact rate differential (IZ - chase)",
             "Signed diff shift (chase - IZ)"),
  Average = round(c(zone_lg_avg$contact_rate_in_zone,
                    zone_lg_avg$contact_rate_chase,
                    zone_lg_avg$whiff_rate_in_zone,
                    zone_lg_avg$whiff_rate_chase,
                    zone_lg_avg$avg_signed_diff_in_zone,
                    zone_lg_avg$avg_signed_diff_chase,
                    zone_lg_avg$contact_rate_diff,
                    zone_lg_avg$signed_diff_shift), 2)
) %>% print()
cat("\n")

cat("Correlations with overall contact rate (zone table):\n")
zone_joined <- player_zone %>%
  left_join(player_overall %>% select(player_name, contact_rate), by = "player_name")

tribble(
  ~Metric,                                   ~r,
  "Signed diff — in-zone",
  cor(zone_joined$avg_signed_diff_in_zone,    zone_joined$contact_rate, use = "complete.obs"),
  "Signed diff — chase",
  cor(zone_joined$avg_signed_diff_chase,      zone_joined$contact_rate, use = "complete.obs"),
  "Signed diff shift (chase - in-zone)",
  cor(zone_joined$signed_diff_shift,          zone_joined$contact_rate, use = "complete.obs"),
  "Contact rate differential (IZ - chase)",
  cor(zone_joined$contact_rate_diff,          zone_joined$contact_rate, use = "complete.obs"),
  "Rel. contact rate — in-zone vs avg",
  cor(zone_joined$rel_contact_rate_in_zone,   zone_joined$contact_rate, use = "complete.obs"),
  "Rel. contact rate — chase vs avg",
  cor(zone_joined$rel_contact_rate_chase,     zone_joined$contact_rate, use = "complete.obs"),
  "Rel. signed diff shift vs avg",
  cor(zone_joined$rel_signed_diff_shift,      zone_joined$contact_rate, use = "complete.obs")
) %>%
  mutate(r = round(r, 3)) %>%
  arrange(r) %>%
  print()
cat("\n")


# ---- Table 3: Pitch-type splits ----
# One row per player per pitch type. Long format with both raw values and
# values relative to the league average for that pitch type.

player_pitch <- swings_eligible %>%
  group_by(player_name, pitch_group) %>%
  summarise(
    n_swings        = n(),
    contact_rate    = mean(contact,     na.rm = TRUE) * 100,
    whiff_rate      = mean(whiff,       na.rm = TRUE) * 100,
    avg_signed_diff = mean(signed_diff, na.rm = TRUE),
    sd_signed_diff  = sd(signed_diff,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_swings >= MIN_SWINGS_PITCH) %>%
  arrange(player_name, pitch_group)

# League averages per pitch type for relative comparisons
pitch_lg_avg <- player_pitch %>%
  group_by(pitch_group) %>%
  summarise(
    lg_contact_rate    = mean(contact_rate,    na.rm = TRUE),
    lg_whiff_rate      = mean(whiff_rate,      na.rm = TRUE),
    lg_avg_signed_diff = mean(avg_signed_diff, na.rm = TRUE),
    .groups = "drop"
  )

player_pitch <- player_pitch %>%
  left_join(pitch_lg_avg, by = "pitch_group") %>%
  mutate(
    rel_contact_rate    = contact_rate    - lg_contact_rate,
    rel_whiff_rate      = whiff_rate      - lg_whiff_rate,
    rel_avg_signed_diff = avg_signed_diff - lg_avg_signed_diff
  )

cat("=== player_pitch ===\n")
cat("Long format: one row per player × pitch type\n")
cat("Columns: pitch_group | n_swings | contact_rate | whiff_rate | avg_signed_diff\n")
cat("         + rel_ prefix versions (player minus league avg for that pitch type)\n\n")

cat("League averages by pitch type:\n")
pitch_lg_avg %>%
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  arrange(lg_avg_signed_diff) %>%
  print()
cat("\n")

cat("Correlations: pitch-type signed diff vs overall contact rate:\n")
pitch_joined <- player_pitch %>%
  left_join(player_overall %>% select(player_name, contact_rate),
            by = "player_name", suffix = c("_pitch", "_overall"))

pitch_joined %>%
  group_by(pitch_group) %>%
  summarise(
    r_signed_diff     = round(cor(avg_signed_diff,     contact_rate_overall, use = "complete.obs"), 3),
    r_rel_signed_diff = round(cor(rel_avg_signed_diff, contact_rate_overall, use = "complete.obs"), 3),
    r_rel_contact     = round(cor(rel_contact_rate,    contact_rate_overall, use = "complete.obs"), 3),
    n_players         = n(),
    .groups = "drop"
  ) %>%
  arrange(r_signed_diff) %>%
  print()
cat("\n")


# ---- Table 4: Two-strike approach ----
# How does a player's swing plane and contact rate change with two strikes?
# A good two-strike approach typically involves a flatter swing (lower
# signed_diff) to extend the contact window. Players who maintain contact
# with two strikes have a meaningful skill beyond overall contact rate.

player_twostrike <- swings_eligible %>%
  filter(!is.na(strikes)) %>%
  group_by(player_name) %>%
  summarise(
    n_swings_0strike      = sum(strikes == 0, na.rm = TRUE),
    n_swings_2strike      = sum(strikes == 2, na.rm = TRUE),
    contact_rate_0strike  = mean(contact[strikes == 0], na.rm = TRUE) * 100,
    contact_rate_1strike  = mean(contact[strikes == 1], na.rm = TRUE) * 100,
    contact_rate_2strike  = mean(contact[strikes == 2], na.rm = TRUE) * 100,
    whiff_rate_2strike    = mean(whiff[strikes == 2],   na.rm = TRUE) * 100,
    avg_signed_diff_0k    = mean(signed_diff[strikes == 0], na.rm = TRUE),
    avg_signed_diff_2k    = mean(signed_diff[strikes == 2], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Two-strike degradation in contact (negative = worse with two strikes)
    contact_degradation_2k  = contact_rate_2strike - contact_rate_0strike,
    # Swing plane adjustment: negative = flatter (good), positive = steeper
    plane_adjustment_2k     = avg_signed_diff_2k   - avg_signed_diff_0k,
    # Suppress splits with insufficient sample
    across(c(contact_rate_0strike, avg_signed_diff_0k),
           ~ if_else(n_swings_0strike >= MIN_SWINGS_ZONE, .x, NA_real_)),
    across(c(contact_rate_2strike, contact_rate_1strike,
             whiff_rate_2strike, avg_signed_diff_2k,
             contact_degradation_2k, plane_adjustment_2k),
           ~ if_else(n_swings_2strike >= MIN_SWINGS_ZONE, .x, NA_real_))
  )

# League averages for two-strike splits
twostrike_lg_avg <- list(
  contact_rate_0strike   = mean(player_twostrike$contact_rate_0strike,  na.rm = TRUE),
  contact_rate_2strike   = mean(player_twostrike$contact_rate_2strike,  na.rm = TRUE),
  whiff_rate_2strike     = mean(player_twostrike$whiff_rate_2strike,    na.rm = TRUE),
  contact_degradation_2k = mean(player_twostrike$contact_degradation_2k,na.rm = TRUE),
  plane_adjustment_2k    = mean(player_twostrike$plane_adjustment_2k,   na.rm = TRUE)
)

player_twostrike <- player_twostrike %>%
  mutate(
    rel_contact_rate_2strike   = contact_rate_2strike   - twostrike_lg_avg$contact_rate_2strike,
    rel_whiff_rate_2strike     = whiff_rate_2strike     - twostrike_lg_avg$whiff_rate_2strike,
    rel_contact_degradation_2k = contact_degradation_2k - twostrike_lg_avg$contact_degradation_2k,
    rel_plane_adjustment_2k    = plane_adjustment_2k    - twostrike_lg_avg$plane_adjustment_2k
  )

cat("=== player_twostrike ===\n")
cat("Columns: contact/whiff rates by strike count | two-strike degradation\n")
cat("         plane_adjustment_2k | rel_ prefix versions vs league avg\n\n")

cat("League averages (two-strike):\n")
tibble(
  Metric  = c("Contact rate — 0 strikes", "Contact rate — 2 strikes",
              "Whiff rate — 2 strikes",
              "Contact degradation (2K - 0K)", "Plane adjustment (2K - 0K diff)"),
  Average = round(c(twostrike_lg_avg$contact_rate_0strike,
                    twostrike_lg_avg$contact_rate_2strike,
                    twostrike_lg_avg$whiff_rate_2strike,
                    twostrike_lg_avg$contact_degradation_2k,
                    twostrike_lg_avg$plane_adjustment_2k), 2)
) %>% print()
cat("\n")

cat("Correlations with overall contact rate (two-strike table):\n")
twostrike_joined <- player_twostrike %>%
  left_join(player_overall %>% select(player_name, contact_rate), by = "player_name")

tribble(
  ~Metric,                                       ~r,
  "Contact rate — 2 strikes",
  cor(twostrike_joined$contact_rate_2strike,    twostrike_joined$contact_rate, use = "complete.obs"),
  "Whiff rate — 2 strikes",
  cor(twostrike_joined$whiff_rate_2strike,      twostrike_joined$contact_rate, use = "complete.obs"),
  "Contact degradation (2K - 0K)",
  cor(twostrike_joined$contact_degradation_2k,  twostrike_joined$contact_rate, use = "complete.obs"),
  "Plane adjustment 2K (signed diff shift)",
  cor(twostrike_joined$plane_adjustment_2k,     twostrike_joined$contact_rate, use = "complete.obs"),
  "Rel. contact rate — 2 strikes vs avg",
  cor(twostrike_joined$rel_contact_rate_2strike,twostrike_joined$contact_rate, use = "complete.obs"),
  "Rel. contact degradation vs avg",
  cor(twostrike_joined$rel_contact_degradation_2k, twostrike_joined$contact_rate, use = "complete.obs")
) %>%
  mutate(r = round(r, 3)) %>%
  arrange(r) %>%
  print()
cat("\n")

cat("Best two-strike contact rate (min", MIN_SWINGS_ZONE, "two-strike swings):\n")
player_twostrike %>%
  filter(!is.na(contact_rate_2strike)) %>%
  select(player_name, n_swings_2strike, contact_rate_0strike,
         contact_rate_2strike, contact_degradation_2k,
         plane_adjustment_2k, whiff_rate_2strike) %>%
  arrange(desc(contact_rate_2strike)) %>%
  head(15) %>%
  mutate(across(where(is.numeric), ~ round(., 1))) %>%
  print()

cat("\nWorst two-strike contact rate:\n")
player_twostrike %>%
  filter(!is.na(contact_rate_2strike)) %>%
  select(player_name, n_swings_2strike, contact_rate_0strike,
         contact_rate_2strike, contact_degradation_2k,
         plane_adjustment_2k, whiff_rate_2strike) %>%
  arrange(contact_rate_2strike) %>%
  head(15) %>%
  mutate(across(where(is.numeric), ~ round(., 1))) %>%
  print()

cat("\nBest two-strike plane adjustment (flattened swing the most):\n")
player_twostrike %>%
  filter(!is.na(plane_adjustment_2k)) %>%
  select(player_name, avg_signed_diff_0k, avg_signed_diff_2k, plane_adjustment_2k,
         contact_rate_2strike, contact_degradation_2k) %>%
  arrange(plane_adjustment_2k) %>%
  head(15) %>%
  mutate(across(where(is.numeric), ~ round(., 1))) %>%
  print()
cat("\n")


# ---- Table display: top/bottom by overall signed diff ----
cat("Top 20 — highest avg signed diff:\n")
player_overall %>%
  head(20) %>%
  mutate(across(where(is.numeric), ~ round(., 1))) %>%
  print()

cat("\nBottom 20 — lowest avg signed diff:\n")
player_overall %>%
  tail(20) %>%
  mutate(across(where(is.numeric), ~ round(., 1))) %>%
  print()


# ============================================================================
# HELPER: player_profile()
# ============================================================================
#
# Pulls from all three tables to print a formatted single-player summary.
# Usage: player_profile("Freddie Freeman")

player_profile <- function(name) {
  
  o  <- player_overall   %>% filter(player_name == name)
  z  <- player_zone      %>% filter(player_name == name)
  pt <- player_pitch     %>% filter(player_name == name) %>% arrange(desc(n_swings))
  ts <- player_twostrike %>% filter(player_name == name)
  
  if (nrow(o) == 0) {
    cat(sprintf("Player not found: %s\n", name))
    return(invisible(NULL))
  }
  
  W <- 62  # output width
  
  # Helper: format a value with a parenthetical vs-avg note inline
  # e.g.  83.2%  (+2.1 pp vs avg)
  fmt_with_rel <- function(val, rel, val_fmt, rel_fmt, rel_unit = "pp") {
    if (is.na(val)) return("—")
    val_str <- sprintf(val_fmt, val)
    if (is.na(rel)) return(val_str)
    sprintf("%s  (%s %s vs avg)", val_str, sprintf(rel_fmt, rel), rel_unit)
  }
  
  # Helper: tidy NA display
  na_dash <- function(x, fmt) if (is.na(x)) "—" else sprintf(fmt, x)
  
  # ══════════════════════════════════════════════════════════════
  cat(strrep("═", W), "\n", sep = "")
  cat(sprintf("  %s\n", o$player_name))
  cat(sprintf("  %s  ·  %d swings\n", o$swing_plane_type, o$n_swings))
  cat(strrep("═", W), "\n", sep = "")
  cat("\n")
  
  # ── OVERVIEW ──────────────────────────────────────────────────
  cat("  OVERVIEW\n")
  cat(strrep("─", W), "\n", sep = "")
  cat(sprintf("  %-24s %.1f%%\n",   "Contact rate:",  o$contact_rate))
  cat(sprintf("  %-24s %.1f%%\n",   "Whiff rate:",    o$whiff_rate))
  cat(sprintf("  %-24s %.1f mph\n", "Avg bat speed:", o$avg_bat_speed))
  cat(sprintf("  %-24s %+.1f pp (%.1f%% actual vs %.1f%% expected)\n",
              "Bat-to-ball skill:",
              o$bat_to_ball_skill,
              o$contact_rate,
              o$expected_contact_rate))
  cat("\n")
  
  # ── SWING PLANE ───────────────────────────────────────────────
  cat("  SWING PLANE\n")
  cat(strrep("─", W), "\n", sep = "")
  cat(sprintf("  %-24s %+.1f°  (SD %.1f°)\n",
              "Avg signed diff:", o$avg_signed_diff, o$sd_signed_diff))
  cat("\n")
  
  # Window distribution bar
  # Layout: [▓▓▓░░░░░░░░░░░░] with % labels underneath
  bar_w   <- 36
  n_below <- round(o$pct_below_window / 100 * bar_w)
  n_in    <- round(o$pct_in_window    / 100 * bar_w)
  n_above <- max(0, bar_w - n_below - n_in)
  bar_str <- paste0("  [",
                    strrep("·", n_below),
                    strrep("█", n_in),
                    strrep("·", n_above),
                    "]")
  cat(sprintf("  Productive window distribution  (window = %+.0f° to %+.0f°)\n",
              productive_window$low, productive_window$high))
  cat(bar_str, "\n")
  cat(sprintf("   %-13s %-13s %-13s\n",
              sprintf("Below %.0f%%", o$pct_below_window),
              sprintf("In    %.0f%%", o$pct_in_window),
              sprintf("Above %.0f%%", o$pct_above_window)))
  cat("\n")
  
  # ── CONTACT vs EXPECTATION ────────────────────────────────────
  cat("  CONTACT vs EXPECTATION\n")
  cat(strrep("─", W), "\n", sep = "")
  coe_dir  <- if (o$contact_over_expected >= 0) "above" else "below"
  btb_dir  <- if (o$bat_to_ball_skill     >= 0) "above" else "below"
  cat(sprintf("  %-24s %.1f%%  (expected from angles: %.1f%%)\n",
              "Angle-based:",
              o$contact_rate, o$expected_contact_rate))
  cat(sprintf("  %-24s %+.1f pp %s expected\n",
              "", o$contact_over_expected, coe_dir))
  cat(sprintf("  %-24s %+.1f pp %s angles + bat speed\n",
              "Bat-to-ball skill:",
              o$bat_to_ball_skill, btb_dir))
  cat("\n")
  
  # ── ZONE SPLITS ───────────────────────────────────────────────
  if (nrow(z) > 0) {
    cat("  ZONE SPLITS\n")
    cat(strrep("─", W), "\n", sep = "")
    cat(sprintf("  %-22s  %s\n", "",
                sprintf("%-16s %-16s %s", "In-Zone", "Chase", "IZ – Chase")))
    
    # Signed diff row with inline vs-avg
    iz_sd  <- fmt_with_rel(z$avg_signed_diff_in_zone, z$rel_signed_diff_in_zone,
                           "%+.1f°", "%+.1f°", "vs avg")
    ch_sd  <- fmt_with_rel(z$avg_signed_diff_chase,   z$rel_signed_diff_chase,
                           "%+.1f°", "%+.1f°", "vs avg")
    cat(sprintf("  %-22s  %-16s %-16s %s\n", "Signed diff:",
                iz_sd, ch_sd,
                na_dash(z$signed_diff_shift, "%+.1f°")))
    
    # Contact rate row with inline vs-avg
    iz_cr  <- fmt_with_rel(z$contact_rate_in_zone, z$rel_contact_rate_in_zone,
                           "%.1f%%", "%+.1f")
    ch_cr  <- fmt_with_rel(z$contact_rate_chase,   z$rel_contact_rate_chase,
                           "%.1f%%", "%+.1f")
    cat(sprintf("  %-22s  %-16s %-16s %s\n", "Contact rate:",
                iz_cr, ch_cr,
                na_dash(z$contact_rate_diff, "%+.1f pp")))
    
    cat(sprintf("  %-22s  %-16s %-16s\n", "Swings:",
                na_dash(z$n_swings_in_zone, "%d"),
                na_dash(z$n_swings_chase,   "%d")))
    cat("\n")
  }
  
  # ── BY PITCH TYPE ─────────────────────────────────────────────
  if (nrow(pt) > 0) {
    cat("  BY PITCH TYPE\n")
    cat(strrep("─", W), "\n", sep = "")
    cat(sprintf("  %-18s  %5s  %12s  %12s  %10s\n",
                "", "N", "Signed Diff", "Contact%", "Whiff%"))
    for (i in seq_len(nrow(pt))) {
      r <- pt[i, ]
      # Raw values
      cat(sprintf("  %-18s  %5d  %+11.1f°  %11.1f%%  %9.1f%%\n",
                  r$pitch_group, r$n_swings,
                  r$avg_signed_diff, r$contact_rate, r$whiff_rate))
      # Vs-avg on the same indented line below
      sd_rel <- na_dash(r$rel_avg_signed_diff, "%+.1f°")
      cr_rel <- na_dash(r$rel_contact_rate,    "%+.1f pp")
      wr_rel <- na_dash(r$rel_whiff_rate,      "%+.1f pp")
      cat(sprintf("  %-18s  %5s  %12s  %12s  %10s  vs avg\n",
                  "", "", sd_rel, cr_rel, wr_rel))
    }
    cat("\n")
  }
  
  # ── TWO-STRIKE APPROACH ───────────────────────────────────────
  if (nrow(ts) > 0 && !is.na(ts$contact_rate_2strike)) {
    cat("  TWO-STRIKE APPROACH\n")
    cat(strrep("─", W), "\n", sep = "")
    cat(sprintf("  %-24s  %s\n", "",
                sprintf("%-12s %-12s %s", "0 Strikes", "2 Strikes", "Change")))
    
    # Contact rate
    cr_change <- na_dash(ts$contact_degradation_2k, "%+.1f pp")
    cr_rel    <- na_dash(ts$rel_contact_rate_2strike, "%+.1f pp vs avg")
    cat(sprintf("  %-24s  %-12s %-12s %s\n", "Contact rate:",
                na_dash(ts$contact_rate_0strike,  "%.1f%%"),
                na_dash(ts$contact_rate_2strike,  "%.1f%%"),
                cr_change))
    cat(sprintf("  %-24s  %s\n", "",
                sprintf("2-strike contact: %s", cr_rel)))
    
    # Swing plane adjustment
    pl_change <- na_dash(ts$plane_adjustment_2k, "%+.1f° (neg = flatter, good)")
    cat(sprintf("  %-24s  %-12s %-12s %s\n", "Avg signed diff:",
                na_dash(ts$avg_signed_diff_0k, "%+.1f°"),
                na_dash(ts$avg_signed_diff_2k, "%+.1f°"),
                pl_change))
    
    # Whiff rate with vs-avg
    wr_2k <- fmt_with_rel(ts$whiff_rate_2strike, ts$rel_whiff_rate_2strike,
                          "%.1f%%", "%+.1f")
    cat(sprintf("  %-24s  %s\n", "2-strike whiff rate:", wr_2k))
    cat(sprintf("  %-24s  %d swings\n", "Sample:", ts$n_swings_2strike))
    cat("\n")
  }
  
  cat(strrep("═", W), "\n", sep = "")
  invisible(list(overall = o, zone = z, pitch = pt, twostrike = ts))
}


# ============================================================================
# 4. VISUALIZATIONS
# ============================================================================

cat("Generating plots...\n")

# ---- Plot 1: GAM contact curve ----
# Continuous predicted contact probability across the mismatch range.
# Shaded band = pointwise 95% CI. Productive window highlighted.

gam_se <- predict(contact_gam,
                  newdata = tibble(signed_diff = seq(-20, 45, by = 0.5)),
                  se.fit  = TRUE)

contact_curve_plot <- tibble(
  signed_diff  = seq(-20, 45, by = 0.5),
  pred_contact = plogis(gam_se$fit) * 100,
  ci_lo        = plogis(gam_se$fit - 1.96 * gam_se$se.fit) * 100,
  ci_hi        = plogis(gam_se$fit + 1.96 * gam_se$se.fit) * 100
)

p1 <- ggplot(contact_curve_plot, aes(x = signed_diff, y = pred_contact)) +
  annotate("rect",
           xmin = productive_window$low, xmax = productive_window$high,
           ymin = -Inf, ymax = Inf, fill = "#2ecc71", alpha = 0.12) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = "#3498db", alpha = 0.2) +
  geom_line(color = "#2c3e50", linewidth = 1.2) +
  geom_vline(xintercept = productive_window$peak,
             linetype = "dashed", color = "#e74c3c", linewidth = 0.8) +
  annotate("text",
           x = productive_window$peak, y = Inf,
           vjust = 1.5, hjust = -0.15, size = 3.2, color = "#e74c3c",
           label = sprintf("Peak\n(%+.0f°, %.0f%%)",
                           productive_window$peak, productive_window$peak_contact)) +
  annotate("text",
           x = (productive_window$low + productive_window$high) / 2,
           y = -Inf, vjust = -0.5, size = 3, color = "#27ae60",
           label = sprintf("Productive window\n(%+.0f° to %+.0f°)",
                           productive_window$low, productive_window$high)) +
  scale_x_continuous(breaks = seq(-20, 45, by = 5)) +
  labs(
    title    = "Contact Rate vs Signed Mismatch (GAM)",
    subtitle = sprintf("Shaded band = 95%% CI | Green = productive window (within %d pp of peak)",
                       CONTACT_WINDOW_THRESHOLD),
    x        = "Signed Mismatch (°)  [attack_angle – vaa_contact]",
    y        = "Predicted Contact Rate (%)"
  ) +
  theme_minimal(base_size = 12)

# ---- Plot 2: Player avg signed diff vs contact rate ----
p2 <- ggplot(player_overall, aes(x = avg_signed_diff, y = contact_rate)) +
  geom_point(aes(size = n_swings, color = avg_bat_speed), alpha = 0.65) +
  geom_smooth(method = "loess", se = TRUE, color = "#e67e22", linewidth = 1) +
  annotate("rect",
           xmin = productive_window$low, xmax = productive_window$high,
           ymin = -Inf, ymax = Inf, fill = "#2ecc71", alpha = 0.08) +
  scale_color_viridis_c(option = "plasma", name = "Bat speed\n(mph)") +
  annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 3.5,
           label = sprintf("r = %.3f",
                           cor(player_overall$avg_signed_diff,
                               player_overall$contact_rate, use = "complete.obs"))) +
  labs(
    title    = "Player Avg Signed Mismatch vs Contact Rate",
    subtitle = "Green band = productive window | Color = bat speed",
    x        = "Avg Signed Mismatch (°)",
    y        = "Contact Rate (%)",
    size     = "Swings"
  ) +
  theme_minimal(base_size = 12)

# ---- Plot 3: In-zone vs chase signed diff ----
p3 <- player_zone %>%
  filter(!is.na(avg_signed_diff_in_zone), !is.na(avg_signed_diff_chase)) %>%
  left_join(player_overall %>% select(player_name, contact_rate, n_swings),
            by = "player_name") %>%
  ggplot(aes(x = avg_signed_diff_in_zone, y = avg_signed_diff_chase)) +
  geom_point(aes(size = n_swings, color = contact_rate), alpha = 0.65) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
  scale_color_viridis_c(option = "plasma", direction = -1,
                        name = "Contact\nrate (%)") +
  annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5, size = 3.5,
           label = sprintf("r = %.3f",
                           cor(player_zone$avg_signed_diff_in_zone,
                               player_zone$avg_signed_diff_chase,
                               use = "complete.obs"))) +
  labs(
    title    = "In-Zone vs Chase Signed Mismatch",
    subtitle = "Dashed = equal mismatch in both contexts",
    x        = "Avg Signed Mismatch — In-Zone (°)",
    y        = "Avg Signed Mismatch — Chase (°)",
    size     = "Swings"
  ) +
  theme_minimal(base_size = 12)

# ---- Plot 4: Swing plane type vs contact rate ----
p4 <- ggplot(player_overall,
             aes(x = swing_plane_type, y = contact_rate, fill = swing_plane_type)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 16, outlier.alpha = 0.5) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 0.8) +
  scale_fill_manual(values = c(
    "Downward" = "#e74c3c",
    "Level"    = "#e67e22",
    "Moderate" = "#f1c40f",
    "Optimal"  = "#2ecc71",
    "Steep"    = "#3498db"
  )) +
  labs(
    title    = "Contact Rate by Swing Plane Type",
    subtitle = sprintf("Optimal = within productive window (%+.0f° to %+.0f°)",
                       productive_window$low, productive_window$high),
    x        = NULL,
    y        = "Contact Rate (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# ---- Plot 5: Distribution of swings by outcome and mismatch ----
#
# Overlaid density curves for contact, whiff, and foul outcomes.
# Shows where each outcome clusters along the mismatch spectrum.
# Restricted to the reliable data range (-20° to +45°).
#
# A second panel shows the same data as stacked proportions — at each
# mismatch value, what share of swings end in each outcome?

outcome_data <- swings %>%
  filter(!is.na(signed_diff), between(signed_diff, -20, 45)) %>%
  mutate(outcome = case_when(
    contact == 1 & ball_in_play == 1 ~ "Ball in play",
    contact == 1                     ~ "Foul",
    TRUE                             ~ "Whiff"
  )) %>%
  mutate(outcome = factor(outcome, levels = c("Whiff", "Foul", "Ball in play")))

outcome_colors <- c(
  "Ball in play" = "#2ecc71",
  "Foul"         = "#f39c12",
  "Whiff"        = "#e74c3c"
)

# Panel A: density curves (normalized within each outcome — shows shape, not volume)
p5a <- ggplot(outcome_data, aes(x = signed_diff, color = outcome, fill = outcome)) +
  annotate("rect",
           xmin = productive_window$low, xmax = productive_window$high,
           ymin = -Inf, ymax = Inf, fill = "#2ecc71", alpha = 0.07) +
  geom_density(alpha = 0.15, linewidth = 0.9, bw = 1.5) +
  geom_vline(xintercept = productive_window$peak,
             linetype = "dashed", color = "grey40", linewidth = 0.7) +
  scale_color_manual(values = outcome_colors, name = NULL) +
  scale_fill_manual( values = outcome_colors, name = NULL) +
  scale_x_continuous(breaks = seq(-20, 45, by = 5)) +
  labs(
    title    = "Swing Outcome Distributions by Signed Mismatch",
    subtitle = "Normalized density — shows where each outcome clusters, not raw counts",
    x        = "Signed Mismatch (°)  [attack_angle – vaa_contact]",
    y        = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# Panel B: stacked proportions — outcome share at each mismatch value
# Binned at 1° for smoothness, then smoothed with GAM per outcome
prop_bins <- swings %>%
  filter(!is.na(signed_diff), between(signed_diff, -20, 45)) %>%
  mutate(signed_diff_bin = round(signed_diff)) %>%
  group_by(signed_diff_bin) %>%
  summarise(
    n_total    = n(),
    pct_bip    = mean(contact == 1 & ball_in_play == 1, na.rm = TRUE) * 100,
    pct_foul   = mean(contact == 1 & ball_in_play == 0, na.rm = TRUE) * 100,
    pct_whiff  = mean(whiff == 1,                        na.rm = TRUE) * 100,
    .groups    = "drop"
  ) %>%
  filter(n_total >= 100) %>%
  pivot_longer(cols = starts_with("pct_"),
               names_to  = "outcome",
               names_prefix = "pct_",
               values_to = "pct") %>%
  mutate(outcome = recode(outcome,
                          bip   = "Ball in play",
                          foul  = "Foul",
                          whiff = "Whiff"),
         outcome = factor(outcome, levels = c("Whiff", "Foul", "Ball in play")))

p5b <- ggplot(prop_bins, aes(x = signed_diff_bin, y = pct, fill = outcome)) +
  geom_area(position = "stack", alpha = 0.85) +
  geom_vline(xintercept = productive_window$peak,
             linetype = "dashed", color = "white", linewidth = 0.8) +
  annotate("text",
           x = productive_window$peak, y = 100,
           vjust = 1.5, hjust = -0.1, size = 3, color = "white",
           label = sprintf("Peak\n(%+.0f°)", productive_window$peak)) +
  scale_fill_manual(values = outcome_colors, name = NULL) +
  scale_x_continuous(breaks = seq(-20, 45, by = 5)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title    = "Outcome Share by Signed Mismatch",
    subtitle = "1° bins, n ≥ 100 per bin",
    x        = "Signed Mismatch (°)",
    y        = "Share of Swings"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

p1; p2; p3; p4
gridExtra::grid.arrange(p5a, p5b, ncol = 1, top = "Swing Outcomes by Mismatch")


# ============================================================================
# 5. BAT SPEED ADJUSTED EXPECTED CONTACT
# ============================================================================
#
# The original contact_gam treats every swing as if bat speed were equal.
# But faster swings produce more whiffs at any given signed_diff — harder
# swingers are more likely to pass under the ball even in the productive
# window. A GAM that includes bat speed gives each player an expected contact
# rate adjusted for the difficulty of their own swing speed.
#
# ============================================================================
# 5. BAT SPEED AND EXPECTED CONTACT — INDEPENDENCE CHECK
# ============================================================================
#
# Initially we tried adding bat speed directly to the GAM to produce an
# adjusted expected contact rate. That failed: at the swing level, conditional
# on signed_diff, faster bat speed is *positively* associated with contact
# (harder swings at the same angle make more contact). The negative player-
# level correlation between bat speed and contact is mediated by swing plane —
# hard swingers have more uppercut, which the angle GAM already penalizes.
#
# However, the player-level mediation test below shows bat speed retains large
# independent variance after expected_contact_rate is controlled. The two
# metrics are measuring separate things:
#
#   expected_contact_rate — angle quality: where does the player's swing plane
#                           sit relative to the contact curve?
#   avg_bat_speed         — contact difficulty: harder swings miss more even
#                           at good angles, likely due to timing margin
#
# The residual from the joint model (actual - predicted from both) is a
# cleaner measure of bat-to-ball skill than either residual alone.

cat("=== BAT SPEED + EXPECTED CONTACT: INDEPENDENCE CHECK ===\n\n")

m_angle_only <- lm(contact_rate ~ expected_contact_rate,                data = player_overall)
m_bs_only    <- lm(contact_rate ~ avg_bat_speed,                        data = player_overall)
m_joint      <- lm(contact_rate ~ expected_contact_rate + avg_bat_speed,data = player_overall)

cat("Sequential R²:\n")
tibble(
  Model               = c("Expected contact only",
                          "Bat speed only",
                          "Expected contact + bat speed"),
  R2                  = round(c(summary(m_angle_only)$r.squared,
                                summary(m_bs_only)$r.squared,
                                summary(m_joint)$r.squared), 3),
  Adj_R2              = round(c(summary(m_angle_only)$adj.r.squared,
                                summary(m_bs_only)$adj.r.squared,
                                summary(m_joint)$adj.r.squared), 3)
) %>% print()
cat("\n")

cat("Joint model coefficients:\n")
broom::tidy(m_joint) %>%
  mutate(across(where(is.numeric), ~ round(., 4)),
         sig = case_when(p.value < 0.001 ~ "***",
                         p.value < 0.01  ~ "**",
                         p.value < 0.05  ~ "*",
                         TRUE            ~ "")) %>%
  print()
cat("\n")

# Add joint-model residual to player_overall as bat_to_ball_skill
player_overall <- player_overall %>%
  mutate(
    bat_to_ball_skill = residuals(m_joint)[match(player_name,
                                                 names(residuals(m_joint)))]
  )

# Refit with named residuals to ensure alignment
m_joint_named <- lm(contact_rate ~ expected_contact_rate + avg_bat_speed,
                    data = player_overall)
player_overall <- player_overall %>%
  mutate(bat_to_ball_skill = residuals(m_joint_named))

cat("Top 15 — bat-to-ball skill (outperforming angles + bat speed):\n")
player_overall %>%
  select(player_name, contact_rate, expected_contact_rate,
         avg_bat_speed, bat_to_ball_skill) %>%
  arrange(desc(bat_to_ball_skill)) %>%
  head(15) %>%
  mutate(across(where(is.numeric), ~ round(., 1))) %>%
  print()

cat("\nBottom 15 — bat-to-ball skill (underperforming angles + bat speed):\n")
player_overall %>%
  select(player_name, contact_rate, expected_contact_rate,
         avg_bat_speed, bat_to_ball_skill) %>%
  arrange(bat_to_ball_skill) %>%
  head(15) %>%
  mutate(across(where(is.numeric), ~ round(., 1))) %>%
  print()
cat("\n")


# ============================================================================
# 5B. BAT_TO_BALL_SKILL — RESIDUAL DIAGNOSTICS
# ============================================================================
#
# bat_to_ball_skill is a residual from an OLS model, so it inherits any
# violations of that model's assumptions. Three checks:
#
#   1. Residuals vs fitted — is variance constant across the predicted range?
#      A funnel shape means the metric is noisier for some players than others.
#
#   2. Residuals vs bat speed — do hard swingers have more residual variance?
#      Greater swing-to-swing volatility at high bat speed could inflate
#      season-level contact rate noise.
#
#   3. Residuals vs n_swings — are extreme values driven by low sample sizes?
#      The most practically important check: if the top/bottom of the
#      leaderboard are 200-swing players, the metric isn't trustworthy there.

cat("=== BAT_TO_BALL_SKILL RESIDUAL DIAGNOSTICS ===\n\n")

diag_data <- player_overall %>%
  filter(!is.na(bat_to_ball_skill), !is.na(avg_bat_speed), !is.na(n_swings)) %>%
  mutate(fitted_contact = contact_rate - bat_to_ball_skill)

# Breusch-Pagan test for heteroscedasticity
bp_test <- lmtest::bptest(m_joint_named)
cat(sprintf("Breusch-Pagan test for heteroscedasticity: p = %.4f\n", bp_test$p.value))
cat(sprintf("  %s\n\n",
            if_else(bp_test$p.value < 0.05,
                    "Evidence of heteroscedasticity — residual variance is not constant.",
                    "No significant heteroscedasticity detected.")))

# Correlation of |residual| with key variables
cat("Correlation of |bat_to_ball_skill| with sample size and bat speed:\n")
tibble(
  Variable = c("n_swings", "avg_bat_speed", "fitted_contact"),
  r = c(
    cor(abs(diag_data$bat_to_ball_skill), diag_data$n_swings,        use = "complete.obs"),
    cor(abs(diag_data$bat_to_ball_skill), diag_data$avg_bat_speed,   use = "complete.obs"),
    cor(abs(diag_data$bat_to_ball_skill), diag_data$fitted_contact,  use = "complete.obs")
  )
) %>%
  mutate(r = round(r, 3)) %>%
  print()
cat("\n")


player_profile("Ohtani, Shohei")
player_profile("Guerrero Jr., Vladimir")
player_profile("Clement, Ernie")
player_profile("Arraez, Luis")

