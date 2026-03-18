# ============================================================================
# VHAngle_utils.R — Shared Constants and Preprocessing Functions
# ============================================================================
# Sourced by both VHAngleAnalysis.R and VHAngleValidation.R to ensure
# identical preprocessing across all datasets.
# ============================================================================

library(tidyverse)

# ============================================================================
# SHARED CONSTANTS
# ============================================================================

# Minimum swings for a player to be included in player-level analysis
MIN_SWINGS_FOR_PLAYER_ANALYSIS <- 200

# Approximate y-coordinate (feet) of the front of home plate in Statcast
PLATE_Y_FEET <- 1.417

# ============================================================================
# PITCH TYPE GROUPINGS
# ============================================================================

PITCH_GROUPS <- list(
  Fastball       = c("FF", "SI", "FC", "FA"),
  `Slider/Sweeper` = c("SL", "ST", "SV"),
  Curveball      = c("CU", "KC"),
  Offspeed       = c("CH", "FS", "FO")
)

# ============================================================================
# preprocess_statcast()
# ============================================================================
# Computes all angle metrics from raw Statcast data.
#
# Adds the following columns:
#   vaa, haa             — vertical/horizontal approach angle at release
#   vaa_contact, haa_contact  — same angles recomputed at the contact point
#                               using kinematic equations (more accurate for
#                               breaking balls, which deviate from release vector)
#   vaa_shift, haa_shift — difference between contact and release angles
#                          (larger shift = ball moved more after release)
#   vert_angle_diff_contact_abs, horiz_angle_diff_contact_abs
#                        — absolute angle mismatch (bat vs pitch) at contact
#   contact_depth        — renamed alias for intercept_ball_minus_batter_pos_y_inches
#   pitch_group          — 5-level pitch type category
#   pfx_z_abs, pfx_x_abs — absolute pitch movement components
# ============================================================================

preprocess_statcast <- function(df) {
  df %>%
    mutate(
      # ------------------------------------------------------------------
      # Release-point approach angles
      # ------------------------------------------------------------------
      vaa = atan(vz0 / abs(vy0)) * (180 / pi),
      haa = atan(vx0 / abs(vy0)) * (180 / pi),
      
      # ------------------------------------------------------------------
      # Contact-point approach angles
      # ------------------------------------------------------------------
      # Kinematic equation: y(t) = release_pos_y + vy0*t + 0.5*ay*t^2
      # Solve for t when ball reaches PLATE_Y_FEET (front of plate).
      # Then use vx(t) = vx0 + ax*t, etc. to get velocity at contact.
      # This corrects for spin-induced movement, which is especially
      # significant for breaking balls.
      quad_a       = 0.5 * ay,
      quad_b       = vy0,
      quad_c       = release_pos_y - PLATE_Y_FEET,
      discriminant = quad_b^2 - 4 * quad_a * quad_c,
      t_contact    = if_else(
        discriminant >= 0,
        (-quad_b - sqrt(pmax(discriminant, 0))) / (2 * quad_a),
        NA_real_
      ),
      vx_contact   = vx0 + ax * t_contact,
      vy_contact   = vy0 + ay * t_contact,
      vz_contact   = vz0 + az * t_contact,
      vaa_contact  = atan(vz_contact / abs(vy_contact)) * (180 / pi),
      haa_contact  = atan(vx_contact / abs(vy_contact)) * (180 / pi),
      
      # ------------------------------------------------------------------
      # Angle mismatches (bat angle minus pitch angle)
      # ------------------------------------------------------------------
      vert_angle_diff_contact      = attack_angle     - vaa_contact,
      horiz_angle_diff_contact     = attack_direction - haa_contact,
      vert_angle_diff_contact_abs  = abs(vert_angle_diff_contact),
      horiz_angle_diff_contact_abs = abs(horiz_angle_diff_contact),
      
      # Release-point angle mismatches (retained for shift analysis)
      vert_angle_diff_abs  = abs(attack_angle     - vaa),
      horiz_angle_diff_abs = abs(attack_direction - haa),
      
      # How much did the angle shift from release to contact point?
      # (diagnostic — larger shift = release-point angles were misleading)
      vaa_shift = vaa_contact - vaa,
      haa_shift = haa_contact - haa,
      
      # ------------------------------------------------------------------
      # Contact depth and pitch movement
      # ------------------------------------------------------------------
      contact_depth = intercept_ball_minus_batter_pos_y_inches,
      pfx_z_abs     = abs(pfx_z),
      pfx_x_abs     = abs(pfx_x),
      total_break   = sqrt(pfx_x^2 + pfx_z^2),
      
      # ------------------------------------------------------------------
      # Pitch type grouping
      # ------------------------------------------------------------------
      pitch_group = case_when(
        pitch_type %in% PITCH_GROUPS[["Fastball"]]        ~ "Fastball",
        pitch_type %in% PITCH_GROUPS[["Slider/Sweeper"]]  ~ "Slider/Sweeper",
        pitch_type %in% PITCH_GROUPS[["Curveball"]]       ~ "Curveball",
        pitch_type %in% PITCH_GROUPS[["Offspeed"]]        ~ "Offspeed",
        TRUE                                               ~ "Other"
      )
    ) %>%
    # Drop intermediate quadratic variables
    select(-quad_a, -quad_b, -quad_c, -discriminant,
           -t_contact, -vx_contact, -vy_contact, -vz_contact)
}


# ============================================================================
# build_swings()
# ============================================================================
# Filters a preprocessed Statcast dataset to swings only and adds
# binary outcome variables.
#
# Filters: must have attack_angle, attack_direction, vaa, haa, bat_speed,
#          and be a swing (type S) or ball in play (type X).
#
# Adds:
#   contact     — 1 if bat made contact (BIP, foul, foul_tip); 0 if whiff
#   whiff       — 1 if swinging_strike / swinging_strike_blocked / missed_bunt
#   ball_in_play — 1 if type == "X"
# ============================================================================

build_swings <- function(df) {
  df %>%
    filter(
      !is.na(attack_angle),
      !is.na(attack_direction),
      !is.na(vaa),
      !is.na(haa),
      !is.na(bat_speed),
      type %in% c("X", "S")
    ) %>%
    mutate(
      # Zone classification: 1-9 = strike zone, 11-14 = out of zone (chase)
      in_zone = case_when(
        zone %in% 1:9    ~ TRUE,
        zone %in% 11:14  ~ FALSE,
        TRUE             ~ NA
      ),
      contact = case_when(
        type == "X"                                    ~ 1,
        description %in% c("foul", "foul_tip",
                           "foul_bunt")               ~ 1,
        description %in% c("swinging_strike",
                           "swinging_strike_blocked",
                           "missed_bunt")             ~ 0,
        TRUE ~ NA_real_
      ),
      whiff        = if_else(
        description %in% c("swinging_strike",
                           "swinging_strike_blocked",
                           "missed_bunt"), 1, 0
      ),
      ball_in_play = if_else(type == "X", 1, 0)
    ) %>%
    filter(!is.na(contact))
}


# ============================================================================
# add_pitch_type_differentials()
# ============================================================================
# Adds breaking-ball and offspeed disadvantage columns to a player-level
# data frame that already has pitch-type conditional vert_contact columns.
# `suffix` is the year suffix used in column names, e.g. "2025".
# ============================================================================

add_pitch_type_differentials <- function(df, suffix) {
  fb <- paste0("vert_contact_fb_", suffix)
  sl <- paste0("vert_contact_sl_", suffix)
  cu <- paste0("vert_contact_cu_", suffix)
  os <- paste0("vert_contact_os_", suffix)
  
  df %>%
    mutate(
      # Pairwise disadvantages vs fastball baseline (positive = worse)
      diff_sl_fb        = .data[[sl]] - .data[[fb]],
      diff_cu_fb        = .data[[cu]] - .data[[fb]],
      diff_os_fb        = .data[[os]] - .data[[fb]],
      
      # Breaking-ball composite disadvantage (avg slider+curve vs fastball)
      # Primary profile metric: isolates pitch-reading skill from overall swing quality
      break_disadvantage = ((.data[[sl]] + .data[[cu]]) / 2) - .data[[fb]],
      
      # Offspeed disadvantage
      os_disadvantage    = .data[[os]] - .data[[fb]],
      
      # Spread of skill across pitch types
      pitch_range = pmax(.data[[fb]], .data[[sl]],
                         .data[[cu]], .data[[os]], na.rm = TRUE) -
        pmin(.data[[fb]], .data[[sl]],
             .data[[cu]], .data[[os]], na.rm = TRUE),
      
      pitch_sd = apply(
        cbind(.data[[fb]], .data[[sl]], .data[[cu]], .data[[os]]),
        MARGIN = 1, FUN = sd, na.rm = TRUE
      )
    ) %>%
    rename_with(
      ~ paste0(.x, "_", suffix),
      c(diff_sl_fb, diff_cu_fb, diff_os_fb,
        break_disadvantage, os_disadvantage, pitch_range, pitch_sd)
    )
}