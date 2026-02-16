# ============================================================================
# Combined 3D Swing Adjustability Analysis
# ============================================================================

library(tidyverse)
library(lme4)
library(broom.mixed)
library(ggplot2)
library(gridExtra)
library(viridis)

# ============================================================================
# CONFIGURATION
# ============================================================================

MIN_SWINGS_FOR_PLAYER_ANALYSIS <- 200


DATA_FILE <- "savantData2025.csv"

# ============================================================================
# 1. LOAD DATA AND CALCULATE ALL ANGLES
# ============================================================================

cat("Loading data and calculating 3D metrics...\n")
swing_data <- read_csv(DATA_FILE)

swing_data <- swing_data %>%
  mutate(
    # Vertical Approach Angle
    vaa = atan(vz0 / abs(vy0)) * (180/pi),
    
    # Horizontal Approach Angle
    haa = atan(vx0 / abs(vy0)) * (180/pi),
    
    # Vertical angle difference
    vert_angle_diff = attack_angle - vaa,
    vert_angle_diff_abs = abs(vert_angle_diff),
    
    # Horizontal angle difference
    horiz_angle_diff = attack_direction - haa,
    horiz_angle_diff_abs = abs(horiz_angle_diff),
    
    # Contact depth
    contact_depth = intercept_ball_minus_batter_pos_y_inches,
    
    # Pitch groupings
    pitch_group = case_when(
      pitch_type %in% c("FF", "SI", "FC", "FA") ~ "Fastball",
      pitch_type %in% c("SL", "CU", "KC", "SV") ~ "Breaking Ball",
      pitch_type %in% c("CH", "FS", "FO") ~ "Offspeed",
      TRUE ~ "Other"
    )
  )

# Create swings dataset
swings <- swing_data %>%
  filter(
    !is.na(attack_angle),
    !is.na(attack_direction),
    !is.na(vaa),
    !is.na(haa),
    !is.na(bat_speed),
    type %in% c("X", "S")
  ) %>%
  mutate(
    contact = case_when(
      type == "X" ~ 1,
      description %in% c("foul", "foul_tip", "foul_bunt") ~ 1,
      description %in% c("swinging_strike", "swinging_strike_blocked", 
                         "missed_bunt") ~ 0,
      TRUE ~ NA_real_
    ),
    whiff = if_else(description %in% c("swinging_strike", "swinging_strike_blocked", 
                                       "missed_bunt"), 1, 0),
    ball_in_play = if_else(type == "X", 1, 0)
  ) %>%
  filter(!is.na(contact))

cat("Total swings:", nrow(swings), "\n")
cat("Contact rate:", sprintf("%.1f%%\n\n", mean(swings$contact) * 100))

# ============================================================================
# 2. CALCULATE PITCH-TYPE-SPECIFIC WEIGHTS FROM DATA
# ============================================================================

cat("=== CALCULATING PITCH-TYPE-SPECIFIC WEIGHTS FROM DATA ===\n\n")

# Fit models for each pitch type to get coefficients
cat("Fitting models to derive weights...\n")

pitch_types_to_model <- c("Fastball", "Breaking Ball", "Offspeed")
pitch_weights_list <- list()

for (ptype in pitch_types_to_model) {
  pitch_data <- swings %>%
    filter(pitch_group == ptype, !is.na(contact_depth))
  
  if (nrow(pitch_data) < 100) {
    cat("Warning: Insufficient data for", ptype, "(n =", nrow(pitch_data), ")\n")
    cat("Using overall coefficients as fallback\n")
    next
  }
  
  # Fit model with both horizontal and vertical
  model <- glm(
    contact ~ horiz_angle_diff_abs + vert_angle_diff_abs + 
      bat_speed + plate_x + plate_z + contact_depth + release_speed,
    data = pitch_data,
    family = binomial(link = "logit")
  )
  
  # Extract coefficients
  horiz_coef <- coef(model)["horiz_angle_diff_abs"]
  vert_coef <- coef(model)["vert_angle_diff_abs"]
  
  pitch_weights_list[[ptype]] <- tibble(
    pitch_group = ptype,
    horiz_coef = horiz_coef,
    vert_coef = vert_coef
  )
  
  cat("  ", ptype, "- Horizontal:", round(horiz_coef, 4), 
      "| Vertical:", round(vert_coef, 4), "\n")
}

# Also fit overall model for "Other" category
overall_data <- swings %>% filter(!is.na(contact_depth))
overall_model <- glm(
  contact ~ horiz_angle_diff_abs + vert_angle_diff_abs + 
    bat_speed + plate_x + plate_z + contact_depth + release_speed,
  data = overall_data,
  family = binomial(link = "logit")
)

pitch_weights_list[["Other"]] <- tibble(
  pitch_group = "Other",
  horiz_coef = coef(overall_model)["horiz_angle_diff_abs"],
  vert_coef = coef(overall_model)["vert_angle_diff_abs"]
)

# Combine into single dataframe
pitch_weights <- bind_rows(pitch_weights_list)

# Calculate relative weights for each pitch type
pitch_weights <- pitch_weights %>%
  mutate(
    total_effect = abs(horiz_coef) + abs(vert_coef),
    horiz_weight = abs(horiz_coef) / total_effect,
    vert_weight = abs(vert_coef) / total_effect
  )

cat("\nDerived pitch-type specific weights:\n")
print(pitch_weights %>% select(pitch_group, horiz_coef, vert_coef, horiz_weight, vert_weight))

# Join weights to swings data
swings <- swings %>%
  left_join(pitch_weights, by = "pitch_group")

# Create combined metrics
swings <- swings %>%
  mutate(
    # Method 1: Euclidean distance (simple 3D distance)
    combined_diff_euclidean = sqrt(
      horiz_angle_diff_abs^2 + vert_angle_diff_abs^2
    ),
    
    # Method 2: Weighted Euclidean (pitch-type specific weights)
    combined_diff_weighted = sqrt(
      (horiz_angle_diff_abs * horiz_weight)^2 + 
        (vert_angle_diff_abs * vert_weight)^2
    ),
    
    # Method 3: Weighted sum (linear combination)
    combined_diff_linear = (horiz_angle_diff_abs * horiz_weight) + 
      (vert_angle_diff_abs * vert_weight),
    
    # Method 4: Maximum of the two (worst plane determines overall)
    combined_diff_max = pmax(
      horiz_angle_diff_abs * horiz_weight,
      vert_angle_diff_abs * vert_weight
    ),
    
    # Create quality categories based on weighted metric
    adjustability_quality = case_when(
      combined_diff_weighted <= 5 ~ "Elite (≤5°)",
      combined_diff_weighted <= 8 ~ "Good (5-8°)",
      combined_diff_weighted <= 12 ~ "Average (8-12°)",
      combined_diff_weighted <= 16 ~ "Below Avg (12-16°)",
      TRUE ~ "Poor (>16°)"
    ),
    adjustability_quality = factor(adjustability_quality,
                                   levels = c("Elite (≤5°)", "Good (5-8°)", 
                                              "Average (8-12°)", "Below Avg (12-16°)", 
                                              "Poor (>16°)"))
  )

cat("\n\nCombined adjustability metric distributions:\n")
cat("Weighted Euclidean - Mean:", round(mean(swings$combined_diff_weighted, na.rm = TRUE), 2), "°\n")
cat("Weighted Euclidean - Median:", round(median(swings$combined_diff_weighted, na.rm = TRUE), 2), "°\n")

cat("\nAdjustability quality distribution:\n")
print(table(swings$adjustability_quality))

# ============================================================================
# 3. CONTACT RATE BY COMBINED ADJUSTABILITY
# ============================================================================

cat("\n\n=== CONTACT RATE BY COMBINED ADJUSTABILITY ===\n\n")

contact_by_combined <- swings %>%
  group_by(adjustability_quality) %>%
  summarise(
    n = n(),
    contact_rate = mean(contact, na.rm = TRUE) * 100,
    whiff_rate = mean(whiff, na.rm = TRUE) * 100,
    avg_horiz_diff = mean(horiz_angle_diff_abs, na.rm = TRUE),
    avg_vert_diff = mean(vert_angle_diff_abs, na.rm = TRUE),
    avg_combined = mean(combined_diff_weighted, na.rm = TRUE),
    se = sqrt(contact_rate/100 * (1 - contact_rate/100) / n) * 100
  )

print(contact_by_combined)

# By pitch type
cat("\n=== CONTACT RATE BY ADJUSTABILITY AND PITCH TYPE ===\n\n")

contact_by_combined_pitch <- swings %>%
  filter(pitch_group != "Other") %>%
  group_by(pitch_group, adjustability_quality) %>%
  summarise(
    n = n(),
    contact_rate = mean(contact, na.rm = TRUE) * 100,
    avg_combined = mean(combined_diff_weighted, na.rm = TRUE),
    .groups = "drop"
  )

print(contact_by_combined_pitch)

# ============================================================================
# 4. VISUALIZATIONS
# ============================================================================

cat("\nGenerating 3D adjustability visualizations...\n")

# Plot 1: Contact Rate by Combined Adjustability
p1 <- ggplot(contact_by_combined, aes(x = adjustability_quality, y = contact_rate)) +
  geom_col(fill = "#e67e22", alpha = 0.8) +
  geom_errorbar(aes(ymin = contact_rate - se, ymax = contact_rate + se),
                width = 0.3, size = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", contact_rate)),
            vjust = -0.5, size = 4, fontface = "bold") +
  geom_text(aes(label = paste0("n=", scales::comma(n))),
            vjust = 1.5, size = 3, color = "white") +
  labs(title = "Contact Rate by 3D Swing Adjustability",
       subtitle = "Combined horizontal + vertical angle matching (weighted by pitch type)",
       x = "Adjustability Quality",
       y = "Contact Rate (%)") +
  ylim(0, 100) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Plot 2: 3D scatter - Horizontal vs Vertical colored by contact
p2 <- ggplot(swings %>% sample_n(min(5000, nrow(swings))) %>%
                          mutate(contact_label = factor(contact, levels = c(0, 1), 
                                                                                                      labels = c("Whiff", "Contact"))), 
                        aes(x = horiz_angle_diff_abs, y = vert_angle_diff_abs, color = contact_label)) +
    geom_point(alpha = 0.4, size = 2) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "gray40", alpha = 0.5) +
   geom_hline(yintercept = 10, linetype = "dashed", color = "gray40", alpha = 0.5) +
  scale_color_manual(values = c("Whiff" = "#e74c3c", "Contact" = "#2ecc71")) +
  labs(title = "3D Adjustability Space: Horizontal vs Vertical Matching",
                 subtitle = "Bottom-left quadrant = elite adjustability in both planes",
                 x = "Horizontal Angle Difference (degrees)",
                y = "Vertical Angle Difference (degrees)",
                color = "Outcome") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Plot 3: Contact rate heatmap
contact_heatmap_data <- swings %>%
  mutate(
    horiz_bin = cut(horiz_angle_diff_abs, 
                    breaks = seq(0, max(horiz_angle_diff_abs, na.rm = TRUE) + 5, by = 3)),
    vert_bin = cut(vert_angle_diff_abs,
                   breaks = seq(0, max(vert_angle_diff_abs, na.rm = TRUE) + 5, by = 3))
  ) %>%
  group_by(horiz_bin, vert_bin) %>%
  summarise(
    contact_rate = mean(contact, na.rm = TRUE) * 100,
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 50)  # Only bins with sufficient data

p3 <- ggplot(contact_heatmap_data, 
             aes(x = horiz_bin, y = vert_bin, fill = contact_rate)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Contact Rate Heatmap: 3D Adjustability Space",
       subtitle = "Darker = higher contact rate",
       x = "Horizontal Angle Difference (degrees)",
       y = "Vertical Angle Difference (degrees)",
       fill = "Contact\nRate (%)") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())



# Plot 4: Contact by combined adjustability and pitch type
p4 <- ggplot(contact_by_combined_pitch, 
             aes(x = adjustability_quality, y = contact_rate, fill = pitch_group)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", contact_rate)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 2.8) +
  scale_fill_manual(values = c("Fastball" = "#e74c3c", 
                               "Breaking Ball" = "#3498db", 
                               "Offspeed" = "#2ecc71")) +
  labs(title = "Contact Rate by 3D Adjustability and Pitch Type",
       x = "3D Adjustability Quality",
       y = "Contact Rate (%)",
       fill = "Pitch Type") +
  ylim(0, 100) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")



# Plot 5: Distribution of combined adjustability metric
p5 <- ggplot(swings, aes(x = combined_diff_weighted)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, 
                 fill = "#3498db", alpha = 0.7, color = "white") +
  geom_density(color = "#e74c3c", size = 1.5) +
  geom_vline(xintercept = median(swings$combined_diff_weighted, na.rm = TRUE),
             linetype = "dashed", color = "#2c3e50", size = 1) +
  labs(title = "Distribution of 3D Swing Adjustability",
       subtitle = paste0("Median = ", 
                         round(median(swings$combined_diff_weighted, na.rm = TRUE), 1), 
                         "° (weighted combination of horizontal + vertical)"),
       x = "Combined Angle Difference (degrees)",
       y = "Density") +
  theme_minimal(base_size = 12)


p1
p2
p3
p4
p5

median(player_3d_metrics$avg_3d_diff)

# ============================================================================
# 5. STATISTICAL MODELS - COMBINED ADJUSTABILITY
# ============================================================================

cat("\n\n=== STATISTICAL MODELS: COMBINED ADJUSTABILITY ===\n\n")

# Model 1: Combined metric only (weighted)
cat("--- MODEL 1: COMBINED WEIGHTED METRIC ---\n")
model_combined_weighted <- glm(
  contact ~ combined_diff_weighted + bat_speed + plate_x + plate_z + 
    contact_depth + release_speed,
  data = swings %>% filter(!is.na(contact_depth)),
  family = binomial(link = "logit")
)
print(summary(model_combined_weighted))

# Model 2: Combined metric + pitch type interaction
cat("\n\n--- MODEL 2: COMBINED METRIC × PITCH TYPE INTERACTION ---\n")
model_combined_pitch_int <- glm(
  contact ~ combined_diff_weighted * pitch_group + bat_speed + 
    plate_x + plate_z + contact_depth + release_speed,
  data = swings %>% filter(!is.na(contact_depth), pitch_group != "Other"),
  family = binomial(link = "logit")
)
print(summary(model_combined_pitch_int))

# Model 3: Separate horizontal and vertical (for comparison)
cat("\n\n--- MODEL 3: SEPARATE HORIZONTAL AND VERTICAL (BASELINE) ---\n")
model_separate <- glm(
  contact ~ horiz_angle_diff_abs + vert_angle_diff_abs + 
    bat_speed + plate_x + plate_z + contact_depth + release_speed,
  data = swings %>% filter(!is.na(contact_depth)),
  family = binomial(link = "logit")
)
print(summary(model_separate))

# Model 4: Interaction between horizontal and vertical
cat("\n\n--- MODEL 4: HORIZONTAL × VERTICAL INTERACTION ---\n")
model_interaction <- glm(
  contact ~ horiz_angle_diff_abs * vert_angle_diff_abs + 
    bat_speed + plate_x + plate_z + contact_depth + release_speed,
  data = swings %>% filter(!is.na(contact_depth)),
  family = binomial(link = "logit")
)
print(summary(model_interaction))

# Compare model fits
cat("\n\n=== MODEL COMPARISON (AIC) ===\n")
model_comparison <- data.frame(
  Model = c("Combined Weighted", "Combined × Pitch Type", 
            "Separate H+V", "H×V Interaction"),
  AIC = c(AIC(model_combined_weighted), AIC(model_combined_pitch_int),
          AIC(model_separate), AIC(model_interaction)),
  df = c(model_combined_weighted$df.residual, model_combined_pitch_int$df.residual,
         model_separate$df.residual, model_interaction$df.residual)
) %>%
  arrange(AIC) %>%
  mutate(delta_AIC = AIC - min(AIC))

print(model_comparison)
cat("\nLower AIC = better fit. Delta AIC < 2 = models essentially equivalent\n")

# ============================================================================
# 6. PLAYER-LEVEL 3D ADJUSTABILITY PROFILES
# ============================================================================

cat("\n\n=== PLAYER-LEVEL 3D ADJUSTABILITY PROFILES ===\n")
cat("Minimum swings threshold:", MIN_SWINGS_FOR_PLAYER_ANALYSIS, "\n\n")

player_3d_metrics <- swings %>%
  group_by(player_name) %>%
  summarise(
    n_swings = n(),
    
    # Individual plane metrics
    avg_horiz_diff = mean(horiz_angle_diff_abs, na.rm = TRUE),
    avg_vert_diff = mean(vert_angle_diff_abs, na.rm = TRUE),
    
    # Combined 3D metric
    avg_3d_diff = mean(combined_diff_weighted, na.rm = TRUE),
    sd_3d_diff = sd(combined_diff_weighted, na.rm = TRUE),
    
    # Outcomes
    contact_rate = mean(contact, na.rm = TRUE) * 100,
    whiff_rate = mean(whiff, na.rm = TRUE) * 100,
    
    # Other metrics
    avg_bat_speed = mean(bat_speed, na.rm = TRUE),
    
    # Quality classification
    pct_elite = mean(adjustability_quality == "Elite (≤5°)", na.rm = TRUE) * 100,
    pct_good_or_better = mean(adjustability_quality %in% c("Elite (≤5°)", "Good (5-8°)"), 
                              na.rm = TRUE) * 100,
    pct_poor = mean(adjustability_quality == "Poor (>16°)", na.rm = TRUE) * 100
  ) %>%
  filter(n_swings >= MIN_SWINGS_FOR_PLAYER_ANALYSIS) %>%
  mutate(
    # Classify player type
    player_type = case_when(
      avg_3d_diff <= 8 ~ "Elite Adjuster",
      avg_3d_diff <= 10 ~ "Good Adjuster",
      avg_3d_diff <= 13 ~ "Average Adjuster",
      TRUE ~ "Limited Adjuster"
    )
  )

cat("Players analyzed:", nrow(player_3d_metrics), "\n\n")

cat("Top 20 players by 3D ADJUSTABILITY:\n")
top_adjusters <- player_3d_metrics %>%
  arrange(avg_3d_diff) %>%
  select(player_name, n_swings, avg_3d_diff, avg_horiz_diff, avg_vert_diff, 
         contact_rate, player_type) %>%
  head(20)
print(top_adjusters)

cat("\n\nBottom 20 players by 3D ADJUSTABILITY:\n")
bottom_adjusters <- player_3d_metrics %>%
  arrange(desc(avg_3d_diff)) %>%
  select(player_name, n_swings, avg_3d_diff, avg_horiz_diff, avg_vert_diff, 
         contact_rate, player_type) %>%
  head(20)
print(bottom_adjusters)

cat("\n\nPlayer type distribution:\n")
print(table(player_3d_metrics$player_type))

# Correlation analysis
cat("\n\n3D ADJUSTABILITY vs OUTCOMES:\n")
cat("3D adjustability vs contact rate:", 
    round(cor(player_3d_metrics$avg_3d_diff, 
              player_3d_metrics$contact_rate, use = "complete.obs"), 3), "\n")
cat("3D adjustability vs whiff rate:", 
    round(cor(player_3d_metrics$avg_3d_diff, 
              player_3d_metrics$whiff_rate, use = "complete.obs"), 3), "\n")
cat("3D adjustability vs bat speed:", 
    round(cor(player_3d_metrics$avg_3d_diff, 
              player_3d_metrics$avg_bat_speed, use = "complete.obs"), 3), "\n")

# Plot 6: Player 3D adjustability vs contact rate
p6 <- ggplot(player_3d_metrics, 
             aes(x = avg_3d_diff, y = contact_rate, color = avg_bat_speed)) +
  geom_point(aes(size = n_swings), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black", size = 1.5) +
  scale_color_gradient2(low = "blue",mid = "grey", high = "red", midpoint = 70) +
  labs(title = "Player-Level: 3D Adjustability vs Contact Rate",
       subtitle = paste0("Players with ≥", MIN_SWINGS_FOR_PLAYER_ANALYSIS, " swings"),
       x = "Average 3D Angle Difference (degrees)",
       y = "Contact Rate (%)",
       size = "Swings",
       color = "Player Type") +
  theme_minimal(base_size = 12)

p6


# Plot 7: Player profiles - horizontal vs vertical with 3D as size
p7 <- ggplot(player_3d_metrics, 
             aes(x = avg_horiz_diff, y = avg_vert_diff)) +
  geom_point(aes(size = contact_rate, color = avg_3d_diff), alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Player Profiles: Horizontal vs Vertical Adjustability",
       subtitle = "Diagonal = balanced; Below = better horizontal; Above = better vertical",
       x = "Avg Horizontal Angle Difference (degrees)",
       y = "Avg Vertical Angle Difference (degrees)",
       size = "Contact\nRate (%)",
       color = "3D\nDifference") +
  theme_minimal(base_size = 12)

p7


# Plot 8: Pitch-type weights visualization
p8 <- pitch_weights %>%
  filter(pitch_group != "Other") %>%
  select(pitch_group, horiz_weight, vert_weight) %>%
  pivot_longer(cols = c(horiz_weight, vert_weight),
               names_to = "plane", values_to = "weight") %>%
  mutate(plane = if_else(plane == "horiz_weight", "Horizontal", "Vertical")) %>%
  ggplot(aes(x = pitch_group, y = weight, fill = plane)) +
  geom_col(position = position_dodge(width = 0.8), alpha = 0.8) +
  geom_text(aes(label = sprintf("%.2f", weight)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Horizontal" = "#9b59b6", "Vertical" = "#e67e22")) +
  labs(title = "Data-Derived Pitch-Type Specific Weights",
       subtitle = "Weights calculated from logistic regression coefficients",
       x = "Pitch Type",
       y = "Weight (proportion of total effect)",
       fill = "Plane",
       caption = "Weights automatically adjust to your data") +
  ylim(0, 0.7) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

p8


# ============================================================================
# 7. PITCH-TYPE SPECIFIC PLAYER ANALYSIS
# ============================================================================

cat("\n\n=== PLAYER ADJUSTABILITY BY PITCH TYPE ===\n\n")

player_by_pitch <- swings %>%
  filter(pitch_group != "Other") %>%
  group_by(player_name, pitch_group) %>%
  summarise(
    n = n(),
    avg_3d_diff = mean(combined_diff_weighted, na.rm = TRUE),
    avg_horiz_diff = mean(horiz_angle_diff_abs, na.rm = TRUE),
    avg_vert_diff = mean(vert_angle_diff_abs, na.rm = TRUE),
    contact_rate = mean(contact, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  filter(n >= 15)  # At least 15 swings per pitch type

# Find specialists
cat("Players who adjust BEST to Breaking Balls:\n")
bb_specialists <- player_by_pitch %>%
  filter(pitch_group == "Breaking Ball") %>%
  arrange(avg_3d_diff) %>%
  select(player_name, n, avg_3d_diff, contact_rate) %>%
  head(10)
print(bb_specialists)

cat("\n\nPlayers who adjust BEST to Fastballs:\n")
fb_specialists <- player_by_pitch %>%
  filter(pitch_group == "Fastball") %>%
  arrange(avg_3d_diff) %>%
  select(player_name, n, avg_3d_diff, contact_rate) %>%
  head(10)
print(fb_specialists)

cat("\n\nPlayers who adjust BEST to Offspeed:\n")
os_specialists <- player_by_pitch %>%
  filter(pitch_group == "Offspeed") %>%
  arrange(avg_3d_diff) %>%
  select(player_name, n, avg_3d_diff, contact_rate) %>%
  head(10)
print(os_specialists)



# ============================================================================
# 8. PREDICTIVE POWER ANALYSIS
# ============================================================================

cat("\n\n=== PREDICTIVE POWER: 3D ADJUSTABILITY FOR CONTACT ===\n\n")

# Create bins for cross-validation style analysis
swings_pred <- swings %>%
  filter(!is.na(contact_depth)) %>%
  mutate(
    adjustability_bin = ntile(combined_diff_weighted, 10)
  )

pred_power <- swings_pred %>%
  group_by(adjustability_bin) %>%
  summarise(
    n = n(),
    avg_3d_diff = mean(combined_diff_weighted, na.rm = TRUE),
    predicted_contact = mean(contact, na.rm = TRUE) * 100,
    se = sqrt(predicted_contact/100 * (1-predicted_contact/100) / n) * 100
  )

cat("Contact rate by 3D adjustability decile:\n")
print(pred_power)

# Calculate R-squared equivalent for binomial
cat("\n\nPseudo R-squared (McFadden) for different models:\n")
null_model <- glm(contact ~ 1, data = swings_pred, family = binomial)
r2_combined <- 1 - (logLik(model_combined_weighted) / logLik(null_model))
r2_separate <- 1 - (logLik(model_separate) / logLik(null_model))

cat("Combined 3D metric:", round(as.numeric(r2_combined), 4), "\n")
cat("Separate H+V:", round(as.numeric(r2_separate), 4), "\n")
cat("Difference:", round(as.numeric(r2_separate - r2_combined), 4), "\n")
cat("(Separate should be slightly better - uses more information)\n")


