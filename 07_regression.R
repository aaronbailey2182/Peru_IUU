# =============================================================================
# 07_regression.R
#
# Research question: Does illegal fishing activity (dark vessel detections)
# reduce legitimate catch landings in Peru's EEZ?
#
# Model:
#   log(total_catch_TM) = β1(dark_vessels) + β2(sst) + β3(chla) +
#                         β4(air_temp) + β5(precip) + β6(mei) +
#                         port FE + time FE + ε
#
# Key coefficient of interest: β1 — effect of dark vessel pressure on catch
# A negative β1 supports the hypothesis that illegal fishing hurts the sector.
#
# Note: MEI ENSO index removed due to collinearity with time fixed effects.
# Monthly time FE absorbs all common time-varying shocks including ENSO.
#
# Requires: 06_grid_port_panel_construction.R to have been run first
# Output:   regression_results.csv, regression_summary.txt
# =============================================================================

rm(list = ls(all = TRUE))
source("C:/Users/aaron/OneDrive/Documents/R/GAR/PeruFishing/00_setup.R")

library(fixest)    # fast fixed effects estimation (feols)
library(modelsummary) # clean regression tables

# =============================================================================
# SECTION 1: LOAD AND PREPARE PANEL
# =============================================================================
panel_raw <- import(file.path(base_path, "outputs/grid_monthly_panel.csv")) %>%
  mutate(period = as.Date(period))

# -----------------------------------------------------------------------------
# Aggregate dark vessel pressure from grid cell to port level
# We want: for each port × month, how many dark vessel detections occurred
# in all grid cells assigned to that port?
# -----------------------------------------------------------------------------
dark_pressure <- panel_raw %>%
  filter(matched_category == "unmatched") %>%
  group_by(puerto_clean, period) %>%
  summarise(
    dark_detections  = sum(detections, na.rm = TRUE),
    dark_grid_cells  = n_distinct(grid_id),   # spatial footprint of dark activity
    .groups = "drop"
  )

# Legitimate fishing vessel pressure (for comparison/robustness)
fishing_pressure <- panel_raw %>%
  filter(matched_category == "matched_fishing") %>%
  group_by(puerto_clean, period) %>%
  summarise(
    fishing_vessels = sum(active, na.rm = TRUE),
    .groups = "drop"
  )

# Port-level catch + met data (one row per port × month)
# Take met averages across all grid cells assigned to each port
port_panel <- panel_raw %>%
  group_by(puerto_clean, period) %>%
  summarise(
    total_catch_TM = first(total_catch_TM),   # same for all rows in port × month
    sst_c_mean     = mean(sst_c_mean,    na.rm = TRUE),
    chla_mean      = mean(chla_mean,     na.rm = TRUE),
    air_temp_mean  = mean(air_temp_mean, na.rm = TRUE),
    precip_mean    = mean(precip_mean,   na.rm = TRUE),
    mei_index      = first(mei_index),
    .groups = "drop"
  ) %>%
  left_join(dark_pressure,    by = c("puerto_clean", "period")) %>%
  left_join(fishing_pressure, by = c("puerto_clean", "period")) %>%
  replace_na(list(dark_detections = 0, dark_grid_cells = 0, fishing_vessels = 0))

# -----------------------------------------------------------------------------
# Transform variables
# -----------------------------------------------------------------------------
reg_data <- port_panel %>%
  filter(total_catch_TM > 0) %>%           # log requires positive catch
  mutate(
    log_catch       = log(total_catch_TM),
    log_dark        = log1p(dark_detections),  # log1p handles zeros
    log_fishing     = log1p(fishing_vessels),
    year_month      = format(period, "%Y-%m"), # time fixed effect variable
    precip_mm       = precip_mean * 1000       # convert metres → mm for readability
  )


message("Regression sample: ", nrow(reg_data), " port × month observations")
message("Ports: ", n_distinct(reg_data$puerto_clean))
message("Time periods: ", n_distinct(reg_data$year_month))

# =============================================================================
# SECTION 1B: PORT × YEAR ANNUAL AGGREGATION
# Aggregates to annual port level to maximise sample and reduce noise
# from sparse monthly dark vessel detections.
# MEI can now be included since year FE does not fully absorb annual variation.
# =============================================================================
dark_annual <- panel_raw %>%
  filter(matched_category == "unmatched") %>%
  mutate(year = lubridate::year(period)) %>%
  group_by(puerto_clean, year) %>%
  summarise(
    dark_detections = sum(detections, na.rm = TRUE),
    dark_grid_cells = n_distinct(grid_id),
    dark_months     = n_distinct(period),
    .groups = "drop"
  )

fishing_annual <- panel_raw %>%
  filter(matched_category == "matched_fishing") %>%
  mutate(year = lubridate::year(period)) %>%
  group_by(puerto_clean, year) %>%
  summarise(fishing_vessels = sum(active, na.rm = TRUE), .groups = "drop")

port_annual <- panel_raw %>%
  mutate(year = lubridate::year(period)) %>%
  group_by(puerto_clean, year) %>%
  summarise(
    total_catch_TM = sum(total_catch_TM / 12, na.rm = TRUE),
    sst_c_mean     = mean(sst_c_mean,    na.rm = TRUE),
    chla_mean      = mean(chla_mean,     na.rm = TRUE),
    air_temp_mean  = mean(air_temp_mean, na.rm = TRUE),
    precip_mean    = mean(precip_mean,   na.rm = TRUE),
    mei_index      = mean(mei_index,     na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(dark_annual,    by = c("puerto_clean", "year")) %>%
  left_join(fishing_annual, by = c("puerto_clean", "year")) %>%
  replace_na(list(dark_detections = 0, dark_grid_cells = 0,
                  dark_months = 0, fishing_vessels = 0))

reg_annual <- port_annual %>%
  filter(total_catch_TM > 0) %>%
  mutate(
    log_catch   = log(total_catch_TM),
    log_dark    = log1p(dark_detections),
    log_fishing = log1p(fishing_vessels),
    precip_mm   = precip_mean * 1000,
    year_fe     = as.factor(year)
  )

message("Annual sample: ", nrow(reg_annual), " port x year observations")
message("Ports: ", n_distinct(reg_annual$puerto_clean))
message("Years: ", n_distinct(reg_annual$year))

# Annual models
a1 <- feols(log_catch ~ log_dark | puerto_clean + year_fe,
            data = reg_annual, cluster = ~puerto_clean)

a2 <- feols(log_catch ~ log_dark + sst_c_mean + chla_mean +
              air_temp_mean + precip_mm + mei_index |
              puerto_clean + year_fe,
            data = reg_annual, cluster = ~puerto_clean)

a3 <- feols(log_catch ~ log_dark + log_fishing + sst_c_mean + chla_mean +
              air_temp_mean + precip_mm + mei_index |
              puerto_clean + year_fe,
            data = reg_annual, cluster = ~puerto_clean)

annual_coef_labels <- c(
  log_dark      = "Dark vessels (log)",
  log_fishing   = "Fishing vessels (log)",
  sst_c_mean    = "SST (degrees C)",
  chla_mean     = "Chlorophyll-a (mg/m3)",
  air_temp_mean = "Air temperature (degrees C)",
  precip_mm     = "Precipitation (mm)",
  mei_index     = "MEI ENSO index"
)

message("\n--- Annual Port-Level Results ---")
modelsummary(
  list("Baseline" = a1, "+ Met controls" = a2, "+ Fishing vessels" = a3),
  coef_map = annual_coef_labels,
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map  = c("nobs", "r.squared", "adj.r.squared"),
  title    = "Annual Port-Level: Effect of Dark Vessel Activity on Catch",
  notes    = "Dependent variable: log(annual catch TM). Port and year FE. SEs clustered at port level.",
  output   = file.path(base_path, "outputs/regression_results_annual.html")
)

# =============================================================================
# SECTION 2: DESCRIPTIVE STATISTICS
# =============================================================================
desc_vars <- reg_data %>%
  select(
    total_catch_TM, dark_detections, fishing_vessels,
    sst_c_mean, chla_mean, air_temp_mean, precip_mm
  )

message("\n--- Descriptive Statistics ---")
print(summary(desc_vars))

# =============================================================================
# SECTION 3: REGRESSIONS
# =============================================================================

# --- Model 1: Dark vessels only + fixed effects (baseline) ---
m1 <- feols(
  log_catch ~ log_dark | puerto_clean + year_month,
  data    = reg_data,
  cluster = ~puerto_clean   # cluster SEs at port level
)

# --- Model 2: Add environmental controls ---
m2 <- feols(
  log_catch ~ log_dark + sst_c_mean + chla_mean + air_temp_mean +
    precip_mm | puerto_clean + year_month,
  data    = reg_data,
  cluster = ~puerto_clean
)

# --- Model 3: Add legitimate fishing vessel activity as control ---
m3 <- feols(
  log_catch ~ log_dark + log_fishing + sst_c_mean + chla_mean +
    air_temp_mean + precip_mm |
    puerto_clean + year_month,
  data    = reg_data,
  cluster = ~puerto_clean
)

# --- Model 4: Robustness — dark vessel spatial footprint instead of count ---
m4 <- feols(
  log_catch ~ dark_grid_cells + log_fishing + sst_c_mean + chla_mean +
    air_temp_mean + precip_mm |
    puerto_clean + year_month,
  data    = reg_data,
  cluster = ~puerto_clean
)

# =============================================================================
# SECTION 4: RESULTS TABLE
# =============================================================================
coef_labels <- c(
  log_dark        = "Dark vessels (log)",
  dark_grid_cells = "Dark vessel grid cells",
  log_fishing     = "Fishing vessels (log)",
  sst_c_mean      = "SST (°C)",
  chla_mean       = "Chlorophyll-a (mg/m³)",
  air_temp_mean   = "Air temperature (°C)",
  precip_mm       = "Precipitation (mm)"
)

modelsummary(
  list("Baseline" = m1, "+ Met controls" = m2,
       "+ Fishing vessels" = m3, "Spatial footprint" = m4),
  coef_map   = coef_labels,
  stars      = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map    = c("nobs", "r.squared", "adj.r.squared"),
  title      = "Effect of Dark Vessel Activity on Fishing Catch in Peru EEZ",
  notes      = "Dependent variable: log(catch in TM). Port and time fixed effects included in all models. Standard errors clustered at port level.",
  output     = file.path(base_path, "outputs/regression_results.html")
)

# Print to console too
modelsummary(
  list("Baseline" = m1, "+ Met controls" = m2,
       "+ Fishing vessels" = m3, "Spatial footprint" = m4),
  coef_map = coef_labels,
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map  = c("nobs", "r.squared", "adj.r.squared")
)

# =============================================================================
# SECTION 5: COEFFICIENT PLOT
# =============================================================================
modelplot(
  list("Baseline" = m1, "+ Met controls" = m2, "+ Fishing vessels" = m3),
  coef_map = coef_labels["log_dark"],   # focus on key coefficient
  color    = c("steelblue", "darkorange", "darkgreen")
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title    = "Effect of Dark Vessel Activity on Log Catch",
    subtitle = "Point estimates with 95% confidence intervals",
    x        = "Coefficient (log_dark)",
    y        = NULL
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# =============================================================================
# SECTION 6: SAVE REGRESSION SAMPLE FOR TRANSPARENCY
# =============================================================================
write.csv(reg_data,
          file.path(base_path, "outputs/regression_sample.csv"),
          row.names = FALSE)
message("Regression sample saved to outputs/regression_sample.csv")
message("Results table saved to outputs/regression_results.html")