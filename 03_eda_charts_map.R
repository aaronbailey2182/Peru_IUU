# =============================================================================
# 03_eda_charts_map.R
# Exploratory visualizations of vessel activity around Peru EEZ.
# Plots: (A) Spatial map of vessels + ports
#        (B) Active vessels per year by category
#        (C) Dark vessel detections by year (faceted map)
#        (D) Average monthly vessel activity (stacked bar)
#        (E) Monthly time series 2017–2021
# Requires: 01_database.R and 02_port_geocode.R to have been run first
# =============================================================================

rm(list = ls(all = TRUE))
source("C:/Users/aaron/OneDrive/Documents/R/GAR/PeruFishing/00_setup.R")

Peru_eez          <- load_eez()
Peru_eez_buffered <- make_eez_buffer(Peru_eez)
peru_land         <- load_peru_land()

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------
vessels_peru <- import(file.path(base_path, "data/vessels_peru_eez_buffer.csv")) %>% add_time_cols()

# Fishing ports from geocoded catch data (produced by 02_port_geocode.R)
fishing_ports_sf <- import(file.path(base_path, "data/peru_fishing_ports_catch_coords_2017_2021.csv")) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  distinct(puerto_clean, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# -----------------------------------------------------------------------------
# 2. MAP: Vessels + EEZ + Buffer + Fishing Ports
# -----------------------------------------------------------------------------
ggplot() +
  geom_sf(data = peru_land, fill = "grey92", color = "grey40", linewidth = 0.4) +
  geom_sf(data = Peru_eez,          fill = "lightblue", color = "blue",  alpha = 0.3, linewidth = 1) +
  geom_sf(data = Peru_eez_buffered, fill = NA,           color = "red",   linetype = "dotted", linewidth = 1) +
  geom_point(data = vessels_peru,
             aes(x = lon, y = lat, color = matched_category),
             size = 0.4, alpha = 0.4) +
  geom_sf(data = fishing_ports_sf,  shape = 8, color = "gold", size = 3, stroke = 1.5) +
  scale_color_manual(values = vessel_colors, name = "Vessel Category") +
  coord_sf(xlim = c(-85, -68), ylim = c(-22, -2), expand = FALSE) +
  labs(
    title    = "Fishing Ports and Vessel Activity around Peru EEZ",
    subtitle = "EEZ = Blue | Buffer = Red Dotted | Fishing Ports = Gold Stars",
    x = "Longitude", y = "Latitude"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  theme_minimal() +
  theme(legend.position = "bottom")

# -----------------------------------------------------------------------------
# 3. CHART: Active Vessels per Year by Matched Category
# -----------------------------------------------------------------------------
vessels_yearly_matched <- vessels_peru %>%
  filter(matched_category != "unmatched") %>%
  group_by(year, matched_category) %>%
  summarise(num_active_vessels = n_distinct(mmsi), .groups = "drop")

vessels_yearly_unmatched <- vessels_peru %>%
  filter(matched_category == "unmatched") %>%
  group_by(year, matched_category) %>%
  summarise(num_active_vessels = n(), .groups = "drop")

vessels_yearly <- bind_rows(vessels_yearly_matched, vessels_yearly_unmatched)

ggplot(vessels_yearly, aes(x = factor(year), y = num_active_vessels, fill = matched_category)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = vessel_colors, name = "Vessel Category") +
  labs(
    title    = "Number of Active Vessels per Year by Vessel Type",
    subtitle = "Matched categories = distinct MMSIs | Unmatched (dark) = raw detections",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title  = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# vessels_peru is already filtered to EEZ + buffer by 01_database.R
vessels_in_eez_buffer <- vessels_peru

# -----------------------------------------------------------------------------
# 4. CHART C: Dark Vessel Detections by Year (Faceted Map)
# -----------------------------------------------------------------------------
dark_vessels <- vessels_in_eez_buffer %>%
  filter(matched_category == "unmatched")

ggplot() +
  geom_sf(data = peru_land, fill = "grey92", color = "grey40", linewidth = 0.4) +
  geom_sf(data = Peru_eez, fill = NA, color = "blue", linewidth = 0.8) +
  geom_point(data = dark_vessels,
             aes(x = lon, y = lat),
             color = "black", alpha = 0.5, size = 0.6) +
  coord_sf(xlim = c(-85, -68), ylim = c(-22, -2), expand = FALSE) +
  labs(
    title    = "Dark Vessel Detections in and Near Peru's EEZ",
    subtitle = "Unmatched (non-AIS) satellite detections shown by year",
    x = "Longitude", y = "Latitude"
  ) +
  facet_wrap(~ year) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(size = 10, face = "bold")
  )

# -----------------------------------------------------------------------------
# 5. CHART D: Average Monthly Vessel Activity (stacked bar, all years combined)
# -----------------------------------------------------------------------------
vessels_monthly_avg <- vessels_in_eez_buffer %>%
  group_by(year, month, matched_category) %>%
  summarise(detections = n(), .groups = "drop") %>%
  group_by(month, matched_category) %>%
  summarise(avg_detections = mean(detections), .groups = "drop")

ggplot(vessels_monthly_avg, aes(x = month, y = avg_detections, fill = matched_category)) +
  geom_col(position = "stack", alpha = 0.85) +
  scale_fill_manual(values = vessel_colors, name = "Vessel Type") +
  labs(
    title    = "Average Vessel Activity",
    subtitle = "Mean vessel detections per month by category (across all years)",
    x = "Month", y = "Average Vessel Detections"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 45, hjust = 1),
    plot.title       = element_text(face = "bold", size = 14)
  )

# -----------------------------------------------------------------------------
# 6. CHART E: Monthly Time Series 2017–2021
# -----------------------------------------------------------------------------
vessels_ts <- vessels_in_eez_buffer %>%
  mutate(ym = floor_date(date, "month")) %>%
  filter(ym >= as.Date("2017-01-01"), ym <= as.Date("2021-12-01")) %>%
  group_by(ym, matched_category) %>%
  summarise(detections = n(), .groups = "drop")

ggplot(vessels_ts, aes(x = ym, y = detections, color = matched_category)) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  scale_color_manual(values = vessel_colors, name = "Vessel Type") +
  labs(
    title    = "Monthly Vessel Activity by Category (Jan 2017 – Dec 2021)",
    subtitle = "Detections inside Peru's EEZ + 20 km buffer",
    x = "Year", y = "Monthly Vessel Detections"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title       = element_text(face = "bold", size = 14),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )