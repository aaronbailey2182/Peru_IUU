# =============================================================================
# 04_grid_level_database.R
# Grid-level vessel summaries, EDA charts, and time series.
# Requires: 01_database-Aaron.R to have been run first (produces
#           vessels_peru_eez_buffer.csv)
# =============================================================================

rm(list = ls(all = TRUE))
source("C:/Users/aaron/OneDrive/Documents/R/GAR/PeruFishing/00_setup.R")

# -----------------------------------------------------------------------------
# LOAD INPUTS
# -----------------------------------------------------------------------------
Peru_eez          <- load_eez()
Peru_eez_buffered <- make_eez_buffer(Peru_eez)
peru_land         <- load_peru_land()

vessels_peru <- import(file.path(base_path, "data/vessels_peru_eez_buffer.csv"))

################  Building the database grid view
#################################################

# Convert vessel detections to sf object
vessels_sf <- vessels_to_sf(vessels_peru)

# Keep only vessels within EEZ + 20 km buffer
vessels_in_eez_buffer <- st_intersection(vessels_sf, Peru_eez_buffered)

# Grid size defined in 00_setup.R as GRID_SIZE_DEG = 0.5
grid_size <- GRID_SIZE_DEG

# Build gridded summary dataset
grid_summary <- vessels_in_eez_buffer %>%
  mutate(
    lat_bin = floor(lat / grid_size) * grid_size,
    lon_bin = floor(lon / grid_size) * grid_size,
    grid_id = paste(lat_bin, lon_bin, sep = "_"),
    vessel_type = case_when(
      is.na(mmsi) | mmsi < 100000000 ~ "Dark",
      TRUE ~ "AIS"
    )
  ) %>%
  group_by(year, grid_id, lat_bin, lon_bin) %>%
  summarise(
    total_vessels = n(),
    dark_vessels = sum(vessel_type == "Dark", na.rm = TRUE),
    .groups = "drop"
  )

# Display the top rows
print(head(grid_summary, 20))

# this creates a table that can be read as such:
# year = calender year
# grid_id = ID from the .5 x .5 grid cell
# lat_bin/lon_bin = defining the grid cell's SW corner (bottom left)
# total_vessles = vessels detected inside that cell, per year
# dark_vessels = vessels classified as dark within that cell, per year

###############################################################
### ACTIVE VESSELS PER YEAR BY MATCHED CATEGORY (4 TYPES)
###############################################################

# Ensure timestamp → year
vessels_in_eez_buffer <- vessels_in_eez_buffer %>%
  mutate(
    year = year(as.Date(timestamp))
  )

# Separate unmatched (dark) and matched categories
dark_yearly <- vessels_in_eez_buffer %>%
  filter(matched_category == "unmatched") %>%
  group_by(year, matched_category) %>%
  summarise(num_active = n(), .groups = "drop")   # detection count

matched_yearly <- vessels_in_eez_buffer %>%
  filter(matched_category != "unmatched") %>%
  group_by(year, matched_category) %>%
  summarise(num_active = n_distinct(mmsi), .groups = "drop")  # distinct AIS vessels

# Combine
vessels_yearly_4types <- bind_rows(dark_yearly, matched_yearly)

# Color palette (consistent with your map)
vessel_colors_4 <- c(
  "matched_fishing" = "green3",
  "matched_nonfishing" = "orange",
  "matched_unknown" = "purple",
  "unmatched" = "black"
)

# Plot: grouped bar chart
ggplot(vessels_yearly_4types,
       aes(x = factor(year), y = num_active, fill = matched_category)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = vessel_colors_4, name = "Vessel Category") +
  labs(
    title = "Active Vessels per Year by Category (Inside Peru EEZ + 20 km Buffer)",
    subtitle = "Matched categories counted as unique MMSIs; Unmatched counted as detections",
    x = "Year",
    y = "Number of Active Vessels / Detections"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

###############################################################
### MAP: DARK VESSEL DETECTIONS BY YEAR (FACETED, NO PORTS)
###############################################################

# Filter dark vessels
dark_vessels_sf <- vessels_in_eez_buffer %>%
  filter(matched_category == "unmatched") %>%
  mutate(year = year(timestamp))   # extract year

ggplot() +
  # EEZ outline only
  geom_sf(data = Peru_eez, fill = NA, color = "blue", size = 0.8) +
  
  # Dark vessel detections (black points)
  geom_sf(data = dark_vessels_sf, color = "black", alpha = 0.5, size = 0.6) +
  
  coord_sf(
    xlim = c(-90, -70),
    ylim = c(-20, 0),
    expand = FALSE
  ) +
  
  labs(
    title = "Dark Vessel Detections in and Near Peru's EEZ",
    subtitle = "Unmatched (non-AIS) satellite detections shown by year",
    x = "Longitude",
    y = "Latitude"
  ) +
  
  facet_wrap(~ year) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(size = 10, face = "bold")
  )

###############################################################
### PLOT B: Monthly Vessel Activity by Type (Averaged)
###############################################################

# Step 1 — Build year–month dataset
vessels_monthly <- vessels_in_eez_buffer %>%
  mutate(
    date = as.Date(timestamp),
    year = year(date),
    month = month(date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(year, month, matched_category) %>%
  summarise(
    detections = n(),
    .groups = "drop"
  )

# Step 2 — Compute multi-year monthly averages
vessels_monthly_avg <- vessels_monthly %>%
  group_by(month, matched_category) %>%
  summarise(
    avg_detections = mean(detections),
    .groups = "drop"
  )

# Step 3 — Plot the averaged stacked bars
ggplot(vessels_monthly_avg,
       aes(x = month, y = avg_detections, fill = matched_category)) +
  
  geom_col(position = "stack", alpha = 0.85) +
  
  scale_fill_manual(values = vessel_colors,
                    name = "Vessel Type") +
  
  labs(
    title = "Average Vessel Activity",
    subtitle = "Mean vessel detections per month by category (across all years)",
    x = "Month",
    y = "Average Vessel Detections"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )

###############################################################
### TIME SERIES: Vessel Detections 2017–2021
###############################################################

# Prepare dataset with correct date granularity
vessels_ts <- vessels_in_eez_buffer %>%
  mutate(
    date = as.Date(timestamp),
    year = year(date),
    month = month(date),
    ym = floor_date(date, "month")   # first day of the month
  ) %>%
  
  # *** Restrict to your study period ***
  filter(ym >= as.Date("2017-01-01"),
         ym <= as.Date("2021-12-01")) %>%
  
  group_by(ym, matched_category) %>%
  summarise(
    detections = n(),
    .groups = "drop"
  )

# Time-series plot
ggplot(vessels_ts, aes(x = ym, y = detections, color = matched_category)) +
  
  geom_line(linewidth = 0.9, alpha = 0.85) +
  
  scale_color_manual(values = vessel_colors,
                     name = "Vessel Type") +
  
  labs(
    title = "Monthly Vessel Activity by Category (Jan 2017 – Dec 2021)",
    subtitle = "Detections inside Peru's EEZ + 20 km buffer",
    x = "Year",
    y = "Monthly Vessel Detections"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )