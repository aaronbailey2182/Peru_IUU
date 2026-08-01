# =============================================================================
# 01_database.R
# Load raw vessel data, filter to Peru EEZ + buffer, attach fishing ports.
# Merges the logic of old 01_database.R and 02_EEZ_buffer_vessel_filter.R.
# Output: vessels_peru_eez_buffer.csv (saved to data/)
# =============================================================================

rm(list = ls(all = TRUE))
source("C:/Users/aaron/OneDrive/Documents/R/GAR/PeruFishing/00_setup.R")

Peru_eez          <- load_eez()
Peru_eez_buffered <- make_eez_buffer(Peru_eez)
peru_land         <- load_peru_land()

# -----------------------------------------------------------------------------
# 1. LOAD RAW VESSELS AND FILTER TO EEZ + BUFFER
# -----------------------------------------------------------------------------
vessels <- import(paths$raw_vessels)

vessels_sf <- vessels_to_sf(vessels)

vessels_in_eez_buffer <- st_intersection(vessels_sf, Peru_eez_buffered)

# Clean up and tag vessel type
vessels_export <- vessels_in_eez_buffer %>%
  st_drop_geometry() %>%
  add_vessel_type() %>%
  select(
    scene_id,
    timestamp,
    year,
    lat,
    lon,
    mmsi,
    vessel_type,
    matched_category,
    fishing_score,
    overpasses_2017_2021
  )

write.csv(vessels_export, file.path(base_path, "data/vessels_peru_eez_buffer.csv"), row.names = FALSE)
message("Saved filtered vessels to: ", file.path(base_path, "data/vessels_peru_eez_buffer.csv"))


# -----------------------------------------------------------------------------
# 2. SANITY CHECKS
# -----------------------------------------------------------------------------

# --- Console summary ---
message("\n--- Vessel filter summary ---")
message("Raw detections loaded:        ", nrow(vessels))
message("Kept (inside EEZ + buffer):   ", nrow(vessels_export))
message("Dropped:                      ", nrow(vessels) - nrow(vessels_export))
message("Years covered:                ", paste(sort(unique(vessels_export$year)), collapse = ", "))
message("Matched category breakdown:")
print(table(vessels_export$matched_category))

# --- Map: individual vessel detections as points, color coded by category ---
ggplot() +
  geom_sf(data = peru_land, fill = "grey92", color = "grey40", linewidth = 0.4) +
  geom_sf(data = Peru_eez,          fill = "lightblue", color = "blue", alpha = 0.2, linewidth = 0.8) +
  geom_sf(data = Peru_eez_buffered, fill = NA,          color = "red",  linewidth = 0.5, linetype = "dotted") +
  
  geom_point(data = vessels_export,
             aes(x = lon, y = lat, color = matched_category),
             size = 0.4, alpha = 0.4) +
  
  scale_color_manual(values = vessel_colors, name = "Vessel Category") +
  
  coord_sf(xlim = c(-85, -68), ylim = c(-22, -2), expand = FALSE) +
  labs(
    title    = "Sanity Check: Vessel Detections inside Peru EEZ + 20km Buffer",
    subtitle = paste0(
      format(nrow(vessels_export), big.mark = ","), " detections across ",
      length(unique(vessels_export$year)), " years"
    ),
    x = "Longitude", y = "Latitude"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "bottom"
  )

# --- Sanity Check 2: January 2017 only ---
vessels_jan2017 <- vessels_export %>%
  mutate(date = as.Date(timestamp)) %>%
  filter(year(date) == 2017, month(date) == 1)

message("\nJanuary 2017 detections: ", nrow(vessels_jan2017))

ggplot() +
  geom_sf(data = peru_land, fill = "grey92", color = "grey40", linewidth = 0.4) +
  geom_sf(data = Peru_eez,          fill = "lightblue", color = "blue", alpha = 0.2, linewidth = 0.8) +
  geom_sf(data = Peru_eez_buffered, fill = NA,          color = "red",  linewidth = 0.5, linetype = "dotted") +
  
  geom_point(data = vessels_jan2017,
             aes(x = lon, y = lat, color = matched_category),
             size = 0.8, alpha = 0.6) +
  
  scale_color_manual(values = vessel_colors, name = "Vessel Category") +
  
  coord_sf(xlim = c(-85, -68), ylim = c(-22, -2), expand = FALSE) +
  labs(
    title    = "Sanity Check: Vessel Detections — January 2017",
    subtitle = paste0(format(nrow(vessels_jan2017), big.mark = ","), " detections"),
    x = "Longitude", y = "Latitude"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "bottom"
  )