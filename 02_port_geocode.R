# =============================================================================
# 02_port_geocode.R
# Geocode fishing landing ports from PESCA catch data and spatial-filter
# to the Peruvian coast.
# Requires: geometry.geojson (via 00_setup.R)
# Output:   peru_fishing_ports_catch_coords_2017_2021.csv
# =============================================================================

rm(list = ls(all = TRUE))
source("C:/Users/aaron/OneDrive/Documents/R/GAR/PeruFishing/00_setup.R")

Peru_eez          <- load_eez()
Peru_eez_buffered <- make_eez_buffer(Peru_eez)
peru_land         <- load_peru_land()

library(tidygeocoder)

# -----------------------------------------------------------------------------
# 1. LOAD AND FILTER LANDINGS DATA
# -----------------------------------------------------------------------------
desembarque <- import(paths$landings_xlsx, which = 2)

fishlandings <- desembarque %>%
  mutate(puerto_clean = trimws(tolower(`PUERTO / LOCALIDAD`))) %>%
  filter(AÑO >= min(STUDY_YEARS), AÑO <= max(STUDY_YEARS))

# -----------------------------------------------------------------------------
# 2. FILTER TO MARITIME PORTS ONLY
# The PESCA data has an AMBITO column: MARITIMO vs CONTINENTAL/LACUSTRE.
# Filter to MARITIMO to keep only coastal marine fishing ports.
# -----------------------------------------------------------------------------
fishlandings <- fishlandings %>%
  filter(trimws(toupper(AMBITO)) == "MARITIMO")

message("Ports after MARITIMO filter: ",
        n_distinct(fishlandings$puerto_clean))

# -----------------------------------------------------------------------------
# 3. GEOCODE UNIQUE PORTS
# -----------------------------------------------------------------------------
ports_to_geocode <- fishlandings %>%
  filter(!puerto_clean %in% c("otros", "otro", "", NA)) %>%
  distinct(puerto_clean) %>%
  mutate(address = paste(puerto_clean, "peru"))

port_locations <- ports_to_geocode %>%
  geocode(address = address, method = "osm",
          lat = latitude, long = longitude, verbose = TRUE)

# -----------------------------------------------------------------------------
# 4. SPATIAL FILTER: keep only ports within 5 km of EEZ (coastal ports)
# -----------------------------------------------------------------------------
port_locations_sf <- port_locations %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

coastal_ports <- port_locations_sf[
  st_is_within_distance(port_locations_sf, Peru_eez, dist = 50000, sparse = FALSE)[, 1], ]

# Show dropped / ambiguous ports for optional manual review
dropped_ports <- port_locations_sf[
  !(port_locations_sf$puerto_clean %in% coastal_ports$puerto_clean), ]
message(nrow(dropped_ports), " ports dropped (outside 5 km of EEZ). View 'dropped_ports' to inspect.")

# -----------------------------------------------------------------------------
# 5. JOIN COORDS TO CATCH DATA AND SUMMARISE
# -----------------------------------------------------------------------------
landings_with_coords <- fishlandings %>%
  left_join(st_drop_geometry(coastal_ports), by = "puerto_clean")

catch_summary_by_port <- landings_with_coords %>%
  group_by(puerto_clean, latitude, longitude, AÑO, MES) %>%
  summarize(total_catch_TM = sum(`DESEMBARQUE (EN TM)`, na.rm = TRUE), .groups = "drop") %>%
  mutate(month_floor = as.Date(paste(AÑO, MES, "01", sep = "-")))

write.csv(catch_summary_by_port, file.path(base_path, "data/peru_fishing_ports_catch_coords_2017_2021.csv"), row.names = FALSE)
message("Port catch summary saved to: ", file.path(base_path, "data/peru_fishing_ports_catch_coords_2017_2021.csv"))

# -----------------------------------------------------------------------------
# 6. SANITY CHECKS
# -----------------------------------------------------------------------------

# --- Console summary ---
message("\n--- Geocoding summary ---")
message("Ports in landings data:    ", nrow(ports_to_geocode))
message("Successfully geocoded:     ", nrow(port_locations %>% filter(!is.na(latitude))))
message("Kept (within 5km of EEZ):  ", nrow(coastal_ports))
message("Dropped:                   ", nrow(dropped_ports))
message("Total catch records:       ", nrow(catch_summary_by_port))
message("Years covered:             ", paste(sort(unique(catch_summary_by_port$AÑO)), collapse = ", "))
message("Monthly records:           ", nrow(catch_summary_by_port))

# --- Map: vessels + ports overlaid ---
# Load vessels (produced by 01_database.R)
vessels <- import(file.path(base_path, "data/vessels_peru_eez_buffer.csv"))

ggplot() +
  geom_sf(data = peru_land, fill = "grey92", color = "grey40", linewidth = 0.4) +
  geom_sf(data = Peru_eez,          fill = "lightblue", color = "blue", alpha = 0.2, linewidth = 0.8) +
  geom_sf(data = Peru_eez_buffered, fill = NA,          color = "red",  linewidth = 0.5, linetype = "dotted") +
  
  # Vessel detections color coded by category
  geom_point(data = vessels,
             aes(x = lon, y = lat, color = matched_category),
             size = 0.4, alpha = 0.4) +
  
  # Geocoded fishing ports as gold stars
  geom_point(data = st_drop_geometry(coastal_ports),
             aes(x = longitude, y = latitude),
             color = "gold", shape = 8, size = 3, stroke = 1.2) +
  
  scale_color_manual(values = vessel_colors, name = "Vessel Category") +
  
  coord_sf(xlim = c(-85, -68), ylim = c(-22, -2), expand = FALSE) +
  labs(
    title    = "Sanity Check: Vessel Detections + Geocoded Fishing Ports",
    subtitle = paste0(
      format(nrow(vessels), big.mark = ","), " vessel detections  |  ",
      nrow(coastal_ports), " fishing ports (gold stars)"
    ),
    x = "Longitude", y = "Latitude"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "bottom"
  )