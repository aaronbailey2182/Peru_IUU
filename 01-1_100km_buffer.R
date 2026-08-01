############################################
### Industrial Vessels within 100 km of Peru EEZ
############################################

# ---- Load Required Libraries ----
library(sf)
library(ggplot2)
library(dplyr)
library(rio)

# ---- Import Datasets ----
# Vessel detections (MASTER dataset)
vessels <- import("C:/Users/aaron/OneDrive/Documents/R/GAR/PeruFishing/data/industrial_vessels_v20240102_MASTER_COPY.csv")

# Peru EEZ geometry
Peru_eez <- st_read("data/geometry.geojson")

# (Optional) Fishing ports layer if available
# fishing_ports_sf <- st_read("data/fishing_ports.geojson")

# ---- Validate and Prepare EEZ Geometry ----
Peru_eez <- st_make_valid(Peru_eez)

# Project to meters, buffer 100 km (100,000 m), reproject back to WGS84
Peru_eez_buffered <- st_buffer(st_transform(Peru_eez, 3857), dist = 100000) %>%
  st_transform(4326)

# ---- Convert Vessel Data to Spatial Points ----
vessels_sf <- st_as_sf(vessels, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# ---- Identify Vessels Within EEZ and 100 km Buffer ----
vessels_sf <- vessels_sf %>%
  mutate(
    inside_eez     = st_within(vessels_sf, Peru_eez, sparse = FALSE)[, 1],
    inside_100km   = st_within(vessels_sf, Peru_eez_buffered, sparse = FALSE)[, 1],
    zone = case_when(
      inside_eez ~ "Inside EEZ",
      !inside_eez & inside_100km ~ "0–100 km Buffer",
      TRUE ~ "Outside 100 km"
    )
  )

# ---- Define Color Palette for Vessel Categories ----
vessel_colors <- c(
  "matched_fishing"     = "green3",
  "matched_nonfishing"  = "orange",
  "matched_unknown"     = "purple",
  "unmatched"           = "black"   # Dark vessels (no AIS)
)

# ---- Save Filtered Data for EEZ + Buffer ----
vessels_in_range <- vessels_sf %>%
  filter(zone != "Outside 100 km") %>%
  st_drop_geometry()

write.csv(
  vessels_in_range,
  "C:/Users/aaron/OneDrive/Documents/R/GAR/PeruFishing/data/vessels_peru_eez_100km_buffer.csv",
  row.names = FALSE
)

cat("\n✅ Saved vessels within 100 km of Peru EEZ.\n")

# ---- Create Map ----
ggplot() +
  # EEZ polygon (solid blue)
  geom_sf(data = Peru_eez, fill = "lightblue", color = "blue", alpha = 0.3, size = 1) +
  
  # 100 km buffer (red dotted)
  geom_sf(data = Peru_eez_buffered, fill = NA, color = "red", linetype = "dotted", size = 1) +
  
  # Vessels colored by matched category
  geom_sf(
    data = vessels_sf %>% filter(zone != "Outside 100 km"),
    aes(color = matched_category),
    size = 0.6, alpha = 0.6
  ) +
  
  # Optional: Add fishing ports as gold stars
  # geom_sf(data = fishing_ports_sf, shape = 8, color = "gold", size = 3, stroke = 1.5) +
  
  scale_color_manual(
    values = vessel_colors,
    name = "Vessel Category"
  ) +
  
  coord_sf(xlim = c(-90, -70), ylim = c(-25, 0), expand = FALSE) +
  
  labs(
    title = "Industrial Vessels within 100 km of Peru’s EEZ",
    subtitle = "EEZ = Blue | Buffer = Red Dotted | Dark Vessels = Black",
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )
