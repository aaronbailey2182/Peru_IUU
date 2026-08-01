# =============================================================================
# 05_map_with_ports.R
# Two maps:
#   Map A — vessels + geocoded fishing ports (from catch data)
#   Map B — same, plus 100 km port buffer rings and Peru land polygon
# Requires: 01_database.R and 02_port_geocode.R to have been run first
# =============================================================================

rm(list = ls(all = TRUE))
source("C:/Users/aaron/OneDrive/Documents/R/GAR/PeruFishing/00_setup.R")

Peru_eez          <- load_eez()
Peru_eez_buffered <- make_eez_buffer(Peru_eez)

library(rnaturalearth)

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------
port_catch <- import(file.path(base_path, "data/peru_fishing_ports_catch_coords_2017_2021.csv")) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  distinct(puerto_clean, longitude, latitude, .keep_all = TRUE)

port_catch_sf <- st_as_sf(
  port_catch, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

vessels_peru <- import(file.path(base_path, "data/vessels_peru_eez_buffer.csv")) %>% add_time_cols()
vessels_sf   <- vessels_to_sf(vessels_peru)

# Peru land polygon (for Map B)
peru_land <- ne_countries(country = "Peru", returnclass = "sf") %>%
  st_transform(4326)

# Bounding box from EEZ buffer (used by both maps)
bb <- st_bbox(Peru_eez_buffered)

# -----------------------------------------------------------------------------
# 2. MAP A: Vessels + EEZ + Geocoded Ports (no buffer rings)
# -----------------------------------------------------------------------------
ggplot() +
  geom_sf(data = Peru_eez,          fill = "lightblue", color = "blue",  alpha = 0.3, linewidth = 1) +
  geom_sf(data = Peru_eez_buffered, fill = NA,           color = "red",   linetype = "dotted", linewidth = 1) +
  geom_sf(data = vessels_sf,        aes(color = matched_category), size = 0.5, alpha = 0.5) +
  geom_sf(data = port_catch_sf,     shape = 8, color = "gold", size = 2, stroke = 1) +
  scale_color_manual(values = vessel_colors, name = "Vessel Category") +
  coord_sf(xlim = c(-85, -70), ylim = c(-20, -2), expand = FALSE) +
  labs(
    title    = "Fishing Ports and Vessel Activity around Peru EEZ",
    subtitle = "EEZ = Blue | Buffer = Red Dotted | Ports = Gold Stars",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
# =============================================================================
# 06_map_with_ports.R
# Two maps:
#   Map A — vessels + geocoded fishing ports (from catch data)
#   Map B — same, plus 100 km port buffer rings and Peru land polygon
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
port_catch <- import(file.path(base_path, "data/peru_fishing_ports_catch_coords_2017_2021.csv")) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  distinct(puerto_clean, longitude, latitude, .keep_all = TRUE)

port_catch_sf <- st_as_sf(
  port_catch, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

vessels_peru <- import(file.path(base_path, "data/vessels_peru_eez_buffer.csv")) %>% add_time_cols()

# Bounding box from EEZ buffer (used by both maps)
bb <- st_bbox(Peru_eez_buffered)

# -----------------------------------------------------------------------------
# 2. MAP A: Vessels + EEZ + Geocoded Ports (no buffer rings)
# -----------------------------------------------------------------------------
ggplot() +
  geom_sf(data = peru_land,          fill = "grey92", color = "grey40", linewidth = 0.4) +
  geom_sf(data = Peru_eez,           fill = "lightblue", color = "blue", alpha = 0.3, linewidth = 1) +
  geom_sf(data = Peru_eez_buffered,  fill = NA, color = "red", linetype = "dotted", linewidth = 1) +
  geom_point(data = vessels_peru,
             aes(x = lon, y = lat, color = matched_category),
             size = 0.4, alpha = 0.4) +
  geom_sf(data = port_catch_sf, shape = 8, color = "gold", size = 2, stroke = 1) +
  scale_color_manual(values = vessel_colors, name = "Vessel Category") +
  coord_sf(xlim = c(-85, -68), ylim = c(-22, -2), expand = FALSE) +
  labs(
    title    = "Fishing Ports and Vessel Activity around Peru EEZ",
    subtitle = "EEZ = Blue | Buffer = Red Dotted | Ports = Gold Stars",
    x = "Longitude", y = "Latitude"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  theme_minimal() +
  theme(legend.position = "bottom")

# -----------------------------------------------------------------------------
# 3. MAP B: Same map + 100 km port buffer rings + Peru land polygon
# -----------------------------------------------------------------------------
port_buffers_100km <- st_transform(port_catch_sf, 3857) %>%
  st_buffer(dist = 100000) %>%
  st_transform(4326)

port_buffers_100km$puerto_clean <- port_catch_sf$puerto_clean

ggplot() +
  geom_sf(data = peru_land,          fill = "grey92",  color = "grey40", linewidth = 0.4) +
  geom_sf(data = Peru_eez,           fill = NA,         color = "blue",   linewidth = 1) +
  geom_sf(data = Peru_eez_buffered,  fill = NA,         color = "blue",   linetype = "dashed", linewidth = 0.8) +
  geom_point(data = vessels_peru,
             aes(x = lon, y = lat, color = matched_category),
             size = 0.4, alpha = 0.4) +
  geom_sf(data = port_catch_sf,      color = "gold", shape = 8, size = 3, stroke = 1.5) +
  geom_sf(data = port_buffers_100km, fill = NA, color = "red", linewidth = 1.1) +
  scale_color_manual(values = vessel_colors, name = "Vessel Category", drop = FALSE) +
  coord_sf(xlim = c(-85, -68), ylim = c(-22, -2), expand = FALSE) +
  labs(
    title    = "Vessels, Ports, and 100-km Port Buffers (Peru)",
    subtitle = "Grey = Peru land | Blue = EEZ | Red rings = 100 km around ports"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")