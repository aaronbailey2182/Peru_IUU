# =============================================================================
# 00_setup.R
# Shared setup for all Peru Fishing scripts.
# Source this at the top of every script instead of repeating boilerplate.
# =============================================================================

# -----------------------------------------------------------------------------
# 1. LIBRARIES
# -----------------------------------------------------------------------------
library(rio)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(sf)
library(lubridate)
library(ggrepel)

# -----------------------------------------------------------------------------
# 2. FILE PATHS
# (Change the base path here once and it propagates everywhere)
# -----------------------------------------------------------------------------
base_path <- "C:/Users/aaron/OneDrive/Documents/R/GAR/PeruFishing"

paths <- list(
  # Raw inputs — source files, never modified
  raw_vessels   = file.path(base_path, "data/industrial_vessels_v20240102_MASTER_COPY.csv"),
  eez_geojson   = file.path(base_path, "data/geometry.geojson"),
  landings_xlsx = file.path(base_path, "data/PESCA_TB_source_tableropesquero.xlsx"),
  
  out_dir       = file.path(base_path, "outputs"),
  
  # Met/environmental data — add each file path here as you acquire the data
  met = list(
    sst  = file.path(base_path, "data/peru_eez_sst_data.nc"),
    chla = file.path(base_path, "data/peru_eez_chlorophylla_2017-2021data.nc"), # monthly, Feb 2017–Dec 2021
    era5_temp  = file.path(base_path, "data/era5_t2m_d2m_2017-2021.nc"),  # t2m, d2m — monthly
    era5_precip = file.path(base_path, "data/era5_tp_2017-2021.nc"),          # tp — monthly
    enso = file.path(base_path, "data/meiv2_ENSO.data")         # MEI v2 index (NOAA PSL, 1979–2026)
  )
)

# Create outputs directory if needed
if (!dir.exists(paths$out_dir)) dir.create(paths$out_dir, recursive = TRUE)

# -----------------------------------------------------------------------------
# 3. SHARED CONSTANTS
# -----------------------------------------------------------------------------
STUDY_YEARS        <- 2017:2021
EEZ_BUFFER_KM      <- 20          # km buffer around Peru EEZ
GRID_SIZE_DEG      <- 0.5         # degrees, for coarse vessel grid
GRID_SIZE_FINE     <- 0.1         # degrees, for SST-merged grid
# MAX_PORT_DIST_KM removed — all detections fall within 149km of nearest port
# so no distance cap is needed. Nearest-port assignment used for all EEZ cells.

# Consistent vessel color palette used across all maps and charts
vessel_colors <- c(
  "matched_fishing"    = "green3",
  "matched_nonfishing" = "orange",
  "matched_unknown"    = "purple",
  "unmatched"          = "black"
)

library(rnaturalearth)

# -----------------------------------------------------------------------------
# 4. EEZ GEOMETRY HELPERS
# Call these at the top of any script that needs spatial filtering.
#   Peru_eez          <- load_eez()
#   Peru_eez_buffered <- make_eez_buffer(Peru_eez)
# -----------------------------------------------------------------------------
load_eez <- function() {
  st_read(paths$eez_geojson, quiet = TRUE) %>%
    st_make_valid() %>%
    st_transform(4326)
}

make_eez_buffer <- function(eez, buffer_km = EEZ_BUFFER_KM) {
  st_buffer(st_transform(eez, 3857), dist = buffer_km * 1000) %>%
    st_transform(4326)
}

load_peru_land <- function() {
  ne_countries(country = "Peru", returnclass = "sf") %>%
    st_transform(4326)
}

# -----------------------------------------------------------------------------
# 5. HELPER: convert vessels data frame → sf points
# -----------------------------------------------------------------------------
vessels_to_sf <- function(df, lon_col = "lon", lat_col = "lat") {
  st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE)
}

# -----------------------------------------------------------------------------
# 6. HELPER: add vessel_type flag (AIS vs Dark)
# -----------------------------------------------------------------------------
add_vessel_type <- function(df) {
  df %>% mutate(
    vessel_type = if_else(is.na(mmsi) | mmsi < 100000000, "Dark", "AIS")
  )
}

# -----------------------------------------------------------------------------
# 7. HELPER: add date/year/month columns from timestamp
# -----------------------------------------------------------------------------
add_time_cols <- function(df) {
  df %>% mutate(
    date  = as.Date(timestamp),
    year  = year(date),
    month = lubridate::month(date, label = TRUE, abbr = TRUE)
  )
}

message("00_setup.R loaded. Call load_eez() and make_eez_buffer() to initialise geometry.")