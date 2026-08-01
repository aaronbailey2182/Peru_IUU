# =============================================================================
# 06_grid_port_panel_construction.R
#
# Builds the core spatial panel dataset:
#   grid cell (0.1°) × time period → vessel counts + met variables
#   → assigned to nearest port → joined with port catch data
#
# ADDING A NEW MET VARIABLE (netCDF):
#   1. Add its path to paths$met in 00_setup.R
#   2. Call extract_netcdf_var() with the right variable name + unit conversion
#   3. Add the output column name to the left_join block in Section 6
#   4. Add a mean() line in aggregate_panel() in Section 8
#
# ADDING A NEW MET VARIABLE (CSV index, e.g. El Niño):
#   1. Load and format as a date-keyed tibble with one value column
#   2. Join to vessels_merged by month in Section 6
#
# Requires: 01_database.R and 02_port_geocode.R to have been run first
#
# Outputs (all written to paths$out_dir):
#   vessels_grid_port_met_pointlevel.csv   — one row per detection
#   grid_weekly_panel.csv
#   grid_biweekly_panel.csv
#   grid_monthly_panel.csv
# =============================================================================

rm(list = ls(all = TRUE))
source("C:/Users/aaron/OneDrive/Documents/R/GAR/PeruFishing/00_setup.R")

Peru_eez          <- load_eez()
Peru_eez_buffered <- make_eez_buffer(Peru_eez)

library(terra)
library(ncdf4)

# =============================================================================
# SECTION 1: LOAD PORTS
# =============================================================================
ports_df <- import(file.path(base_path, "data/peru_fishing_ports_catch_coords_2017_2021.csv")) %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  distinct(puerto_clean, longitude, latitude, .keep_all = TRUE)

ports_sf <- st_as_sf(ports_df, coords = c("longitude", "latitude"),
                     crs = 4326, remove = FALSE)

# =============================================================================
# SECTION 2: BUILD 0.1° GRID AND ASSIGN EACH CELL TO NEAREST PORT
# (stable lookup — only needs to run once per study area)
# =============================================================================
grid_polys <- st_make_grid(Peru_eez_buffered, cellsize = GRID_SIZE_FINE, square = TRUE)

grid_sf <- st_sf(grid_id = seq_along(grid_polys), geometry = grid_polys) %>%
  st_intersection(Peru_eez_buffered)

grid_centroids_3857 <- st_transform(st_centroid(grid_sf), 3857)
ports_3857          <- st_transform(ports_sf[, "puerto_clean"], 3857)
nearest_idx         <- st_nearest_feature(grid_centroids_3857, ports_3857)

grid_port_lookup <- grid_sf %>%
  mutate(
    puerto_clean = ports_sf$puerto_clean[nearest_idx],
    dist_km = as.numeric(
      st_distance(grid_centroids_3857, ports_3857[nearest_idx, ], by_element = TRUE)
    ) / 1000
  )

# Note: no distance cap applied — all grid cells within the EEZ buffer
# are assigned to their nearest port. The max detection distance is ~149km
# which is within reasonable range for all coastal ports.
message(sprintf("Grid cells assigned to ports: %d", nrow(grid_port_lookup)))

write.csv(st_drop_geometry(grid_port_lookup),
          file.path(paths$out_dir, "grid_port_lookup_0p1deg.csv"),
          row.names = FALSE)

# =============================================================================
# SECTION 3: LOAD VESSELS → assign to grid → inherit port
# =============================================================================
vessels_df <- import(file.path(base_path, "data/vessels_peru_eez_buffer.csv")) %>%
  mutate(date = as.Date(timestamp), det_id = row_number())

vessels_sf_pts <- vessels_to_sf(vessels_df)

# Use st_intersects instead of st_within so nearshore detections
# that fall on cell boundaries or just inside the coastal edge are retained
vessels_with_grid <- st_join(
  vessels_sf_pts,
  grid_sf[, "grid_id"],
  join = st_intersects,
  left = FALSE
) %>%
  left_join(
    st_drop_geometry(grid_port_lookup) %>% select(grid_id, puerto_clean, dist_km),
    by = "grid_id"
  ) %>%
  st_drop_geometry()

message(sprintf("Detections assigned to capped grid: %d of %d (%.1f%%)",
                nrow(vessels_with_grid),
                nrow(vessels_df),
                nrow(vessels_with_grid) / nrow(vessels_df) * 100))

# =============================================================================
# SECTION 4: MODULAR MET EXTRACTION FUNCTION
#
# extract_netcdf_var(vessels_df, path, var_pattern, new_col, offset, scale)
#
#   vessels_df  : the detection-level data frame (needs det_id, date, lon, lat)
#   path        : file path to the netCDF (from paths$met list in 00_setup.R)
#   var_pattern : regex matching the layer name in terra (e.g. "adjusted_sea")
#   new_col     : name for the extracted column in the output
#   offset      : additive unit conversion applied after scale (e.g. -273.15 K→°C)
#   scale       : multiplicative unit conversion (default 1)
#
# Returns a tibble: det_id | <new_col>
# One row per detection, matched to the raster layer whose date = detection date.
# =============================================================================
extract_netcdf_var <- function(vessels_df, path, var_pattern,
                               new_col, offset = 0, scale = 1) {
  message("Extracting: ", new_col, " from ", basename(path))
  
  rast_all <- rast(path)
  rast_var <- rast_all[[grep(var_pattern, names(rast_all))]]
  
  n_layers <- nlyr(rast_var)
  
  # This function assumes day-of-year climatology: exactly 365 layers,
  # one per calendar day, with the time index labelled as a single reference
  # year (e.g. 2021 for all layers). Detection dates are matched by day-of-year
  # (1–365), not by calendar year, so the function works correctly across
  # the full 2017–2021 study period.
  #
  # If a new netCDF uses calendar-date indexing (i.e. one layer per actual
  # date rather than per day-of-year), this function will silently mismatch
  # detections to the wrong layers. In that case, use extract_era5_var()
  # instead, which handles calendar-date indexed rasters correctly.
  if (n_layers != 365) {
    warning(
      "extract_netcdf_var() expected 365 layers (day-of-year climatology) ",
      "but got ", n_layers, " layers in ", basename(path), ".\n",
      "If this file uses calendar-date indexing (one layer per date), ",
      "use extract_era5_var() instead to avoid silent mismatches."
    )
  }
  
  # Extract all layer values at each vessel location, then pick the layer
  # matching that detection's day-of-year (1–365).
  v_spv   <- terra::vect(
    st_as_sf(vessels_df, coords = c("lon", "lat"), crs = 4326)
  )
  val_mat <- terra::extract(rast_var, v_spv)  # nrow = nrow(vessels_df), ncol = 1 + n_layers
  
  # Apply unit conversion
  val_mat[ , -1] <- val_mat[ , -1] * scale + offset
  
  # For each detection pick the value from the layer = day-of-year
  doy <- pmin(as.integer(format(vessels_df$date, "%j")), 365)  # cap at 365 for leap years
  
  extracted_vals <- vapply(
    seq_len(nrow(vessels_df)),
    function(i) val_mat[i, doy[i] + 1],  # +1 because col 1 is ID
    numeric(1)
  )
  
  tibble(
    det_id        = vessels_df$det_id,
    !!new_col    := extracted_vals
  )
}

# =============================================================================
# SECTION 5: EXTRACT ALL MET VARIABLES
#
# Each block extracts one variable → det_id-keyed tibble.
# Uncomment each block as you acquire the data and add its path to 00_setup.R.
# =============================================================================

# --- SST (IFREMER netCDF, Kelvin → Celsius) ---
sst_vals <- extract_netcdf_var(
  vessels_df  = vessels_with_grid,
  path        = paths$met$sst,
  var_pattern = "adjusted_sea_surface_temperature",
  new_col     = "sst_c",
  offset      = -273.15
)

# --- Chlorophyll-a (Copernicus CMEMS, monthly, Feb 2017–Dec 2021) ---
# Monthly resolution: match each detection to its year-month layer.
# Variable name pattern: "chl_depth=0.49402538"
chla_rast       <- rast(paths$met$chla)
chla_rast       <- chla_rast[[grep("^CHL_", names(chla_rast))]]
chla_times      <- as.Date(terra::time(chla_rast))
chla_month_keys <- format(chla_times, "%Y-%m")   # e.g. "2017-02"

# Extract all layer values at each vessel location in one pass
chla_vect  <- terra::vect(
  st_as_sf(vessels_with_grid, coords = c("lon", "lat"), crs = 4326)
)
chla_mat   <- terra::extract(chla_rast, chla_vect)  # nrow = ndetections, ncol = 1 + nlyr

# For each detection find the layer index matching its year-month
det_month_keys <- format(vessels_with_grid$date, "%Y-%m")
layer_idx      <- match(det_month_keys, chla_month_keys)  # NA if month not in file

chla_vals <- tibble(
  det_id      = vessels_with_grid$det_id,
  chla_mg_m3  = vapply(
    seq_len(nrow(vessels_with_grid)),
    function(i) {
      li <- layer_idx[i]
      if (is.na(li)) return(NA_real_)
      chla_mat[i, li + 1]   # +1 because col 1 is ID
    },
    numeric(1)
  )
)
message("Chl-a extracted. NAs (outside file coverage): ",
        sum(is.na(chla_vals$chla_mg_m3)))

# --- ERA5 helper: extract monthly variable using Unix timestamp layer names ---
# ERA5 from CDS embeds valid_time as Unix seconds in layer names (e.g. "t2m_valid_time=1483228800")
# rather than a proper time dimension, so we parse timestamps from names directly.
extract_era5_var <- function(vessels_df, path, var_prefix, new_col, offset = 0, scale = 1) {
  message("Extracting ERA5: ", new_col, " from ", basename(path))
  
  rast_all  <- rast(path)
  rast_var  <- rast_all[[grep(paste0("^", var_prefix, "_valid_time="), names(rast_all))]]
  
  # Parse Unix timestamps from layer names → convert to year-month key
  unix_times    <- as.numeric(sub(paste0("^", var_prefix, "_valid_time="), "", names(rast_var)))
  layer_dates   <- as.Date(as.POSIXct(unix_times, origin = "1970-01-01", tz = "UTC"))
  layer_keys    <- format(layer_dates, "%Y-%m")
  
  # Apply unit conversion
  rast_var <- rast_var * scale + offset
  
  # Extract all layer values at each vessel location in one pass
  v_spv   <- terra::vect(
    st_as_sf(vessels_df, coords = c("lon", "lat"), crs = 4326)
  )
  val_mat <- terra::extract(rast_var, v_spv)
  
  # Match each detection to its year-month layer
  det_keys  <- format(vessels_df$date, "%Y-%m")
  layer_idx <- match(det_keys, layer_keys)
  
  tibble(
    det_id     = vessels_df$det_id,
    !!new_col := vapply(
      seq_len(nrow(vessels_df)),
      function(i) {
        li <- layer_idx[i]
        if (is.na(li)) return(NA_real_)
        val_mat[i, li + 1]
      },
      numeric(1)
    )
  )
}

# --- Air temperature (t2m, Kelvin → Celsius) ---
airtemp_vals <- extract_era5_var(
  vessels_df  = vessels_with_grid,
  path        = paths$met$era5_temp,
  var_prefix  = "t2m",
  new_col     = "air_temp_c",
  offset      = -273.15
)

# --- Total precipitation (tp, metres) ---
precip_vals <- extract_era5_var(
  vessels_df  = vessels_with_grid,
  path        = paths$met$era5_precip,
  var_prefix  = "tp",
  new_col     = "precip_m"
)

# --- El Niño / La Niña: MEI v2 index (NOAA wide format) ---
# Rows = years, columns = 12 months. First line is year range header — skip it.
enso_raw <- read.table(paths$met$enso, skip = 1, fill = TRUE,
                       col.names = c("year", paste0("m", 1:12)))

enso <- enso_raw %>%
  filter(year >= min(STUDY_YEARS), year <= max(STUDY_YEARS)) %>%
  pivot_longer(cols = starts_with("m"),
               names_to  = "month_num",
               values_to = "mei_index") %>%
  mutate(
    month_num   = as.integer(sub("m", "", month_num)),
    month_floor = as.Date(paste(year, month_num, "01", sep = "-"))
  ) %>%
  select(month_floor, mei_index)

# =============================================================================
# SECTION 6: MERGE ALL MET VARIABLES ONTO DETECTION-LEVEL TABLE
# Add a left_join() for each new variable extracted above.
# =============================================================================
vessels_merged <- vessels_with_grid %>%
  left_join(sst_vals,     by = "det_id") %>%
  left_join(chla_vals,    by = "det_id") %>%
  left_join(airtemp_vals, by = "det_id") %>%
  left_join(precip_vals,  by = "det_id") %>%
  mutate(month_floor = floor_date(date, "month")) %>%
  left_join(enso, by = "month_floor")

# =============================================================================
# SECTION 7: ADD TIME BINS
# =============================================================================
vessels_merged <- vessels_merged %>%
  mutate(
    week_floor   = floor_date(date, unit = "week",  week_start = 1),
    month_period = floor_date(date, unit = "month"),
    biweek_floor = as.Date("1970-01-05") +
      14 * ((as.integer(week_floor - as.Date("1970-01-05"))) %/% 14)
  )

# =============================================================================
# SECTION 8: AGGREGATE TO GRID × TIME PANEL
#
# When you add a new met variable:
#   1. Add a mean() line in the summarise() block below
#   2. Use first() instead of mean() for period-level indices like ENSO
# =============================================================================
aggregate_panel <- function(df, period_col) {
  period_col <- rlang::ensym(period_col)
  
  df %>%
    group_by(grid_id, puerto_clean, period = !!period_col, matched_category) %>%
    summarise(
      detections = n(),
      active     = if_else(matched_category == "unmatched",
                           detections, n_distinct(mmsi)),
      # --- met averages: add new variables below this line ---
      sst_c_mean      = mean(sst_c,     na.rm = TRUE),
      chla_mean      = mean(chla_mg_m3, na.rm = TRUE),
      air_temp_mean  = mean(air_temp_c, na.rm = TRUE),
      precip_mean    = mean(precip_m,   na.rm = TRUE),
      mei_index      = first(mei_index),    # period-level index, not averaged
      .groups = "drop"
    )
}

grid_weekly   <- aggregate_panel(vessels_merged, week_floor)
grid_biweekly <- aggregate_panel(vessels_merged, biweek_floor)
grid_monthly  <- aggregate_panel(vessels_merged, month_period)

# =============================================================================
# SECTION 9: JOIN PORT CATCH DATA
# Catch is annual, so join at the year level across all time resolutions.
# =============================================================================
# Catch is now monthly — join on puerto_clean + month_floor
catch <- import(file.path(base_path, "data/peru_fishing_ports_catch_coords_2017_2021.csv")) %>%
  select(puerto_clean, month_floor, total_catch_TM) %>%
  mutate(month_floor = as.Date(month_floor))

add_catch <- function(panel_df) {
  panel_df %>%
    mutate(month_floor = floor_date(period, "month")) %>%
    left_join(catch, by = c("puerto_clean", "month_floor")) %>%
    select(-month_floor)
}

grid_weekly   <- add_catch(grid_weekly)
grid_biweekly <- add_catch(grid_biweekly)
grid_monthly  <- add_catch(grid_monthly)

# =============================================================================
# SECTION 10: EXPORT
# =============================================================================
write.csv(vessels_merged,
          file.path(paths$out_dir, "vessels_grid_port_met_pointlevel.csv"), row.names = FALSE)
write.csv(grid_weekly,
          file.path(paths$out_dir, "grid_weekly_panel.csv"),   row.names = FALSE)
write.csv(grid_biweekly,
          file.path(paths$out_dir, "grid_biweekly_panel.csv"), row.names = FALSE)
write.csv(grid_monthly,
          file.path(paths$out_dir, "grid_monthly_panel.csv"),  row.names = FALSE)

message("Panel dataset complete. Outputs written to: ", paths$out_dir)


# =============================================================================
# SECTION 10B: 100KM COASTAL SUBSAMPLE PANEL
#
# Restricts the panel to grid cells within 100km of the Peru coastline.
# This focuses on nearshore fishing grounds and excludes deep offshore cells.
# Used as a robustness check in the regression.
# =============================================================================

# Build 100km coastal buffer from land boundary
peru_land_06 <- load_peru_land()
coast_buf_100km <- st_transform(peru_land_06, 3857) %>%
  st_buffer(100000) %>%
  st_transform(4326)

# Find grid cells whose centroids fall within 100km of coast
grid_centroids_4326 <- st_transform(grid_centroids_3857, 4326)
coastal_cell_idx    <- st_within(grid_centroids_4326, coast_buf_100km, sparse = FALSE)[, 1]
coastal_grid_ids    <- grid_sf$grid_id[coastal_cell_idx]

message(sprintf("Grid cells within 100km of coast: %d of %d (%.1f%%)",
                sum(coastal_cell_idx), nrow(grid_sf),
                mean(coastal_cell_idx) * 100))

# Filter all panels to coastal cells only
grid_monthly_coastal   <- grid_monthly   %>% filter(grid_id %in% coastal_grid_ids)
grid_weekly_coastal    <- grid_weekly    %>% filter(grid_id %in% coastal_grid_ids)
grid_biweekly_coastal  <- grid_biweekly %>% filter(grid_id %in% coastal_grid_ids)

message(sprintf("Monthly panel rows — full: %d | coastal: %d (%.1f%%)",
                nrow(grid_monthly), nrow(grid_monthly_coastal),
                nrow(grid_monthly_coastal) / nrow(grid_monthly) * 100))

# Export coastal panels
write.csv(grid_monthly_coastal,
          file.path(paths$out_dir, "grid_monthly_panel_coastal100km.csv"),  row.names = FALSE)
write.csv(grid_weekly_coastal,
          file.path(paths$out_dir, "grid_weekly_panel_coastal100km.csv"),   row.names = FALSE)
write.csv(grid_biweekly_coastal,
          file.path(paths$out_dir, "grid_biweekly_panel_coastal100km.csv"), row.names = FALSE)

message("Coastal subsample panels saved.")

# =============================================================================
# SECTION 11: QUICK DIAGNOSTIC PLOT
# =============================================================================
ggplot(grid_weekly, aes(x = period, y = detections, color = matched_category)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  scale_color_manual(values = vessel_colors, name = "Vessel category") +
  labs(
    title = "Average weekly detections across all grid cells (0.1° grid)",
    x = "Week", y = "Mean detections per grid"
  ) +
  theme_minimal()