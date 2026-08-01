Peru IUU Fishing Pipeline
Research project: Effect of dark vessel (IUU fishing) activity on legitimate catch landings in Peru's EEZ, 2017–2021

Overview
This pipeline processes satellite vessel detection data (SAR/AIS) from Peru's Exclusive Economic Zone to construct a spatial panel dataset linking dark vessel activity to official catch landing records. The core research question is whether illegal, unreported, and unregulated (IUU) fishing — proxied by unmatched "dark" vessel detections — depresses legitimate catch volumes at Peruvian fishing ports.
The pipeline runs in numbered order from raw data ingestion through regression output. All file paths are centralized in 00_setup.R — change the base_path variable once and it propagates everywhere.

Repository Structure
PeruFishing/
├── 00_setup.R                           # Shared libraries, paths, constants, helpers
├── 01_database.R                        # [legacy] Initial vessel filter
├── 01_database-Aaron.R                  # Load raw vessels, filter to EEZ + buffer, sanity checks
├── 01_1_produce_data_extraction.R       # Supplementary data extraction utility
├── 01-1_100km_buffer.R                  # Coastal 100km subsample utility
├── 02_EEZ_buffer_vessel_filter.R        # [legacy] EEZ buffer + port join
├── 02_port_geocode.R                    # Geocode fishing ports from PESCA landings data
├── 03_eda_charts_map.R                  # Exploratory data analysis: charts and maps
├── 03_port_integration_mapping.R        # Port integration and mapping
├── 04_grid_level_database.R             # Grid-level vessel summaries and time series
├── 04_monthly_maps.R                    # Monthly vessel activity maps
├── 05_map_with_ports.R                  # Maps overlaying vessel detections and ports
├── 05_monthly_maps.R                    # Monthly maps (updated version)
├── 06_grid_port_panel_construction.R    # Core panel builder: grid x time + met + catch
├── 07_regression.R                      # Fixed effects regression models and output tables
│
├── data/                                # Raw inputs (never modified) and intermediates
│   ├── industrial_vessels_v20240102_MASTER_COPY.csv
│   ├── geometry.geojson                 # Peru EEZ boundary
│   ├── PESCA_TB_source_tableropesquero.xlsx
│   ├── peru_eez_sst_data.nc
│   ├── peru_eez_chlorophylla_2017-2021data.nc
│   ├── era5_t2m_d2m_2017-2021.nc
│   ├── era5_tp_2017-2021.nc
│   ├── meiv2_ENSO.data
│   └── [intermediate CSVs written by pipeline steps]
│
└── outputs/                             # All pipeline outputs
    ├── grid_port_lookup_0p1deg.csv
    ├── vessels_grid_port_met_pointlevel.csv
    ├── grid_weekly_panel.csv
    ├── grid_biweekly_panel.csv
    ├── grid_monthly_panel.csv
    ├── grid_monthly_panel_coastal100km.csv
    ├── grid_weekly_panel_coastal100km.csv
    ├── grid_biweekly_panel_coastal100km.csv
    ├── regression_sample.csv
    ├── regression_results.html
    └── regression_results_annual.html


Pipeline Steps
00_setup.R — Shared Configuration
Source this at the top of every script. Defines all file paths via a single base_path variable, study period constants (STUDY_YEARS = 2017:2021, EEZ_BUFFER_KM = 20, grid sizes 0.5° and 0.1°), a consistent vessel color palette used across all maps, and reusable spatial helper functions: load_eez(), make_eez_buffer(), load_peru_land(), vessels_to_sf(), add_vessel_type(), add_time_cols().
To move the project to a new machine: change only base_path in this file.

01_database-Aaron.R — Vessel Filter
Input: industrial_vessels_v20240102_MASTER_COPY.csv, geometry.geojson
 Output: data/vessels_peru_eez_buffer.csv
Loads the full raw vessel detection dataset, applies a 20 km buffer around the Peru EEZ, and uses st_intersection() to retain only detections within the buffered zone. Tags each detection as AIS (matched, MMSI ≥ 100,000,000) or Dark (unmatched or missing MMSI). Produces a console summary of detection counts by category and year, plus two sanity check maps: all years combined and January 2017 alone.
01_database.R is the legacy version of this script. Use 01_database-Aaron.R for all current work — it sources 00_setup.R rather than repeating boilerplate.

02_port_geocode.R — Port Geocoding
Input: PESCA_TB_source_tableropesquero.xlsx (sheet 2), geometry.geojson
 Output: data/peru_fishing_ports_catch_coords_2017_2021.csv
Extracts unique maritime port names from Peru's official PESCA catch database, filters to AMBITO == MARITIMO to exclude inland ports, and geocodes each port via the OpenStreetMap Nominatim API (tidygeocoder). Spatially filters to ports within 50 km of the EEZ to exclude geocoding errors. Joins coordinates back to catch records and summarizes total catch (metric tons) by port × year × month. Includes a console summary of geocoding success rates and a sanity check map overlaying vessel detections and geocoded ports.

04_grid_level_database.R — Grid Summaries and EDA
Input: vessels_peru_eez_buffer.csv
 Output: Console tables and diagnostic plots
Builds a 0.5° × 0.5° gridded summary of vessel detections by year, distinguishing AIS from dark vessels. Produces four diagnostic visualizations: annual vessel counts by category (grouped bar chart), dark vessel detections faceted by year (maps), average monthly vessel activity stacked by category, and a monthly time series 2017–2021.
This script assumes Peru_eez and vessels_peru are already in the environment. Source 00_setup.R and run 01_database-Aaron.R first, or load those objects explicitly before running.

06_grid_port_panel_construction.R — Core Panel Builder
Input: vessels_peru_eez_buffer.csv, peru_fishing_ports_catch_coords_2017_2021.csv, met netCDF files
 Output: grid_port_lookup_0p1deg.csv, vessels_grid_port_met_pointlevel.csv, grid_weekly_panel.csv, grid_biweekly_panel.csv, grid_monthly_panel.csv, plus coastal 100 km variants of each
This is the central script of the pipeline. It proceeds in ten sections:
1. Grid construction and port assignment. Builds a 0.1° grid over the EEZ + buffer and assigns each cell to its nearest fishing port using st_nearest_feature(). No distance cap is applied — all detections fall within ~149 km of a port. The grid-to-port lookup is saved to grid_port_lookup_0p1deg.csv and only needs to be rebuilt if ports or grid parameters change.
2. Vessel-to-grid assignment. Joins each vessel detection to a grid cell via st_join(st_intersects), then inherits the port assignment from the lookup table.
3. Meteorological variable extraction. A modular extract_netcdf_var() function extracts point-level met values at each detection location:
SST (IFREMER, Kelvin → Celsius) — matched by day-of-year climatology (365 layers)
Chlorophyll-a (Copernicus CMEMS, monthly) — matched by year-month
Air temperature and precipitation (ERA5, monthly) — matched by year-month via extract_era5_var()
MEI v2 ENSO index (NOAA) — wide-format text pivoted to long format by year-month
4. Panel aggregation. aggregate_panel() groups by grid × time period × vessel category and computes detection counts, active vessel counts (distinct MMSIs for AIS; detection count for dark), and met variable means. Produces weekly, biweekly, and monthly panels.
5. Catch join. Monthly port catch data (metric tons) joined to all panels by puerto_clean + month_floor.
6. Coastal subsample. Filters to grid cells within 100 km of the Peru coastline for use as a robustness check in regression.
Adding a new met variable (netCDF):
Add its path to paths$met in 00_setup.R
Call extract_netcdf_var() with the variable name and unit conversion
Add a left_join() in Section 6
Add a mean() line in aggregate_panel() in Section 8
Adding a new met variable (CSV index):
Load and format as a date-keyed tibble
Join to vessels_merged by month in Section 6

07_regression.R — Fixed Effects Regression
Input: outputs/grid_monthly_panel.csv
 Output: outputs/regression_results.html, outputs/regression_results_annual.html, outputs/regression_sample.csv
Estimates the effect of dark vessel pressure on legitimate catch landings using two-way fixed effects regression via fixest::feols().
Research question: Does illegal fishing activity (dark vessel detections) reduce official catch landings at Peruvian ports?
Model:
log(catch_TM) = β₁·log_dark + β₂·SST + β₃·Chla + β₄·air_temp + β₅·precip
                + port FE + time FE + ε

log_dark uses log1p() to handle port-months with zero dark vessel detections. Standard errors are clustered at the port level.
Monthly models (m1–m4):
Model
Specification
m1
Dark vessels only + port × month FE (baseline)
m2
+ Environmental controls (SST, Chla, air temp, precip)
m3
+ Legitimate fishing vessel count as control
m4
Robustness: dark vessel spatial footprint (grid cell count) instead of detection count

MEI ENSO index is excluded from monthly models due to collinearity with month fixed effects — monthly time FE absorbs all common time-varying shocks including ENSO.
Annual models (a1–a3): Aggregates to port × year level to reduce noise from sparse monthly dark vessel detections. MEI is included since year FE does not fully absorb annual ENSO variation. Output as HTML table via modelsummary.

Data Sources
Dataset
Source
Coverage
Vessel detections (SAR/AIS)
Global Fishing Watch / partner SAR imagery
2017–2021, Peru EEZ
Peru EEZ boundary
Marine Regions
—
Catch landings (PESCA)
PRODUCE / tablero pesquero
2017–2021, port × month
SST (IFREMER)
Copernicus Marine Service
Daily climatology
Chlorophyll-a
Copernicus CMEMS
Monthly, Feb 2017–Dec 2021
ERA5 air temperature + precip
ECMWF via Copernicus
Monthly, 2017–2021
MEI v2 ENSO index
NOAA PSL
1979–2026


Key Design Decisions
EEZ buffer. A 20 km buffer is applied around the Peru EEZ to capture nearshore activity just outside the formal zone boundary. All detections in the filtered dataset fall within ~149 km of a port, so no additional distance cap is applied for port assignment.
Vessel classification. AIS-matched vessels are classified by matched_category (fishing, non-fishing, unknown). Unmatched detections — those without a valid MMSI or with MMSI < 100,000,000 — are treated as dark/IUU vessels and constitute the primary treatment variable.
Grid resolution. A 0.5° grid is used for EDA visualization; a 0.1° grid is used for the regression panel to capture finer spatial variation in vessel activity near ports.
Port assignment. Nearest-port assignment without a distance cap. The lookup table grid_port_lookup_0p1deg.csv is stable and only needs to be rebuilt if ports or grid parameters change.
Catch aggregation. The annual panel divides monthly catch by 12 before summing to annual totals to avoid inflating figures from month-level duplication.
Coastal subsample. A 100 km coastal buffer panel is produced as a robustness check, focusing on nearshore fishing grounds where the relationship between local vessel activity and port-level catch is most direct.

Requirements
# Core
rio, dplyr, tidyr, data.table, ggplot2, sf, lubridate, ggrepel, rnaturalearth

# Spatial / raster
terra, ncdf4

# Geocoding
tidygeocoder

# Regression
fixest, modelsummary

Install all R packages:
install.packages(c("rio","dplyr","tidyr","data.table","ggplot2","sf","lubridate",
                   "ggrepel","rnaturalearth","terra","ncdf4","tidygeocoder",
                   "fixest","modelsummary"))


Running the Pipeline
Run scripts in numbered order. Each active script sources 00_setup.R at the top.
source("01_database-Aaron.R")       # Filter vessels to EEZ + buffer
source("02_port_geocode.R")         # Geocode fishing ports and summarize catch
source("03_eda_charts_map.R")       # [optional] EDA
source("04_grid_level_database.R")  # [optional] Grid-level summaries
source("06_grid_port_panel_construction.R")      # Build panel
source("07_regression.R")                        # Run regressions

Before first run: Open 00_setup.R and update base_path to your local directory.

Known Issues and Limitations
Legacy scripts. 01_database.R and 02_EEZ_buffer_vessel_filter.R are preserved for reference only. Their logic is consolidated into 01_database-Aaron.R and 06_grid_port_panel_construction.R. Do not use legacy scripts for new work.
Environment dependency in 04_grid_level_database.R. This script does not source 00_setup.R and assumes Peru_eez and vessels_peru are already loaded. It should be refactored to be self-contained.
365-layer assumption in extract_netcdf_var(). The function expects exactly 365 layers (day-of-year climatology). It will warn but not error if the count differs — inspect any warning before proceeding with a new SST file.
Sentinel-1B shutdown. The Sentinel-1B satellite went offline in August 2021, reducing scene coverage density in the second half of 2021. This affects temporal comparisons and should be noted in any analysis of 2021 trends.
MEI file format. The NOAA MEI v2 file uses a non-standard wide format with a header line that must be skipped (skip = 1). If NOAA updates the file format, the read step in 06_grid_port_panel_construction.R Section 5 may need adjustment.

