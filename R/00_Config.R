#----------------------------------------------------------#
# Load packages -----
#----------------------------------------------------------#
packages <- c(
  "flextable",
  "officer",
  "tidyverse",
  "broom",
  "janitor",
  "sf", 
  "dendextend",
  "sp", 
  "proj4", 
  "openxlsx",
  "fuzzyjoin", 
  "remotes",
  "ggtext",
  "vegan",
  "mgcv",
  "googledrive",
  "ggplot2",
  "ggdendro",
  "ggrepel",
  "ggforce",
  "ggspatial",
  "rnaturalearth",
  "rnaturalearthdata",
  "MASS",
  "DHARMa",
  "patchwork",
  "sjPlot",
  "report"
)

# Standard package
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

#----------------------------------------------------------#
# Load data -----
#----------------------------------------------------------#
#--------------------------------------------------#
## Site locations -----
#--------------------------------------------------#
locations <- readr::read_csv(
  "Data/Inputs/locations_20251105.csv",
)  %>%
  dplyr::mutate(
    type_code = str_sub(code, 1, 1),
    site_type = dplyr::case_when(
      type_code == "G" ~ "grassland",
      type_code == "F" ~ "forest",
      type_code == "W" ~ "wetland",
      type_code == "O" ~ "other"
    )
  ) %>%
  sf::st_as_sf(
    coords = c(
      "LON_4326", 
      "LAT_4326"
    ), 
    crs = 4326)

deploy_info <- readr::read_csv(
  "Data/Inputs/deployment.csv",
) %>%
  dplyr::rename(
    partner = `Partner acronym`,
    code = `Deployment code...3`,
    device_code = `Deployment code...4`,
    device_id = `Device ID`,
    deployment_date = `Device deployment date (double-click cell to bring up calendar)`,
    retrieval_date = `Device retrieval date (double-click cell to bring up calendar)`
  ) %>%
  dplyr::mutate(
    type_code = str_sub(code, 1, 1),
    site_type = dplyr::case_when(
      type_code == "G" ~ "grassland",
      type_code == "F" ~ "forest",
      type_code == "W" ~ "wetland",
      type_code == "O" ~ "other"
    )
  ) 

locations_2025 <-
  locations %>%
  dplyr::filter(year == 2025)

locations_2024 <-
  locations %>%
  dplyr::filter(year == 2024)

locations_comb <- locations_2025 %>%
  # Define unique key for matching
  dplyr::mutate(key = paste0(partner_folder, "_", code)) %>%
  # Bind 2024 data but keep only those not present in 2025
  dplyr::bind_rows(
    locations_2024 %>%
      dplyr::mutate(key = paste0(partner_folder, "_", code)) %>%
      dplyr::filter(!key %in% unique(paste0(locations_2025$partner_folder, "_", locations_2025$code)))
  ) %>%
  dplyr::select(-key)

locations_bbox <-
  locations_2025 %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%        
  sf::st_buffer(125000) %>%
  sf::st_bbox()

target_bbox <-
  locations_2025 %>%
  sf::st_bbox() %>%
  sf::st_as_sfc() %>%        
  sf::st_buffer(1500000) %>%
  sf::st_bbox()

# Europe polygons
europe <- rnaturalearth::ne_countries(
  continent = "Europe", 
  scale = "medium",   
  returnclass = "sf"
  ) %>%
  dplyr::filter(
    continent == "Europe"
  )

# --- Load all countries globally, not just Europe ---
world <- ne_countries(scale = "medium", returnclass = "sf")

# --- Select European + adjacent regions (Turkey + Caucasus) ---
countries_europe_extended <- c(
  "Ireland", "Spain", "Portugal", "France", "United Kingdom", "Belgium", "Netherlands",
  "Luxembourg", "Germany", "Switzerland", "Austria", "Czechia", "Poland", "Slovakia",
  "Hungary", "Slovenia", "Croatia", "Bosnia and Herzegovina", "Serbia", "Montenegro",
  "Kosovo", "North Macedonia", "Albania", "Greece", "Bulgaria", "Romania", "Moldova",
  "Ukraine", "Belarus", "Lithuania", "Latvia", "Estonia", "Norway", "Sweden", "Finland",
  "Denmark", "Iceland", "Italy", "Malta", "Turkey", "Georgia", "Armenia", "Azerbaijan",
  "Russia", "Morocco", "Algeria", "Tunisia", "Libya", "Egypt", "Republic of Serbia"
)

europe_ext <- world %>% 
  filter(admin %in% countries_europe_extended)

# Major lakes and rivers for context
lakes  <- ne_download(scale = "large", type = "lakes", category = "physical", returnclass = "sf")
rivers <- ne_download(scale = "large", type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")

# Major cities (optional context)
cities <- ne_download(scale = 10, type = "populated_places", category = "cultural", returnclass = "sf") %>%
  st_transform(crs = 4326) %>%
  filter(POP_MAX > 1000000)   # label only big cities

# Extract coordinates for city labels
cities_coords <- cities %>%
  dplyr::mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  )

crs_europe <- 3035  # Lambert Azimuthal Equal Area
europe  <- st_transform(europe, crs_europe)
europe_ext  <- st_transform(europe_ext, crs_europe)
lakes   <- st_transform(lakes, crs_europe)
rivers  <- st_transform(rivers, crs_europe)
cities  <- st_transform(cities, crs_europe)
locations_2025   <- st_transform(locations_2025, crs_europe)
locations_2024   <- st_transform(locations_2024, crs_europe)
locations_comb   <- st_transform(locations_comb, crs_europe)
locations_bbox <- st_transform(locations_bbox, crs_europe)
europe_bbox <- st_bbox(europe_ext)  # crs already 3035
target_bbox <- st_transform(target_bbox, crs_europe)

target_countries <- c("Ireland", "Spain", "Croatia", "Bulgaria", "Czechia", 
                      "Finland", "Slovakia", "Netherlands", "Italy", "Belgium",
                      "Denmark", "Sweden")

# Okabe-Ito colourblind-friendly palette (works for up to 8 categories)
okabe_ito <- setNames(
  c("#009E73", "#E69F00","#0072B2", "#D55E00"),
  c("F", "G", "W", "O")
)

## Red Lists ----
red_list <- 
  readr::read_csv(
    "Data/Inputs/European_Red_List_2024_December_fixed.csv"
  )

## Load acoustic data ----
# Extract the file ID from your Google Drive link:
# URL: https://drive.google.com/file/d/1fSjoRQjQ9Ub03eSpKgcLiMZ5A1y9wwaO/view?usp=drive_link
# ID:                                 1fSjoRQjQ9Ub03eSpKgcLiMZ5A1y9wwaO
gdrive_file_id <- "1fSjoRQjQ9Ub03eSpKgcLiMZ5A1y9wwaO"

# Create a temporary file path to store the downloaded zip
# This file will be automatically cleaned up
temp_zip_path <- tempfile(fileext = ".zip")

# ----------------------------------------------------------------- #
# 1. Download the file from Google Drive ----
# ----------------------------------------------------------------- #

# Tell googledrive not to use a logged-in user.
# This is best practice for public files.
drive_deauth()

message("Downloading file from Google Drive...")
drive_download(
  file = as_id(gdrive_file_id), # Use as_id() to specify it's an ID
  path = temp_zip_path,
  overwrite = TRUE
)
message("Download complete: ", temp_zip_path)

# ----------------------------------------------------------------- #
# 2. Find the CSV file *inside* the zip ----
# ----------------------------------------------------------------- #

# Get a list of all files inside the zip archive
files_in_zip <- unzip(temp_zip_path, list = TRUE)

# Find the name of the file that ends in .csv (case-insensitive)
csv_file_name <- grep("\\.csv$", files_in_zip$Name, value = TRUE, ignore.case = TRUE)

# Error handling: Stop if no CSV is found or if multiple are found
if (length(csv_file_name) == 0) {
  stop("Error: No .csv file was found inside the downloaded zip archive.")
} else if (length(csv_file_name) > 1) {
  message("Warning: Multiple .csv files found. Reading the first one: ", csv_file_name[1])
  csv_file_name <- csv_file_name[1] # Select the first one
} else {
  message("Found CSV file in zip: ", csv_file_name)
}

# ----------------------------------------------------------------- #
# 3. Read the CSV directly from the zip ----
# ----------------------------------------------------------------- #

# Use the unz() function to create a connection to the file
# *without* fully extracting it to disk.
# readr::read_csv() can read directly from this connection.
message("Reading CSV data...")
acoustic_data_raw <- 
  readr::read_csv(
    unz(temp_zip_path, csv_file_name)
    ) 
message("Successfully read data.")

acoustic_data <- 
  acoustic_data_raw %>%
  dplyr::mutate(
    species_name = stringr::str_extract(species, "^[^_]+"),
    date_raw = str_extract(filename, "(?<=_)\\d{8}(?=_)"),
    time_raw = str_extract(filename, "(?<=_)\\d{6}(?=\\.wav)"),
    date = ymd(date_raw),
    time = hms(
      str_c(
        substr(time_raw, 1, 2), ":",
        substr(time_raw, 3, 4), ":",
        substr(time_raw, 5, 6)
        )
      ),
    datetime = ymd_hms(str_c(date_raw, time_raw))
  ) %>%
  dplyr::select(-date_raw, -time_raw) %>%
  dplyr::left_join(
    .,
    red_list %>%
      dplyr::select(
        scientificName,
        europeanRegionalRedListCategory
      ),
    by = c("species_name" = "scientificName")
  ) 

# ----------------------------------------------------------------- #
# 4. Success! ----
# ----------------------------------------------------------------- #

# Clean up the temporary zip file
file.remove(temp_zip_path)

# Show the first few rows of your new data frame
print(head(acoustic_data))

# Your data is now in the 'data' data frame

