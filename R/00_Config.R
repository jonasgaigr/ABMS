#----------------------------------------------------------#
# Load packages -----
#----------------------------------------------------------#
packages <- c(
  "tidyverse",
  "broom",
  "janitor",
  "sf", 
  "sp", 
  "proj4", 
  "openxlsx",
  "fuzzyjoin", 
  "remotes",
  "ggtext",
  "vegan",
  "ggplot2",
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
locations <- readr::read_csv(
  "Data/Inputs/locations_20251104.csv",
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



# Europe polygons
europe <- rnaturalearth::ne_countries(
  continent = "Europe", 
  scale = "medium",   
  returnclass = "sf"
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
  "Russia", "Morocco", "Algeria", "Tunisia", "Libya", "Egypt"
)

europe_ext <- world %>% 
  filter(admin %in% countries_europe_extended)

# Major lakes and rivers for context
lakes  <- ne_download(scale = 10, type = "lakes", category = "physical", returnclass = "sf")
rivers <- ne_download(scale = 10, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")

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
sites   <- st_transform(locations_2025, crs_europe)

target_countries <- c("Ireland", "Spain", "Croatia", "Bulgaria", "Czechia", 
                      "Finland", "Slovakia", "Netherlands", "Italy", "Belgium",
                      "Denmark", "Sweden")

