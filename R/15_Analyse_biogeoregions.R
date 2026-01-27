library(officer)
library(flextable)

# ----------------------------------------------------
# 1) Align CRS and intersect
# ----------------------------------------------------

biogeo <- st_transform(biogeoregions, 4326)
locs <- st_transform(locations_2025, 4326)

# spatial join: attach region attributes to locations
locs_join <- st_join(
  locs,
  biogeo %>%
    dplyr::select(
      name,
      short_name,
      code
    ), # region attrs only
  left = TRUE
)

# ----------------------------------------------------
# 2) Summary statistics
# ----------------------------------------------------

# basic summary: number of sites per biogeographic region
region_summary <- locs_join %>%
  st_set_geometry(NULL) %>%
  group_by(name) %>%
  summarise(
    n_sites = n(),
    n_grassland = sum(site_type == "grassland", na.rm = TRUE),
    n_forest = sum(site_type == "forest", na.rm = TRUE),
    n_wetland = sum(site_type == "wetland", na.rm = TRUE),
    n_other = sum(site_type == "other", na.rm = TRUE)
  ) %>%
  arrange(desc(n_sites))

# site-type proportions (optional but useful)
site_type_summary <- locs_join %>%
  st_set_geometry(NULL) %>%
  count(name, site_type) %>%
  group_by(name) %>%
  mutate(percent = n / sum(n) * 100)

# partner coverage per region (useful small analysis)
partner_summary <- locs_join %>%
  st_set_geometry(NULL) %>%
  group_by(name) %>%
  summarise(
    n_partners = n_distinct(acronym),
    partners   = paste(sort(unique(acronym)), collapse = ", ")
  )

# ----------------------------------------------------
# 3) Save CSVs
# ----------------------------------------------------

dir.create("Outputs", showWarnings = FALSE)

readr::write_csv(region_summary, "Outputs/Results/sites_per_region.csv")
readr::write_csv(site_type_summary, "Outputs/Results/site_types_per_region.csv")
readr::write_csv(partner_summary, "Outputs/Results/partners_per_region.csv")

# ----------------------------------------------------
# 4) DOCX export
# ----------------------------------------------------

doc <- read_docx()

doc <- doc %>%
  body_add_par("Site Summary by Biogeographic Region", style = "heading 1")

doc <- doc %>%
  body_add_par("Sites per region", style = "heading 2") %>%
  body_add_flextable(qflextable(region_summary))

doc <- doc %>%
  body_add_par("Site-type distribution", style = "heading 2") %>%
  body_add_flextable(qflextable(site_type_summary))

doc <- doc %>%
  body_add_par("Partners per region", style = "heading 2") %>%
  body_add_flextable(qflextable(partner_summary))

print(doc, target = "Outputs/Results/locations_summary.docx")
