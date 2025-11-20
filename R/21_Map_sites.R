# Map of Europe ----
## 2025 combined ----
# --- STEP 1: Spatial Clustering (Corrected for Units) ---

# 1. Transform to EPSG:3035 (Meters) to ensure 'h=10000' means 10km
#    We use this object for calculations to get accurate distances.
sites_metric <- st_transform(locations_2025, crs = 3035)

# 2. Calculate Distance Matrix on the METRIC data
dist_matrix <- st_distance(sites_metric)

# 3. Cluster: Group points within 10,000 METERS (10km)
hc <- hclust(as.dist(dist_matrix), method = "complete")
sites_metric$cluster_id <- cutree(hc, h = 10000) 

# --- STEP 2: Aggregation ---

# 1. Calculate Centroids for each cluster (in the metric system)
cluster_centroids <- sites_metric %>%
  group_by(cluster_id) %>%
  summarise(geometry = st_centroid(st_union(geometry))) %>%
  mutate(
    X_center = st_coordinates(geometry)[,1],
    Y_center = st_coordinates(geometry)[,2]
  ) %>%
  st_drop_geometry()

# 2. Count Habitats per cluster
cluster_counts <- sites_metric %>%
  st_drop_geometry() %>%
  group_by(cluster_id, type_code) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = type_code, values_from = count, values_fill = 0) %>%
  mutate(total_sites = rowSums(across(where(is.numeric))))

# 3. Join and Clean
final_plot_data <- left_join(cluster_counts, cluster_centroids, by = "cluster_id")

# --- STEP 3: Split Data for Plotting ---

# Define your habitat columns
habitat_cols <- c("F", "G", "W", "O")
# Safety check: ensure columns exist even if data is sparse
for(col in habitat_cols) {
  if(!col %in% names(final_plot_data)) final_plot_data[[col]] <- 0
}

# A) Solos: Clusters with exactly 1 site
data_solo <- final_plot_data %>% 
  filter(total_sites == 1) %>%
  # Determine which type this single site is (for coloring)
  mutate(main_type = colnames(.)[max.col(.[habitat_cols])])

# B) Overlaps: Clusters with >1 site
data_pie <- final_plot_data %>% 
  filter(total_sites > 1)

# --- STEP 4: The Plot ---

# Note: We are plotting using the METRIC coordinates (EPSG:3035).
# We must ensure the base map is also compatible or allowed to transform.
# ggplot handles the base map projection automatically if we use geom_sf.
map_europe_2025 <- 
  ggplot() +
  # --- Base Map ---
  geom_sf(data = europe_ext, fill = "gray98", color = "gray70", size = 0.3) +
  geom_sf(data = lakes, fill = "lightblue", color = NA, alpha = 0.4) +
  geom_sf(data = rivers, color = "lightblue", size = 0.2, alpha = 0.6) +
  
  # --- LAYER 1: Solo Clusters (Standard Points) ---
  geom_point(
    data = data_solo,
    aes(x = X_center, y = Y_center, fill = main_type),
    shape = 21,      
    color = NA,       # <--- Removes the white border
    size = 1.8,       # <--- Smaller size (previously 3.5)
    alpha = 0.9
  ) +
  
  # --- LAYER 2: Overlap Clusters (Pie Charts) ---
  geom_scatterpie(
    data = data_pie,
    aes(x = X_center, y = Y_center),
    cols = habitat_cols,
    pie_scale = 0.8,  # <--- Smaller scale (previously 1.5)
    color = NA,       # <--- Removes lines between slices
    alpha = 0.9
  ) +
  
  # --- Shared Scales & Theme ---
  scale_fill_manual(
    values = okabe_ito,
    breaks = c("F", "G", "W", "O"),
    labels = c("forest", "grassland", "wetland", "other"),
    name   = "Habitat Type"
  ) +
  
  # Ensure you are still using the Metric CRS from the previous step
  coord_sf(
    xlim = c(locations_bbox["xmin"], locations_bbox["xmax"]),
    ylim = c(locations_bbox["ymin"], locations_bbox["ymax"]),
    expand = FALSE
  ) +
  
  theme_minimal(base_family = "Roboto") +
  theme(
    panel.background  = element_rect(fill = "#EAF2F8", color = NA),
    panel.grid.major  = element_line(color = "white"),
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    # Legend anchored inside the map near the right edge
    legend.position   = c(1, 0.55),      # inside, vertical middle
    legend.justification = c(1, 0.5),      # right-middle of legend box aligns at x=0.95
    legend.background = element_rect(fill = alpha("white", 1), color = NA),
    legend.key.size   = unit(1.5, "lines"),  # ×2
    legend.text       = element_text(size = 20),  # ×2
    legend.title      = element_text(size = 22, face = "bold")  # ×2
  ) +
  
  guides(color = guide_legend(override.aes = list(size = 4.2, alpha = 1))) +
  
  labs(
    title = "Automated Biodiversity Monitoring Stations Across Europe – 2025",
    x = NULL,
    y = NULL
  )

map_europe_2025

### --- Save the European map ----
ggsave(
  filename = "Outputs/Maps/Map_Europe_Extended_2025_pie.png",
  plot = map_europe_2025,
  width = 12, height = 8, dpi = 300
)


## 2025 ----
map_europe_2025 <- 
  ggplot() +
  geom_sf(data = europe_ext, fill = "gray98", color = "gray70", size = 0.3) +
  geom_sf(data = lakes, fill = "lightblue", color = NA, alpha = 0.4) +
  geom_sf(data = rivers, color = "lightblue", size = 0.2, alpha = 0.6) +
  
  # Sites — increase point size 1.5×
  geom_sf(
    data = locations_2025,
    aes(color = type_code),
    size = 2.8 * 1.5,   # 2.8 × 1.5 ≈ 4.2
    alpha = 0.9
  ) +
  
  scale_color_manual(
    values = okabe_ito,
    breaks = c("F", "G", "W", "O"),
    labels = c("forest", "grassland", "wetland", "other"),
    name   = "Habitat Type"
  ) +
  
  coord_sf(
    xlim = c(locations_bbox["xmin"], locations_bbox["xmax"]),
    ylim = c(locations_bbox["ymin"], locations_bbox["ymax"]),
    expand = FALSE
  ) +
  
  theme_minimal(base_family = "Roboto") +
  theme(
    panel.background  = element_rect(fill = "#EAF2F8", color = NA),
    panel.grid.major  = element_line(color = "white"),
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    # Legend anchored inside the map near the right edge
    legend.position   = c(1, 0.55),      # inside, vertical middle
    legend.justification = c(1, 0.5),      # right-middle of legend box aligns at x=0.95
    legend.background = element_rect(fill = alpha("white", 1), color = NA),
    legend.key.size   = unit(1.5, "lines"),  # ×2
    legend.text       = element_text(size = 20),  # ×2
    legend.title      = element_text(size = 22, face = "bold")  # ×2
  ) +
  
  guides(color = guide_legend(override.aes = list(size = 4.2, alpha = 1))) +
  
  labs(
    title = "Automated Biodiversity Monitoring Stations Across Europe – 2025",
    x = NULL,
    y = NULL
  )

# --- Save the European map ---
ggsave(
  filename = "Outputs/Maps/Map_Europe_Extended_2025.png",
  plot = map_europe_2025,
  width = 12, height = 8, dpi = 300
)

## 2024 ----
map_europe_2024 <- 
  ggplot() +
  # Base map layers
  geom_sf(data = europe_ext, fill = "gray98", color = "gray70", size = 0.3) +
  geom_sf(data = lakes, fill = "lightblue", color = NA, alpha = 0.4) +
  geom_sf(data = rivers, color = "lightblue", size = 0.2, alpha = 0.6) +
  
  # Sites — increase point size 1.5×
  geom_sf(
    data = locations_2024,
    aes(color = type_code),
    size = 2.8 * 1.5,   # 2.8 × 1.5 ≈ 4.2
    alpha = 0.9
  ) +
  
  scale_color_manual(
    values = okabe_ito,
    breaks = c("F", "G", "W", "O"),
    labels = c("forest", "grassland", "wetland", "other"),
    name   = "Habitat Type"
  ) +
  
  coord_sf(
    xlim = c(locations_bbox["xmin"], locations_bbox["xmax"]),
    ylim = c(locations_bbox["ymin"], locations_bbox["ymax"]),
    expand = FALSE
  ) +
  
  theme_minimal(base_family = "Roboto") +
  theme(
    panel.background  = element_rect(fill = "#EAF2F8", color = NA),
    panel.grid.major  = element_line(color = "white"),
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    # Legend anchored inside the map near the right edge
    legend.position   = c(1, 0.55),      # inside, vertical middle
    legend.justification = c(1, 0.5),      # right-middle of legend box aligns at x=0.95
    legend.background = element_rect(fill = alpha("white", 1), color = NA),
    legend.key.size   = unit(1.5, "lines"),  # ×2
    legend.text       = element_text(size = 20),  # ×2
    legend.title      = element_text(size = 22, face = "bold")  # ×2
  ) +
  
  guides(color = guide_legend(override.aes = list(size = 4.2, alpha = 1))) +
  
  labs(
    title = "Automated Biodiversity Monitoring Stations Across Europe – 2024",
    x = NULL,
    y = NULL
  )

# --- Save the European map ---
ggsave(
  filename = "Outputs/Maps/Map_Europe_Extended_2024.png",
  plot = map_europe_2024,
  width = 12, height = 8, dpi = 300
)

## Combined ----
map_europe <- 
  ggplot() +
  # Base map layers
  geom_sf(data = europe_ext, fill = "gray98", color = "gray70", size = 0.3) +
  geom_sf(data = lakes, fill = "lightblue", color = NA, alpha = 0.4) +
  geom_sf(data = rivers, color = "lightblue", size = 0.2, alpha = 0.6) +
  
  # Sites — increase point size 1.5×
  geom_sf(
    data = locations_comb,
    aes(color = type_code),
    size = 2.8 * 1.5,   # 2.8 × 1.5 ≈ 4.2
    alpha = 0.9
  ) +
  
  scale_color_manual(
    values = okabe_ito,
    breaks = c("F", "G", "W", "O"),
    labels = c("forest", "grassland", "wetland", "other"),
    name   = "Habitat Type"
  ) +
  
  coord_sf(
    xlim = c(locations_bbox["xmin"], locations_bbox["xmax"]),
    ylim = c(locations_bbox["ymin"], locations_bbox["ymax"]),
    expand = FALSE
  ) +
  
  theme_minimal(base_family = "Roboto") +
  theme(
    panel.background  = element_rect(fill = "#EAF2F8", color = NA),
    panel.grid.major  = element_line(color = "white"),
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    # Legend anchored inside the map near the right edge
    legend.position   = c(1, 0.55),      # inside, vertical middle
    legend.justification = c(1, 0.5),      # right-middle of legend box aligns at x=0.95
    legend.background = element_rect(fill = alpha("white", 1), color = NA),
    legend.key.size   = unit(1.5, "lines"),  # ×2
    legend.text       = element_text(size = 20),  # ×2
    legend.title      = element_text(size = 22, face = "bold")  # ×2
  ) +
  
  guides(color = guide_legend(override.aes = list(size = 4.2, alpha = 1))) +
  
  labs(
    title = "Automated Biodiversity Monitoring Stations Across Europe – 2024-2025",
    x = NULL,
    y = NULL
  )

# --- Save the European map ---
ggsave(
  filename = "Outputs/Maps/Map_Europe_Extended.png",
  plot = map_europe,
  width = 12, height = 8, dpi = 300
)

# Country maps ----
# --- Output folder ---
dir.create("Outputs/Maps", showWarnings = FALSE, recursive = TRUE)

# --- Loop ---
# Compute Europe bbox once (already in crs_europe)
europe_bbox <- st_bbox(europe)

# Function to crop country to European mainland
filter_mainland <- function(shape, target_bbox) {
  if (st_crs(shape)$epsg != 3035) {
    shape <- st_transform(shape, 3035)
  }
  st_crop(shape, target_bbox)
}

for (country in target_countries) {
  
  # Country shape cropped to European mainland
  country_shape <- europe %>% 
    filter(admin == country) %>%
    #st_transform(crs_europe) %>%
    filter_mainland(target_bbox)
  
  countries_other <- europe %>% 
    filter(admin != country) %>%
    #st_transform(crs_europe) %>%
    filter_mainland(target_bbox)
  
  # Filter sites inside this country
  country_sites <- locations_comb %>%
    #st_transform(crs_europe) %>%
    st_intersection(st_buffer(country_shape, 5000))
  
  # Country bbox for zoom
  country_bbox <- st_bbox(st_buffer(country_shape, 25000))
  
  # Build map
  map_country <- ggplot() +
    geom_sf(data = countries_other, fill = "grey80", color = "grey60", alpha = 0.3, size = 0.15) +
    geom_sf(data = country_shape, fill = "grey95", color = "grey60", size = 0.2) +
    geom_sf(
      data = country_sites,
      aes(color = type_code),
      size = 2.8 * 1.5,  # same as Europe map
      alpha = 0.9
    ) +
    geom_sf(data = lakes %>% st_filter(., country_shape), fill = "lightblue", color = NA, alpha = 0.4) +
    geom_sf(data = rivers %>% st_filter(., country_shape), color = "lightblue", size = 0.2, alpha = 0.6) + 
    scale_color_manual(
      values = okabe_ito,
      breaks = c("F", "G", "W", "O"),
      labels = c("forest", "grassland", "wetland", "other"),
      name   = "Habitat Type"
    ) +
    coord_sf(
      xlim = c(country_bbox["xmin"], country_bbox["xmax"]),
      ylim = c(country_bbox["ymin"], country_bbox["ymax"]),
      expand = FALSE
    ) +
    theme_minimal(base_family = "Roboto") +
    theme(
      panel.background = element_rect(fill = "#EAF2F8", color = NA),
      panel.grid.major = element_line(color = "white"),
      #legend.position  = c(0.85, 0.85),        # inside top-right
      #legend.justification = c(1, 1),
      legend.background = element_rect(fill = alpha("white", 0.85), color = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 22, face = "bold"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    ) +
    guides(color = guide_legend(override.aes = list(size = 4.2, alpha = 1))) +
    labs(
      title = paste("Automated Biodiversity Monitoring\nStations in", country),
      x = NULL, y = NULL
    )
  
  # Save map
  ggsave(
    filename = paste0("Outputs/Maps/Map_", country, ".png"),
    plot = map_country,
    width = 8, height = 6, dpi = 300
  )
}

