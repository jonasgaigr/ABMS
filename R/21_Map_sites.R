# Map of Europe ----
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
    title = "ABMS Sites Across Europe – 2025",
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
    title = "ABMS Sites Across Europe – 2024",
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
    title = "ABMS Sites Across Europe – 2024-2025",
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
  
  # Filter sites inside this country
  country_sites <- locations_comb %>%
    #st_transform(crs_europe) %>%
    st_intersection(st_buffer(country_shape, 5000))
  
  # Country bbox for zoom
  country_bbox <- st_bbox(st_buffer(country_shape, 25000))
  
  # Build map
  map_country <- ggplot() +
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
      title = paste("Monitoring Sites in", country),
      x = NULL, y = NULL
    )
  
  # Save map
  ggsave(
    filename = paste0("Outputs/Maps/Map_", country, ".png"),
    plot = map_country,
    width = 8, height = 6, dpi = 300
  )
}

