# Map of Europe ----
## 2025 ----
map_europe_2025 <- 
  ggplot() +
  # Base map layers
  geom_sf(data = europe_ext, fill = "gray98", color = "gray70", size = 0.3) +
  geom_sf(data = lakes, fill = "lightblue", color = NA, alpha = 0.4) +
  geom_sf(data = rivers, color = "lightblue", size = 0.2, alpha = 0.6) +
  
  # Sites
  geom_sf(data = locations_2025,
          aes(color = factor(site_type)),
          size = 2,
          alpha = 0.8) +
  
  # Major cities for orientation
  #geom_sf(data = cities, color = "black", size = 1, alpha = 0.7) +
  #geom_text(
  #  data = cities_coords,
  #  aes(x = lon, y = lat, label = NAME),
  #  size = 3,
  #  hjust = -0.1,
  #  vjust = 0.5,
  #  color = "gray20"
  #) +
  
  # Scale bar & north arrow
  annotation_scale(location = "bl", width_hint = 0.3, text_cex = 0.8) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(1.5, "cm"), width = unit(1, "cm")
  ) +
  
  # Labels and legend
  labs(
    title = "ABMS Sites Across Europe – 2025",
    subtitle = "High-resolution base map with rivers, lakes and major cities",
    color = "Site type"
  ) +
  
  # Coordinate limits for Europe
  coord_sf(xlim = c(2500000, 6000000), ylim = c(1400000, 5500000)) +
  
  # Theme & aesthetics
  scale_color_viridis_d() +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "gray90", size = 0.3),
    panel.background  = element_rect(fill = "aliceblue"),
    plot.background   = element_rect(fill = "white", color = NA),
    legend.position   = "right",
    legend.title      = element_text(face = "bold"),
    plot.title        = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 12, hjust = 0.5, color = "gray40")
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
  
  # Sites
  geom_sf(data = locations_2024,
          aes(color = factor(site_type)),
          size = 2,
          alpha = 0.8) +
  
  # Major cities for orientation
  #geom_sf(data = cities, color = "black", size = 1, alpha = 0.7) +
  #geom_text(
  #  data = cities_coords,
  #  aes(x = lon, y = lat, label = NAME),
  #  size = 3,
  #  hjust = -0.1,
  #  vjust = 0.5,
  #  color = "gray20"
  #) +
  
  # Scale bar & north arrow
  annotation_scale(location = "bl", width_hint = 0.3, text_cex = 0.8) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(1.5, "cm"), width = unit(1, "cm")
  ) +
  
  # Labels and legend
  labs(
    title = "ABMS Sites Across Europe – 2024",
    subtitle = "High-resolution base map with rivers, lakes and major cities",
    color = "Site type"
  ) +
  
  # Coordinate limits for Europe
  coord_sf(xlim = c(2500000, 6000000), ylim = c(1400000, 5500000)) +
  
  # Theme & aesthetics
  scale_color_viridis_d() +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "gray90", size = 0.3),
    panel.background  = element_rect(fill = "aliceblue"),
    plot.background   = element_rect(fill = "white", color = NA),
    legend.position   = "right",
    legend.title      = element_text(face = "bold"),
    plot.title        = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 12, hjust = 0.5, color = "gray40")
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
  
  # Sites
  geom_sf(data = locations_comb,
          aes(color = factor(site_type)),
          size = 2,
          alpha = 0.8) +
  
  # Major cities for orientation
  #geom_sf(data = cities, color = "black", size = 1, alpha = 0.7) +
  #geom_text(
  #  data = cities_coords,
  #  aes(x = lon, y = lat, label = NAME),
  #  size = 3,
  #  hjust = -0.1,
  #  vjust = 0.5,
  #  color = "gray20"
  #) +
  
  # Scale bar & north arrow
  annotation_scale(location = "bl", width_hint = 0.3, text_cex = 0.8) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(1.5, "cm"), width = unit(1, "cm")
  ) +
  
  # Labels and legend
  labs(
    title = "ABMS Sites Across Europe – 2024-2025",
    subtitle = "High-resolution base map with rivers, lakes and major cities",
    color = "Site type"
  ) +
  
  # Coordinate limits for Europe
  coord_sf(xlim = c(2500000, 6000000), ylim = c(1400000, 5500000)) +
  
  # Theme & aesthetics
  scale_color_viridis_d() +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "gray90", size = 0.3),
    panel.background  = element_rect(fill = "aliceblue"),
    plot.background   = element_rect(fill = "white", color = NA),
    legend.position   = "right",
    legend.title      = element_text(face = "bold"),
    plot.title        = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 12, hjust = 0.5, color = "gray40")
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
for (country in target_countries) {
  
  country_shape <- world %>% filter(admin == country)
  country_sites <- locations_comb[country_shape, ]  # spatial filter
  
  map_country <- ggplot() +
    geom_sf(data = country_shape, fill = "grey95", color = "grey60", size = 0.2) +
    geom_sf(data = country_sites, aes(color = type_code), size = 3, alpha = 0.85) +
    scale_color_brewer(palette = "Dark2", name = "Site type") +
    theme_minimal(base_family = "Roboto") +
    theme(
      panel.background = element_rect(fill = "#EAF2F8", color = NA),
      panel.grid.major = element_line(color = "white"),
      legend.position = "bottom",
      legend.background = element_rect(fill = alpha("white", 0.8), color = NA),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    ) +
    labs(
      title = paste("Monitoring Sites in", country),
      x = NULL, y = NULL
    )
  
  # --- Save map for each country ---
  ggsave(
    filename = paste0("Outputs/Maps/Map_", country, ".png"),
    plot = map_country,
    width = 8, height = 6, dpi = 300
  )
}
