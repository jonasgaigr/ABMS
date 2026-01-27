# -----------------------------------------------------------------#
## Compositional Analysis (NMDS) ----
# -----------------------------------------------------------------#

# Use the same 'species_count_matrix' created above.
# We will NOT use decostand(method = "total") because Bray-Curtis
# is designed for raw count data.

# Standardize by total site detections (Standardization Method 'total')
# This converts the raw counts to relative proportions within each site.
# This controls for the varying *number of total detections* per site.
standardized_matrix <- vegan::decostand(
  species_count_matrix,
  method = "total"
)
# Use the same 'species_count_matrix' created above.
# We will NOT use decostand(method = "total") because Bray-Curtis
# is designed for raw count data.

# Run NMDS on the standardized data
nmds_result <- vegan::metaMDS(
  standardized_matrix,
  distance = "bray", # Bray-Curtis distance is standard for abundance data
  k = 2, # 2 dimensions for easy plotting
  trymax = 100 # Try up to 100 random starts to find a stable solution
)

# Extract coordinates and merge with partner/habitat metadata for plotting
data_scores <- data.frame(vegan::scores(nmds_result, display = "sites")) %>%
  # The row names are "belgium_F1", etc.
  # We'll rename the new column to 'partner_deployment' to be clear.
  tibble::rownames_to_column(var = "partner_deployment") %>%
  # Merge back with your UNIQUE data to get the 'partner' column
  dplyr::left_join(
    # Use the 'data_filtered_unique' table we created earlier
    data_filtered_unique %>%
      dplyr::select(partner_deployment, partner) %>%
      dplyr::distinct(), # Get unique rows
    by = "partner_deployment" # Join by the correct, unique ID
  ) %>%
  dplyr::mutate(
    deployment = if_else(
      str_detect(partner_deployment, "_[WFGO][0-9]+$"),
      str_extract(partner_deployment, "[WFGO][0-9]+$"),
      NA_character_
    ) %>%
      as.factor(), # W1, F2, G1 …
    habitat = if_else(
      !is.na(deployment),
      str_sub(deployment, 1, 1),
      NA_character_
    ) %>%
      as.factor()
  )

# Plot the NMDS results (this code is now correct)
nmds_plot <- ggplot2::ggplot(
  data_scores,
  ggplot2::aes(x = NMDS1, y = NMDS2, color = habitat)
) +
  ggplot2::geom_point(size = 3, alpha = 0.7) +
  ggplot2::stat_ellipse() +
  scale_color_manual(
    values = okabe_ito,
    breaks = c("F", "G", "W", "O"),
    labels = c("forest", "grassland", "wetland", "other"),
    name   = "Habitat Type"
  ) +
  ggplot2::labs(
    title = paste0("Compositional Dissimilarity (NMDS)"),
    caption = paste0("Stress = ", round(nmds_result$stress, 3))
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  guides(shape = "none") +
  labs(color = "Habitat type") +
  scale_fill_manual(values = c("#009E73", "#E69F00", "#56B4E9")) +
  ggplot2::theme_bw()

print(nmds_plot)
ggplot2::ggsave("Outputs/Figures/nmds_plot.png", nmds_plot, height = 12, width = 15, units = "cm")
# ----------------------------------------------------------------- #
## Deployment Similarity Dendrogram (ggplot2 solution) ----
# (Requires 'vegan', 'ggdendro', 'ggplot2', 'dplyr')
# ----------------------------------------------------------------- #

# ----------------------------------------------------------------- #
### 1. & 2. Calculate Dissimilarity & Cluster ----
# ----------------------------------------------------------------- #
# (This part is identical to the previous script)
# We must use the 'species_count_matrix' with UNIQUE row names
# (e.g., "czech_republic_F1")

message("Calculating Bray-Curtis dissimilarity matrix...")
bray_dissim_matrix <- vegan::vegdist(species_count_matrix, method = "bray")

message("Running hierarchical clustering (hclust)...")
h_cluster <- hclust(bray_dissim_matrix, method = "average")

# ----------------------------------------------------------------- #
### 3. Extract Data for ggplot ----
# ----------------------------------------------------------------- #
message("Extracting dendrogram data for ggplot...")

# ggdendro::dendro_data() converts the hclust object into
# a list of data frames that ggplot can use.
dendro_data <- ggdendro::dendro_data(h_cluster, type = "rectangle")

# ----------------------------------------------------------------- #
### 4. Augment Label Data with 'partner' Info ----
# ----------------------------------------------------------------- #
# We need to add the 'partner' column to the labels data frame
# so we can use it for the 'color' aesthetic.

# A) Create the lookup table from our unique filtered data
partner_lookup <- data_filtered_unique %>%
  dplyr::select(partner_deployment, partner) %>%
  dplyr::distinct()

# B) Get the label data frame from the ggdendro object
label_data <- dendro_data$labels

# C) Join the partner info.
# The 'label' column in 'label_data' contains our 'partner_deployment' ID
label_data <- label_data %>%
  dplyr::left_join(partner_lookup, by = c("label" = "partner_deployment"))

# ----------------------------------------------------------------- #
### 5. Build the ggplot ----
# ----------------------------------------------------------------- #
message("Building ggplot dendrogram...")

dendro_plot <- ggplot2::ggplot() +

  # Plot the segments (the tree branches)
  # ggdendro::segment() is a helper to get the segment data
  ggplot2::geom_segment(
    data = ggdendro::segment(dendro_data),
    ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
  ) +

  # Plot the labels (the text)
  # We use our *augmented* 'label_data' here
  ggplot2::geom_text(
    data = label_data,
    ggplot2::aes(x = x, y = y, label = label, color = partner),
    hjust = 0, # Justify text to the left (starts at the tip)
    size = 2.5 # Adjust size as needed
  ) +

  # Flip to a horizontal dendrogram
  ggplot2::coord_flip() +

  # Reverse the y-axis (which is now the x-axis)
  # This makes the tree read from left to right
  # We add 'expand' to give the text labels room
  ggplot2::scale_y_reverse(expand = c(0.2, 0)) +

  # Add labels
  ggplot2::labs(
    title = "Species Composition Similarity by Deployment",
    x = "", # The deployments are the labels, so no axis text needed
    y = "Bray-Curtis Dissimilarity",
    color = "Partner" # This sets the legend title
  ) +

  # Clean up the theme (minimal is a good start)
  ggplot2::theme_bw() +
  ggplot2::theme(
    # Remove the y-axis text, ticks, and gridlines (they are redundant)
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    legend.position = "none"
  )

# ----------------------------------------------------------------- #
### 6. Save and Print the Plot ----
# ----------------------------------------------------------------- #

# Print the plot to the RStudio viewer
print(dendro_plot)

# Save the plot
ggplot2::ggsave(
  "Outputs/Figures/dendrogram_ggplot.png",
  dendro_plot,
  width = 12, # May need to be wide to fit labels
  height = 9,
  bg = "white"
)
