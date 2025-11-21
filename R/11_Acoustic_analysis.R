# ----------------------------------------------------------------- #
# Exploratory Data Analysis (EDA) of Acoustic Detections
# ----------------------------------------------------------------- #
# -----------------------------------------------------------------#
# Setup & Filtering----
# -----------------------------------------------------------------#

# Define your confidence threshold
confidence_threshold <- 0.7

species_thresholds <- thresholds %>%
  dplyr::mutate(
    #spec_threshold = dplyr::case_when(
    #  F_090 < F_095 ~ Threshold_095,
    #  F_090 > F_095 ~ Threshold_090
    #  )
    spec_threshold = Threshold_090
  ) %>%
  dplyr::mutate(
    spec_threshold = dplyr::case_when(
      is.na(spec_threshold) == TRUE ~ confidence_threshold,
      spec_threshold > 1 ~ confidence_threshold,
      spec_threshold < 0 ~ confidence_threshold,
      spec_threshold < 0.5 ~ 0.5,
      spec_threshold > 0.9 ~ 0.9,
      TRUE ~ spec_threshold
    )
  ) %>%
  dplyr::select(Species, spec_threshold)

# Create a filtered data frame. This will be the basis for most analysis.
# We keep only rows with confidence >= the threshold.
message("Filtering data and creating confidence bins...")

# Define the order of bins for the plot legend
# We do this so "> 0.95" appears on top, not alphabetically
bin_levels <- c("> 0.9", "0.7 - 0.9", "0.5 - 0.7")

# Create a filtered data frame with an added 'confidence_bin' column
data_filtered <- acoustic_data %>%
  dplyr::left_join(
    .,
    species_thresholds,
    by = c("species_name" = "Species")
    ) %>%
  dplyr::mutate(
    spec_threshold = dplyr::case_when(
      is.na(spec_threshold) == FALSE ~ spec_threshold,
      TRUE ~ 0.7
    )
  ) %>%
  dplyr::filter(confidence >= spec_threshold) %>%
  dplyr::mutate(
    confidence_bin = dplyr::case_when(
      confidence >= 0.9 ~ bin_levels[1],
      confidence >= 0.7 ~ bin_levels[2],
      TRUE ~ bin_levels[3] # All remaining (0.70 - 0.849...)
    ),
    # Convert 'confidence_bin' to a factor to control stacking order
    confidence_bin = factor(confidence_bin, levels = bin_levels)
  ) %>%
  dplyr::mutate(
    habitat = as.character(stringr::str_sub(deployment, 1, 1))
  )

message("Data filtered and binned successfully.")

# ----------------------------------------------------------------- #
# High-Level Summaries ----
# ----------------------------------------------------------------- #

# Get a high-level overview
# Use dplyr::n() to get row count
# Use dplyr::n_distinct() to count unique values
summary_stats <- data_filtered %>%
  dplyr::summarise(
    total_detections_above_threshold = dplyr::n(),
    unique_species = dplyr::n_distinct(species),
    unique_recordings = dplyr::n_distinct(sourcefileid),
    earliest_year = min(year, na.rm = TRUE),
    latest_year = max(year, na.rm = TRUE)
  )

# Print the summary stats
message("--- High-Level Summary (Confidence >= ", confidence_threshold, ") ---")
print(summary_stats)

message("--- Total Raw Detections (Before Filtering) ---")
print(nrow(acoustic_data))

# ----------------------------------------------------------------- #
# Plot 1: Confidence Score Distribution (on *original* data) ----
# ----------------------------------------------------------------- #
# This helps you see if your 0.7 threshold is reasonable.

message("Generating Plot 1: Confidence Distribution...")

confidence_histogram <- ggplot2::ggplot(acoustic_data, ggplot2::aes(x = confidence)) +
  ggplot2::geom_histogram(
    bins = 50,
    fill = "skyblue",
    color = "black",
    alpha = 0.8
  ) +
  ggplot2::geom_vline(
    xintercept = confidence_threshold,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  ggplot2::annotate(
    "text",
    x = confidence_threshold,
    y = Inf,
    label = paste("Threshold =", confidence_threshold),
    hjust = -0.1, # Justify text to the right of the line
    vjust = 2,    # Move text down from the top
    color = "red",
    fontface = "bold"
  ) +
  ggplot2::labs(
    title = "Distribution of All Confidence Scores",
    subtitle = "Red dashed line shows the 0.7 threshold",
    x = "Confidence Score",
    y = "Count of Detections"
  ) +
  ggplot2::theme_minimal()

# You can print the plot to view it in RStudio
# print(confidence_histogram)

# Or save it to a file
ggplot2::ggsave(
  "Outputs/Figures/1_confidence_histogram.png",
  confidence_histogram,
  width = 8,
  height = 5
)

# ----------------------------------------------------------------- #
# Plot 2.1: Top Species ----
# ----------------------------------------------------------------- #
# What are the most common species found, stacked by confidence?

message("Generating Plot 2: Top Species (Stacked)...")

# Set how many top species you want to see
top_n_species <- 20

# 1. Find the *names* of the top 20 species based on *total* count
top_species_names <- data_filtered %>%
  dplyr::count(species, sort = TRUE) %>%
  dplyr::slice_head(n = top_n_species) %>%
  dplyr::pull(species) # pull() extracts just the 'species' column as a vector

# 2. Create the summary data for plotting
# Filter for *only* those top species, then count detections *within each bin*
top_species_data <- data_filtered %>%
  dplyr::filter(species %in% top_species_names) %>%
  dplyr::count(species, confidence_bin, name = "n")

# 3. Create the stacked bar plot
# We map 'fill' to 'confidence_bin'
top_species_plot <- ggplot2::ggplot(
  top_species_data,
  # x-axis: Reorder 'species' factor by the SUM of 'n' (total count)
  # y-axis: 'n' (the count for each bin)
  # fill: 'confidence_bin' to create the stacks
  ggplot2::aes(
    x = forcats::fct_reorder(species, n, .fun = sum),
    y = n,
    fill = confidence_bin
  )
) +
  ggplot2::geom_col() + # geom_col() is correct for stacked bars (position="stack" is default)
  ggplot2::coord_flip() + # Flip coordinates so names are readable
  ggplot2::scale_fill_brewer(palette = "cool", direction = -1) + # Use a nice color scale
  ggplot2::labs(
    title = paste("Top", top_n_species, "Most Frequent Species"),
    x = "Species",
    y = "Number of Detections",
    fill = "Confidence Bin" # Legend title
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(face = "italic") # Italicize species names
  )

# print(top_species_plot)
ggplot2::ggsave(
  "Outputs/Figures/2_1_top_species_plot_stacked.png", # Changed filename
  top_species_plot,
  width = 11, # Increased width slightly for legend
  height = 7
)

message("Stacked species plot saved.")

# ----------------------------------------------------------------- #
# Plot 2.2: Top RL Species (MODIFIED for stacked bars) ----
# ----------------------------------------------------------------- #
# What are the most common species found, stacked by confidence?

message("Generating Plot 2: Top RL Species (Stacked)...")

# keep same RL filtering you used before (exclude LC, RE, NA and NA values)
rl_filter <- data_filtered %>%
  dplyr::filter(
    europeanRegionalRedListCategory != "LC" &
      europeanRegionalRedListCategory != "RE" &
      europeanRegionalRedListCategory != "NA" &
      !is.na(europeanRegionalRedListCategory)
  )

# 1) Find top species PER red-list category (counting all records)
top_species_per_cat <- rl_filter %>%
  count(europeanRegionalRedListCategory, species, name = "total") %>%
  group_by(europeanRegionalRedListCategory) %>%
  slice_max(order_by = total, n = top_n_species, with_ties = FALSE) %>%
  ungroup()

# 2) Build the plotting dataset: keep only those top species (per category),
#    then count occurrences per confidence_bin
top_species_data_rl <- data_filtered %>%
  semi_join(top_species_per_cat, by = c("europeanRegionalRedListCategory", "species")) %>%
  count(europeanRegionalRedListCategory, species, confidence_bin, name = "n")

# 3) (Optional) make species an ordered factor within each facet so bars are sorted
#    We rely on fct_reorder() with sum(n) — since species belong to a single category
#    this effectively orders them per-facet.
top_species_data_rl <- top_species_data_rl %>%
  group_by(europeanRegionalRedListCategory, species) %>%
  mutate(total_per_species = sum(n)) %>%
  ungroup() %>%
  mutate(species = forcats::fct_reorder(species, total_per_species, .fun = sum))

# 4) Plot with one facet per red-list category
top_species_plot_rl <- ggplot(top_species_data_rl,
                              aes(x = species, y = n, fill = confidence_bin)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ europeanRegionalRedListCategory, scales = "free_y", ncol = 2) +
  scale_fill_brewer(palette = "Set2", direction = 1) +
  labs(
    title = paste("Top", top_n_species, "species per Red-List category"),
    x = "Species",
    y = "Number of detections",
    fill = "Confidence bin"
  ) +
  scale_y_log10() +
  theme_minimal() +
  theme(
    axis.text.y = element_text(face = "italic"),
    strip.text = element_text(face = "bold")
  )

# save
ggsave(
  filename = "Outputs/Figures/2_2_top_rl_species_plot_faceted.png",
  plot = top_species_plot_rl,
  width = 12,
  height = 9
)

message("Stacked species plot saved.")

# ----------------------------------------------------------------- #
# Plot 2.3: Top Species by partner ----
# ----------------------------------------------------------------- #
# directory for outputs
out_dir <- "Outputs/Figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# which partners to process
partners <- data_filtered %>%
  distinct(partner) %>%
  drop_na() %>%
  pull(partner)

message("Partners detected: ", paste(partners, collapse = ", "))

# number of species shown
top_n_species <- 20

for (p in partners) {
  message("Processing: ", p)
  
  # ------------------------------------------------------------------ #
  # 1) Filter dataset to one partner
  # ------------------------------------------------------------------ #
  df_p <- data_filtered %>%
    filter(partner == p)
  
  if (nrow(df_p) == 0) {
    message("  -> No data for this partner. Skipping.")
    next
  }
  
  # ------------------------------------------------------------------ #
  # 2) Select top N species *within this partner*
  # ------------------------------------------------------------------ #
  top_species_names <- df_p %>%
    count(species, sort = TRUE) %>%
    slice_head(n = top_n_species) %>%
    pull(species)
  
  if (length(top_species_names) == 0) {
    message("  -> No species for this partner. Skipping.")
    next
  }
  
  # ------------------------------------------------------------------ #
  # 3) Count confidence-bin stats (partner-filtered)
  # ------------------------------------------------------------------ #
  top_species_data <- df_p %>%
    filter(species %in% top_species_names) %>%
    count(species, confidence_bin, name = "n") %>%
    group_by(species) %>%
    mutate(total = sum(n)) %>%
    ungroup() %>%
    mutate(species = fct_reorder(species, total, .fun = sum))
  
  # ------------------------------------------------------------------ #
  # 4) Plot
  # ------------------------------------------------------------------ #
  top_species_plot <- ggplot(
    top_species_data,
    aes(x = species, y = n, fill = confidence_bin)
  ) +
    geom_col() +
    coord_flip() +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = paste0("Top ", top_n_species, " Species — ", p),
      x = "Species",
      y = "Number of Detections",
      fill = "Confidence Bin"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(face = "italic")
    )
  
  # ------------------------------------------------------------------ #
  # 5) Save (partner-safe filename)
  # ------------------------------------------------------------------ #
  p_safe <- str_replace_all(p, "[^A-Za-z0-9_-]", "_")
  
  ggsave(
    filename = file.path(out_dir, paste0("2_1_top_species_plot_stacked_", p_safe, ".png")),
    plot = top_species_plot,
    width = 11,
    height = 7,
    dpi = 300
  )
  
  message("  -> Saved for partner: ", p)
}

message("All partner-specific plots completed.")

# ----------------------------------------------------------------- #
# Plot 2.4: Top Species by habitat ----
# ----------------------------------------------------------------- #
# small helper: generate n shades from very light to base colour
make_shades <- function(base_col, n) {
  # gradient from a very light grey to the base colour
  grDevices::colorRampPalette(c("grey95", base_col))(n)
}

top_n_species <- 10
habitats <- c("F","G","W")
plots <- list()
legends_grobs <- list()
habitat_labels <- c(F = "Forest", G = "Grassland", W = "Wetland", O = "Other")

for (h in habitats) {
  df_h <- data_filtered %>% filter(habitat == h)
  if (nrow(df_h) == 0) {
    message("No data for habitat ", h, " — skipping")
    next
  }
  
  habitat_name <- habitat_labels[h]
  if (is.na(habitat_name)) habitat_name <- h
  
  # top species within habitat
  top_species_names <- df_h %>%
    count(species, sort = TRUE) %>%
    slice_head(n = top_n_species) %>%
    pull(species)
  
  top_species_data <- df_h %>%
    filter(species %in% top_species_names) %>%
    count(species, confidence_bin, name = "n") %>%
    group_by(species) %>%
    mutate(total = sum(n)) %>%
    ungroup() %>%
    mutate(species = fct_reorder(species, total, .fun = sum))
  
  # robust extraction + ascending ordering of confidence bins
  bin_levels <- top_species_data %>%
    distinct(confidence_bin) %>%
    pull(confidence_bin) %>%
    as.character() %>%
    na.omit() %>%
    unique() %>%
    tibble(bin = .) %>%
    mutate(
      # extract a numeric lower bound: e.g. "0.70 - 0.85" -> 0.70, "> 0.95" -> 0.95
      lower = case_when(
        str_detect(bin, "^>\\s*[0-9.]") ~ as.numeric(str_replace(bin, "^>\\s*", "")),
        str_detect(bin, "^[0-9.]") ~ as.numeric(str_extract(bin, "^[0-9.]+")),
        TRUE ~ NA_real_
      )
    ) %>%
    arrange(lower) %>%      # ascending: low -> high
    pull(bin)
  
  if (length(bin_levels) == 0) {
    message("No confidence bins for habitat ", h, " — skipping")
    next
  }
  
  # create shades for this habitat (darkest = base colour -> map to the highest-confidence bin)
  shades <- make_shades(okabe_ito[h], length(bin_levels))
  names(shades) <- bin_levels  # map names to bin values
  
  p <- ggplot(top_species_data, aes(x = species, y = n, fill = confidence_bin)) +
    geom_col() +
    coord_flip(expand = FALSE) +
    scale_fill_manual(values = shades, na.value = "grey60", drop = FALSE) +
    labs(
      title = paste0("Top ", top_n_species, " Species — ", habitat_name),
      x = "Species",
      y = "Detections",
      fill = "Confidence bin"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.y = element_text(face = "italic", size = 8),
      axis.title.x = element_text(size = 11, face = "bold"),
      axis.title.y = element_text(size = 11, face = "bold"),
      plot.title = element_text(face = "bold", size = 14, color = okabe_ito[h]),
      plot.subtitle = element_text(size = 10),
      plot.margin = margin(6, 6, 6, 6),
      legend.position = "bottom",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.key.width = unit(0.9, "cm")
    )
  
  # store plot
  plots[[h]] <- p
  
  # extract legend grob for this plot (we'll arrange these under the combined plot)
  legends_grobs[[h]] <- cowplot::get_legend(p + theme(legend.position = "bottom",
                                             legend.direction = "vertical",
                                             legend.key.size = unit(0.6, "lines")))
}

# remove legends from the main plots (we'll add the legends row below)
plots_no_legend <- lapply(plots, function(pp) pp + theme(legend.position = "none"))

# combine the three panels in a single row using cowplot::plot_grid
# ensure order F, G, W
plot_row <- cowplot::plot_grid(
  plots_no_legend[["F"]],
  plots_no_legend[["G"]],
  plots_no_legend[["W"]],
  ncol = 3,
  align = "hv",
  rel_widths = c(1,1,1)
)

# Check for nulls and remove (already done in your code)
legend_plots <- legend_plots[!vapply(legend_plots, is.null, logical(1))]

if (length(legend_plots) == 0) {
  # no legends found — just save the plot_row
  final_plot <- plot_row
} else {
  # --- FIX 1: Use do.call to pass the list elements as arguments ---
  # We use the corrected function signature:
  legend_row <- do.call(
    cowplot::plot_grid, 
    c(
      plotlist = legend_plots, 
      list(
        ncol = length(legend_plots), 
        rel_widths = rep(1, length(legend_plots))
      )
    )
  )
  
  # Note: A simpler, more reliable way (if you don't need complex rel_widths) is:
  # legend_row <- plot_grid(plotlist = legend_plots, ncol = length(legend_plots)) 
  # Wait, the error suggests even 'plotlist' is unused. The simple fix is:
  
  
  # --- FIX 1 (Revised and simpler): Use do.call directly on the plot list ---
  legend_row <- do.call(
    cowplot::plot_grid, 
    c(
      legend_plots, 
      list(
        ncol = length(legend_plots), 
        # You may omit rel_widths if they are all equal
        rel_widths = rep(1, length(legend_plots))
      )
    )
  )
  
  # stack the main row and the legend row
  final_plot <- cowplot::plot_grid(
    plot_row, 
    legend_row, 
    ncol = 1, 
    rel_heights = c(1, 0.18)
  )
}

# save final figure
ggsave(
  filename = "Outputs/Figures/2_4_top_species_by_habitat_shaded.png",
  plot = final_plot,
  width = 20,
  height = 9,
  dpi = 300,
  units = "in"
)

message("Saved combined habitat plot with per-habitat confidence-shaded bars and legends underneath.")

# ----------------------------------------------------------------- #
# Plot 3: Detections per Recording (on *filtered* data) ----
# ----------------------------------------------------------------- #
# How "busy" are the recordings? Do most have 1-2 detections or 50+?

message("Generating Plot 3: Detections per Recording...")

# First, count detections per file
detections_per_file <- data_filtered %>%
  dplyr::count(sourcefileid, name = "detections_per_file")

# Now, create a histogram of *those counts*
detections_per_file_hist <- ggplot2::ggplot(
  detections_per_file,
  ggplot2::aes(x = detections_per_file)
) +
  ggplot2::geom_histogram(
    bins = 30,
    fill = "darkgreen",
    color = "black",
    alpha = 0.8
  ) +
  ggplot2::labs(
    title = "Distribution of Detections per Recording",
    subtitle = paste("Based on detections with confidence >=", confidence_threshold),
    x = "Number of Detections in a Single Recording",
    y = "Count of Recordings"
  ) +
  #ggplot2::scale_y_log10() + # Use a log scale if distribution is heavily skewed
  ggplot2::theme_minimal()

# print(detections_per_file_hist)
ggplot2::ggsave(
  "Outputs/Figures/3_detections_per_file_hist.png",
  detections_per_file_hist,
  width = 8,
  height = 5
)

# ----------------------------------------------------------------- #
# Summary Table by Group (e.g., deployment) ----
# ----------------------------------------------------------------- #

message("Generating Summary Table by Deployment...")

# Group by 'deployment' (or 'partner') to see summaries
deployment_summary_sitenational <- data_filtered %>%
  dplyr::group_by(partner, deployment) %>%
  dplyr::summarise(
    total_detections = dplyr::n(),
    unique_species = dplyr::n_distinct(species),
    # Calculate mean confidence for the group
    mean_confidence = mean(confidence, na.rm = TRUE)
  ) %>%
  dplyr::arrange(dplyr::desc(total_detections)) # Sort by most detections

# Group by 'deployment' (or 'partner') to see summaries
deployment_summary_national <- data_filtered %>%
  dplyr::group_by(partner) %>%
  dplyr::summarise(
    total_detections = dplyr::n(),
    unique_species = dplyr::n_distinct(species),
    # Calculate mean confidence for the group
    mean_confidence = mean(confidence, na.rm = TRUE)
  ) %>%
  dplyr::arrange(dplyr::desc(total_detections)) # Sort by most detections

# Group by 'deployment' (or 'partner') to see summaries
deployment_summary_site <- data_filtered %>%
  dplyr::group_by(deployment) %>%
  dplyr::summarise(
    total_detections = dplyr::n(),
    unique_species = dplyr::n_distinct(species),
    # Calculate mean confidence for the group
    mean_confidence = mean(confidence, na.rm = TRUE)
  ) %>%
  dplyr::arrange(dplyr::desc(total_detections)) # Sort by most detections

# ----------------------------------------------------------------- #
# Save All Summary Tables ---- 
# ----------------------------------------------------------------- #

# --- Define Output Filenames ---
# We define clear, distinct names for each output file.
file_sitenational <- "Outputs/Results/deployment_summary_sitenational"
file_national <- "Outputs/Results/deployment_summary_national"
file_site <- "Outputs/Results/deployment_summary_site"


# ----------------------------------------------------------------- #
## 1. Save all summaries as CSV files ----
# ----------------------------------------------------------------- #

message("--- Saving CSV files ---")

# Save the sitenational summary
readr::write_csv(
  deployment_summary_sitenational,
  paste0(file_sitenational, ".csv")
)
message("Saved: ", paste0(file_sitenational, ".csv"))

# Save the national summary
readr::write_csv(
  deployment_summary_national,
  paste0(file_national, ".csv")
)
message("Saved: ", paste0(file_national, ".csv"))

# Save the site summary
readr::write_csv(
  deployment_summary_site,
  paste0(file_site, ".csv")
)
message("Saved: ", paste0(file_site, ".csv"))

message("--- CSV saving complete. ---")

# ----------------------------------------------------------------- #
## 2. Save all summaries as Word (.docx) Tables ----
# ----------------------------------------------------------------- #

message("--- Saving Word (.docx) files ---")

# --- Sitenational ---
ft_sitenational <- flextable::flextable(deployment_summary_sitenational)
ft_sitenational <- flextable::autofit(ft_sitenational)
ft_sitenational <- flextable::theme_booktabs(ft_sitenational)
flextable::save_as_docx(
  ft_sitenational,
  path = paste0(file_sitenational, ".docx")
)
message("Saved: ", paste0(file_sitenational, ".docx"))

# --- National ---
ft_national <- flextable::flextable(deployment_summary_national)
ft_national <- flextable::autofit(ft_national)
ft_national <- flextable::theme_booktabs(ft_national)
flextable::save_as_docx(
  ft_national,
  path = paste0(file_national, ".docx")
)
message("Saved: ", paste0(file_national, ".docx"))

# --- Site ---
ft_site <- flextable::flextable(deployment_summary_site)
ft_site <- flextable::autofit(ft_site)
ft_site <- flextable::theme_booktabs(ft_site)
flextable::save_as_docx(
  ft_site,
  path = paste0(file_site, ".docx")
)
message("Saved: ", paste0(file_site, ".docx"))

message("--- Word saving complete. ---")
message("--- All files saved. ---")

message("--- EDA script complete. ---")

# ----------------------------------------------------------------- #
# Rarefaction and Extrapolation ---- 
# ----------------------------------------------------------------- #
# ----------------------------------------------------------------- #
# Create Species-Site Matrix (MODIFIED)
# ----------------------------------------------------------------- #
# ROOT CAUSE: 'deployment' names (e.g., "F1") are not unique
# across 'partners'.
#
# SOLUTION: Create a new, unique identifier 'partner_deployment'
# by combining 'partner' and 'deployment'.
# ----------------------------------------------------------------- #

# 1. Create a unique site identifier in the original filtered data
data_filtered_unique <- data_filtered %>%
  dplyr::mutate(
    # Create the new ID, e.g., "czech_republic_F1"
    partner_deployment = paste0(partner, "_", deployment)
  )

# 2. Summarize data using the new unique ID
species_counts_wide <- data_filtered_unique %>%
  # Group by the new unique ID and species
  dplyr::group_by(partner_deployment, species) %>%
  dplyr::summarise(
    n = dplyr::n(),
    .groups = 'drop'
  ) %>%
  # Convert long to wide
  tidyr::pivot_wider(
    id_cols = partner_deployment, # Use the unique ID
    names_from = species,
    values_from = n,
    values_fill = 0
  ) %>%
  # Move the unique ID column into the row names
  tibble::column_to_rownames(var = "partner_deployment")

# 3. Create the new, correct matrix
# All subsequent code (rarefaction, NMDS, dendrogram)
# MUST use this new 'species_count_matrix'
species_count_matrix <- as.matrix(species_counts_wide)

message("New 'species_count_matrix' created with unique 'partner_deployment' rows.")

# Convert to a simple matrix (iNEXT/vegan prefer this)
species_count_matrix <- as.matrix(species_counts_wide)

# Determine the standardization level: Use the lowest non-zero sample size.
# This is the max depth we can rarefy all samples to.
min_sample_size <- min(rowSums(species_count_matrix[rowSums(species_count_matrix) > 0, ]))

# Use vegan::rarefy() to estimate species richness at the minimum effort
rarefied_richness <- vegan::rarefy(
  species_count_matrix,
  sample = min_sample_size
)

# Convert the result to a data frame for plotting/analysis
rarefied_df <- data.frame(
  deployment = names(rarefied_richness),
  richness_rarefied = as.numeric(rarefied_richness)
)

message("Richness rarefied to an effort of ", min_sample_size, " detections.")
print(head(rarefied_df))

# -----------------------------------------------------------------#
# Compositional Analysis (NMDS) ----
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
  k = 2,              # 2 dimensions for easy plotting
  trymax = 100        # Try up to 100 random starts to find a stable solution
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
      as.factor(),    # W1, F2, G1 …
    habitat = if_else(
      !is.na(deployment),
      str_sub(deployment, 1, 1),
      NA_character_
    ) %>%
      as.factor()
  )

# Plot the NMDS results (this code is now correct)
nmds_plot <- ggplot2::ggplot(data_scores, ggplot2::aes(x = NMDS1, y = NMDS2, color = partner)) +
  ggplot2::geom_point(size = 3, alpha = 0.7) +
  ggplot2::stat_ellipse(mapping = aes(colour = habitat)) + 
  #scale_color_viridis_d() +
  ggplot2::labs(
    title = "Compositional Dissimilarity (NMDS)",
    subtitle = "Based on Bray-Curtis dissimilarity on raw counts",
    caption = paste("Stress:", round(nmds_result$stress, 3))
  ) +
  ggplot2::theme_minimal()

print(nmds_plot)
# ggplot2::ggsave("Outputs/Figures/nmds_plot.png", nmds_plot, width = 8, height = 6)
# ----------------------------------------------------------------- #
# Deployment Similarity Dendrogram (ggplot2 solution) ----
# (Requires 'vegan', 'ggdendro', 'ggplot2', 'dplyr')
# ----------------------------------------------------------------- #

# ----------------------------------------------------------------- #
## 1. & 2. Calculate Dissimilarity & Cluster ----
# ----------------------------------------------------------------- #
# (This part is identical to the previous script)
# We must use the 'species_count_matrix' with UNIQUE row names
# (e.g., "czech_republic_F1")

message("Calculating Bray-Curtis dissimilarity matrix...")
bray_dissim_matrix <- vegan::vegdist(species_count_matrix, method = "bray")

message("Running hierarchical clustering (hclust)...")
h_cluster <- hclust(bray_dissim_matrix, method = "average")

# ----------------------------------------------------------------- #
## 3. Extract Data for ggplot ----
# ----------------------------------------------------------------- #
message("Extracting dendrogram data for ggplot...")

# ggdendro::dendro_data() converts the hclust object into
# a list of data frames that ggplot can use.
dendro_data <- ggdendro::dendro_data(h_cluster, type = "rectangle")

# ----------------------------------------------------------------- #
## 4. Augment Label Data with 'partner' Info ----
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
## 5. Build the ggplot ----
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
  ggplot2::theme_minimal() +
  ggplot2::theme(
    # Remove the y-axis text, ticks, and gridlines (they are redundant)
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank()
  )

# ----------------------------------------------------------------- #
## 6. Save and Print the Plot ----
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

# -----------------------------------------------------------------#
# Phenology Analysis with Effort Control (GAM) ----
# -----------------------------------------------------------------#

# --- CONFIGURATION ---
# (ASSUMPTIONS - REPLACE WITH YOUR DATA)

# This is the recording log I asked about. You MUST provide this.
# It should have a complete list of ALL audio files deployed.
# We will create a hypothetical example.
# 
# *** REPLACE 'recording_metadata' WITH YOUR ACTUAL FILE LIST ***
# 
# It needs 'sourcefileid', 'starttime', 'endtime', 'deployment', and 'year'.
# For this example, we'll assume the detections file ('data_filtered')
# *mistakenly* contains all files. THIS IS A BAD ASSUMPTION.
# 
# -----------------------------------------------------------------#
# !! DANGER !! 
# The code below will ONLY work correctly if 'data_filtered' 
# *also* contains files with ZERO detections.
# 
# If your 'data_filtered' only has successful detections,
# you MUST load a different file for 'recording_metadata'.
# 
# For now, we proceed assuming you have a file 'recording_metadata'
# 
# -----------------------------------------------------------------#

# Let's *pretend* 'data' (your original file) is the recording log.
# This is a placeholder.
recording_metadata <- data_filtered %>%
  # We only need one row per file to calculate effort
  dplyr::distinct(sourcefileid, deployment, year, starttime, endtime, date) %>%
  # Calculate duration of each recording in HOURS
  dplyr::mutate(
    duration_hours = (endtime - starttime) / 3600
  )

# -----------------------------------------------------------------#
## 1. Choose Target Species ----
# -----------------------------------------------------------------#
target_species <- top_species_names[1]

# Filter detections for *only* our target species
species_detections <- data_filtered %>%
  #dplyr::filter(partner == "czech_republic") %>%
  dplyr::filter(species == target_species)

# -----------------------------------------------------------------#
## 2. Prepare Data for Modeling ----
# -----------------------------------------------------------------#
message("Aggregating effort and detections by day...")


# --- Now we can proceed with the *actual* logic ---

# A) Calculate TOTAL RECORDING EFFORT per day
effort_by_day <- recording_metadata %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(
    total_effort_hours = sum(duration_hours, na.rm = TRUE)
  ) %>%
  # Add day of year (DOY)
  dplyr::mutate(doy = lubridate::yday(date))

# B) Calculate TOTAL DETECTIONS per day (for target species)
detections_by_day <- species_detections %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(
    total_detections = dplyr::n()
  )

# C) Combine effort and detections
# We need a full calendar of all days, including gap days
phenology_data <- data.frame(
  date = seq(min(effort_by_day$date), max(effort_by_day$date), by = "day")
) %>%
  dplyr::left_join(effort_by_day, by = "date") %>%
  dplyr::left_join(detections_by_day, by = "date") %>%
  # IMPORTANT: fill NA with 0
  # A day with effort but NA detections = 0 detections
  # A day with NA effort = 0 effort (a true gap)
  dplyr::mutate(
    total_detections = tidyr::replace_na(total_detections, 0),
    total_effort_hours = tidyr::replace_na(total_effort_hours, 0),
    doy = lubridate::yday(date)
  ) %>%
  # We can't model days with zero effort. Filter them out.
  # The GAM will spline *over* these missing days.
  dplyr::filter(total_effort_hours > 0) %>%
  # Add a small constant to effort to avoid log(0) if effort is tiny
  dplyr::mutate(
    effort_for_offset = total_effort_hours + 0.001
  )


# -----------------------------------------------------------------#
## 3. Run the GAM (The Phenology Indicator) ----
# -----------------------------------------------------------------#
message("Fitting GAM model...")

# k = number of "knots" or "basis functions".
# k=20 is a good start for seasonal data (it allows ~19 "wiggles").
pheno_model <- mgcv::gam(
  total_detections ~ s(doy, bs = "tp", k = 20),
  data = phenology_data,
  family = "poisson", # We are modeling count data
  offset = log(effort_for_offset) # This controls for effort!
)

# Use 'bs="cc"' for cyclic cubic splines (DOY 365 wraps to 1)
# Use 'bs="tp"' if your data is not cyclic (e.d. only one season)

# Check the model summary
# print(summary(pheno_model))

# -----------------------------------------------------------------#
## 4. Plot the Phenology Curve ----
# -----------------------------------------------------------------#
message("Generating phenology plot...")

# A) Create a "prediction" data frame for a smooth curve
# We want to predict the rate for every day, assuming 1 hour of effort
doy_sequence <- seq(min(phenology_data$doy), max(phenology_data$doy), by = 1)
prediction_data <- data.frame(
  doy = doy_sequence,
  effort_for_offset = 1  # Predict the rate per *1 hour* of effort
)

# B) Get predictions from the model
# type="link" gives us the log-rate. We must add the offset.
# 'se.fit=TRUE' gives us standard errors for confidence bands
predictions <- predict(
  pheno_model,
  newdata = prediction_data,
  type = "link",
  se.fit = TRUE
)

# C) Combine and transform back to response scale (detections/hour)
# The prediction is log(rate) = link
# The rate = exp(link)
prediction_data <- prediction_data %>%
  dplyr::mutate(
    predicted_rate = exp(predictions$fit),
    se_high = exp(predictions$fit + 2 * predictions$se.fit),
    se_low = exp(predictions$fit - 2 * predictions$se.fit)
  )

# D) Plot the results
phenology_plot <- ggplot2::ggplot(prediction_data, ggplot2::aes(x = doy)) +
  # Confidence band
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = se_low, ymax = se_high),
    fill = "skyblue",
    alpha = 0.4
  ) +
  # Predicted phenology curve
  ggplot2::geom_line(
    ggplot2::aes(y = predicted_rate),
    color = "blue",
    linewidth = 1
  ) +
  # Add the raw, standardized data points for comparison
  ggplot2::geom_point(
    data = phenology_data,
    ggplot2::aes(y = total_detections / effort_for_offset),
    alpha = 0.2,
    color = "grey30"
  ) +
  ggplot2::labs(
    title = paste("Phenology Curve for", target_species),
    subtitle = "Modeled using a GAM controlling for sampling effort",
    x = "Day of Year (DOY)",
    y = "Predicted Detections / Hour"
  ) +
  ggplot2::theme_minimal()

# Save the plot
ggplot2::ggsave(
  "Outputs/Figures/phenology_curve.png",
  phenology_plot,
  width = 10,
  height = 6
)
# print(phenology_plot)

# -----------------------------------------------------------------#
## 5. Extract Derived Metrics (Peak, Onset, etc.) ----
# -----------------------------------------------------------------#
message("Extracting key phenology dates...")

# Find the single, maximum peak rate value
peak_rate_value <- max(prediction_data$predicted_rate, na.rm = TRUE)

# Find the peak day (we still want this for the summary)
peak_activity_day <- prediction_data %>%
  dplyr::filter(predicted_rate == peak_rate_value) %>%
  dplyr::slice(1) # Take the first day if multiple have the same peak

# Define a threshold for onset/offset
# We use our simple numeric value
season_threshold_value <- 0.10 * peak_rate_value

# Find onset and offset
# This filter will now compare a vector to a single number, which is safe
season_dates <- prediction_data %>%
  dplyr::filter(predicted_rate >= season_threshold_value)

# --- The rest of the code works with these new variables ---

season_onset <- season_dates %>%
  dplyr::filter(doy == min(doy))

season_end <- season_dates %>%
  dplyr::filter(doy == max(doy))

# Print a final summary
message(paste("--- Phenology Summary for", target_species, "---"))
message(paste(
  "Peak Activity (DOY):",
  peak_activity_day$doy,
  "(Rate:", round(peak_activity_day$predicted_rate, 2), "det/hr)"
))
message(paste("Season Onset (10%): DOY", season_onset$doy))
message(paste("Season End (10%):   DOY", season_end$doy))

# -----------------------------------------------------------------#
# Grouped Phenology ----
# -----------------------------------------------------------------#

# format(date, "%m-%d") converts dates to "03-15", "09-30", etc.
# This allows us to filter the window regardless of the year (2020, 2021, etc.)
season_filtered_data <- data_filtered %>%
  filter(format(date, "%m-%d") >= "03-15" & format(date, "%m-%d") <= "09-30")

# A. Identify Countries with > 120 recording days
# We count distinct dates per country to ensure they have a wide enough timeline.
valid_countries <- season_filtered_data %>%
  group_by(partner, habitat) %>%
  summarise(recording_days = n_distinct(date)) %>%
  filter(recording_days >= 105) %>%  # The Constraint
  group_by(partner) %>%
  summarise(valid_habitat = n_distinct(habitat)) %>%
  filter(valid_habitat >= 2) %>%
  pull(partner)

print(paste("Countries included:", paste(valid_countries, collapse = ", ")))

# B. Filter the main dataset
# We keep only valid countries AND only the specific habitats requested (F, G, W)
plot_data <- season_filtered_data %>%
  filter(partner %in% valid_countries) %>%
  filter(habitat %in% c("F", "G", "W")) %>%
  dplyr::group_by(partner, habitat, date) %>%
  dplyr::reframe(
    total_detections = n()
  )

# --- 3. THE VISUALIZATION (FACET GRID) ---

# Define a "Labeller" to convert codes to names in the plot headers
habitat_labeller <- c(
  "F" = "Forest",
  "G" = "Grassland",
  "W" = "Wetland"
)

phenology_plot <- ggplot(plot_data, aes(x = date, y = total_detections)) +
  
  # Apply Color and Fill based on Habitat
  # We set alpha (transparency) for the fill so the ribbon isn't too solid
  geom_smooth(
    aes(color = habitat, fill = habitat), 
    method = "loess", 
    span = 0.3, 
    se = TRUE, 
    alpha = 0.3
  ) +
  
  # Apply the Okabe-Ito Palette
  scale_color_manual(values = okabe_ito) +
  scale_fill_manual(values = okabe_ito) +
  
  # Facet Grid with Labeller
  # We pass the 'habitat_labeller' to rename F/G/W to Forest/Grassland/Wetland headers
  facet_grid(
    rows = vars(partner), 
    cols = vars(habitat), 
    scales = "free_y",
    labeller = labeller(habitat = habitat_labeller) 
  ) +
  
  # Formatting
  theme_bw() +
  labs(
    title = "Relative Bird Activity (March 15 - Sept 30)",
    y = "Relative Activity Index",
    x = "Date"
  ) +
  theme(
    strip.background = element_rect(fill = "#f0f0f0"),
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "none" # Remove legend since headers explain the colors
  )

# Display the plot
print(phenology_plot)

# --- 4. EXPORT TO A4 PORTRAIT ---
ggsave("Outputs/Figures/phenology_facet.png", plot = phenology_plot, width = 210, height = 297, units = "mm")

# Grouped GAM Phenology ----
# A. Filter Date Window & Habitats
seasonal_data <- data_filtered %>%
  dplyr::filter(format(date, "%m-%d") >= "03-15" & format(date, "%m-%d") <= "09-30") %>%
  dplyr::filter(habitat %in% c("F", "G", "W"))

# B. Calculate EFFORT (Duration in Hours)
# This calculation remains unchanged as it counts all recording duration
effort_by_day <- seasonal_data %>% 
  dplyr::distinct(sourcefileid, partner, habitat, starttime, endtime, date) %>%
  dplyr::mutate(duration_hours = (endtime - starttime) / 3600) %>%
  dplyr::group_by(partner, habitat, date) %>%
  dplyr::summarise(total_effort_hours = sum(duration_hours, na.rm = TRUE), .groups = "drop")

# C. Calculate DETECTIONS (FIX: Total Community Count)
detections_by_day <- seasonal_data %>%
  # *** NO SPECIES FILTER HERE ***
  dplyr::group_by(partner, habitat, date) %>%
  dplyr::summarise(
    total_detections = dplyr::n(), # Counts ALL rows/detections
    .groups = "drop"
  )

# D. COMBINE Effort and Detections
model_input_data <- effort_by_day %>%
  dplyr::left_join(detections_by_day, by = c("partner", "habitat", "date")) %>%
  dplyr::mutate(
    total_detections = tidyr::replace_na(total_detections, 0),
    doy = lubridate::yday(date),
    effort_for_offset = total_effort_hours + 0.001
  ) %>%
  dplyr::filter(total_effort_hours > 0) # This removes the true "gaps" to be interpolated

# -----------------------------------------------------------------#
# 3. FILTER COUNTRIES (The "N" Rows Logic)
# -----------------------------------------------------------------#

# Identify Valid Countries (>120 days with recording effort in this window)
valid_countries <- model_input_data %>%
  dplyr::group_by(partner) %>%
  dplyr::summarise(n_days = dplyr::n_distinct(date)) %>%
  dplyr::filter(n_days > 120) %>%
  dplyr::pull(partner)

message("Countries included: ", paste(valid_countries, collapse = ", "))

# Filter the dataset for modeling
final_modeling_data <- model_input_data %>%
  dplyr::filter(partner %in% valid_countries)

# -----------------------------------------------------------------#
# 4. ITERATIVE GAM MODELING
# -----------------------------------------------------------------#

fit_gam_phenology <- function(df) {
  
  # Safety check: Skip if data is too sparse for k=20
  if(nrow(df) < 25) return(NULL)
  
  tryCatch({
    # 1. Fit the Model (Your Code)
    m <- mgcv::gam(
      total_detections ~ s(doy, bs = "tp", k = 20),
      data = df,
      family = "poisson",
      offset = log(effort_for_offset) # Controls for your calculated hours
    )
    
    # 2. Predict for Standardized Effort
    # We predict for the full window (Mar 15 - Sept 30) roughly DOY 74-273
    pred_grid <- data.frame(
      doy = seq(min(df$doy), max(df$doy), length.out = 100),
      effort_for_offset = 1 # Standardize to 1 hour!
    )
    
    preds <- predict(m, newdata = pred_grid, type = "link", se.fit = TRUE)
    
    # 3. Return formatted results
    pred_grid %>%
      mutate(
        fit_link = preds$fit,
        se_link = preds$se.fit,
        predicted_count = exp(fit_link),
        lower_ci = exp(fit_link - 1.96 * se_link),
        upper_ci = exp(fit_link + 1.96 * se_link),
        date = as.Date(doy, origin = paste0(year(Sys.Date()), "-01-01")) # Dummy year for plotting
      )
  }, error = function(e) return(NULL))
}

message("Fitting GAMs... (This accounts for recording duration)")

# Map over every Country/Habitat combination
plot_predictions <- final_modeling_data %>%
  dplyr::group_by(partner, habitat) %>%
  nest() %>%
  dplyr::mutate(gam_preds = map(data, fit_gam_phenology)) %>%
  dplyr::select(-data) %>%
  unnest(gam_preds)

# -----------------------------------------------------------------#
# 5. VISUALIZATION
# -----------------------------------------------------------------#

phenology_plot <- ggplot(plot_predictions, aes(x = date, y = predicted_count)) +
  
  # Ribbons (CI)
  geom_ribbon(aes(fill = habitat, ymin = lower_ci, ymax = upper_ci), alpha = 0.3) +
  
  # Trend Lines
  geom_line(aes(color = habitat), linewidth = 1) +
  
  # Styles
  scale_color_manual(values = okabe_ito) +
  scale_fill_manual(values = okabe_ito) +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") + 
  
  # Facet Grid
  facet_grid(
    rows = vars(partner), 
    cols = vars(habitat), 
    scales = "free_y",
    labeller = labeller(habitat = habitat_labeller)
  ) +
  
  theme_bw() +
  labs(
    title = "Modeled Bird Activity (March 15 - Sept 30)",
    subtitle = "GAM (Poisson) accounting for variable recording hours",
    y = "Standardized Activity Index (per hour)",
    x = NULL
  ) +
  theme(
    strip.background = element_rect(fill = "#f0f0f0"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

print(phenology_plot)

#ggsave("phenology_gam_duration_corrected.png", plot = phenology_plot, width = 210, height = 297, units = "mm")

# -----------------------------------------------------------------#
# Phenology Analysis in a Loop ----
# -----------------------------------------------------------------#
# -----------------------------------------------------------------#
## 0. Setup & Configuration ----
# -----------------------------------------------------------------#

# (Load libraries: dplyr, lubridate, mgcv, ggplot2, tidyr, readr)

# --- Placeholder: Create recording_metadata from data_filtered ---
recording_metadata <- data_filtered %>%
  dplyr::distinct(sourcefileid, partner, deployment, year, starttime, endtime, date) %>%
  dplyr::mutate(
    duration_hours = (endtime - starttime) / 3600
  )

# --- Loop Configuration ---
n_top_species <- 10
partners_list <- unique(data_filtered$partner)

# --- Get Overall Top Species ---
message(paste("--- Finding", n_top_species, "OVERALL top species... ---"))
overall_top_species_list <- data_filtered %>%
  dplyr::count(species, sort = TRUE) %>%
  dplyr::slice_head(n = n_top_species) %>%
  dplyr::pull(species)
message("...Overall top species found:")
print(overall_top_species_list)

# --- Get Global DOY Range for Consistent VISUALIZATION ---
message("--- Finding Global DOY Range for consistent plot axes... ---")
global_doy_range <- recording_metadata %>%
  dplyr::mutate(doy = lubridate::yday(date)) %>%
  dplyr::summarise(
    min_doy = min(doy, na.rm = TRUE),
    max_doy = max(doy, na.rm = TRUE)
  )
global_min_doy <- global_doy_range$min_doy
global_max_doy <- global_doy_range$max_doy
global_max_doy <- 280
message(paste("... Global DOY range set from", global_min_doy, "to", global_max_doy))

# --- Create empty lists to store results ---
all_phenology_results <- list()
all_prediction_data_list <- list()

# -----------------------------------------------------------------#
## 1. Start Outer Loop (Partners) ----
# -----------------------------------------------------------------#
message(paste("--- Starting Phenology Loop for", length(partners_list), "Partners ---"))

for (current_partner in partners_list) {
  
  message(paste("\n--- Processing Partner:", current_partner, "---"))
  
  # 1a. Find top N species for this partner
  partner_top_species_list <- data_filtered %>%
    dplyr::filter(partner == current_partner) %>%
    dplyr::count(species, sort = TRUE) %>%
    dplyr::slice_head(n = n_top_species) %>%
    dplyr::pull(species)
  
  # 1b. Combine partner list with overall list
  species_to_process <- union(partner_top_species_list, overall_top_species_list)
  
  message(paste(
    "... Found", length(partner_top_species_list), "top species for partner.",
    "Total unique species to process (w/ overall list):", length(species_to_process)
  ))
  
  # 1c. Filter metadata for this partner
  partner_metadata <- recording_metadata %>%
    dplyr::filter(partner == current_partner)
  
  if (nrow(partner_metadata) == 0) {
    message("... Skipping: No recording metadata found for this partner.")
    next
  }
  
  # -----------------------------------------------------------------#
  ## 2. Start Inner Loop (Species) ----
  # -----------------------------------------------------------------#
  
  for (current_species in species_to_process) {
    
    message(paste("... Processing Species:", current_species))
    
    tryCatch({
      
      # 2a. Filter detections
      species_detections <- data_filtered %>%
        dplyr::filter(
          partner == current_partner,
          species == current_species
        )
      
      if (nrow(species_detections) == 0) {
        message("... ... Skipping: No detections found for this species.")
        next
      }
      
      # -----------------------------------------------------------------#
      ## 3. Prepare Data for Modeling (per partner/species) ----
      # -----------------------------------------------------------------#
      
      # A) Calculate TOTAL RECORDING EFFORT per day
      effort_by_day <- partner_metadata %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(
          total_effort_hours = sum(duration_hours, na.rm = TRUE)
        ) %>%
        dplyr::mutate(doy = lubridate::yday(date))
      
      # B) Calculate TOTAL DETECTIONS per day
      detections_by_day <- species_detections %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(
          total_detections = dplyr::n()
        )
      
      # C) Combine effort and detections
      date_range <- seq(min(effort_by_day$date), max(effort_by_day$date), by = "day")
      
      phenology_data <- data.frame(date = date_range) %>%
        dplyr::left_join(effort_by_day, by = "date") %>%
        dplyr::left_join(detections_by_day, by = "date") %>%
        dplyr::mutate(
          total_detections = tidyr::replace_na(total_detections, 0),
          total_effort_hours = tidyr::replace_na(total_effort_hours, 0),
          doy = lubridate::yday(date)
        ) %>%
        dplyr::filter(total_effort_hours > 0) %>%
        dplyr::mutate(
          effort_for_offset = total_effort_hours + 0.001
        )
      
      if (nrow(phenology_data) < 15) {
        message("... ... Skipping: Not enough recording days (<15) to fit a model.")
        next
      }
      
      # -----------------------------------------------------------------#
      ## 4. Run the GAM (The Phenology Indicator) ----
      # -----------------------------------------------------------------#
      
      # 'k' is based on the unique days *in the local data*
      k_val <- min(20, length(unique(phenology_data$doy)) - 1)
      
      if (k_val < 3) {
        message("... ... Skipping: Not enough unique days to set GAM knots.")
        next
      }
      
      # The model is fit *only* to the local phenology_data
      pheno_model <- mgcv::gam(
        total_detections ~ s(doy, bs = "tp", k = k_val),
        data = phenology_data,
        family = "poisson",
        offset = log(effort_for_offset)
      )
      
      # -----------------------------------------------------------------#
      ## 5. Create & Save Plot (*** MODIFIED ***) ----
      # -----------------------------------------------------------------#
      
      # Create the prediction sequence based *only* on the local data
      local_doy_sequence <- seq(min(phenology_data$doy), max(phenology_data$doy), by = 1)
      
      prediction_data <- data.frame(
        doy = local_doy_sequence,
        effort_for_offset = 1
      )
      
      predictions <- predict(
        pheno_model,
        newdata = prediction_data,
        type = "link",
        se.fit = TRUE
      )
      
      # Remember to use as.numeric() to ensure bind_rows() works later!
      prediction_data <- prediction_data %>%
        dplyr::mutate(
          predicted_rate = as.numeric(exp(predictions$fit)),
          se_high = as.numeric(exp(predictions$fit + 2 * predictions$se.fit)),
          se_low = as.numeric(exp(predictions$fit - 2 * predictions$se.fit))
        )
      
      # --- NEW: CALCULATE MAX Y-AXIS VALUE ---
      # The maximum Y should be the greater of:
      # 1. The highest raw data point (Rate)
      # 2. The highest point of the predicted upper confidence interval (se_high)
      max_raw_rate <- max(phenology_data$total_detections / phenology_data$effort_for_offset, na.rm = TRUE)
      max_ci_rate <- max(prediction_data$se_high, na.rm = TRUE)
      
      # We use 'max' to find the true ceiling, then add a small buffer (1.1x)
      max_y_value <- max(max_raw_rate, max_ci_rate) * 1.1
      
      
      # Create plot
      phenology_plot <- ggplot2::ggplot(prediction_data, ggplot2::aes(x = doy)) +
        ggplot2::geom_ribbon(
          ggplot2::aes(ymin = se_low, ymax = se_high),
          fill = "skyblue",
          alpha = 0.4
        ) +
        ggplot2::geom_line(
          ggplot2::aes(y = predicted_rate),
          color = "blue",
          linewidth = 1
        ) +
        ggplot2::geom_point(
          data = phenology_data,
          ggplot2::aes(y = total_detections / effort_for_offset),
          alpha = 0.2,
          color = "grey30"
        ) +
        ggplot2::labs(
          title = paste("Phenology:", current_species),
          subtitle = paste("Partner:", current_partner, "| Modeled with GAM"),
          x = "Day of Year (DOY)",
          y = "Predicted Detections / Hour"
        ) +
        ggplot2::theme_minimal() +
        
        # --- Apply the enforced Y-Axis limit ---
        ggplot2::ylim(c(0, max_y_value)) +
        
        # Force the plot's X-AXIS to the GLOBAL range for visualization
        ggplot2::coord_cartesian(xlim = c(global_min_doy, global_max_doy))
      
      
      # Create a clean filename
      clean_species_name <- gsub("[^a-zA-Z0-9_]", "-", current_species)
      plot_filename <- paste0(
        "Outputs/Figures/Phenology/phenology_",
        current_partner,
        "_",
        clean_species_name,
        ".png"
      )
      
      # Save the plot
      ggplot2::ggsave(
        plot_filename,
        phenology_plot,
        width = 10,
        height = 6,
        bg = "white"
      )
      
      # -----------------------------------------------------------------#
      ## 6. Extract & Store Metrics ----
      # -----------------------------------------------------------------#
      # (This section is now correct, as 'prediction_data' is local)
      
      peak_rate_value <- max(prediction_data$predicted_rate, na.rm = TRUE)
      
      peak_activity_day <- prediction_data %>%
        dplyr::filter(predicted_rate == peak_rate_value) %>%
        dplyr::slice(1)
      
      season_threshold_value <- 0.10 * peak_rate_value
      
      season_dates <- prediction_data %>%
        dplyr::filter(predicted_rate >= season_threshold_value)
      
      if (nrow(season_dates) == 0) {
        season_onset_doy <- NA
        season_end_doy <- NA
      } else {
        season_onset_doy <- min(season_dates$doy)
        season_end_doy <- max(season_dates$doy)
      }
      
      # Create a one-row data frame with the results
      result_row <- data.frame(
        partner = current_partner,
        species = current_species,
        peak_doy = peak_activity_day$doy,
        peak_rate_per_hour = peak_activity_day$predicted_rate,
        season_onset_doy = season_onset_doy,
        season_end_doy = season_end_doy,
        model_k_value = k_val,
        n_detections = nrow(species_detections)
      )
      
      # Add this row to our big results list
      all_phenology_results[[length(all_phenology_results) + 1]] <- result_row
      
      # --- Store the plot data (now correctly local) ---
      prediction_data_to_save <- prediction_data %>%
        dplyr::mutate(
          partner = current_partner,
          species = current_species
        )
      
      # Add this data frame to our big list
      all_prediction_data_list[[length(all_prediction_data_list) + 1]] <- prediction_data_to_save
      
      
      message("... ... Success. Plot saved and metrics recorded.")
      
    }, error = function(e) {
      message(paste("... ... ERROR for", current_species, ":", e$message))
    }) # End of tryCatch
    
  } # End of Inner Loop (Species)
  
} # End of Outer Loop (Partners)

# -----------------------------------------------------------------#
## 7. Compile and Save Final CSV ----
# -----------------------------------------------------------------#
message("\n--- Loop Complete. Compiling final results. ---")

# Combine all the one-row data frames from the list into one big table
final_phenology_summary <- dplyr::bind_rows(all_phenology_results)

# Save the final CSV
readr::write_csv(
  final_phenology_summary,
  "Outputs/Results/phenology_summary_all.csv"
)
message("All results saved to Outputs/Results/phenology_summary_all.csv")

# --- Compile and save all plot data ---
message("Compiling and saving all plot data points...")
final_plot_data <- dplyr::bind_rows(all_prediction_data_list)
readr::write_csv(
  final_plot_data,
  "Outputs/Results/phenology_plot_data_all.csv"
)
message("All plot data saved to Outputs/Results/phenology_plot_data_all.csv")

# -----------------------------------------------------------------#
# END SCRIPT ----
# -----------------------------------------------------------------#