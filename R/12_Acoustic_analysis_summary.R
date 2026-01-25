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
## Plot 1: Confidence Score Distribution (on *original* data) ----
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
    vjust = 2, # Move text down from the top
    color = "red",
    fontface = "bold"
  ) +
  ggplot2::labs(
    title = "Distribution of All Confidence Scores",
    subtitle = "Red dashed line shows the 0.7 threshold",
    x = "Confidence Score",
    y = "Count of Detections"
  ) +
  ggplot2::theme_bw()

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
## Plot 2.1.1: Top Species ----
# ----------------------------------------------------------------- #
# What are the most common species found, stacked by confidence?

message("Generating Plot 2: Top Species (Stacked)...")

# Set how many top species you want to see
top_n_species <- 20

# 1. Find the *names* of the top 20 species based on *total* count
top_species_names <- data_filtered %>%
  dplyr::count(species_name, sort = TRUE) %>%
  dplyr::slice_head(n = top_n_species) %>%
  dplyr::pull(species_name) # pull() extracts just the 'species' column as a vector

# 2. Create the summary data for plotting
# Filter for *only* those top species, then count detections *within each bin*
top_species_data <- data_filtered %>%
  dplyr::filter(species_name %in% top_species_names) %>%
  dplyr::count(species_name, confidence_bin, name = "n")

# 3. Create the stacked bar plot
# We map 'fill' to 'confidence_bin'
top_species_plot <- ggplot2::ggplot(
  top_species_data,
  # x-axis: Reorder 'species' factor by the SUM of 'n' (total count)
  # y-axis: 'n' (the count for each bin)
  # fill: 'confidence_bin' to create the stacks
  ggplot2::aes(
    x = forcats::fct_reorder(species_name, n, .fun = sum),
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
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(face = "italic") # Italicize species names
  )

# print(top_species_plot)
ggplot2::ggsave(
  "Outputs/Figures/2_1_1_top_species_plot_stacked.png", # Changed filename
  top_species_plot,
  width = 11, # Increased width slightly for legend
  height = 7
)

message("Stacked species plot saved.")

# ----------------------------------------------------------------- #
## Plot 2.1.2: Top Species ----
# ----------------------------------------------------------------- #
# What are the most common species found, stacked by confidence?

message("Generating Plot 2: Top Species (Stacked)...")

# Set how many top species you want to see
top_n_species <- 20

# 1. Find the *names* of the top 20 species based on *total* count
top_species_names <- data_presence_per_file %>%
  dplyr::count(species_name, sort = TRUE) %>%
  dplyr::slice_head(n = top_n_species) %>%
  dplyr::pull(species_name) # pull() extracts just the 'species' column as a vector

# 2. Create the summary data for plotting
# Filter for *only* those top species, then count detections *within each bin*
top_species_data <- data_filtered %>%
  dplyr::filter(species_name %in% top_species_names) %>%
  dplyr::count(species_name, confidence_bin, name = "n")

# 3. Create the stacked bar plot
# We map 'fill' to 'confidence_bin'
top_species_plot <- ggplot2::ggplot(
  top_species_data,
  # x-axis: Reorder 'species' factor by the SUM of 'n' (total count)
  # y-axis: 'n' (the count for each bin)
  # fill: 'confidence_bin' to create the stacks
  ggplot2::aes(
    x = forcats::fct_reorder(species_name, n, .fun = sum),
    y = n
    # fill = confidence_bin,
  )
) +
  ggplot2::geom_col(fill = "#009E73") + # geom_col() is correct for stacked bars (position="stack" is default)
  ggplot2::coord_flip() + # Flip coordinates so names are readable
  # ggplot2::scale_fill_brewer(palette = "cool", direction = -1) + # Use a nice color scale
  ggplot2::labs(
    title = paste("Top", top_n_species, "Most Frequent Species"),
    x = "Species",
    y = "Number of Detections"
    # fill = "Confidence Bin" # Legend title
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(face = "italic"), # Italicize species names
    legend.position = "none"
  )

# print(top_species_plot)
ggplot2::ggsave(
  "Outputs/Figures/2_1_2_top_species_plot_stacked.png", # Changed filename
  top_species_plot,
  width = 11, # Increased width slightly for legend
  height = 7
)

message("Stacked species plot saved.")

# ----------------------------------------------------------------- #
## Plot 2.2: Top RL Species (MODIFIED for stacked bars) ----
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
top_species_plot_rl <- ggplot(
  top_species_data_rl,
  aes(x = species, y = n, fill = confidence_bin)
) +
  geom_col() +
  coord_flip() +
  facet_wrap(~europeanRegionalRedListCategory, scales = "free_y", ncol = 2) +
  scale_fill_brewer(palette = "Set2", direction = 1) +
  labs(
    title = paste("Top", top_n_species, "species per Red-List category"),
    x = "Species",
    y = "Number of detections",
    fill = "Confidence bin"
  ) +
  scale_y_log10() +
  theme_bw() +
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
## Plot 2.3: Top Species by partner ----
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
    theme_bw() +
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
## Plot 2.4.1: Top Species by habitat ----
# ----------------------------------------------------------------- #
# small helper: generate n shades from very light to base colour
make_shades <- function(base_col, n) {
  # gradient from a very light grey to the base colour
  grDevices::colorRampPalette(c("grey95", base_col))(n)
}

top_n_species <- 10
habitats <- c("F", "G", "W")
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
    count(species_name, sort = TRUE) %>%
    slice_head(n = top_n_species) %>%
    pull(species_name)

  top_species_data <- df_h %>%
    filter(species_name %in% top_species_names) %>%
    count(species_name, confidence_bin, name = "n") %>%
    group_by(species_name) %>%
    mutate(total = sum(n)) %>%
    ungroup() %>%
    mutate(species = fct_reorder(species_name, n, .fun = sum))

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
    arrange(lower) %>% # ascending: low -> high
    pull(bin)

  if (length(bin_levels) == 0) {
    message("No confidence bins for habitat ", h, " — skipping")
    next
  }

  # create shades for this habitat (darkest = base colour -> map to the highest-confidence bin)
  shades <- make_shades(okabe_ito[h], length(bin_levels))
  names(shades) <- bin_levels # map names to bin values

  p <- ggplot(top_species_data, aes(x = species, y = n, fill = confidence_bin)) +
    geom_col() +
    coord_flip(expand = FALSE) +
    scale_fill_manual(values = shades, na.value = "grey60", drop = FALSE) +
    labs(
      title = paste0(habitat_name),
      x = "Species",
      y = "Detections",
      fill = "Confidence bin"
    ) +
    theme_bw(base_size = 11) +
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
  legends_grobs[[h]] <- cowplot::get_legend(p + theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.key.size = unit(0.6, "lines")
  ))
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
  rel_widths = c(1, 1, 1)
)

# now create a single legend row by placing each legend grob side by side
# convert grobs to ggdraw objects for consistent plotting
legend_plots <- lapply(legends_grobs, function(g) {
  if (is.null(g)) {
    return(NULL)
  }
  cowplot::ggdraw(g)
})

# keep only non-null legends and align them

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
## Plot 2.4.2: Top Species by habitat (Simplified - No Bins) ----
# ----------------------------------------------------------------- #
top_n_species <- 10
habitats <- c("F", "G", "W")
plots <- list()

# Define custom colors to match the image
habitat_colors <- c(
  F = "#009E73", # Forest Green
  G = "#E69F00", # Grassland Orange/Gold
  W = "#56B4E9" # Wetland Sky Blue
)

habitat_labels <- c(F = "Forest", G = "Grassland", W = "Wetland", O = "Other")

for (h in habitats) {
  df_h <- data_presence_per_file %>% filter(habitat == h)
  if (nrow(df_h) == 0) {
    message("No data for habitat ", h, " — skipping")
    next
  }

  habitat_name <- habitat_labels[h]
  if (is.na(habitat_name)) habitat_name <- h

  # 1. Identify Top Species
  top_species_names <- df_h %>%
    count(species_name, sort = TRUE) %>%
    slice_head(n = top_n_species) %>%
    pull(species_name)

  # 2. Prepare Data
  top_species_data <- df_h %>%
    filter(species_name %in% top_species_names) %>%
    count(species_name, name = "n") %>%
    mutate(species_name = str_wrap(species_name, width = 12)) %>%
    mutate(species = fct_reorder(species_name, n)) %>%
    # Add a column for faceting to get the boxed title
    mutate(habitat_facet = habitat_name)

  # 3. Plot
  p <- ggplot(top_species_data, aes(x = species, y = n)) +
    # Use custom color fill with a thin black border
    geom_col(fill = habitat_colors[h]) +
    coord_flip(expand = FALSE) +
    # Use facet_grid to create the boxed title effect
    facet_grid(~habitat_facet) +
    labs(
      x = NULL, # No label for species names
      y = "Number of detections" # Match image label
    ) +
    # Use theme_bw for the boxed plot area and grid lines
    theme_bw(base_size = 12) +
    theme(
      # Italicize species names
      axis.text.y = element_text(face = "italic", color = "black", size = 14),
      # fill = "white" removes the grey. color = "black" keeps the border box.
      strip.background = ggplot2::element_rect(fill = "white", color = "black"),
      strip.text = ggplot2::element_text(face = "bold", size = 14),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5), # Angle 0 is usually readable for numbers
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "none"
    )

  # store plot
  plots[[h]] <- p
}

# 4. Combine (Simple Grid)
final_plot <- cowplot::plot_grid(
  plots[["F"]],
  plots[["G"]],
  plots[["W"]],
  ncol = 3,
  align = "hv"
)

# 5. Save
ggsave(
  filename = "Outputs/Figures/2_4_2_top_species_by_habitat_formatted.png",
  plot = final_plot,
  width = 18,
  height = 6,
  dpi = 300,
  units = "in",
  bg = "white"
)
message("Saved simplified habitat plot (solid bars, no bins).")

# ----------------------------------------------------------------- #
## Plot 3: Detections per Recording (on *filtered* data) ----
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
  # ggplot2::scale_y_log10() + # Use a log scale if distribution is heavily skewed
  ggplot2::theme_bw()

# print(detections_per_file_hist)
ggplot2::ggsave(
  "Outputs/Figures/3_detections_per_file_hist.png",
  detections_per_file_hist,
  width = 8,
  height = 5
)

# ----------------------------------------------------------------- #
## Summary Table by Group (e.g., deployment) ----
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
### Save All Summary Tables ----
# ----------------------------------------------------------------- #

# --- Define Output Filenames ---
# We define clear, distinct names for each output file.
file_sitenational <- "Outputs/Results/deployment_summary_sitenational"
file_national <- "Outputs/Results/deployment_summary_national"
file_site <- "Outputs/Results/deployment_summary_site"


# ----------------------------------------------------------------- #
### 1. Save all summaries as CSV files ----
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
### 2. Save all summaries as Word (.docx) Tables ----
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
