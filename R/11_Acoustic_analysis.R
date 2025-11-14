# ----------------------------------------------------------------- #
# Exploratory Data Analysis (EDA) of Acoustic Detections

# -----------------------------------------------------------------#
# 1. Setup & Filtering----
# -----------------------------------------------------------------#

# Define your confidence threshold
confidence_threshold <- 0.7

# Create a filtered data frame. This will be the basis for most analysis.
# We keep only rows with confidence >= the threshold.
message("Filtering data and creating confidence bins...")

# Define the order of bins for the plot legend
# We do this so "> 0.95" appears on top, not alphabetically
bin_levels <- c("> 0.95", "0.85 - 0.95", "0.70 - 0.85")

# Create a filtered data frame with an added 'confidence_bin' column
data_filtered <- acoustic_data %>%
  dplyr::filter(confidence >= confidence_threshold) %>%
  dplyr::mutate(
    confidence_bin = dplyr::case_when(
      confidence >= 0.95 ~ bin_levels[1],
      confidence >= 0.85 ~ bin_levels[2],
      TRUE ~ bin_levels[3] # All remaining (0.70 - 0.849...)
    ),
    # Convert 'confidence_bin' to a factor to control stacking order
    confidence_bin = factor(confidence_bin, levels = bin_levels)
  )

message("Data filtered and binned successfully.")

# ----------------------------------------------------------------- #
# 3. High-Level Summaries ----
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
print(nrow(data))

# ----------------------------------------------------------------- #
# 4. Plot 1: Confidence Score Distribution (on *original* data) ----
# ----------------------------------------------------------------- #
# This helps you see if your 0.7 threshold is reasonable.

message("Generating Plot 1: Confidence Distribution...")

confidence_histogram <- ggplot2::ggplot(data, ggplot2::aes(x = confidence)) +
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
# 5. Plot 2: Top Species (MODIFIED for stacked bars)
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
    subtitle = paste("Based on detections with confidence >=", confidence_threshold),
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
  "Outputs/Figures/2_top_species_plot_stacked.png", # Changed filename
  top_species_plot,
  width = 11, # Increased width slightly for legend
  height = 7
)

message("Stacked species plot saved.")

# ----------------------------------------------------------------- #
# 6. Plot 3: Detections per Recording (on *filtered* data) ----
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
# 7. Summary Table by Group (e.g., deployment) ----
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

# Print the summary table
print(deployment_summary)

# ----------------------------------------------------------------- #
# Save All Summary Tables ---- 
# ----------------------------------------------------------------- #

# 1. Install required packages (if you don't have them)
# install.packages("readr")     # For saving CSV
# install.packages("flextable") # For creating the table object
# install.packages("officer")   # For saving as .docx

# 2. Load the libraries
library(readr)
library(flextable)
library(officer)

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
# 1. Summarize the filtered data to get detection counts per species per site
# We use the filtered data (confidence >= 0.7)
species_counts_wide <- data_filtered %>%
  # Group by the desired comparison level (e.g., deployment) and species
  dplyr::group_by(deployment, species) %>%
  dplyr::summarise(
    n = dplyr::n(),
    .groups = 'drop'
  ) %>%
  # Convert long format to wide format (Species-by-Sample matrix)
  tidyr::pivot_wider(
    id_cols = deployment,
    names_from = species,
    values_from = n,
    values_fill = 0
  ) %>%
  # Move the 'deployment' column into the row names for the matrix format
  tibble::column_to_rownames(var = "deployment")

# Convert to a simple matrix (iNEXT/vegan prefer this)
species_count_matrix <- as.matrix(species_counts_wide)

library(vegan)

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

# You must install 'iNEXT' if you haven't already
# install.packages("iNEXT")
library(iNEXT)

# Run iNEXT: Estimates diversity (q=0 for richness) based on standardized effort
# iNEXT uses the count matrix directly.
iNEXT_output <- iNEXT::iNEXT(
  x = species_count_matrix,
  q = 0,               # q=0 is Species Richness (the number of unique species)
  datatype = "abundance" # The input data is counts/abundances
)

iNEXT_output_quick <- iNEXT::iNEXT(
  x = species_count_matrix, # Use your original large matrix
  q = 0,
  datatype = "abundance",
  nboot = 10     # Reduced bootstrap repetitions
)

# Visualize the resulting diversity curves
# This plot will show how species richness changes as sampling effort (number of detections) increases,
# allowing you to see if you have adequately sampled each site.
iNEXT::ggiNEXT(iNEXT_output) +
  ggplot2::labs(
    title = "Rarefaction and Extrapolation Curve (Species Richness)",
    x = "Number of Detections (Standardized Effort)",
    y = "Species Richness (q=0)"
  )

# Use the same 'species_count_matrix' created above.

# Standardize by total site detections (Standardization Method 'total')
# This converts the raw counts to relative proportions within each site.
# This controls for the varying *number of total detections* per site.
standardized_matrix <- vegan::decostand(
  species_count_matrix,
  method = "total"
)

# Run NMDS on the standardized data
nmds_result <- vegan::metaMDS(
  standardized_matrix,
  distance = "bray", # Bray-Curtis distance is standard for abundance data
  k = 2,              # 2 dimensions for easy plotting
  trymax = 100        # Try up to 100 random starts to find a stable solution
)

# Extract coordinates and merge with partner/habitat metadata for plotting
data_scores <- data.frame(vegan::scores(nmds_result, display = "sites")) %>%
  tibble::rownames_to_column(var = "deployment") %>%
  # Merge back with your original data to get the 'partner' column
  dplyr::left_join(
    data_filtered %>% dplyr::select(deployment, partner) %>% dplyr::distinct(),
    by = "deployment"
  )

# Plot the NMDS results
nmds_plot <- ggplot2::ggplot(data_scores, ggplot2::aes(x = NMDS1, y = NMDS2, color = partner)) +
  ggplot2::geom_point(size = 3, alpha = 0.7) +
  ggplot2::stat_ellipse() + # Draw a confidence ellipse around each partner group
  ggplot2::labs(
    title = "Compositional Dissimilarity (NMDS)",
    subtitle = "Standardized by total site detections",
    caption = paste("Stress:", round(nmds_result$stress, 3))
  ) +
  ggplot2::theme_minimal()
