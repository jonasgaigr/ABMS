# ----------------------------------------------------------------- #
# Exploratory Data Analysis (EDA) of Acoustic Detections

# -----------------------------------------------------------------#
# Setup & Filtering----
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
    subtitle = paste("Based on detections with confidence >=", confidence_threshold),
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
      subtitle = paste("Based on detections with confidence ≥", confidence_threshold),
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
# 1. Choose Target Species ----
# -----------------------------------------------------------------#
target_species <- top_species_names[1]

# Filter detections for *only* our target species
species_detections <- data_filtered %>%
  #dplyr::filter(partner == "czech_republic") %>%
  dplyr::filter(species == target_species)

# -----------------------------------------------------------------#
# 2. Prepare Data for Modeling ----
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
# 3. Run the GAM (The Phenology Indicator) ----
# -----------------------------------------------------------------#
message("Fitting GAM model...")

# k = number of "knots" or "basis functions".
# k=20 is a good start for seasonal data (it allows ~19 "wiggles").
pheno_model <- mgcv::gam(
  total_detections ~ s(doy, bs = "cc", k = 20),
  data = phenology_data,
  family = "poisson", # We are modeling count data
  offset = log(effort_for_offset) # This controls for effort!
)

# Use 'bs="cc"' for cyclic cubic splines (DOY 365 wraps to 1)
# Use 'bs="tp"' if your data is not cyclic (e.d. only one season)

# Check the model summary
# print(summary(pheno_model))

# -----------------------------------------------------------------#
# 4. Plot the Phenology Curve ----
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
# 5. Extract Derived Metrics (Peak, Onset, etc.) ----
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
# Phenology Analysis in a Loop ----
# -----------------------------------------------------------------#
# -----------------------------------------------------------------#
# 0. Setup & Configuration ----
# -----------------------------------------------------------------#

# (ASSUMPTION: 'data_filtered' is your main data)
# (ASSUMPTION: 'top_species_names' is your global list, which we won't use)

# --- CRITICAL ASSUMPTION ---
# This script assumes your 'data_filtered' object now has a real 'date' column.
# The fake date simulation block has been REMOVED.
#
# Your data MUST look like this:
# sourcefileid | partner | deployment | ... | species | ... | date (Date)
#
# ---------------------------#

# --- Placeholder: Create recording_metadata from data_filtered ---
# This still assumes 'data_filtered' contains ALL files (even w/ 0 detections)
# If not, you must load your REAL metadata log here.
recording_metadata <- data_filtered %>%
  dplyr::distinct(sourcefileid, partner, deployment, year, starttime, endtime, date) %>%
  dplyr::mutate(
    duration_hours = (endtime - starttime) / 3600
  )

# --- Loop Configuration ---
n_top_species <- 10 # Get top 10 species per partner
partners_list <- unique(data_filtered$partner)

# --- NEW: Get Overall Top Species ---
message(paste("--- Finding", n_top_species, "OVERALL top species... ---"))
overall_top_species_list <- data_filtered %>%
  dplyr::count(species, sort = TRUE) %>%
  dplyr::slice_head(n = n_top_species) %>%
  dplyr::pull(species)

message("...Overall top species found:")
print(overall_top_species_list)

# Create an empty list to store the results from each loop
all_phenology_results <- list()

# -----------------------------------------------------------------#
# 1. Start Outer Loop (Partners) ----
# -----------------------------------------------------------------#
message(paste("--- Starting Phenology Loop for", length(partners_list), "Partners ---"))

for (current_partner in partners_list) {
  
  message(paste("\n--- Processing Partner:", current_partner, "---"))
  
  # 1a. Find the top N species *for this partner*
  partner_top_species_list <- data_filtered %>%
    dplyr::filter(partner == current_partner) %>%
    dplyr::count(species, sort = TRUE) %>%
    dplyr::slice_head(n = n_top_species) %>%
    dplyr::pull(species) # Get species names as a vector
  
  # 1b. (NEW) Combine partner list with overall list
  # We use union() to get a single, unique list of species to process
  species_to_process <- union(partner_top_species_list, overall_top_species_list)
  
  message(paste(
    "... Found", length(partner_top_species_list), "top species for partner.",
    "Total unique species to process (w/ overall list):", length(species_to_process)
  ))
  
  # 1c. Filter the recording metadata *once* for this partner
  partner_metadata <- recording_metadata %>%
    dplyr::filter(partner == current_partner)
  
  if (nrow(partner_metadata) == 0) {
    message("... Skipping: No recording metadata found for this partner.")
    next # Skip to the next partner
  }
  
  # -----------------------------------------------------------------#
  # 2. Start Inner Loop (Species) ----
  # -----------------------------------------------------------------#
  
  # (MODIFIED) This now loops over the combined 'species_to_process' list
  for (current_species in species_to_process) {
    
    # ... (The rest of your script from here down is unchanged) ...
    
    message(paste("... Processing Species:", current_species))
    
    tryCatch({
      
      # 2a. Filter detections for the current partner AND species
      species_detections <- data_filtered %>%
        dplyr::filter(
          partner == current_partner,
          species == current_species
        )
      
      if (nrow(species_detections) == 0) {
        message("... ... Skipping: No detections found for this species.")
        next # Skip to the next species
      }
      
      # -----------------------------------------------------------------#
      # 3. Prepare Data for Modeling (per partner/species) ----
      # -----------------------------------------------------------------#
      
      # A) Calculate TOTAL RECORDING EFFORT per day (for this partner)
      effort_by_day <- partner_metadata %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(
          total_effort_hours = sum(duration_hours, na.rm = TRUE)
        ) %>%
        dplyr::mutate(doy = lubridate::yday(date))
      
      # B) Calculate TOTAL DETECTIONS per day (for this species)
      detections_by_day <- species_detections %>%
        dplyr::group_by(date) %>%
        dplyr::summarise(
          total_detections = dplyr::n()
        )
      
      # C) Combine effort and detections
      # Create a full calendar based on the partner's *actual* recording period
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
      
      # Check if we have enough data points to model
      if (nrow(phenology_data) < 15) {
        message("... ... Skipping: Not enough recording days (<15) to fit a model.")
        next
      }
      
      # -----------------------------------------------------------------#
      # 4. Run the GAM (The Phenology Indicator) ----
      # -----------------------------------------------------------------#
      
      # Dynamically set 'k' (knots) to be less than the number of unique days
      # 'k' must be at least 3
      k_val <- min(20, length(unique(phenology_data$doy)) - 1)
      
      if (k_val < 3) {
        message("... ... Skipping: Not enough unique days to set GAM knots.")
        next
      }
      
      pheno_model <- mgcv::gam(
        total_detections ~ s(doy, bs = "cc", k = k_val),
        data = phenology_data,
        family = "poisson",
        offset = log(effort_for_offset)
      )
      
      # -----------------------------------------------------------------#
      # 5. Create & Save Plot ----
      # -----------------------------------------------------------------#
      
      doy_sequence <- seq(min(phenology_data$doy), max(phenology_data$doy), by = 1)
      prediction_data <- data.frame(
        doy = doy_sequence,
        effort_for_offset = 1
      )
      
      predictions <- predict(
        pheno_model,
        newdata = prediction_data,
        type = "link",
        se.fit = TRUE
      )
      
      prediction_data <- prediction_data %>%
        dplyr::mutate(
          predicted_rate = exp(predictions$fit),
          se_high = exp(predictions$fit + 2 * predictions$se.fit),
          se_low = exp(predictions$fit - 2 * predictions$se.fit)
        )
      
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
        ggplot2::theme_minimal()
      
      # Create a clean filename
      clean_species_name <- gsub("[^a-zA-Z0-9_]", "-", current_species) # Clean non-alphanumeric
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
        bg = "white" # Good for saving PNGs
      )
      
      # -----------------------------------------------------------------#
      # 6. Extract & Store Metrics ----
      # -----------------------------------------------------------------#
      
      peak_rate_value <- max(prediction_data$predicted_rate, na.rm = TRUE)
      
      peak_activity_day <- prediction_data %>%
        dplyr::filter(predicted_rate == peak_rate_value) %>%
        dplyr::slice(1)
      
      season_threshold_value <- 0.10 * peak_rate_value
      
      season_dates <- prediction_data %>%
        dplyr::filter(predicted_rate >= season_threshold_value)
      
      # Handle cases where the season never starts/ends (e.g., flat line)
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
      
      message("... ... Success. Plot saved and metrics recorded.")
      
    }, error = function(e) {
      # This function runs if an error occurs *anywhere* inside the tryCatch
      message(paste("... ... ERROR for", current_species, ":", e$message))
    }) # End of tryCatch
    
  } # End of Inner Loop (Species)
  
} # End of Outer Loop (Partners)

# -----------------------------------------------------------------#
# 7. Compile and Save Final CSV ----
# -----------------------------------------------------------------#
message("\n--- Loop Complete. Compiling final results. ---")

# Combine all the one-row data frames from the list into one big table
final_phenology_summary <- dplyr::bind_rows(all_phenology_results)

# Save the final CSV
readr::write_csv(
  final_phenology_summary,
  "Outputs/Results/phenology_summary_all.csv"
)

message("All results saved to Outputs/phenology_summary_all.csv")
