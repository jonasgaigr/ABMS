# ----------------------------------------------------------------- #
# Exploratory Data Analysis (EDA) of Acoustic Detections

# -----------------------------------------------------------------#
# 1. Setup & Filtering----
# -----------------------------------------------------------------#

# Define your confidence threshold
confidence_threshold <- 0.7

# Create a filtered data frame. This will be the basis for most analysis.
# We keep only rows with confidence >= the threshold.
data_filtered <- acoustic_data %>%
  dplyr::filter(confidence >= confidence_threshold)

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
# 5. Plot 2: Top Species (on *filtered* data) ----
# ----------------------------------------------------------------- #
# What are the most common species found?

message("Generating Plot 2: Top Species...")

# Set how many top species you want to see
top_n_species <- 20

# Create a summary data frame of the top N species
top_species_data <- data_filtered %>%
  dplyr::count(species, sort = TRUE) %>%
  dplyr::slice_head(n = top_n_species)

# Create the bar plot
top_species_plot <- ggplot2::ggplot(
  top_species_data,
  # Use forcats::fct_reorder to sort species by count (n)
  ggplot2::aes(x = forcats::fct_reorder(species, n), y = n)
) +
  ggplot2::geom_col(fill = "steelblue") +
  ggplot2::coord_flip() + # Flip coordinates so names are readable
  ggplot2::labs(
    title = paste("Top", top_n_species, "Most Frequent Species"),
    subtitle = paste("Based on detections with confidence >=", confidence_threshold),
    x = "Species",
    y = "Number of Detections"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(face = "italic") # Italicize species names
  )

# print(top_species_plot)
ggplot2::ggsave(
  "Outputs/Figures/2_top_species_plot.png",
  top_species_plot,
  width = 10,
  height = 7
)

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
  ggplot2::scale_y_log10() + # Use a log scale if distribution is heavily skewed
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
