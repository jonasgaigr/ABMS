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
    #  F_050 > F_095 & F_050 > F_090 & F_050 > F_070 ~ Threshold_050,
    #  F_070 > F_095 & F_070 > F_090 & F_070 > F_050 ~ Threshold_070,
    #  F_090 > F_050 & F_090 > F_070 & F_090 > F_095 ~ Threshold_090,
    #  F_095 > F_050 & F_095 > F_070 & F_095 > F_090 ~ Threshold_095
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
      TRUE ~ bin_levels[3] # All remaining (0.50 - 0.699...)
    ),
    # Convert 'confidence_bin' to a factor to control stacking order
    confidence_bin = factor(confidence_bin, levels = bin_levels)
  ) %>%
  dplyr::mutate(
    habitat = as.character(stringr::str_sub(deployment, 1, 1))
  )

message("Data filtered and binned successfully.")

# ----------------------------------------------------------------- #
## Filter: Keep One Detection Per Species Per Recording ----
# ----------------------------------------------------------------- #

message("Original row count: ", nrow(data_filtered))

data_presence_per_file <- data_filtered %>%
  # 1. Group by the unique recording identifier AND species
  # We use partner + deployment + filename to ensure uniqueness across dataset
  dplyr::group_by(filename, species_name) %>%
  
  # 2. Keep only the single best detection (highest confidence)
  # with_ties = FALSE ensures we strictly get 1 row even if two have same score
  dplyr::slice_max(confidence, n = 1, with_ties = FALSE) %>%
  
  # 3. Ungroup to return to a normal data frame
  dplyr::ungroup()

message("New row count (Presence per file): ", nrow(data_presence_per_file))

# Optional: Check the result
# This should now be 1 for every row
data_presence_per_file %>% count(partner, deployment, filename, species_name) %>% pull(n) %>% max()
