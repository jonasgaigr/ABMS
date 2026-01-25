# ----------------------------------------------------------------- #
## Rarefaction and Extrapolation ---- 
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
