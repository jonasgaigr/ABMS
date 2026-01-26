# ----------------------------------------------------------------- #
## Species list by site ----
# ----------------------------------------------------------------- #

species_list_deployment <-
  data_filtered %>%
  dplyr::group_by(partner, deployment, species) %>%
  dplyr::reframe(
    mean_confidence = mean(confidence, na.rm = TRUE),
    median_confidence = median(confidence, na.rm = TRUE),
    min_confidence = min(confidence, na.rm = TRUE),
    max_confidence = max(confidence, na.rm = TRUE),
    sd_confidence = sd(confidence, na.rm = TRUE),
    n_recordings = n()
  )

species_list_habitat <-
  data_filtered %>%
  dplyr::group_by(partner, habitat, species) %>%
  dplyr::reframe(
    mean_confidence = mean(confidence, na.rm = TRUE),
    median_confidence = median(confidence, na.rm = TRUE),
    min_confidence = min(confidence, na.rm = TRUE),
    max_confidence = max(confidence, na.rm = TRUE),
    sd_confidence = sd(confidence, na.rm = TRUE),
    n_recordings = n()
  )

# ----------------------------------------------------------------- #
### 1. Save all summaries as CSV files ----
# ----------------------------------------------------------------- #
file_species_list_deployment <- "Outputs/Results/species_list_deployment"
file_species_list_habitat <- "Outputs/Results/species_list_habitat"

message("--- Saving CSV files ---")

# Save the species_list_deployment
readr::write_csv(
  species_list_deployment,
  paste0(file_species_list_deployment, ".csv")
)
message("Saved: ", paste0(file_species_list_deployment, ".csv"))

# Save the species_list_habitat
readr::write_csv(
  species_list_habitat,
  paste0(file_species_list_habitat, ".csv")
)
message("Saved: ", paste0(file_species_list_habitat, ".csv"))
