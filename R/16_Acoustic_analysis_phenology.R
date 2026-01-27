# -----------------------------------------------------------------#
## Phenology Analysis with Effort Control (GAM) ----
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
### 1. Choose Target Species ----
# -----------------------------------------------------------------#
target_species <- top_species_names[1]

# Filter detections for *only* our target species
species_detections <- data_filtered %>%
  # dplyr::filter(partner == "czech_republic") %>%
  dplyr::filter(species == target_species)

# -----------------------------------------------------------------#
### 2. Prepare Data for Modeling ----
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
### 3. Run the GAM (The Phenology Indicator) ----
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
### 4. Plot the Phenology Curve ----
# -----------------------------------------------------------------#
message("Generating phenology plot...")

# A) Create a "prediction" data frame for a smooth curve
# We want to predict the rate for every day, assuming 1 hour of effort
doy_sequence <- seq(min(phenology_data$doy), max(phenology_data$doy), by = 1)
prediction_data <- data.frame(
  doy = doy_sequence,
  effort_for_offset = 1 # Predict the rate per *1 hour* of effort
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
  ggplot2::theme_bw()

# Save the plot
ggplot2::ggsave(
  "Outputs/Figures/phenology_curve.png",
  phenology_plot,
  width = 10,
  height = 6
)
# print(phenology_plot)

# -----------------------------------------------------------------#
### 5. Extract Derived Metrics (Peak, Onset, etc.) ----
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
## Grouped Phenology (Day of Year 120-260) ----
# -----------------------------------------------------------------#

# 1. Prepare Data with DOY filtering
# We calculate Day of Year (doy) and filter immediately
season_filtered_data <- data_presence_per_file %>%
  dplyr::mutate(doy = lubridate::yday(date)) %>%
  dplyr::filter(doy >= 120 & doy <= 260) %>%
  dplyr::left_join(
    .,
    partner_country,
    by = c("partner" = "partner")
  )

# A. Identify Countries with enough data
# We count distinct DOYs to ensure timeline coverage
valid_countries <- season_filtered_data %>%
  dplyr::group_by(partner, habitat) %>%
  dplyr::summarise(recording_days = dplyr::n_distinct(doy)) %>% # Changed to count distinct DOYs
  dplyr::filter(recording_days >= 80) %>% # The Constraint
  dplyr::group_by(partner) %>%
  dplyr::summarise(valid_habitat = dplyr::n_distinct(habitat)) %>%
  dplyr::filter(valid_habitat >= 2) %>%
  dplyr::pull(partner)

print(paste("Countries included:", paste(valid_countries, collapse = ", ")))

# B. Filter and Aggregate the main dataset
plot_data <- season_filtered_data %>%
  dplyr::filter(partner %in% valid_countries) %>%
  dplyr::filter(habitat %in% c("F", "G", "W")) %>%
  # IMPORTANT: Group by 'doy' now, not 'date'.
  # This combines multiple years into one seasonal curve.
  dplyr::group_by(country, habitat, doy) %>%
  dplyr::reframe(
    total_detections = dplyr::n()
  )

# --- 3. THE VISUALIZATION (FACET GRID) ---

# Define a "Labeller" to convert codes to names in the plot headers
habitat_labeller <- c(
  "F" = "Forest",
  "G" = "Grassland",
  "W" = "Wetland"
)

# Define the North-to-South order
# (Update this list with the actual countries in your dataset)
ns_order <- c(
  "Finland", "Sweden", "Denmark", "Netherlands", "Belgium",
  "Slovakia", "Croatia", "Bulgaria", "Spain"
)

# Update the factor levels in the data
plot_data$country <- factor(plot_data$country, levels = ns_order)

phenology_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = doy, y = total_detections)) +

  # Apply Color and Fill based on Habitat
  ggplot2::geom_smooth(
    ggplot2::aes(color = habitat, fill = habitat),
    method = "loess",
    span = 0.3,
    se = TRUE,
    alpha = 0.3
  ) +

  # Apply the Okabe-Ito Palette
  ggplot2::scale_color_manual(values = okabe_ito) +
  ggplot2::scale_fill_manual(values = okabe_ito) +

  # Facet Grid
  ggplot2::facet_grid(
    rows = ggplot2::vars(country),
    cols = ggplot2::vars(habitat),
    scales = "free_y",
    labeller = ggplot2::labeller(habitat = habitat_labeller)
  ) +

  # Formatting
  ggplot2::theme_bw() +
  ggplot2::labs(
    # title = "Relative Bird Activity",
    y = "Relative Activity Index",
    x = "Day of Year"
  ) +
  ggplot2::theme(
    # --- CHANGE 2: Remove grey background ---
    # fill = "white" removes the grey. color = "black" keeps the border box.
    strip.background = ggplot2::element_rect(fill = "white", color = "black"),
    strip.text = ggplot2::element_text(face = "bold", size = 10),
    axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5), # Angle 0 is usually readable for numbers
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "none"
  )

# Display the plot
print(phenology_plot)

# --- 4. EXPORT TO A4 PORTRAIT ---
ggsave("Outputs/Figures/phenology_facet.png", plot = phenology_plot, width = 210, height = 297, units = "mm")

## Grouped GAM Phenology ----
# A. Filter Date Window & Habitats
seasonal_data_effort <- acoustic_data %>%
  dplyr::filter(format(date, "%m-%d") >= "03-15" & format(date, "%m-%d") <= "09-30") %>%
  dplyr::mutate(habitat = as.character(stringr::str_sub(deployment, 1, 1))) %>%
  dplyr::filter(habitat %in% c("F", "G", "W"))

seasonal_data <- data_presence_per_file %>%
  dplyr::filter(format(date, "%m-%d") >= "03-15" & format(date, "%m-%d") <= "09-30") %>%
  dplyr::filter(habitat %in% c("F", "G", "W"))

# B. Calculate EFFORT (Duration in Hours)
# This calculation remains unchanged as it counts all recording duration
effort_by_day <- seasonal_data_effort %>%
  dplyr::distinct(sourcefileid, partner, deployment, habitat, starttime, endtime, date) %>%
  dplyr::mutate(duration_hours = (endtime - starttime) / 3600) %>%
  dplyr::group_by(partner, habitat, deployment, date) %>%
  dplyr::summarise(total_effort_hours = n(), .groups = "drop") %>%
  dplyr::group_by(partner, habitat, date) %>%
  dplyr::summarise(total_effort_hours = sum(total_effort_hours, na.rm = TRUE), .groups = "drop")

# C. Calculate DETECTIONS (FIX: Total Community Count)
detections_by_day <- seasonal_data %>%
  # *** NO SPECIES FILTER HERE ***
  dplyr::group_by(partner, habitat, date) %>%
  dplyr::summarise(
    total_detections = dplyr::n(), # Counts ALL rows/detections
    .groups = "drop"
  )

# D. COMBINE Effort and Detections
model_input_data <- detections_by_day %>%
  dplyr::left_join(effort_by_day, by = c("partner", "habitat", "date")) %>%
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
  dplyr::group_by(partner, habitat) %>%
  dplyr::summarise(recording_days = dplyr::n_distinct(doy)) %>% # Changed to count distinct DOYs
  dplyr::filter(recording_days >= 80) %>% # The Constraint
  dplyr::group_by(partner) %>%
  dplyr::summarise(valid_habitat = dplyr::n_distinct(habitat)) %>%
  dplyr::filter(valid_habitat >= 2) %>%
  dplyr::pull(partner)

message("Countries included: ", paste(valid_countries, collapse = ", "))

# Filter the dataset for modeling
final_modeling_data <- model_input_data %>%
  dplyr::filter(partner %in% valid_countries) %>%
  dplyr::filter(doy >= 120 & doy <= 260)


final_modeling_data$partner_habitat <-
  interaction(
    final_modeling_data$partner,
    final_modeling_data$habitat,
    drop = TRUE
  )

# -----------------------------------------------------------------#
# 4. ITERATIVE GAM MODELING (High Flexibility)
# -----------------------------------------------------------------#

fit_gam_phenology <- function(df) {
  # Safety check
  if (nrow(df) < 15) {
    return(NULL)
  }

  tryCatch(
    {
      # 1. Fit the Model - HIGH FLEXIBILITY VERSION
      # bs = "ad" (Adaptive smooth): Allows the curve to be very wiggly in active periods
      # and smoother in quiet periods. Ideally suited for phenology peaks.
      # k = 50: High basis dimension to capture fine-scale variation (weekly peaks)

      m <- mgcv::gam(
        total_detections ~
          s(doy, bs = "tp", k = 50),
        data = df,
        family = mgcv::nb(), # Negative Binomial (handles overdispersion)
        offset = log(effort_for_offset),
        method = "fREML"
      )

      # 2. Predict for Standardized Effort
      pred_grid <- data.frame(
        doy = seq(min(df$doy), max(df$doy), length.out = 100),
        effort_for_offset = 0.2 # Standardize to 1 recording per 5 mins
      )

      preds <- predict(m, newdata = pred_grid, type = "link", se.fit = TRUE)

      # 3. Return formatted results
      pred_grid %>%
        dplyr::mutate(
          fit_link = preds$fit,
          se_link = preds$se.fit,
          predicted_count = exp(fit_link),
          lower_ci = exp(fit_link - 1.96 * se_link),
          upper_ci = exp(fit_link + 1.96 * se_link),
          date = as.Date(doy, origin = paste0(lubridate::year(Sys.Date()), "-01-01"))
        )
    },
    error = function(e) {
      message("GAM Fit Error: ", e$message)
      return(NULL)
    }
  )
}

message("Fitting High-Flexibility GAMs (Adaptive Spline, k=50)...")

# Run the mapping (Same as your original code)
plot_predictions <- final_modeling_data %>%
  dplyr::group_by(partner, habitat) %>%
  tidyr::nest() %>%
  dplyr::mutate(gam_preds = purrr::map(data, fit_gam_phenology)) %>%
  dplyr::select(-data) %>%
  tidyr::unnest(gam_preds) %>%
  dplyr::left_join(
    .,
    partner_country,
    by = c("partner" = "partner")
  )

# Define the North-to-South order
# (Update this list with the actual countries in your dataset)
ns_order <- c(
  "Finland", "Sweden", "Denmark", "Netherlands", "Flanders",
  "Czechia", "Slovakia", "Bolzano", "Croatia", "Bulgaria", "Catalonia"
)

# Update the factor levels in the data
plot_predictions$country <- factor(plot_predictions$country, levels = ns_order)

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
    rows = vars(country),
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
  ggplot2::theme(
    # --- CHANGE 2: Remove grey background ---
    # fill = "white" removes the grey. color = "black" keeps the border box.
    strip.background = ggplot2::element_rect(fill = "white", color = "black"),
    strip.text = ggplot2::element_text(face = "bold", size = 10),
    axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5), # Angle 0 is usually readable for numbers
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "none"
  )

print(phenology_plot)

ggsave("Outputs/Figures/phenology_gam_duration_corrected.png", plot = phenology_plot, width = 210, height = 297, units = "mm")

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

    tryCatch(
      {
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
          ggplot2::theme_bw() +

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
      },
      error = function(e) {
        message(paste("... ... ERROR for", current_species, ":", e$message))
      }
    ) # End of tryCatch
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
