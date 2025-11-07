# ---------- Parameters ----------
# If retrieval_date is missing we show the bar up to 'as_of' date.
# By default use Prague local today — change if you prefer a fixed date.
as_of <- lubridate::today(tzone = "Europe/Prague")
as_of <- lubridate::as_date("2025-08-31")

# ---------- Prepare data ----------
deploy <- deploy_info %>%
  # trim whitespace from text columns that matter
  mutate(
    partner = stringr::str_trim(partner),
    code = stringr::str_trim(code),
    device_code = stringr::str_trim(device_code),   # this is the device TYPE column (e.g. "Song Meter Mini 2")
    device_id = stringr::str_trim(device_id),
    # convert blanks or obvious formulas to NA
    deployment_date = na_if(deployment_date, ""),
    retrieval_date   = na_if(retrieval_date, ""),
    deployment_date = if_else(deployment_date %in% c("NA", "na"), NA_character_, deployment_date),
    retrieval_date   = if_else(retrieval_date   %in% c("NA", "na"), NA_character_, retrieval_date)
  ) %>%
  # parse dates in d/m/Y (handles most of the input you showed)
  mutate(
    deployment_date = suppressWarnings(lubridate::dmy(deployment_date)),
    retrieval_date  = suppressWarnings(lubridate::dmy(retrieval_date))
  ) %>%
  # if retrieval_date is NA -> treat as ongoing and set to as_of (but keep a flag)
  mutate(
    ongoing = is.na(retrieval_date),
    retrieval_date = if_else(ongoing, as_of, retrieval_date)
  ) %>%
  # create a human-readable label for the y axis: "DEVICEID — TYPE"
  mutate(
    device_label = paste0(device_id, " — ", coalesce(device_code, "")),
    device_label = str_squish(device_label)
  )# %>%
  # drop rows without device_id at all (optional)
  #filter(!is.na(device_id) & device_id != "")

# ---------- Order devices so each partner facet shows the most recent on top ----------
# We'll create a combined id (partner|device_label) and set factor levels by partner & deployment date
deploy_for_plot <- deploy %>%
  arrange(partner, deployment_date %>% coalesce(as.Date("1900-01-01"))) %>%
  mutate(
    pid = paste(partner, code, sep = " - "),
    # order factors so later deployments appear near the top of each facet
    pid = factor(pid, levels = rev(unique(pid)))
  )

# ---------- Static ggplot Gantt chart ----------
p_gantt <- ggplot(deploy_for_plot) +
  geom_segment(
    aes(x = deployment_date, xend = retrieval_date, y = pid, yend = pid, color = device_code),
    size = 6, lineend = "round"
  ) +
  # start/end markers
  geom_point(aes(x = deployment_date, y = pid), size = 1.8, alpha = 0.9) +
  geom_point(aes(x = retrieval_date, y = pid), size = 1.8, alpha = 0.9, shape = 21) +
  # facet per partner (each partner gets its own small y axis)
  facet_wrap(~partner, scales = "free_y", ncol = 2) +
  labs(
    title = "Device deployment Gantt chart",
    subtitle = paste0("Bars show operational period; ongoing deployments shown up to ", as.character(as_of)),
    x = "Date",
    y = NULL,
    color = "Device type"
  ) +
  scale_x_date(
    expand = c(0, 0),
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 8),        # device labels
    strip.text = element_text(face = "bold"),    # partner facet titles
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

# Print static plot
print(p_gantt)

# ---------- Filter only AMI devices ----------
deploy_for_plot_ami <- deploy_for_plot %>%
  filter(device_code == "AMI")   # if your AMI type has a different spelling, change here

date_min <- min(deploy_for_plot_ami$deployment_date, na.rm = TRUE)
date_max <- max(deploy_for_plot_ami$retrieval_date,  na.rm = TRUE)

# add ±7 days
date_min <- date_min - lubridate::days(14)
date_max <- date_max + lubridate::days(14)

# ---------- One long panel, no facets ----------
p_gantt_ami <- ggplot(deploy_for_plot_ami) +
  geom_segment(
    aes(x = deployment_date, xend = retrieval_date, y = pid, yend = pid, color = type_code),
    size = 6, lineend = "round"
  ) +
  scale_color_manual(values = okabe_ito) +
  labs(
    title = "AMI deployment timeline\n",
    x = "\nDate",
    y = NULL,
    color = "Device type: "
  ) +
  scale_x_date(
    limits = c(date_min, date_max),
    expand = c(0, 0),
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

print(p_gantt_ami)

# count number of rows (devices)
n_devices <- nrow(deploy_for_plot_ami)

# choose how much vertical space each device gets
# 0.35–0.5 usually works well depending on label length
height_inches <- n_devices * 0.2

ggsave(
  filename = "Outputs/Plots/ami_deployments.png",
  plot = p_gantt_ami,
  width = 14,              # adjust if needed
  height = height_inches,  # scales automatically
  dpi = 300
)


# ---------- Filter only acoustic devices ----------
deploy_for_plot_acoustic <- deploy_for_plot %>%
  filter(grepl("Mini", device_code))   # if your AMI type has a different spelling, change here

date_min <- min(deploy_for_plot_acoustic$deployment_date, na.rm = TRUE)
date_max <- max(deploy_for_plot_acoustic$retrieval_date,  na.rm = TRUE)

# add ±7 days
date_min <- date_min - lubridate::days(14)
date_max <- date_max + lubridate::days(14)

# ---------- One long panel, no facets ----------
p_gantt_acoustic <- ggplot(deploy_for_plot_acoustic) +
  geom_segment(
    aes(x = deployment_date, xend = retrieval_date, y = pid, yend = pid, color = type_code),
    size = 6, lineend = "round"
  ) +
  scale_color_manual(values = okabe_ito) +
  labs(
    title = "Acoustic deployment timeline\n",
    x = "\nDate",
    y = NULL,
    color = "Device type: "
  ) +
  scale_x_date(
    limits = c(date_min, date_max),
    expand = c(0, 0),
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

print(p_gantt_acoustic)

# count number of rows (devices)
n_devices <- nrow(deploy_for_plot_acoustic)

# choose how much vertical space each device gets
# 0.35–0.5 usually works well depending on label length
height_inches <- n_devices * 0.1

ggsave(
  filename = "Outputs/Plots/acoustic_deployments.png",
  plot = p_gantt_acoustic,
  width = 14,              # adjust if needed
  height = height_inches,  # scales automatically
  dpi = 300
)
