library(tidyverse)

tracking_movement_data <- read_rds("assets/tracking_movement_data.rds")

library(brms)

step_brms <- brm(
  bf(
    step_length_asin ~ prev_step + adj_bc_x + adj_bc_y + adj_bc_x_from_first_down +
      n_left_bc_defense + n_front_bc_defense + n_left_bc_offense + n_front_bc_offense +
      adj_x + adj_y + adj_x_change + adj_y_change_abs + dist_to_bc + def_s + angle_with_bc +
      (1 | bc_id) + (1 | defensiveTeam),
    decomp = "QR"
  ),
  chains = 4,
  iter = 5000,
  warmup = 2500,
  seed = 14,
  cores = 4,
  init = "0",
  control = list(adapt_delta = 0.9),
  backend = "cmdstanr",
  data = tracking_movement_data
)

write_rds(step_brms, "assets/step_model.rds", compress = "gz")