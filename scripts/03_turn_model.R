library(tidyverse)

tracking_movement_data <- read_rds("assets/tracking_movement_data.rds")

library(brms)

angle_brms <- brm(
  bf(
    turn_angle ~ prev_angle + adj_bc_x + adj_bc_y + adj_bc_x_from_first_down +
      n_left_bc_defense + n_front_bc_defense + n_left_bc_offense + n_front_bc_offense +
      adj_x + adj_y + adj_x_change + adj_y_change_abs + dist_to_bc + def_s + angle_with_bc,
    kappa ~ step_length + (1 | bc_id),
    decomp = "QR"
  ),
  family = von_mises(),
  chains = 4,
  iter = 5000,
  warmup = 2500,
  seed = 12,
  cores = 4,
  init = "0",
  control = list(adapt_delta = 0.9),
  backend = "cmdstanr",
  data = tracking_movement_data
)

write_rds(angle_brms, "assets/turn_model.rds", compress = "gz")