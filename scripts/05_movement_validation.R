library(tidyverse)
library(brms)
library(tidybayes)

step_model <- read_rds("assets/step_model.rds")
turn_model <- read_rds("assets/turn_model.rds")

# model diagnostics

summary(brms::rhat(step_model))
summary(brms::neff_ratio(step_model))

summary(brms::rhat(turn_model))
summary(brms::neff_ratio(turn_model))

pp_check(step_model)
pp_check(turn_model)

# predictive checks for step model

tracking_movement_data <- read_rds("assets/tracking_movement_data.rds")
upper <- max(tracking_movement_data$step_length)
lower <- min(tracking_movement_data$step_length)

step_model |> 
  add_predicted_draws(newdata = tracking_movement_data, ndraws = 25) |> 
  mutate(step_length_pred = (sin(.prediction) ^ 2) * (upper - lower) + lower) |> 
  ggplot() +
  geom_density(aes(x = step_length_pred, group = .draw), color = "lightgray", linewidth = 0.1) +
  geom_density(data = tracking_movement_data, aes(x = step_length)) +
  theme_light()

# step model comparison

# gamma regression
step_model_gamma <- brm(
  bf(
    step_length ~ prev_step + adj_bc_x + adj_bc_y + adj_bc_x_from_first_down +
      n_left_bc_defense + n_front_bc_defense + n_left_bc_offense + n_front_bc_offense +
      adj_x + adj_y + adj_x_change + adj_y_change_abs + dist_to_bc + def_s + angle_with_bc +
      (1 | bc_id) + (1 | defensiveTeam),
    decomp = "QR"
  ),
  family = Gamma(link = "log"),
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

pp_check(step_model_gamma)

write_rds(step_model_gamma, "assets/step_model_gamma.rds", compress = "gz")

# log-normal regression
step_model_lognormal <- brm(
  bf(
    step_length ~
      prev_step + 
      adj_bc_x + adj_bc_y + adj_bc_x_from_first_down +
      n_left_bc_defense + n_front_bc_defense + n_left_bc_offense + n_front_bc_offense +
      adj_x + adj_y + adj_x_change + adj_y_change_abs + dist_to_bc + def_s + angle_with_bc +
      (1 | bc_id) + (1 | defensiveTeam),
    decomp = "QR"
  ),
  family = lognormal(),
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

pp_check(step_model_lognormal)

write_rds(step_model_lognormal, "assets/step_model_lognormal.rds", compress = "gz")
