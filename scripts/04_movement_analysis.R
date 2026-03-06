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


# player rankings

players <- read_csv("data/players.csv")

bc_filtered <- tracking_movement_data |> 
  distinct(gameId, playId, bc_id) |> 
  count(bc_id) |> 
  filter(n >= 70) |> 
  pull(bc_id)

# turn rankings

players_posterior_angle <- turn_model |> 
  spread_draws(r_bc_id__kappa[nflId,term]) |>
  left_join(players) |> 
  filter(nflId %in% bc_filtered) |> 
  group_by(nflId, displayName) |> 
  summarize(posterior_mean = mean(r_bc_id__kappa)) |> 
  ungroup() |> 
  arrange(posterior_mean)

turn_model |> 
  spread_draws(r_bc_id__kappa[nflId,term]) |> 
  ungroup() |> 
  left_join(players) |> 
  filter(nflId %in% bc_filtered) |> 
  mutate(displayName = factor(displayName, levels = players_posterior_angle$displayName)) |> 
  ggplot(aes(x = r_bc_id__kappa, y = displayName)) +
  ggridges::geom_density_ridges(rel_min_height = 0.05, alpha = 0.5) +
  theme_light() +
  labs(x = "RB concentration random effect (turn angle)", y = NULL)


# step rankings

players_posterior_step <- step_model |> 
  spread_draws(r_bc_id[nflId,term]) |>
  left_join(players) |> 
  filter(nflId %in% bc_filtered) |> 
  group_by(nflId, displayName) |> 
  summarize(posterior_mean = mean(r_bc_id)) |> 
  ungroup() |> 
  arrange(posterior_mean)

step_model |> 
  spread_draws(r_bc_id[nflId,term]) |> 
  ungroup() |>
  left_join(players) |> 
  filter(nflId %in% bc_filtered) |>
  mutate(displayName = factor(displayName, levels = players_posterior_step$displayName)) |> 
  ggplot(aes(x = r_bc_id, y = displayName)) +
  ggridges::geom_density_ridges(rel_min_height = 0.03, alpha = 0.4) +
  theme_light() +
  labs(x = "RB random effect (step length)", y = NULL)

# joint distribution

players_posterior_angle |> 
  rename(posterior_mean_angle = posterior_mean) |> 
  full_join(rename(players_posterior_step,
                   posterior_mean_step = posterior_mean)) |> 
  # summarize(cor(posterior_mean_angle, posterior_mean_step))
  mutate(displayName = str_replace_all(displayName, "^(\\w)\\w+ (?=\\w)", "\\1. "),
         displayName = str_replace(displayName, "A.J.", "A.")) |> 
  ggplot(aes(posterior_mean_step, posterior_mean_angle)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = displayName)) 