library(tidyverse)
theme_set(theme_light())
library(brms)
library(tidybayes)

tracking_movement_data <- read_rds("assets/tracking_movement_data.rds")
players <- read_csv("assets/players.csv")

bc_filtered <- tracking_movement_data |> 
  distinct(gameId, playId, bc_id) |> 
  count(bc_id) |> 
  filter(n >= 70) |> 
  pull(bc_id)

# turn rankings

turn_model <- read_rds("assets/turn_model.rds")

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

step_model <- read_rds("assets/step_model.rds")

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
  ggridges::geom_density_ridges(rel_min_height = 0.05, alpha = 0.5) +
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
