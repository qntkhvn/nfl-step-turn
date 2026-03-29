source("scripts/11_alternative_simulation.R")

library(tidyverse)
theme_set(theme_light())

# tracking_yards_eval_alt <- read_rds("assets/tracking_yards_eval_alt.rds")

players_name <- read_csv("assets/players.csv") |> 
  select(nflId, displayName)

tracking_movement_data <- read_rds("assets/tracking_movement_data.rds")
bc_filtered <- tracking_movement_data |> 
  distinct(gameId, playId, bc_id) |> 
  count(bc_id) |> 
  filter(n >= 70) |> 
  pull(bc_id)

tracking_yards_eval_alt |>
  mutate(dev = yards_pred_obs_corrected - yards_pred_sim_corrected) |>
  filter(bc_id %in% bc_filtered) |>
  group_by(gameId, playId, bc_id, frameId) |>
  summarize(frame_ysr = mean(dev > 0)) |>
  group_by(bc_id) |>
  summarize(bc_ysr_alt = mean(frame_ysr)) |>
  rename(nflId = bc_id) |> 
  full_join(read_rds("assets/ysr_generic.rds")) |> 
  left_join(players_name) |> 
  ggplot(aes(bc_ysr_generic, bc_ysr_alt)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.5, size = 3, color = "gray50") +
  ggrepel::geom_text_repel(aes(label = displayName)) +
  expand_limits(y = c(0.43, 0.51), x = 0.528) +
  coord_fixed() +
  labs(y = "Yards success rate (relative to own expectation)",
       x = "Yards success rate (relative to generic baseline)")

tracking_yards_eval_alt |>
  filter(bc_id %in% bc_filtered) |>
  group_by(gameId, playId, bc_id, frameId) |>
  summarize(frame_obs_yards = first(yards_pred_obs_corrected),
            frame_sim_yards_tail = quantile(yards_pred_sim_corrected, 0.95)) |>
  group_by(gameId, playId, bc_id) |>
  summarize(play_explo = mean(frame_obs_yards > frame_sim_yards_tail)) |>
  group_by(bc_id) |>
  summarize(bc_explo_alt = mean(play_explo)) |>
  rename(nflId = bc_id) |> 
  full_join(read_rds("assets/explo_generic.rds")) |> 
  left_join(players_name) |> 
  ggplot(aes(bc_explo_generic, bc_explo_alt)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.5, size = 3, color = "gray50") +
  ggrepel::geom_text_repel(aes(label = displayName)) +
  expand_limits(x = c(0.03, 0.1), y = c(0.03, 0.1)) +
  coord_equal() +
  labs(y = "Explosiveness (relative to own expectation)",
       x = "Explosiveness (relative to generic baseline)")
