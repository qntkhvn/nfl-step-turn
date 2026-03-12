library(tidyverse)
library(brms)
library(tidybayes)

step_model <- read_rds("assets/step_model.rds")
turn_model <- read_rds("assets/turn_model.rds")

tracking_movement_data <- read_rds("assets/tracking_movement_data.rds")
upper <- max(tracking_movement_data$step_length)
lower <- min(tracking_movement_data$step_length)

# create new ball carrier
tracking_movement_data <- tracking_movement_data |>
  rename(ball_carrier_id = bc_id) |>
  mutate(bc_id = 99999)

# first simulate step length
step_draws <- step_model |> 
  add_predicted_draws(newdata = tracking_movement_data,
                      ndraws = 100,
                      seed = 69,
                      re_formula = NULL,
                      allow_new_levels = TRUE,
                      sample_new_levels = "uncertainty")

# backtransform
step_draws <- step_draws |>
  ungroup() |> 
  mutate(step_length = (sin(.prediction) ^ 2) * (upper - lower) + lower) |> 
  select(-starts_with("."))

# now simulate turn angle (conditional on step length)
step_turn_draws <- turn_model |> 
  add_predicted_draws(newdata = step_draws,
                      ndraws = 1,
                      seed = 69,
                      re_formula = NULL,
                      allow_new_levels = TRUE,
                      sample_new_levels = "uncertainty")

# some calculations for later
step_turn_draws <- step_turn_draws |> 
  ungroup() |>
  mutate(pred_turn_angle = .prediction) |>
  rename(pred_step_length = step_length) |> 
  select(-starts_with("."), -turn_angle, -bc_id) |>
  rename(bc_id = ball_carrier_id) |>
  group_by(gameId, playId, frameId) |> 
  mutate(sim_id = row_number()) |> 
  ungroup()

# get hypothetical location
step_turn_sim_tracking <- step_turn_draws |> 
  mutate(
    next_bc_x = bc_x + pred_step_length * cos(heading + pred_turn_angle),
    next_bc_y = bc_y + pred_step_length * sin(heading + pred_turn_angle),
    frameId = frameId + 3
  ) |> 
  select(gameId, playId, frameId, sim_id, bc_id, bc_club,
         bc_x = next_bc_x, bc_y = next_bc_y, 
         pred_step_length, pred_turn_angle)


# truncate if outside of boundary
step_turn_sim_tracking <- step_turn_sim_tracking |>
  mutate(bc_x = ifelse(bc_x < 0, 0, bc_x),
         bc_x = ifelse(bc_x > 110, 110, bc_x),
         bc_y = ifelse(bc_y < 0, 0, bc_y),
         bc_y = ifelse(bc_y > 160/3, 160/3, bc_y))

# compute features for evaluation later
plays <- read_csv("data/plays.csv")
plays <- plays |> 
  mutate(yards_from_endzone =
           ifelse((possessionTeam != yardlineSide) |
                    (yardlineNumber == 50), yardlineNumber,
                  100 - yardlineNumber),
         adj_x_first_down = yards_from_endzone - yardsToGo)

step_turn_sim_tracking <- step_turn_sim_tracking |> 
  mutate(adj_bc_x = 110 - bc_x,
         adj_bc_y = bc_y - (160 / 6)) |> 
  left_join(select(plays, gameId, playId, adj_x_first_down)) |> 
  mutate(adj_bc_x_from_first_down = adj_bc_x - adj_x_first_down) |> 
  select(-adj_x_first_down)

step_turn_sim_tracking |> 
  write_rds("assets/step_turn_sim_tracking.rds", compress = "gz")