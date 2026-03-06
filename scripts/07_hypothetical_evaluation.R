library(tidyverse)

step_turn_sim_tracking <- read_rds("assets/step_turn_sim_tracking.rds")
tracking_other <- read_rds("assets/tracking_other.rds")

# features for yards gained hypothetical evaluation

tracking_other_sim <- step_turn_sim_tracking |> 
  select(gameId, playId, frameId, sim_id, bc_x, bc_y, adj_bc_x, adj_bc_y) |> 
  left_join(tracking_other)

tracking_other_sim <- tracking_other_sim |> 
  mutate(dist_to_bc = sqrt((x - bc_x) ^ 2 + (y - bc_y) ^ 2)) |> 
  group_by(gameId, playId, frameId, sim_id, side) |>
  arrange(dist_to_bc) |> 
  mutate(player_dist_bc_rank = 1:n()) |>
  ungroup()

tracking_other_sim <- tracking_other_sim |> 
  select(gameId, playId, frameId, sim_id,
         side, player_dist_bc_rank, nflId, 
         x, y, s, o, dir, dist_to_bc,
         bc_x, bc_y, adj_bc_x, adj_bc_y) |> 
  mutate(adj_x = 110 - x,
         adj_y = y - (160 / 6),
         adj_x_change = adj_bc_x - adj_x, 
         adj_y_change = adj_bc_y - adj_y,
         adj_y_change_abs = abs(adj_y_change),
         dir_target_endzone = case_when(
           (dir < 270) ~ 90 - dir,
           (dir >= 270) ~ 450 - dir,
           TRUE ~ NA_real_),
         dir_target_endzone_abs = abs(dir_target_endzone),
         o_target_endzone = case_when(
           (o < 270) ~ 90 - o,
           (o >= 270) ~ 450 - o,
           TRUE ~ NA_real_),
         o_target_endzone_abs = abs(o_target_endzone),
         angle_with_bc_rad = atan2(adj_y_change, -adj_x_change),
         angle_with_bc = angle_with_bc_rad * 180 / pi,
         dir_wrt_bc_diff = pmin(
           pmin(abs(angle_with_bc - dir_target_endzone),
                abs(angle_with_bc - (dir_target_endzone - 360))),
           abs(angle_with_bc - (dir_target_endzone + 360))),
         o_wrt_bc_diff = pmin(
           pmin(abs(angle_with_bc - o_target_endzone),
                abs(angle_with_bc - (o_target_endzone - 360))),
           abs(angle_with_bc - (o_target_endzone + 360))))

tracking_other_sim <- tracking_other_sim |> 
  select(-bc_x, -bc_y, -o, -dir,
         -adj_y_change, -dir_target_endzone, -o_target_endzone,
         -angle_with_bc, -angle_with_bc_rad, -adj_bc_x, -adj_bc_y) |> 
  unite(player_type, side:player_dist_bc_rank, sep = "_") |> 
  pivot_wider(names_from = player_type,
              values_from = nflId:o_wrt_bc_diff,
              names_glue = "{player_type}_{.value}")

# full table with all features calculated from hypothetical evaluation
tracking_sim_full <- step_turn_sim_tracking |> 
  left_join(tracking_other_sim, by = c("gameId", "playId", "frameId", "sim_id")) |> 
  mutate(bc_s = pred_step_length / 0.3) |> 
  select(-(offense_1_nflId:offense_10_y))

# yards gained prediction for simulation data
library(catboost)
yards_model <- read_rds("assets/yards_model.rds")

x_test <- tracking_sim_full |> 
  select(all_of(read_rds("assets/yards_features.rds")))
set.seed(91)
test_pool <- catboost.load_pool(data = x_test)
tracking_sim_full$yards_pred_sim <- catboost.predict(yards_model, test_pool)
tracking_yards_obs <- read_rds("assets/tracking_yards_obs.rds")

tracking_yards_eval <- tracking_sim_full |> 
  select(gameId:bc_y, yards_pred_sim) |> 
  left_join(tracking_yards_obs)

# truncate if outside of football field boundary
tracking_yards_eval <- tracking_yards_eval |> 
  mutate(
    eop_yardline_pred_obs = obs_bc_x + yards_pred_obs,
    eop_yardline_pred_obs_corrected = ifelse(eop_yardline_pred_obs > 110, 110, eop_yardline_pred_obs),
    eop_yardline_pred_obs_corrected = ifelse(eop_yardline_pred_obs_corrected < 10, 10, eop_yardline_pred_obs_corrected),
    yards_pred_obs_corrected = eop_yardline_pred_obs_corrected - obs_bc_x,
    eop_yardline_pred_sim = bc_x + yards_pred_sim,
    eop_yardline_pred_sim_corrected = ifelse(eop_yardline_pred_sim > 110, 110, eop_yardline_pred_sim),
    eop_yardline_pred_sim_corrected = ifelse(eop_yardline_pred_sim_corrected < 10, 10, eop_yardline_pred_sim_corrected),
    yards_pred_sim_corrected = eop_yardline_pred_sim_corrected - bc_x,
    yards_from_endzone_obs = 110 - eop_yardline_pred_obs_corrected,
    yards_from_endzone_sim = 110 - eop_yardline_pred_sim_corrected,
  )

tracking_yards_eval |> 
  write_rds("assets/tracking_yards_eval.rds", compress = "gz")