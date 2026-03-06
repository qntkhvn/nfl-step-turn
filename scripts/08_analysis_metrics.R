library(tidyverse)

tracking_yards_eval <- read_rds("assets/tracking_yards_eval.rds")

# example play
tracking_sim_example <- tracking_yards_eval |> 
  filter(gameId == 2022091810, playId == 2764)

# snapshot analysis at first contact

# get tracking data for example play
tracking <- arrow::read_parquet("data/tracking.parquet")

tracking_example <- tracking |> 
  filter(gameId == 2022091810, playId == 2764) |> 
  read_rds("assets/tracking_example.rds") |> 
  mutate(
    pt_color = case_when(
      club == "football" ~ NA,
      club == "DEN" ~ "#FB4F14",
      club == "HOU" ~ "white"
    ),
    pt_color = ifelse(nflId == 53464, "black", pt_color),
    pt_size = ifelse(club == "football", 2, 5)
  )

# get observed and simulated ball carrier information

tracking_bc_path <- tracking_example |> 
  filter(nflId == 53464)

tracking_sim <- tracking_sim_example |> 
  filter(frameId == 114) |> 
  mutate(prev_x = unique(pull(filter(tracking_sim_example, frameId == 111), obs_bc_x)),
         prev_y = unique(pull(filter(tracking_sim_example, frameId == 111), obs_bc_y)))

library(sportyR)
field_params <- list(field_apron = "#21AE5FCC",
                     field_border = "#21AE5FCC",
                     offensive_endzone = "#21AE5FCC",
                     defensive_endzone = "#21AE5FCC",
                     offensive_half = "#21AE5FCC",
                     defensive_half = "#21AE5FCC",
                     directional_arrow = NA,
                     yardage_marker = "#bebebe",
                     major_yard_line = "#bebebe",
                     minor_yard_line = "#bebebe")

field_background <- geom_football(league = "nfl",
                                  display_range = "in_bounds_only",
                                  x_trans = 60,
                                  y_trans = 80 / 3,
                                  color_updates = field_params,
                                  field_updates = list(number_height = 0.5, 
                                                       number_width = 0.5,
                                                       minor_line_thickness = 0.03,
                                                       major_line_thickness = 0.03))

# plot simulated steps
field_background +
  geom_vline(xintercept = 85, color = "midnightblue", linewidth = 1) +
  geom_segment(aes(x = 120 - prev_x, y = 160/3 - prev_y, 
                   xend = 120 - bc_x, yend = 160/3 - bc_y),
               data = tracking_sim, color = "lightgray", linewidth = 0.5, alpha = 0.4) +
  geom_path(aes(x, y), data = filter(tracking_bc_path, frameId %in% c(111, 114)), 
            color = "black", linewidth = 0.6) +
  geom_point(aes(x, y, color = pt_color, size = pt_size),
             data = filter(tracking_example, frameId == 111, nflId == 53464)) +
  geom_point(aes(x, y, color = pt_color, size = pt_size),
             data = filter(frameId == 111, nflId != 53464)) +
  scale_color_identity() +
  scale_size_identity() +
  coord_cartesian(xlim = c(82.5, 86), ylim = c(15.5, 21.5)) +
  theme(panel.border = element_rect(fill = NA, color = "#bebebe"))

# plot distribution of hypothetical yards gained
tracking_sim_example |> 
  filter(frameId == 114) |> 
  ggplot(aes(yards_pred_sim_corrected)) + 
  ggdist::stat_slab(fill = NA, color = "lightgray") + 
  geom_vline(xintercept = filter(tracking_sim_example, frameId == 114)$yards_pred_obs_corrected[1], 
             linewidth = 1.2) +
  labs(x = "Estimated hypothetical yards gained", 
       y = "Density",
       title = "") +
  annotate("text", x = 3.6, y = 0.935, label = "Observed", fontface = "bold", size = rel(3)) +
  theme_light() +
  theme(axis.title = element_text(size = rel(0.9)),
        axis.text = element_text(size = rel(0.8)))

# plot difference in yards gained throughout the play
tracking_sim_example |> 
  mutate(delta = yards_pred_obs_corrected - yards_pred_sim_corrected) |> 
  group_by(frameId) |> 
  summarize(mean_delta = mean(delta),
            lower = quantile(delta, 0.025),
            upper = quantile(delta, 0.975)) |> 
  ggplot() +
  geom_ribbon(aes(frameId, mean_delta, ymin = lower, ymax = upper), alpha = 0.08) +
  geom_line(aes(frameId, mean_delta), color = "red") +
  labs(y = "Difference in yards gained") +
  geom_vline(xintercept = 110) +
  geom_vline(xintercept = 135)


# get players data
players_name <- read_csv("data/players.csv") |> 
  select(bc_id = nflId, displayName)

tracking_movement_data <- read_rds("assets/tracking_movement_data.rds")
bc_filtered <- tracking_movement_data |> 
  distinct(gameId, playId, bc_id) |> 
  count(bc_id) |> 
  filter(n >= 70) |> 
  pull(bc_id)

# yards success rate rankings

tracking_yards_eval |> 
  mutate(delta = yards_pred_obs_corrected - yards_pred_sim_corrected) |> 
  filter(bc_id %in% bc_filtered) |> 
  group_by(gameId, playId, bc_id, frameId) |> 
  summarize(frame_ysr = mean(delta > 0)) |> 
  group_by(bc_id) |> 
  summarize(bc_ysr = mean(frame_ysr)) |> 
  left_join(players_name) |> 
  arrange(-bc_ysr) |> 
  slice(1:5, (n()-4):n())

# explosiveness rankings

tracking_yards_eval |> 
  filter(bc_id %in% bc_filtered) |> 
  group_by(gameId, playId, bc_id, frameId) |> 
  summarize(frame_obs_yards = first(yards_pred_obs_corrected),
            frame_sim_yards_tail = quantile(yards_pred_sim_corrected, 0.95)) |> 
  group_by(gameId, playId, bc_id) |> 
  summarize(play_explo = mean(frame_obs_yards > frame_sim_yards_tail)) |> 
  group_by(bc_id) |> 
  summarize(bc_explo = mean(play_explo)) |> 
  left_join(players_name) |> 
  arrange(-bc_explo) |> 
  slice(1:5, (n()-4):n())
