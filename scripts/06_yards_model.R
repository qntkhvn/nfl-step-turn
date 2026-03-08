library(tidyverse)

# prepare data first

games <- read_csv("data/games.csv")
plays <- read_csv("data/plays.csv")
players <- read_csv("data/players.csv")
player_play <- read_csv("data/player_play.csv")
tracking <- arrow::read_parquet("data/tracking.parquet") |> 
  filter(frameType != "BEFORE_SNAP")

tracking <- tracking |>
  mutate(
    x = ifelse(playDirection == "left", 120 - x, x),
    y = ifelse(playDirection == "left", 160 / 3 - y, y),
    dir = ifelse(playDirection == "left", dir + 180, dir),
    dir = ifelse(dir > 360, dir - 360, dir),
    o = ifelse(playDirection == "left", o + 180, o),
    o = ifelse(o > 360, o - 360, o)
  )

plays <- plays |> 
  mutate(yards_from_endzone =
           ifelse((possessionTeam != yardlineSide) |
                    (yardlineNumber == 50), yardlineNumber,
                  100 - yardlineNumber),
         adj_x_first_down = yards_from_endzone - yardsToGo)

plays_rushers <- player_play |>
  filter(hadRushAttempt == 1) |>
  left_join(select(players, nflId, position)) |>
  filter(position == "RB") |>
  select(gameId, playId, bc_id = nflId, bc_club = teamAbbr)

tracking_all <- tracking |>
  inner_join(plays_rushers)

tracking_all <- tracking_all |>
  group_by(gameId, playId) |>
  mutate(
    frame_handoff = frameId[which(event == "handoff")][1],
    frame_end = frameId[which(event %in% c("out_of_bounds", "tackle", "touchdown"))][1]
  ) |>
  ungroup() |>
  filter(!is.na(frame_handoff), !is.na(frame_end)) |>
  filter(frameId >= frame_handoff & frameId <= frame_end)


# plays from movement data
plays_filtered <- read_rds("assets/tracking_movement_data.rds") |>
  distinct(gameId, playId)

tracking_all <- tracking_all |>
  inner_join(plays_filtered)


# bc features
tracking_bc <- tracking_all |> 
  filter(nflId == bc_id) |> 
  select(gameId, playId, frameId, event,
         bc_id, bc_club,
         bc_x = x, bc_y = y, bc_s = s, bc_a = a,
         bc_dis = dis, bc_o = o, bc_dir = dir) |> 
  mutate(adj_bc_x = 110 - bc_x,
         adj_bc_y = bc_y - (160 / 6)) |> 
  left_join(select(plays, gameId, playId, adj_x_first_down)) |> 
  mutate(adj_bc_x_from_first_down = adj_bc_x - adj_x_first_down)

# response
plays_end_yardline <- tracking_bc |> 
  group_by(gameId, playId) |> 
  # summarize(end_bc_x = max(bc_x)) |>
  summarize(end_bc_x = last(bc_x)) |>
  ungroup()

tracking_bc <- tracking_bc |> 
  left_join(plays_end_yardline) |> 
  mutate(yards_gained = end_bc_x - bc_x) |>
  select(-end_bc_x)


# features for other players
# save for simulation later
tracking_all |>
  filter(displayName != "football", nflId != bc_id) |>
  mutate(side = ifelse(club == bc_club, "offense", "defense")) |>
  inner_join(select(tracking_bc, gameId, playId, frameId)) |>
  select(gameId, playId, nflId, frameId, x, y, s, o, dir, side) |>
  write_rds("assets/tracking_other.rds", compress = "gz")

tracking_other_long <- tracking_all |> 
  filter(displayName != "football", nflId != bc_id) |> 
  mutate(side = ifelse(club == bc_club, "offense", "defense")) |> 
  inner_join(select(tracking_bc, gameId, playId, frameId, 
                    bc_x, bc_y, adj_bc_x, adj_bc_y)) |> 
  mutate(dist_to_bc = sqrt((x - bc_x) ^ 2 + (y - bc_y) ^ 2)) |> 
  group_by(gameId, playId, frameId, side) |>
  arrange(dist_to_bc) |> 
  mutate(player_dist_bc_rank = 1:n()) |>
  ungroup() |> 
  select(gameId, playId, frameId, 
         side, player_dist_bc_rank, nflId, 
         x, y, s, o, dir, dist_to_bc,
         bc_x, bc_y, adj_bc_x, adj_bc_y) |> 
  mutate(adj_x = 110 - x,
         adj_y = y - (160 / 6),
         adj_x_change = adj_bc_x - adj_x, adj_y_change = adj_bc_y - adj_y,
         dir_target_endzone = case_when(
           (dir < 270) ~ 90 - dir,
           (dir >= 270) ~ 450 - dir,
           TRUE ~ NA_real_),
         o_target_endzone = case_when(
           (o < 270) ~ 90 - o,
           (o >= 270) ~ 450 - o,
           TRUE ~ NA_real_),
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

tracking_other_wide <- tracking_other_long |> 
  select(-bc_x, -bc_y, -angle_with_bc, -angle_with_bc_rad, -adj_bc_x, -adj_bc_y) |> 
  unite(player_type, side:player_dist_bc_rank, sep = "_") |> 
  pivot_wider(names_from = player_type,
              values_from = nflId:o_wrt_bc_diff,
              names_glue = "{player_type}_{.value}")

# final data
tracking_yards_data <- tracking_bc |> 
  left_join(tracking_other_wide, by = c("gameId", "playId", "frameId")) |> 
  mutate(across(contains("target_endzone"), abs, .names = "{col}_abs"),
         across(contains("adj_y_change"), abs, .names = "{col}_abs"))

# save for later
write_rds(tracking_yards_data, "assets/tracking_yards_data.rds", compress = "gz")


# get model features
all_cols <- names(tracking_yards_data)

bc_features <- c("adj_bc_x", "adj_bc_y", "adj_bc_x_from_first_down", "bc_s")

defense_features <- map(
  1:11,
  function(i) {
    all_cols |> 
      str_subset(str_c("defense_", i, "_")) |> 
      str_subset("(_adj_x)|(_adj_y)|(_dist_to_bc)|(_adj_x_change)|(_adj_y_change_abs)|(_s)|(_target_endzone_abs)|(_wrt_bc_diff)") |> 
      str_subset("_adj_y_change$", negate = TRUE)
  }) |> 
  unlist()

offense_features <- map(
  1:10,
  function(i) {
    all_cols |> 
      str_subset(str_c("offense_", i, "_")) |> 
      str_subset("(_adj_x)|(_adj_y)|(_dist_to_bc)|(_adj_x_change)|(_adj_y_change_abs)|(_s)|(_target_endzone_abs)|(_wrt_bc_diff)") |> 
      str_subset("_adj_y_change$", negate = TRUE)
  }) |> 
  unlist()

# save for simulation later
c(bc_features, defense_features, offense_features) |> 
  write_rds("assets/yards_features.rds")

# fit model

library(catboost)

y_train <- tracking_yards_data$yards_gained
x_train <- tracking_yards_data |> 
  select(all_of(c(bc_features, defense_features, offense_features)))

train_pool <- catboost.load_pool(data = x_train, label = y_train)
yards_model <- catboost.train(learn_pool = train_pool,
                              params = list(learning_rate = 0.01,
                                            random_seed = 90))
yards_model |> 
  write_rds("assets/yards_model.rds", compress = "gz")

# prediction for observed player-frames
# save for comparison later
test_pool <- catboost.load_pool(data = x_train)
tracking_yards_obs <- tracking_yards_data |> 
  select(gameId, playId, frameId, obs_bc_x = bc_x, obs_bc_y = bc_y) |> 
  mutate(yards_pred_obs = catboost.predict(yards_model, test_pool)) 

tracking_yards_obs |>
  write_rds("assets/tracking_yards_obs.rds", compress = "gz")