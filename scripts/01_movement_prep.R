library(tidyverse)

games <- read_csv("assets/games.csv")
plays <- read_csv("assets/plays.csv")
players <- read_csv("assets/players.csv")
player_play <- read_csv("assets/player_play.csv")
tracking <- arrow::read_parquet("https://www.dropbox.com/scl/fi/vd8ohaorsxk4wfoeq0a3x/tracking.parquet?rlkey=x3oezh0hqj5355lkyf4tzhn0m&st=i8h3jzd7&dl=1") |> 
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
    frame_handoff = frameId[which(event == "handoff")][1] - 6,
    frame_end = frameId[which(event %in% c("out_of_bounds", "tackle", "touchdown"))][1]
  ) |> 
  ungroup() |> 
  filter(!is.na(frame_handoff), !is.na(frame_end)) |> 
  filter(frameId >= frame_handoff & frameId <= frame_end)

frames <- tracking_all |>
  distinct(gameId, playId, frame_handoff, frame_end) |>
  rowwise() |>
  mutate(frameId = list(seq(frame_handoff, frame_end, 1))) |>
  unnest_longer(frameId) |>
  mutate(frameId_corrected = frameId - frame_handoff) |>
  select(gameId, playId, frameId, frame_handoff, frame_end, frameId_corrected) |>
  group_by(gameId, playId) |>
  filter(frameId_corrected %% 3 == 0) |>
  ungroup()

tracking_all <- tracking_all |>
  inner_join(frames)

plays_filtered <- tracking_all |>
  distinct(gameId, playId, frameId) |>
  count(gameId, playId) |>
  filter(n > 5) |>
  select(gameId, playId)

tracking_all <- tracking_all |>
  inner_join(plays_filtered)

# remove fumbles
plays_fumble <- tracking |> 
  filter(str_detect(event, "fumble")) |> 
  distinct(gameId, playId)

# remove trick plays with multiple handoff events
plays_multiple_handoff <- tracking |> 
  filter(str_detect(event, "handoff")) |> 
  distinct(gameId, playId, frameId, event) |> 
  count(gameId, playId) |> 
  filter(n > 1) |> 
  select(gameId, playId)

tracking_all <- tracking_all |> 
  anti_join(plays_fumble) |> 
  anti_join(plays_multiple_handoff)

# bc features
tracking_bc <- tracking_all |> 
  filter(nflId == bc_id) |> 
  select(gameId, playId, frameId, event,
         bc_id, bc_club,
         bc_x = x, bc_y = y, bc_s = s, bc_a = a,
         bc_dis = dis, bc_o = o, bc_dir = dir) |> 
  mutate(adj_bc_x = 110 - bc_x,
         adj_bc_y = bc_y - (160 / 6)) |> 
  left_join(select(plays, gameId, playId, adj_x_first_down, defensiveTeam)) |> 
  mutate(adj_bc_x_from_first_down = adj_bc_x - adj_x_first_down)

# nearest def features  
tracking_def <- tracking_all |> 
  filter(club != bc_club, displayName != "football") |> 
  left_join(select(tracking_bc, gameId, playId, frameId,
                   bc_x, bc_y, adj_bc_x, adj_bc_y),
            by = c("gameId", "playId", "frameId")) |> 
  mutate(dist_to_bc = sqrt((x - bc_x) ^ 2 + (y - bc_y) ^ 2)) |> 
  group_by(gameId, playId, frameId) |>
  arrange(dist_to_bc) |> 
  mutate(player_dist_bc_rank = row_number()) |> 
  ungroup() |> 
  filter(player_dist_bc_rank == 1) |> 
  select(gameId, playId, frameId, playDirection,
         nflId,
         dist_to_bc, def_x = x, def_y = y, def_s = s,
         bc_x, bc_y, adj_bc_x, adj_bc_y) |> 
  mutate(adj_x = 110 - def_x,
         adj_y = def_y - (160 / 6),
         adj_x_change = adj_bc_x - adj_x, 
         adj_y_change = adj_bc_y - adj_y,
         adj_y_change_abs = abs(adj_y_change),
         angle_with_bc = atan2(adj_y_change, -adj_x_change)) |> 
  select(-bc_x, -bc_y, -adj_bc_x, -adj_bc_y)

# counts features
tracking_counts <- tracking_all |>
  filter(displayName != "football", nflId != bc_id) |>
  mutate(side = ifelse(club == bc_club, "offense", "defense")) |>
  left_join(select(tracking_bc, gameId, playId, frameId, bc_x, bc_y)) |>
  group_by(gameId, playId, frameId, side) |>
  summarize(n_left_bc = sum(y > bc_y),
            n_front_bc = sum(x > bc_x)) |>
  pivot_wider(names_from = side,
              values_from = c(n_left_bc, n_front_bc)) |>
  ungroup()

# final data
tracking_movement_data <- tracking_bc |> 
  left_join(tracking_def, by = join_by(gameId, playId, frameId)) |>
  left_join(tracking_counts, by = join_by(gameId, playId, frameId)) |>
  group_by(gameId, playId, bc_id) |> 
  arrange(frameId, .by_group = TRUE) |> 
  mutate(
    step_length = sqrt((bc_x - lead(bc_x)) ^ 2 + (bc_y - lead(bc_y)) ^ 2),
    step_length = ifelse(step_length == 0, 0.005, step_length),
    prev_step = lag(step_length),
    turn_angle = atan2(lead(bc_y) - bc_y, lead(bc_x) - bc_x) - atan2(bc_y - lag(bc_y), bc_x - lag(bc_x)),
    turn_angle = ifelse(turn_angle >= pi, turn_angle - 2 * pi, 
                        ifelse(turn_angle <= -pi, 2 * pi + turn_angle, turn_angle)),
    turn_angle = ifelse(turn_angle > 3.14 | turn_angle < -3.14, 0, turn_angle),
    prev_angle = lag(turn_angle),
    
    heading = atan2(bc_y - lag(bc_y), bc_x - lag(bc_x)),
    heading = ifelse(heading > pi, heading - 2 * pi,
                     ifelse(heading < -pi, 2 * pi + heading, heading))
  ) |> 
  ungroup() |> 
  filter(!is.na(step_length), !is.na(prev_step), !is.na(turn_angle), !is.na(prev_angle))

lower <- min(tracking_movement_data$step_length)
upper <- max(tracking_movement_data$step_length)

tracking_movement_data <- tracking_movement_data |> 
  mutate(step_length_asin = asin(sqrt((step_length - lower) / (upper - lower))),
         prev_step_asin = asin(sqrt((prev_step - lower) / (upper - lower))))

tracking_movement_data |>
  write_rds("assets/tracking_movement_data.rds", compress = "gz")
