library(tidyverse)

games <- read_csv("data/games.csv")
games_week <- games |> 
  select(gameId, week)

tracking_yards_data <- read_rds("assets/tracking_yards_data.rds")
yards_features <- read_rds("assets/yards_features.rds")

tracking_yards_data_cv_weeks <- tracking_yards_data |> 
  left_join(games_week) |> 
  select(gameId, playId, frameId, week, yards_gained, all_of(yards_features)) |> 
  group_by(gameId, playId) |> 
  mutate(adj_frame = frameId - min(frameId)) |> 
  ungroup()

# model comparison

library(mgcv)
library(glmnet)
library(ranger)
library(catboost)

yards_lasso_cv <- read_rds("assets/yards_lasso_cv.rds")

# leave-one-week-out cross-validation

yards_cv <- function(x) {
  
  # training
  train_data <- tracking_yards_data_cv_weeks |> 
    filter(week != x)
  
  x_train <- train_data |> 
    select(all_of(yards_features))
  
  # intercept only
  lm_fit <- lm(yards_gained ~ 1, data = train_data)
  
  # gam
  gam_fit <- gam(yards_gained ~ s(bc_s) + s(defense_1_dist_to_bc), data = train_data)
  
  # rf
  rf_fit <- ranger(y = train_data$yards_gained, x = x_train, num.threads = 30)
  
  # catboost
  train_pool <- catboost.load_pool(data = x_train, label = train_data$yards_gained)
  cb_fit <- catboost.train(learn_pool = train_pool,
                           params = list(iterations = 1000, learning_rate = 0.03, depth = 6))
  
  # lasso
  lasso_fit <- glmnet(as.matrix(x_train), train_data$yards_gained, alpha = 1,
                      lambda = yards_lasso_cv$lambda.1se)
  
  # testing
  test_data <- tracking_yards_data_cv_weeks |> 
    filter(week == x)
  
  x_test <- test_data |> 
    select(all_of(yards_features))
  
  test_pool <- catboost.load_pool(data = x_test)
  
  tibble(lm_pred = predict(lm_fit, newdata = x_test),
         gam_pred = as.numeric(predict(gam_fit, newdata = x_test)),
         rf_pred = predict(rf_fit, num.threads = 30, data = x_test)$predictions,
         cb_pred = catboost.predict(cb_fit, test_pool),
         lasso_pred = as.numeric(predict(lasso_fit, as.matrix(x_test))),
         test_actual = test_data$yards_gained,
         adj_frame = test_data$adj_frame,
         week = x)
}

# run all and combine
set.seed(68)
yards_cv_pred <- purrr::map(1:9, yards_cv) |> 
  bind_rows()

# overall cv rmse
yards_cv_pred |>
  pivot_longer(contains("_pred"), 
               names_to = "type", 
               values_to = "test_pred") |> 
  group_by(type, week) |> 
  summarize(rmse = sqrt(mean((test_actual - test_pred)^2))) |> 
  group_by(type) |> 
  summarize(avg_rmse = mean(rmse))

# rmse vs frames since handoff

yards_cv_pred |>
  pivot_longer(contains("_pred"), 
               names_to = "type", 
               values_to = "test_pred") |> 
  group_by(type, adj_frame) |> 
  summarize(
    rmse = sqrt(mean((test_actual - test_pred)^2)),
    n_frames = n()
  ) |>
  ungroup() |> 
  filter(n_frames > 2) |>
  mutate(
    type = case_when(
      type == "cb_pred" ~ "CatBoost",
      type == "gam_pred" ~ "GAM",
      type == "lasso_pred" ~ "LASSO",
      type == "lm_pred" ~ "Intercept-only",
      type == "rf_pred" ~ "Random forests",
    ),
    type = factor(type, levels = c("Intercept-only", "LASSO", "GAM",
                                   "Random forests", "CatBoost"))
  ) |> 
  ggplot() +
  geom_bar(aes(x = adj_frame, y = rmse, alpha = n_frames),
           stat = "identity", fill = "darkred") +
  facet_wrap(~ type) +
  labs(x = "Frames since handoff", 
       y = "Holdout RMSE",
       alpha = "Number of frames") +
  theme_light()

# residuals frames since handoff

yards_cv_pred |>
  pivot_longer(contains("_pred"), 
               names_to = "type", 
               values_to = "test_pred") |> 
  mutate(resid = test_actual - test_pred) |> 
  group_by(type, adj_frame) |> 
  summarize(avg_resid = mean(resid),
            sd_resid = sd(resid),
            n = n()) |>
  ungroup() |> 
  filter(n > 2) |> 
  mutate(se_resid = sd_resid / sqrt(n),
         lower_resid = avg_resid - 2 * se_resid,
         upper_resid = avg_resid + 2 * se_resid) |>
  mutate(
    type = case_when(
      type == "cb_pred" ~ "CatBoost",
      type == "gam_pred" ~ "GAM",
      type == "lasso_pred" ~ "LASSO",
      type == "lm_pred" ~ "Intercept-only",
      type == "rf_pred" ~ "Random forests",
    ),
    type = factor(type, levels = c("Intercept-only", "LASSO", "GAM", 
                                   "Random forests", "CatBoost"))
  ) |> 
  filter(adj_frame > 1) |> 
  ggplot(aes(adj_frame, avg_resid)) +
  geom_point(alpha = 0.4, size = 0.4) +
  geom_errorbar(aes(ymin = lower_resid, ymax = upper_resid), alpha = 0.5) +
  facet_wrap(~ type) +
  labs(x = "Frames since handoff", 
       y = "Yards gained residual")