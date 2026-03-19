library(tidyverse)

tracking_yards_data <- read_rds("assets/tracking_yards_data.rds")
yards_features <- read_rds("assets/yards_features.rds")

# split into 5 folds by game
set.seed(100)
tracking_yards_data_cv_folds <- tracking_yards_data |> 
  left_join(
    tracking_yards_data |> 
      distinct(gameId) |> 
      mutate(fold_id = sample(rep(1:5, length.out = n())))
  ) |> 
  select(gameId, playId, frameId, fold_id, yards_gained, all_of(yards_features)) |> 
  group_by(gameId, playId) |> 
  mutate(adj_frame = frameId - min(frameId)) |> 
  ungroup()

# tune lasso
  
y_train <- tracking_yards_data$yards_gained
x_train <- tracking_yards_data |> 
  select(all_of(yards_features)) |> 
  as.matrix()

library(glmnet)
set.seed(101)
yards_lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 5, 
                            foldid = tracking_yards_data$fold_id)

# yards_lasso_cv$lambda.1se
# yards_lasso_cv$lambda.min

yards_lasso_cv |> 
  write_rds("assets/yards_lasso_cv.rds", compress = "gz")

# tune catboost

library(catboost)

yards_cb_cv <- function(f, i = 1000, l = 0.01) {
  
  train_data <- tracking_yards_data_cv_folds |> 
    filter(fold_id != f)
  
  x_train <- train_data |> 
    select(all_of(yards_features))
  
  train_pool <- catboost.load_pool(data = x_train, label = train_data$yards_gained)
  cb_fit <- catboost.train(learn_pool = train_pool, 
                           params = list(iterations = i, learning_rate = l, depth = 6))
  
  test_data <- tracking_yards_data_cv_folds |> 
    filter(fold_id == f)
  
  x_test <- test_data |> 
    select(all_of(yards_features))
  
  test_pool <- catboost.load_pool(data = x_test)
  
  tibble(cb_pred = catboost.predict(cb_fit, test_pool),
         test_actual = test_data$yards_gained,
         learning_rate = cb_fit$learning_rate,
         iterations = cb_fit$tree_count,
         adj_frame = test_data$adj_frame,
         fold_id = f)
  
}

set.seed(111)

yards_cb_100_0.01 <- purrr::map(.x = 1:5, .f = \(x) yards_cb_cv(x, i = 100, l = 0.01)) |> bind_rows()
yards_cb_200_0.01 <- purrr::map(.x = 1:5, .f = \(x) yards_cb_cv(x, i = 200, l = 0.01)) |> bind_rows()
yards_cb_500_0.01 <- purrr::map(.x = 1:5, .f = \(x) yards_cb_cv(x, i = 500, l = 0.01)) |> bind_rows()
yards_cb_1000_0.01 <- purrr::map(.x = 1:5, .f = \(x) yards_cb_cv(x, i = 1000, l = 0.01)) |> bind_rows()
yards_cb_1500_0.01 <- purrr::map(.x = 1:5, .f = \(x) yards_cb_cv(x, i = 1500, l = 0.01)) |> bind_rows()

yards_cb_100_0.03 <- purrr::map(.x = 1:5, .f = \(x) yards_cb_cv(x, i = 100, l = 0.03)) |> bind_rows()
yards_cb_200_0.03 <- purrr::map(.x = 1:5, .f = \(x) yards_cb_cv(x, i = 200, l = 0.03)) |> bind_rows()
yards_cb_500_0.03 <- purrr::map(.x = 1:5, .f = \(x) yards_cb_cv(x, i = 500, l = 0.03)) |> bind_rows()
yards_cb_1000_0.03 <- purrr::map(.x = 1:5, .f = \(x) yards_cb_cv(x, i = 1000, l = 0.03)) |> bind_rows()
yards_cb_1500_0.03 <- purrr::map(.x = 1:5, .f = \(x) yards_cb_cv(x, i = 1500, l = 0.03)) |> bind_rows()

yards_cb_100_0.05 <- purrr::map(.x = 1:5, .f = \(x) yards_cb_cv(x, i = 100, l = 0.05)) |> bind_rows()
yards_cb_200_0.05 <- purrr::map(.x = 1:5, .f = \(x) yards_cb_cv(x, i = 200, l = 0.05)) |> bind_rows()
yards_cb_500_0.05 <- purrr::map(.x = 1:5, .f = \(x) yards_cb_cv(x, i = 500, l = 0.05)) |> bind_rows()
yards_cb_1000_0.05 <- purrr::map(.x = 1:5, .f = \(x) yards_cb_cv(x, i = 1000, l = 0.05)) |> bind_rows()
yards_cb_1500_0.05 <- purrr::map(.x = 1:5, .f = \(x) yards_cb_cv(x, i = 1500, l = 0.05)) |> bind_rows()

yards_cb_cv_pred <- yards_cb_100_0.01 |> 
  bind_rows(yards_cb_200_0.01) |> 
  bind_rows(yards_cb_500_0.01) |> 
  bind_rows(yards_cb_1000_0.01) |> 
  bind_rows(yards_cb_1500_0.01) |> 
  bind_rows(yards_cb_100_0.03) |> 
  bind_rows(yards_cb_200_0.03) |> 
  bind_rows(yards_cb_500_0.03) |> 
  bind_rows(yards_cb_1000_0.03) |> 
  bind_rows(yards_cb_1500_0.03) |> 
  bind_rows(yards_cb_100_0.05) |> 
  bind_rows(yards_cb_200_0.05) |> 
  bind_rows(yards_cb_500_0.05) |> 
  bind_rows(yards_cb_1000_0.05) |> 
  bind_rows(yards_cb_1500_0.05)

yards_cb_cv_pred |> 
  group_by(fold_id, learning_rate, iterations) |> 
  summarize(fold_rmse = sqrt(mean((test_actual - cb_pred)^2))) |> 
  group_by(learning_rate, iterations) |> 
  summarize(cv_rmse = mean(fold_rmse)) |> 
  ungroup() |> 
  arrange(cv_rmse)

# 0.03, 1000, 6
