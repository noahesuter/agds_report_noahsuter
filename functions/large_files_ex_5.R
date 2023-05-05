# make model evaluation into a function to reuse code
eval_model <- function(mod, df_train, df_test){
  #loading packages
  library(dplyr)
  library(ggplot2)
  library(yardstick)
  library(cowplot)
  # add predictions to the data frames
  df_train <- df_train |> 
    drop_na()
  df_train$fitted <- predict(mod, newdata = df_train)
  
  df_test <- df_test |> 
    drop_na()
  df_test$fitted <- predict(mod, newdata = df_test)
  
  # get metrics tables
  metrics_train <- df_train |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  metrics_test <- df_test |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  # extract values from metrics tables
  rmse_train <- metrics_train |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_train <- metrics_train |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  
  rmse_test <- metrics_test |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_test <- metrics_test |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  
  # visualise as a scatterplot
  # adding information of metrics as sub-titles
  plot_1 <- ggplot(data = df_train, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_train, digits = 2)) ~~
                              RMSE == .(format(rmse_train, digits = 3))),
         title = "Training set") +
    theme_classic()
  
  plot_2 <- ggplot(data = df_test, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_test, digits = 2)) ~~
                              RMSE == .(format(rmse_test, digits = 3))),
         title = "Test set") +
    theme_classic()
  
  out <- cowplot::plot_grid(plot_1, plot_2)
  
  return(out)
}




get_mae <- function(k) {
  library(rsample)
  library(tidyr)
  library(caret)
  library(recipes)
  library(ggplot2)
  # Define function to train and evaluate KNN model
  
  # Model and pre-processing formulation, use all variables but LW_IN_F
  pp <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, 
                        data = half_hourly_fluxes_train |> drop_na()) |> 
    recipes::step_BoxCox(all_predictors()) |> 
    recipes::step_center(all_numeric(), -all_outcomes()) |>
    recipes::step_scale(all_numeric(), -all_outcomes())
  # Fit KNN model
  mod_knn <- caret::train(
    pp, 
    data = half_hourly_fluxes_train, 
    method = "knn",
    trControl = caret::trainControl(method = "none"),
    tuneGrid = data.frame(k = k),
    metric = "MAE"
  )
  # Evaluate model on test set
  new_data <- drop_na(half_hourly_fluxes_test)
  pred <- predict(mod_knn, newdata = new_data)
  obs <- new_data$GPP_NT_VUT_REF
  mae <- caret::MAE(pred, obs)
  
  # Evaluate model on training set
  pred_train <- predict(mod_knn, newdata = drop_na(half_hourly_fluxes_train))
  obs_train <- drop_na(half_hourly_fluxes_train)$GPP_NT_VUT_REF
  mae_train <- caret::MAE(pred_train, obs_train)
  
  return(list("mae_train" = mae_train, "mae_test" = mae))
}