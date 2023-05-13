# Define function to train and evaluate KNN model for within-site predictions
get_mae_within <- function(train, test, k) {
  library(rsample)
  library(tidyr)
  library(caret)
  library(recipes)
  library(ggplot2)
  
  
  # Model and pre-processing formulation, use all variables but LW_IN_F
  pp <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, 
                        data = train |> drop_na()) |> 
    recipes::step_BoxCox(all_predictors()) |> 
    recipes::step_center(all_numeric(), -all_outcomes()) |>
    recipes::step_scale(all_numeric(), -all_outcomes())
  # Fit KNN model
  mod_knn <- caret::train(
    pp, 
    data = train, 
    method = "knn",
    trControl = caret::trainControl(method = "none"),
    tuneGrid = data.frame(k = k),
    metric = "MAE"
  )
  # Evaluate model on test set
  new_data <- drop_na(test)
  pred <- predict(mod_knn, newdata = new_data)
  obs <- new_data$GPP_NT_VUT_REF
  mae <- caret::MAE(pred, obs)
  
  return(mae)
}



# Define function to train and evaluate KNN model for across-site predictions
get_mae_across <- function(train, test, k) {
  library(rsample)
  library(tidyr)
  library(caret)
  library(recipes)
  library(ggplot2)
  
  # Model and pre-processing formulation, use all variables but LW_IN_F
  pp <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, 
                        data = train |> drop_na()) |> 
    recipes::step_BoxCox(all_predictors()) |> 
    recipes::step_center(all_numeric(), -all_outcomes()) |>
    recipes::step_scale(all_numeric(), -all_outcomes())
  # Fit KNN model
  mod_knn <- caret::train(
    pp, 
    data = train, 
    method = "knn",
    trControl = caret::trainControl(method = "none"),
    tuneGrid = data.frame(k = k),
    metric = "MAE"
  )
  # Evaluate model on test set
  new_data <- drop_na(test)
  pred <- predict(mod_knn, newdata = new_data)
  obs <- new_data$GPP_NT_VUT_REF
  mae <- caret::MAE(pred, obs)
  
  # Evaluate model on training set from the other site
  if (train == FLX_Lae_train) {
    train_across <- FLX_Dav_train
    test_across <- FLX_Dav_test
  } else {
    train_across <- FLX_Lae_train
    test_across <- FLX_Lae_test
  }
  new_data_across <- drop_na(test_across)
}





# Define function to train and evaluate KNN model for across-site predictions
get_mae_across_1 <- function(train, test, k) {
  library(rsample)
  library(tidyr)
  library(caret)
  library(recipes)
  library(ggplot2)
  
  # Model and pre-processing formulation, use all variables but LW_IN_F
  pp <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, 
                        data = train |> drop_na()) |> 
    recipes::step_BoxCox(all_predictors()) |> 
    recipes::step_center(all_numeric(), -all_outcomes()) |>
    recipes::step_scale(all_numeric(), -all_outcomes())
  # Fit KNN model
  mod_knn <- caret::train(
    pp, 
    data = train, 
    method = "knn",
    trControl = caret::trainControl(method = "none"),
    tuneGrid = data.frame(k = k),
    metric = "MAE"
  )
  # Evaluate model on test set
  new_data <- drop_na(test)
  pred <- predict(mod_knn, newdata = new_data)
  obs <- new_data$GPP_NT_VUT_REF
  mae <- caret::MAE(pred, obs)
  
  # Evaluate model on training set from the other site
  if (train == FLX_Lae_train) {
    train_across <- FLX_Dav_train
    test_across <- FLX_Dav_test
  } else {
    train_across <- FLX_Lae_train
    test_across <- FLX_Lae_test
  }
  new_data_across <- drop_na(test_across)
  
  # Predict on the training set from the other site
  pred_across <- predict(mod_knn, newdata = new_data_across)
  obs_across <- new_data_across$GPP_NT_VUT_REF
  mae_across <- caret::MAE(pred_across, obs_across)
  
  # Return the MAE of the test set and across-site prediction on the training set from the other site
  return(list(mae_test = mae, mae_across = mae_across))
}

