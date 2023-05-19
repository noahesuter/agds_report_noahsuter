### Reading and Cleaning the Davos dataset

library(lubridate)
library(dplyr)
FLX_Dav <- readr::read_csv("../data/FLX_CH-Dav_FLUXNET2015_FULLSET_DD_1997-2014_1-3.csv")|> # reading the data for Davos into R   
  
  # select only the variables we are interested in
  dplyr::select(TIMESTAMP,
                GPP_NT_VUT_REF,    # the target
                ends_with("_QC"),  # quality control info
                ends_with("_F"),   # includes all all meteorological covariates
                -contains("JSB")   # weird useless variable
  ) |>
  
  # convert to a nice date object
  dplyr::mutate(TIMESTAMP = lubridate::ymd(TIMESTAMP)) |>
  
  # set all -9999 to NA
  mutate(across(where(is.numeric), ~na_if(., -9999))) |> #deleting all values -9999
  
  
  # retain only data based on >=80% good-quality measurements
  # overwrite bad data with NA (not dropping rows)
  dplyr::mutate(GPP_NT_VUT_REF = ifelse(NEE_VUT_REF_QC < 0.8, NA, GPP_NT_VUT_REF),
                TA_F           = ifelse(TA_F_QC        < 0.8, NA, TA_F),
                SW_IN_F        = ifelse(SW_IN_F_QC     < 0.8, NA, SW_IN_F),
                LW_IN_F        = ifelse(LW_IN_F_QC     < 0.8, NA, LW_IN_F),
                VPD_F          = ifelse(VPD_F_QC       < 0.8, NA, VPD_F),
                PA_F           = ifelse(PA_F_QC        < 0.8, NA, PA_F),
                P_F            = ifelse(P_F_QC         < 0.8, NA, P_F),
                WS_F           = ifelse(WS_F_QC        < 0.8, NA, WS_F)) |> 
  
  # drop QC variables (no longer needed)
  dplyr::select(-ends_with("_QC"))



### Reading and Cleaning the Laegern dataset


library(lubridate)
library(dplyr)
FLX_Lae <- readr::read_csv("../data/FLX_CH-Lae_FLUXNET2015_FULLSET_DD_2004-2014_1-4.csv") |> # reading the data for Laegern into R   
  
  # select only the variables we are interested in
  dplyr::select(TIMESTAMP,
                GPP_NT_VUT_REF,    # the target
                ends_with("_QC"),  # quality control info
                ends_with("_F"),   # includes all all meteorological covariates
                -contains("JSB")   # weird useless variable
  ) |>
  
  # convert to a nice date object
  dplyr::mutate(TIMESTAMP = lubridate::ymd(TIMESTAMP)) |>
  
  # set all -9999 to NA
  mutate(across(where(is.numeric), ~na_if(., -9999))) |> #deleting all values -9999
  
  
  # retain only data based on >=80% good-quality measurements
  # overwrite bad data with NA (not dropping rows)
  dplyr::mutate(GPP_NT_VUT_REF = ifelse(NEE_VUT_REF_QC < 0.8, NA, GPP_NT_VUT_REF),
                TA_F           = ifelse(TA_F_QC        < 0.8, NA, TA_F),
                SW_IN_F        = ifelse(SW_IN_F_QC     < 0.8, NA, SW_IN_F),
                LW_IN_F        = ifelse(LW_IN_F_QC     < 0.8, NA, LW_IN_F),
                VPD_F          = ifelse(VPD_F_QC       < 0.8, NA, VPD_F),
                PA_F           = ifelse(PA_F_QC        < 0.8, NA, PA_F),
                P_F            = ifelse(P_F_QC         < 0.8, NA, P_F),
                WS_F           = ifelse(WS_F_QC        < 0.8, NA, WS_F)) |> 
  
  # drop QC variables (no longer needed)
  dplyr::select(-ends_with("_QC"))


### Splitting the data


# Data splitting for Dav with a 80/20 split
set.seed(1982)  # made for reproducibility
split <- rsample::initial_split(FLX_Dav, prop = 0.8)
FLX_Dav_train_1 <- rsample::training(split)
FLX_Dav_test_1 <- rsample::testing(split)



# Data splitting for Lae with a 80/20 split
set.seed(1982)  # made for reproducibility
split <- rsample::initial_split(FLX_Lae, prop = 0.8)
FLX_Lae_train_1 <- rsample::training(split)
FLX_Lae_test_1 <- rsample::testing(split)


### renaming the dataframes

FLX_Dav_test <- FLX_Dav_test_1
FLX_Dav_train <- FLX_Dav_train_1
FLX_Lae_test <- FLX_Lae_test_1
FLX_Lae_train <- FLX_Lae_train_1

### Deleting the column LW_IN_F


FLX_Dav_test$LW_IN_F <- NULL #deleting the column LW_IN_F in Davos test set



FLX_Dav_train$LW_IN_F <- NULL #deleting the column LW_IN_F in Davos train set


FLX_Lae_test$LW_IN_F <- NULL #deleting the column LW_IN_F in Laegern test set


FLX_Lae_train$LW_IN_F <- NULL #deleting the column LW_IN_F in Laegern train test


### Deleting the column P_F 

FLX_Lae_test$P_F <- NULL #deleting the column P_F in Laegern test set


FLX_Lae_train$P_F <- NULL #deleting the column P_F in Laegern train set


FLX_Dav_test$P_F <- NULL #deleting the column P_F in Davos test set


FLX_Dav_train$P_F <- NULL #deleting the column P_F in Davos train set

### Deleting the rows containing missing values

FLX_Dav_test<- stats::na.omit(FLX_Dav_test) #deleting all the rows with missing values in Davos test set


FLX_Dav_train<- stats::na.omit(FLX_Dav_train) #deleting all the rows with missing values in Davos train set



FLX_Lae_test<- stats::na.omit(FLX_Lae_test) #deleting all the rows with missing values in Laegern test set



FLX_Lae_train<- stats::na.omit(FLX_Lae_train) #deleting all the rows with missing values in Laegern train set


### Defining the recipes of the models for the train datasets (Davos and Laegern)


model_dav_train <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + PA_F + WS_F + TA_F, 
                                   data = FLX_Dav_train) |> #recipe for the model of the Davos dataset
  recipes::step_center(all_numeric(), -all_outcomes()) |>
  recipes::step_scale(all_numeric(), -all_outcomes())


model_Lae_train <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + PA_F + WS_F + TA_F, 
                                   data = FLX_Lae_train) |> #recipe for the model of the Davos dataset
  recipes::step_center(all_numeric(), -all_outcomes()) |>
  recipes::step_scale(all_numeric(), -all_outcomes())





### Using the caret function to determine the best model with maschine learning for Davos

# training the best knn model with the Davos train data
library(tidyverse)
library(recipes)
set.seed(1982)
caret_dav_train_knn <- caret::train(model_dav_train, 
                                    data = FLX_Dav_train |> drop_na(), 
                                    method = "knn",
                                    trControl = caret::trainControl(method = "cv", number = 10),
                                    tuneGrid = data.frame(k = c(2, 5, 10, 15, 20, 25, 30, 35, 40, 60, 100)),
                                    metric = "MAE")



# training the best lm model with the Davos train data
caret_dav_train_lm <- caret::train(model_dav_train, 
                                   data = FLX_Dav_train |> drop_na(),  # drop missing values
                                   trControl = caret::trainControl(method = "none"), # no resampling
                                   method = "lm"
)



### Using the caret function to determine the best model with maschine learning for Laegern   


# training the best knn model with the Laegern train data
library(tidyverse)
library(recipes)
set.seed(1982)
caret_Lae_train_knn <- caret::train(model_Lae_train, 
                                    data = FLX_Lae_train |> drop_na(), 
                                    method = "knn",
                                    trControl = caret::trainControl(method = "cv", number = 10),
                                    tuneGrid = data.frame(k = c(2, 5, 10, 15, 20, 25, 30, 35, 40, 60, 100)),
                                    metric = "MAE")




# training the best lm model with the Laegern train data
caret_Lae_train_lm <- caret::train(model_Lae_train, 
                                   data = FLX_Lae_train |> drop_na(),  # drop missing values
                                   trControl = caret::trainControl(method = "none"), # no resampling
                                   method = "lm"
)





### Pooling the dataframes together (each one for train and test)


library(base)
bind_FLX_train <- rbind(FLX_Lae_train, FLX_Dav_train)



library(base)
bind_FLX_test <- rbind(FLX_Lae_test, FLX_Dav_test)



### Building a recipe for the pooled model

model_combined_train <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + PA_F + WS_F + TA_F, 
                                        data = bind_FLX_train) |> #recipe for the model of the Davos dataset
  recipes::step_center(all_numeric(), -all_outcomes()) |>
  recipes::step_scale(all_numeric(), -all_outcomes())


# training the best knn model with the pooled train data
library(tidyverse)
library(recipes)
set.seed(1982)
caret_combined_train_knn <- caret::train(model_combined_train, 
                                         data = bind_FLX_train |> drop_na(), 
                                         method = "knn",
                                         trControl = caret::trainControl(method = "cv", number = 10),
                                         tuneGrid = data.frame(k = c(2, 5, 10, 15, 20, 25, 30, 35, 40, 60, 100)),
                                         metric = "MAE")




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
  

  # Fit KNN model
  mod_knn <- caret::train(
    model_dav_train, 
    data = FLX_Dav_train, 
    method = "knn",
    trControl = caret::trainControl(method = "none"),
    tuneGrid = data.frame(k = k),
    metric = "MAE"
  )
  # Evaluate model on test set
  new_data <- drop_na(FLX_Dav_test)
  pred <- predict(mod_knn, newdata = new_data)
  obs <- new_data$GPP_NT_VUT_REF
  mae <- caret::MAE(pred, obs)
  
  # Evaluate model on training set
  pred_train <- predict(mod_knn, newdata = drop_na(FLX_Dav_train))
  obs_train <- drop_na(FLX_Dav_train)$GPP_NT_VUT_REF
  mae_train <- caret::MAE(pred_train, obs_train)
  
  return(list("mae_train" = mae_train, "mae_test" = mae))
}
