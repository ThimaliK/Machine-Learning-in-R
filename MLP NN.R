setwd("C:\\Users\\ACER\\L05\\ML")

library(readxl)     # to read the dataset
library(tidyverse)  # to view dataframes
library(neuralnet)  # to build, train and test the NN model
library(MLmetrics)  # to calculate MAE, MAPE and RMSE of predicted results
library(tidymodels) 

# original data set-------------------------------------------------------------

original_dataset <- read_excel("ExchangeUSD.xlsx")
view(original_dataset)

original_exchange_rates <- original_dataset[3]
original_dates <- original_dataset[1] # for visualizations

# Input/Output Vectors ---------------------------------------------------------

lag_1_inputs <- original_exchange_rates$`USD/EUR`[1:499]
lag_1_outputs <- original_exchange_rates$`USD/EUR`[2:500]
exchange_rates_lag1 <- cbind(lag_1_inputs, lag_1_outputs)
view(exchange_rates_lag1)

lag_2_inputs <- original_exchange_rates$`USD/EUR`[1:498]
lag_2_outputs <- original_exchange_rates$`USD/EUR`[3:500]
exchange_rates_lag2 <- cbind(lag_2_inputs, lag_2_outputs)
view(exchange_rates_lag2)

lag_3_inputs <- original_exchange_rates$`USD/EUR`[1:497]
lag_3_outputs <- original_exchange_rates$`USD/EUR`[4:500]
exchange_rates_lag3 <- cbind(lag_3_inputs, lag_3_outputs)
view(exchange_rates_lag3)

lag_4_inputs <- original_exchange_rates$`USD/EUR`[1:496]
lag_4_outputs <- original_exchange_rates$`USD/EUR`[5:500]
exchange_rates_lag4 <- cbind(lag_4_inputs, lag_4_outputs)
view(exchange_rates_lag4)

get_lagged_original_rates <- function(lag_by) {
  if(lag_by==1) {
    return(exchange_rates_lag1)
  } else if(lag_by==2) {
    return(exchange_rates_lag2)
  } else if(lag_by==3) {
    return(exchange_rates_lag3)
  } else if(lag_by==4) {
    return(exchange_rates_lag4)
  } else {
    print("wrong lag_by value")
  }
}

# Data Normalization -----------------------------------------------------------

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

normalized_exchange_rates_lag1 <- lapply(data.frame(exchange_rates_lag1), 
                                        normalize)

normalized_exchange_rates_lag2 <- lapply(data.frame(exchange_rates_lag2), 
                                        normalize)

normalized_exchange_rates_lag3 <- lapply(data.frame(exchange_rates_lag3), 
                                        normalize)

normalized_exchange_rates_lag4 <- lapply(data.frame(exchange_rates_lag4), 
                                        normalize)

df_for_nn <- function(lag_by) {
  if(lag_by==1) {
    return(normalized_exchange_rates_lag1)
  } else if(lag_by==2) {
    return(normalized_exchange_rates_lag2)
  } else if(lag_by==3) {
    return(normalized_exchange_rates_lag3)
  } else if(lag_by==4) {
    return(normalized_exchange_rates_lag4)
  } else {
    print("wrong lag_by value")
  }
}

# Training Model ---------------------------------------------------------------

get_training_data <- function(lag_by) {
  full_df <- df_for_nn(lag_by)
  
  #first 400 rows are always taken for training
  df_train <- data.frame(full_df)[1:400, ]     
  return(df_train)
}

train_model_with_2_hidden_layers <- function(n1, n2, lag_by) {
  
  df_train <- get_training_data(lag_by)   
  output_exchange_rates <- df_train[, 2]
  lagged_exchange_rates <- df_train[, 1]
  
  training_data <- cbind(lagged_exchange_rates, output_exchange_rates)
  
  # Generates a random seed which allows to reproduce results
  set.seed(12345)
  trained_model <- neuralnet(output_exchange_rates~lagged_exchange_rates, 
                             data=training_data, hidden = c(n1, n2), 
                             linear.output=TRUE)
  return(trained_model)
}

train_model_with_1_hidden_layer <- function(n, lag_by) {
  
  df_train <- get_training_data(lag_by)
  output_exchange_rates <- df_train[, 2]
  lagged_exchange_rates <- df_train[, 1]
  
  training_data <- cbind(lagged_exchange_rates, output_exchange_rates)
  
  # Generates a random seed which allows to reproduce results
  set.seed(12345)
  trained_model <- neuralnet(output_exchange_rates~lagged_exchange_rates, 
                             data=training_data, hidden = c(n), 
                             linear.output=TRUE)
  return(trained_model)
}

train_model_with_diff_learning_rate <- function(n, rate, lag_by) {
  
  df_train <- get_training_data(lag_by)
  output_exchange_rates <- df_train[, 2]
  lagged_exchange_rates <- df_train[, 1]
  
  training_data <- cbind(lagged_exchange_rates, output_exchange_rates)
  
  # Generates a random seed which allows to reproduce results
  set.seed(12345)
  trained_model <- neuralnet(output_exchange_rates~lagged_exchange_rates, 
                             data=training_data, hidden = c(n), 
                             linear.output=TRUE, learningrate = rate)
  return(trained_model)
}

train_model_with_diff_actfunction <- function(n, act_function, lag_by) {
  
  df_train <- get_training_data(lag_by)
  output_exchange_rates <- df_train[, 2]
  lagged_exchange_rates <- df_train[, 1]
  
  training_data <- cbind(lagged_exchange_rates, output_exchange_rates)
  
  # Generates a random seed which allows to reproduce results
  set.seed(12345)
  trained_model <- neuralnet(output_exchange_rates~lagged_exchange_rates, 
                             data=training_data, hidden = c(n), 
                             linear.output=TRUE, act.fct = act_function)
  return(trained_model)
}


# Testing the model ------------------------------------------------------------

test_model <- function(model, lag_by) {
  full_dataset <- df_for_nn(lag_by)
  df = data.frame(full_dataset)
  all_inputs = df[, 1]
  test_bound <- 500-lag_by
  inputs = all_inputs[401:test_bound]
  predictions = predict(model, data.frame(inputs))
  return(predictions)
  
}

# un-normalizing model results and predictions ---------------------------------

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

get_unnormalized_predicted_rates <- function(predictions, lag_by) {
  
  exchange_rates <- get_lagged_original_rates(lag_by)
  unnorm_predictions <- unnormalize(predictions, min(exchange_rates[, 2]), 
                                    max(exchange_rates[, 2]))
  print(unnorm_predictions)
  return(unnorm_predictions)
}

get_unnormalized_model_results <- function(model, lag_by) {
  
  model_net_result = model$net.result[[1]][,1]
  exchange_rates <- get_lagged_original_rates(lag_by)
  unnorm_model_results <- unnormalize(model_net_result, 
                                      min(exchange_rates[, 2]), 
                                      max(exchange_rates[, 2]))
  return(unnorm_model_results)
}

# visualizing results ----------------------------------------------------------

get_actual_values <- function(lag_by) {
  full_df <- get_lagged_original_rates(lag_by)
  test_boundary <- 500-lag_by
  df_test <- data.frame(full_df)[401:test_boundary, 2]
  return(df_test)
}

visualize_results <- function(model, predictions, lag_by) {
  
  original_lagged_rates <- get_lagged_original_rates(lag_by)
  unnorm_model_results <- get_unnormalized_model_results(model, lag_by)
  unnorm_predictions <- get_unnormalized_predicted_rates(predictions, lag_by)
  
  upper_bound <- lag_by+1
  middle_bound <- 400+lag_by
  lower_bound <- middle_bound+1
  
  plot(original_dataset$`YYYY/MM/DD`[upper_bound:500], 
       original_lagged_rates[, 2], type="l", col="red", xlab= "Time", 
       ylab = "Exchange Rate", main = "Actual vs Predicted")
  lines(original_dataset$`YYYY/MM/DD`[upper_bound:middle_bound], 
        unnorm_model_results, type="l", col="green")
  lines(original_dataset$`YYYY/MM/DD`[lower_bound:500], unnorm_predictions, 
        type="l", col="blue")
}

# model evaluation -------------------------------------------------------------

get_actual_outputs <- function(lag_by) {
  full_dataset <- df_for_nn(lag_by)
  df = data.frame(full_dataset)
  all_outputs = df[, 2]
  test_bound <- 500-lag_by
  actual <- all_outputs[401:test_bound]
  return(actual)
}

get_mae <- function(lag_by, predictions) {
  actual <- get_actual_outputs(lag_by)
  mae <- MAE(actual, predictions)
  return(mae)
  
}

get_mape <- function(lag_by, predicted) {
  actual <- get_actual_outputs(lag_by)
  mape <- MAPE(actual, predicted)
  return(mape)
}

get_rmse <- function(lag_by, predictions) {
  actual <- get_actual_outputs(lag_by)
  rmse <- RMSE(actual, predictions)
  return(rmse)
}

# ------------------------------------------------------------------------------

error_vs_nodes_2_layers <- function(lag_by) {
  mape_list <- list()
  count <- 1
  for (x in 1:10) {
    for(y in 1:10) {
      model_2 <- train_model_with_2_hidden_layers(x, y, lag_by)
      predicted_2 <- test_model(model_2, lag_by)
      mape_v <- get_mape(lag_by, predicted_2)
      mape_list[count] <- mape_v
      count <- count+1
    }
  }
  return(mape_list)
}

no_of_nodes = list(1)
for (i in 2:100) {
  no_of_nodes[i] = i
}

lag_by = 4
mape_value <- error_vs_nodes_2_layers(lag_by)
mape_value
plot(no_of_nodes, mape_value, type="l", 
     main="Error(MAPE) vs Number of nodes Nodes: lag_by = 1")


error_vs_nodes_for_1_layer <- function(lag_by) {
  
  mape_list = list()
  count <- 1
  for (x in 1:10) {
    model_1 <- train_model_with_1_hidden_layer(x, lag_by)
    predicted_1 <- test_model(model_1, lag_by)
    mape_v <- get_mape(lag_by, predicted_1)
    mape_list[count] <- mape_v
    count <- count+1
  }
  return(mape_list)
}


lag_by = 2
no_of_nodes = list(1)
for (i in 2:10) {
  no_of_nodes[i] = i
}
mape_value <- error_vs_nodes_for_1_layer(lag_by)
mape_value
plot(no_of_nodes, mape_value, type="l", 
     main="Error(MAPE) vs Number of nodes Nodes: lag_by = 2")

# using created functions ----------------------------------------------------------------

lag_by = 2
trained_model = train_model_with_1_hidden_layer(3, lag_by)
plot(trained_model)

predictions = test_model(trained_model, lag_by)

get_mape(lag_by, predictions)
get_mae(lag_by, predictions)
get_rmse(lag_by, predictions)

unnormalized_predictions = get_unnormalized_predicted_rates(predictions, 
                                                            lag_by)

actual_vs_predicted <- data.frame(cbind(get_actual_values(lag_by), 
                                        unnormalized_predictions))  # try to label columns
view(actual_vs_predicted)

visualize_results(trained_model, predictions, lag_by)



lag_by = 1
trained_model_2 = train_model_with_2_hidden_layers(7, 3, lag_by)
plot(trained_model_2)

predicted_2 = test_model(trained_model_2, lag_by)

get_mape(lag_by, predicted_2)
get_mae(lag_by, predicted_2)
get_rmse(lag_by, predicted_2)

unnormalized_predictions = get_unnormalized_predicted_rates(predicted_2, 
                                                            lag_by)

actual_vs_predicted <- data.frame(cbind(get_actual_values(lag_by), 
                                        unnormalized_predictions))  # try to label columns
view(actual_vs_predicted)

actual_vs_predicted %>% 
  rename(
    actual = X1,
    predicted = X2
  )

visualize_results(trained_model_2, predicted_2, lag_by)



lag_by = 1
trained_model_2 = train_model_with_diff_actfunction(3, "tanh", lag_by)
plot(trained_model_2)

predicted_2 = test_model(trained_model_2, lag_by)

get_mape(lag_by, predicted_2)
get_mae(lag_by, predicted_2)
get_rmse(lag_by, predicted_2)

unnormalized_predictions = get_unnormalized_predicted_rates(predicted_2, 
                                                            lag_by)

actual_vs_predicted <- data.frame(cbind(get_actual_values(lag_by), 
                                        unnormalized_predictions))  # try to label columns
view(actual_vs_predicted)

actual_vs_predicted %>% 
  rename(
    actual = X1,
    predicted = X2
  )

visualize_results(trained_model_2, predicted_2, lag_by)