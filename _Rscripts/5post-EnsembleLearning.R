## Post from here ----
# rm(list=ls())
# gc()
# 
# setwd("C:\\Users\\Peter\\Downloads\\ProjektBD\\enernoc\\")

library(feather)
library(data.table)
library(ggplot2)
library(rpart)
library(party)
library(forecast)
library(randomForest)
library(plotly)

DT <- as.data.table(read_feather("DT_load_17weeks"))

# store information of the type of consumer, date, weekday and period
n_date <- unique(DT[, date])
period <- 48

# store my default favourite theme to object for time series with ggplot2 and plot available data
theme_ts <- theme(panel.border = element_rect(fill = NA, 
                                              colour = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.minor = element_line(colour = "grey85"),
                  panel.grid.major = element_line(colour = "grey85"),
                  panel.grid.major.x = element_line(colour = "grey85"),
                  axis.text = element_text(size = 13, face = "bold"),
                  axis.title = element_text(size = 15, face = "bold"),
                  plot.title = element_text(size = 16, face = "bold"),
                  strip.text = element_text(size = 16, face = "bold"),
                  strip.background = element_rect(colour = "black"),
                  legend.text = element_text(size = 15),
                  legend.title = element_text(size = 16, face = "bold"),
                  legend.background = element_rect(fill = "white"),
                  legend.key = element_rect(fill = "white"))

# ggplot(DT, aes(date_time, value)) +
#   geom_line() +
#   theme_ts

# split data on train and test part, pick aggregated Light Industrial consumers as example case.
data_train <- DT[date %in% n_date[1:21]]
data_test <- DT[date %in% n_date[22]]

# plot train part electricity load data

ggplot(data_train, aes(date_time, value)) +
  geom_line() +
  labs(x = "Date", y = "Load (kW)") +
  theme_ts

# look on previous post on simple regression trees methods and forecasting
# trend, lag and fourier!
RpartTrend <- function(data, set_of_date, K, period = 48){
  
  data_train <- data[date %in% set_of_date]
  
  N <- nrow(data_train)
  window <- (N / period) - 1
  
  data_ts <- msts(data_train$value, seasonal.periods = c(period, period*7))
  
  fuur <- fourier(data_ts, K = c(K, K))
  fuur_test <- as.data.frame(fourier(data_ts, K = c(K, K), h = period))
  
  data_ts <- ts(data_train$value, freq = period*7)
  decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)
  new_load <- rowSums(decomp_ts$time.series[, c(1,3)])
  trend_part <- ts(decomp_ts$time.series[,2])
  
  trend_fit <- auto.arima(trend_part)
  trend_for <- as.vector(forecast(trend_fit, period)$mean)
  
  lag_seas <- decomp_ts$time.series[1:(period*window), 1]
  
  matrix_train <- data.table(Load = tail(new_load, window*period),
                             fuur[(period+1):N,],
                             Lag = lag_seas)
  
  tree_1 <- rpart(Load ~ ., data = matrix_train,
                  control = rpart.control(minsplit = 2,
                                          maxdepth = 30,
                                          cp = 0.000001))
  
  test_lag <- decomp_ts$time.series[((period*(window))+1):N, 1]
  
  matrix_test <- data.table(fuur_test,
                            Lag = test_lag)
  
  # new data and prediction
  pred_tree <- predict(tree_1, matrix_test) + trend_for
  
  return(as.vector(pred_tree))
}

CtreeTrend <- function(data, set_of_date, K, period = 48){
  
  # subsetting the dataset by dates
  data_train <- data[date %in% set_of_date]
  
  N <- nrow(data_train)
  window <- (N / period) - 1
  
  data_ts <- msts(data_train$value, seasonal.periods = c(period, period*7))
  
  fuur <- fourier(data_ts, K = c(K, K))
  fuur_test <- as.data.frame(fourier(data_ts, K = c(K, K), h = period))
  
  data_ts <- ts(data_train$value, freq = period*7)
  decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)
  new_load <- rowSums(decomp_ts$time.series[, c(1,3)])
  trend_part <- ts(decomp_ts$time.series[,2])
  
  trend_fit <- auto.arima(trend_part)
  trend_for <- as.vector(forecast(trend_fit, period)$mean)
  
  lag_seas <- decomp_ts$time.series[1:(period*window), 1]
  
  matrix_train <- data.table(Load = tail(new_load, window*period),
                             fuur[(period+1):N,],
                             Lag = lag_seas)
  
  tree_2 <- party::ctree(Load ~ ., data = matrix_train,
                         controls = party::ctree_control(teststat = c("quad"),
                                                         testtype = c("Teststatistic"),
                                                         mincriterion = 0.925,
                                                         minsplit = 1,
                                                         minbucket = 1))
  
  test_lag <- decomp_ts$time.series[((period*(window))+1):N, 1]
  
  matrix_test <- data.table(fuur_test,
                            Lag = test_lag)
  
  pred_tree <- predict(tree_2, matrix_test) + trend_for
  
  return(as.vector(pred_tree))
}

# Bagging ----
# what is Bagging? Bootstrap aggregating -> sampling training set with replacement and aggregating final predictions by average or median.
# simple example how to do it. Sample train set and also sample ratio and some hyperparameteres.
#  Firstly, construct training data matrix as was done in previous post.

data_ts <- ts(data_train$value, freq = period * 7)
decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)$time.series

trend_part <- ts(decomp_ts[,2])

trend_fit <- auto.arima(trend_part)
trend_for <- as.vector(forecast(trend_fit, period)$mean)

data_msts <- msts(data_train$value, seasonal.periods = c(period, period*7))

K <- 2
fuur <- fourier(data_msts, K = c(K, K)) # Fourier features to model (Daily and Weekly)

N <- nrow(data_train)
window <- (N / period) - 1

new_load <- rowSums(decomp_ts[, c(1,3)]) # detrending original time series
lag_seas <- decomp_ts[1:(period*window), 1] # lag feature to model

matrix_train <- data.table(Load = tail(new_load, window*period),
                           fuur[(period + 1):N,],
                           Lag = lag_seas)

# create testing data matrix
test_lag <- decomp_ts[((period*window)+1):N, 1]
fuur_test <- fourier(data_msts, K = c(K, K), h = period)

matrix_test <- data.table(fuur_test,
                          Lag = test_lag)

N_boot <- 100 # number of bootstraps

pred_mat <- matrix(0, nrow = N_boot, ncol = period)
for(i in 1:N_boot) {
  
  matrixSam <- matrix_train[sample(1:(N-period), floor((N-period) * sample(seq(0.7, 0.9, by = 0.01), 1)), replace = TRUE)] # sampling with sampled ratio from 0.7 to 0.9
  tree_bag <- rpart(Load ~ ., data = matrixSam,
                    control = rpart.control(minsplit = sample(2:3, 1),
                                            maxdepth = sample(26:30, 1),
                                            cp = sample(seq(0.0000009, 0.00001, by = 0.0000001), 1)))
  
  # new data and prediction
  pred_mat[i,] <- predict(tree_bag, matrix_test) + mean(trend_for)
}

# Visualize forecasts and median of them
pred_melt_rpart <- data.table(melt(pred_mat))

pred_ave_rpart <- pred_melt_rpart[, .(value = median(value)), by = .(Var2)]
pred_ave_rpart[, Var1 := "RPART_Bagg"]

ggplot(pred_melt_rpart, aes(Var2, value, group = Var1)) +
  geom_line(alpha = 0.75) +
  geom_line(data = pred_ave_rpart, aes(Var2, value), color = "firebrick2", alpha = 0.9, size = 2) +
  labs(x = "Time", y = "Load (kW)", title = "Bagging with RPART") +
  theme_ts

# perform rpart without bagging and compare it with bagged raprt
simple_rpart <- RpartTrend(DT, n_date[1:21], K = 2)
pred_rpart <- data.table(value = simple_rpart, Var2 = 1:48, Var1 = "RPART")

ggplot(pred_melt_rpart, aes(Var2, value, group = Var1)) +
  geom_line(alpha = 0.75) +
  geom_line(data = pred_ave_rpart, aes(Var2, value), color = "firebrick2", alpha = 0.8, size = 1.8) +
  geom_line(data = pred_rpart, aes(Var2, value), color = "dodgerblue2", alpha = 0.8, size = 1.8) +
  labs(x = "Time", y = "Load (kW)", title = "Bagging with RPART and comparison with simple RPART") +
  theme_ts

# difference is evident

# Do the same with CTREE

pred_mat <- matrix(0, nrow = N_boot, ncol = period)
for(i in 1:N_boot) {
  
  matrixSam <- matrix_train[sample(1:(N-period), floor((N-period) * sample(seq(0.7, 0.9, by = 0.01), 1)), replace = TRUE)] # sampling with sampled ratio from 0.7 to 0.9
  tree_bag <- party::ctree(Load ~ ., data = matrixSam,
                           controls = party::ctree_control(teststat = c("quad"),
                                                           testtype = c("Teststatistic"),
                                                           mincriterion = sample(seq(0.88, 0.97, by = 0.005), 1),
                                                           minsplit = 1,
                                                           minbucket = 1,
                                                           mtry = 0, maxdepth = 0))
  
  # new data and prediction
  pred_mat[i,] <- predict(tree_bag, matrix_test) + mean(trend_for)
}

# Visualize forecasts and median of them
pred_melt_ctree <- data.table(melt(pred_mat))

pred_ave_ctree <- pred_melt_ctree[, .(value = median(value)), by = .(Var2)]
pred_ave_ctree[, Var1 := "CTREE_Bagg"]

ggplot(pred_melt_ctree, aes(Var2, value, group = Var1)) +
  geom_line(alpha = 0.75) +
  geom_line(data = pred_ave_ctree, aes(Var2, value), color = "firebrick2", alpha = 0.9, size = 2) +
  labs(x = "Time", y = "Load (kW)", title = "Bagging with CTREE") +
  theme_ts

# perform rpart without bagging and compare it with bagged raprt
simple_ctree <- CtreeTrend(DT, n_date[1:21], K = 2)
pred_ctree <- data.table(value = simple_ctree, Var2 = 1:48, Var1 = "CTREE")

ggplot(pred_melt_ctree, aes(Var2, value, group = Var1)) +
  geom_line(alpha = 0.75) +
  geom_line(data = pred_ave_ctree, aes(Var2, value), color = "firebrick2", alpha = 0.8, size = 1.8) +
  geom_line(data = pred_ctree, aes(Var2, value), color = "dodgerblue2", alpha = 0.8, size = 1.8) +
  labs(x = "Time", y = "Load (kW)", title = "Bagging with CTREE and comparison with simple CTREE") +
  theme_ts

# again the difference is evident

# Try some implemented and broad known ensemble learning method - Random Forest.
# Random Forecast is mixture of bagging and boosting methods...
# RF----

rf_model <- randomForest(Load ~ ., data = data.frame(matrix_train),
                         ntree = 1000, mtry = 3, nodesize = 3, importance = TRUE)

pred_rf <- predict(rf_model, data.frame(matrix_test)) + mean(trend_for)
pred_rf <- data.table(value = pred_rf, Var2 = 1:48, Var1 = "RF")

# Compare predictions from all EL methods so far and real electricity load
pred_true <- data.table(value = data_test$value, Var2 = 1:48, Var1 = "Real")
preds_all <- rbindlist(list(pred_ave_rpart, pred_ave_ctree, pred_rf, pred_true), use.names = T)

ggplot(preds_all, aes(Var2, value, color = as.factor(Var1))) +
  geom_line(alpha = 0.7, size = 1.2) +
  labs(x = "Time", y = "Load (kW)", title = "Comparison of Ensemble Learning forecasts") +
  guides(color=guide_legend(title="Method")) +
  theme_ts

# There is some differences in forecasts, but minimal

# Grid Search ----
RFgrid <- function(data_train, param1, param2, K, period = 48) {
  
  N <- length(data_train)
  window <- (N / period) - 1
  
  data_ts <- msts(data_train, seasonal.periods = c(period, period*7))
  
  fuur <- fourier(data_ts, K = c(K, K))
  fuur_test <- as.data.frame(fourier(data_ts, K = c(K, K), h = period))
  
  data_ts <- ts(data_train, freq = period*7)
  decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)
  new_load <- rowSums(decomp_ts$time.series[, c(1,3)])
  trend_part <- ts(decomp_ts$time.series[,2])
  
  trend_fit <- auto.arima(trend_part)
  trend_for <- as.vector(forecast(trend_fit, period)$mean)
  
  lag_seas <- decomp_ts$time.series[1:(period*window), 1]
  
  matrix_train <- data.frame(Load = tail(new_load, window*period),
                             fuur[(period+1):N,],
                             Lag = lag_seas)
  
  tree_2 <- randomForest(Load ~ ., data = matrix_train,
                         ntree = 1000, mtry = param1, nodesize = param2, importance = TRUE)
  
  test_lag <- decomp_ts$time.series[((period*(window))+1):N, 1]
  
  matrix_test <- data.frame(fuur_test,
                            Lag = test_lag)
  
  pred_tree <- predict(tree_2, matrix_test) + mean(trend_for)
  
  return(as.vector(pred_tree))
}

mape <- function(real, pred){
  return(100 * mean(abs((real - pred)/real))) # MAPE - Mean Absolute Percentage Error
}

gridSearch <- function(Y, train.win = 21, FUN, param1, param2, period = 48) {
  
  days <- length(Y)/period
  test.days <- days - train.win
  mape.matrix <- matrix(0, nrow = length(param1), ncol = length(param2))
  row.names(mape.matrix) <- param1
  colnames(mape.matrix) <- param2
  forecast.svr <- vector(length = test.days*period)
  
  for(i in seq_along(param1)){
    for(j in seq_along(param2)){
      for(k in 0:(test.days-1)){
        train.set <- Y[((k*period)+1):((period*k)+(train.win*period))]
        forecast.svr[((k*period)+1):((k+1)*period)] <- FUN(train.set, param1 = param1[i], param2 = param2[j], K = 2)
      }
      mape.matrix[i,j] <- mape(Y[-(1:(train.win*period))], forecast.svr)
    }
  }
  return(mape.matrix)
}

all_data <- DT$value[1:(period*41)]
res_1 <- gridSearch(all_data, FUN = RFgrid, param1 = c(2,3,4,5,6), param2 = c(2,3,4,5,6))

res_1

c(mtry = row.names(res_1)[which(res_1 == min(res_1), arr.ind = TRUE)[1]],
  nodesize = colnames(res_1)[which(res_1 == min(res_1), arr.ind = TRUE)[2]])

data_grid <- data.table(melt(res_1))
colnames(data_grid) <- c("mtry", "nodesize", "MAPE")

ggplot(data_grid, aes(mtry, nodesize, size = MAPE, color = MAPE)) +
  geom_point() +
  scale_color_distiller(palette = "Reds") +
  theme_ts

# Comparison ----
RFTrend <- function(data, set_of_date, K, period = 48) {
  
  # subsetting the dataset by dates
  data_train <- data[date %in% set_of_date]
  
  N <- nrow(data_train)
  window <- (N / period) - 1
  
  data_ts <- msts(data_train$value, seasonal.periods = c(period, period*7))
  
  fuur <- fourier(data_ts, K = c(K, K))
  fuur_test <- as.data.frame(fourier(data_ts, K = c(K, K), h = period))
  
  data_ts <- ts(data_train$value, freq = period*7)
  decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)
  new_load <- rowSums(decomp_ts$time.series[, c(1,3)])
  trend_part <- ts(decomp_ts$time.series[,2])
  
  trend_fit <- auto.arima(trend_part)
  trend_for <- as.vector(forecast(trend_fit, period)$mean)
  
  lag_seas <- decomp_ts$time.series[1:(period*window), 1]
  
  matrix_train <- data.frame(Load = tail(new_load, window*period),
                             fuur[(period+1):N,],
                             Lag = lag_seas)
  
  tree_2 <- randomForest(Load ~ ., data = matrix_train,
                         ntree = 1000, mtry = 6, nodesize = 2, importance = TRUE)
  
  test_lag <- decomp_ts$time.series[((period*(window))+1):N, 1]
  
  matrix_test <- data.frame(fuur_test,
                            Lag = test_lag)
  
  pred_tree <- predict(tree_2, matrix_test) + mean(trend_for)
  
  return(as.vector(pred_tree))
}

RpartBaggTrend <- function(data, set_of_date, K, period = 48, N_boot = 100) {
  
  # subsetting the dataset by dates
  data_train <- data[date %in% set_of_date]
  
  N <- nrow(data_train)
  window <- (N / period) - 1
  
  data_ts <- msts(data_train$value, seasonal.periods = c(period, period*7))
  
  fuur <- fourier(data_ts, K = c(K, K))
  fuur_test <- as.data.frame(fourier(data_ts, K = c(K, K), h = period))
  
  data_ts <- ts(data_train$value, freq = period*7)
  decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)
  new_load <- rowSums(decomp_ts$time.series[, c(1,3)])
  trend_part <- ts(decomp_ts$time.series[,2])
  
  trend_fit <- auto.arima(trend_part)
  trend_for <- as.vector(forecast(trend_fit, period)$mean)
  
  lag_seas <- decomp_ts$time.series[1:(period*window), 1]
  
  matrix_train <- data.table(Load = tail(new_load, window*period),
                             fuur[(period+1):N,],
                             Lag = lag_seas)
  
  test_lag <- decomp_ts$time.series[((period*(window))+1):N, 1]
  
  matrix_test <- data.table(fuur_test,
                            Lag = test_lag)
  
  pred_mat <- matrix(0, nrow = N_boot, ncol = period)
  for(i in 1:N_boot) {
    matrixSam <- matrix_train[sample(1:(N-period), floor((N-period) * sample(seq(0.7, 0.9, by = 0.01), 1)), replace = TRUE)] # sampling with sampled ratio from 0.7 to 0.9
    tree_bag <- rpart(Load ~ ., data = matrixSam,
                      control = rpart.control(minsplit = sample(2:3, 1),
                                              maxdepth = sample(26:30, 1),
                                              cp = sample(seq(0.0000009, 0.00001, by = 0.0000001), 1)))
    
    # new data and prediction
    pred_mat[i,] <- predict(tree_bag, matrix_test) + mean(trend_for)
  }
  
  return(as.vector(apply(pred_mat, 2, median)))
}

CtreeBaggTrend <- function(data, set_of_date, K, period = 48, N_boot = 100) {
  
  # subsetting the dataset by dates
  data_train <- data[date %in% set_of_date]
  
  N <- nrow(data_train)
  window <- (N / period) - 1
  
  data_ts <- msts(data_train$value, seasonal.periods = c(period, period*7))
  
  fuur <- fourier(data_ts, K = c(K, K))
  fuur_test <- as.data.frame(fourier(data_ts, K = c(K, K), h = period))
  
  data_ts <- ts(data_train$value, freq = period*7)
  decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)
  new_load <- rowSums(decomp_ts$time.series[, c(1,3)])
  trend_part <- ts(decomp_ts$time.series[,2])
  
  trend_fit <- auto.arima(trend_part)
  trend_for <- as.vector(forecast(trend_fit, period)$mean)
  
  lag_seas <- decomp_ts$time.series[1:(period*window), 1]
  
  matrix_train <- data.table(Load = tail(new_load, window*period),
                             fuur[(period+1):N,],
                             Lag = lag_seas)
  
  test_lag <- decomp_ts$time.series[((period*(window))+1):N, 1]
  
  matrix_test <- data.table(fuur_test,
                            Lag = test_lag)
  
  pred_mat <- matrix(0, nrow = N_boot, ncol = period)
  for(i in 1:N_boot) {
    matrixSam <- matrix_train[sample(1:(N-period), floor((N-period) * sample(seq(0.7, 0.9, by = 0.01), 1)), replace = TRUE)] # sampling with sampled ratio from 0.7 to 0.9
    tree_bag <- party::ctree(Load ~ ., data = matrixSam,
                             controls = party::ctree_control(teststat = c("quad"),
                                                             testtype = c("Teststatistic"),
                                                             mincriterion = sample(seq(0.88, 0.97, by = 0.005), 1),
                                                             minsplit = 1,
                                                             minbucket = 1,
                                                             mtry = 0, maxdepth = 0))
    
    # new data and prediction
    pred_mat[i,] <- predict(tree_bag, matrix_test) + mean(trend_for)
  }
  
  return(as.vector(apply(pred_mat, 2, median)))
}

# Evaluation ----
n_days <- floor(length(n_date)) - 21

for_rpart <- sapply(0:(n_days-1), function(i)
  RpartBaggTrend(DT, n_date[(i+1):(i+7*3)], K = 2))

for_ctree <- sapply(0:(n_days-1), function(i)
  CtreeBaggTrend(DT, n_date[(i+1):(i+7*3)], K = 2))

for_rpart_sim <- sapply(0:(n_days-1), function(i)
  RpartTrend(DT, n_date[(i+1):(i+7*3)], K = 2))

for_ctree_sim <- sapply(0:(n_days-1), function(i)
  CtreeTrend(DT, n_date[(i+1):(i+7*3)], K = 2))

for_rf <- sapply(0:(n_days-1), function(i)
  RFTrend(DT, n_date[(i+1):(i+7*3)], K = 2))

err_mape_rpart <- sapply(0:(n_days-1), function(i)
  mape(DT[date %in% n_date[22+i], value],
       for_rpart[,i+1]))

err_mape_ctree <- sapply(0:(n_days-1), function(i)
  mape(DT[date %in% n_date[22+i], value],
       for_ctree[,i+1]))

err_mape_rpart_sim <- sapply(0:(n_days-1), function(i)
  mape(DT[date %in% n_date[22+i], value],
       for_rpart_sim[,i+1]))

err_mape_ctree_sim <- sapply(0:(n_days-1), function(i)
  mape(DT[date %in% n_date[22+i], value],
       for_ctree_sim[,i+1]))

err_mape_rf <- sapply(0:(n_days-1), function(i)
  mape(DT[date %in% n_date[22+i], value],
       for_rf[,i+1]))

summary(err_mape_rpart)
summary(err_mape_ctree)
summary(err_mape_rpart_sim)
summary(err_mape_ctree_sim)
summary(err_mape_rf)

# plotly
library(plotly)
datas <- data.table(MAPE = c(err_mape_rpart_sim, err_mape_ctree_sim, err_mape_rpart, err_mape_ctree, err_mape_rf),
                    Method = factor(c(rep("RPART_Simple", n_days),
                                         rep("CTREE_Simple", n_days),
                                         rep("RPART_Bagging", n_days),
                                         rep("CTREE_Bagging", n_days),
                                         rep("RandomForest", n_days)),
                                    levels = c("RPART_Simple", "RPART_Bagging",
                                               "CTREE_Simple", "CTREE_Bagging",
                                               "RandomForest")))

ggplot(datas, aes(Method, MAPE, fill = Method)) +
  geom_boxplot() +
  theme_ts

ggplotly()

api_create(x = last_plot(), filename = "ensembles_v1",
           sharing = "public")

# There is possibility to use another great methods like extraTrees, gbm or xgboost, lightGBM, catboost (if i will manage installation in windows machine)
# still doing on it
