# rm(list=ls())
# gc()

## Prepare DT ----
library(lubridate)

# setwd("C:\\Users\\Peter\\Downloads\\ProjektBD\\enernoc\\csv\\")

files <- list.files(pattern = "*.csv")
DT <- rbindlist(lapply(files, function(x) cbind(fread(x), gsub(".csv", "", x))))
str(DT)
setnames(DT, c("dttm_utc", "V2"), c("date", "ID"))

DT[, date_time := ymd_hms(DT[["date"]])]
DT[, date := as.Date(DT[["date"]], "%Y-%m-%d")]
DT[, ':='(timestamp = NULL, estimated = NULL, anomaly = NULL)]
str(DT)

# Extract ID's with an whole length (105408)
count_ID <- DT[, .N, ID]
full <- count_ID[N == max(N), .(ID)]
DT <- DT[ID %in% full[, ID]]

# Extract date's with an all measurements during the day (288)
num_date <- DT[ID == 100, .N, .(date)]
DT <- DT[!date %in% num_date[c(1,367), date]] # first and last date has not all measurements - so remove them
unique(DT[,ID]) # our extracted (filtered) ID's

DT_48 <- DT[, .(value = sum(value), date, ID, date_time), by = (seq(nrow(DT)) - 1) %/% 6]
DT_48 <- DT_48[seq(1, nrow(DT_48), by = 6)]
DT_48[, seq := NULL]

DT_agg <- as.data.table(aggregate(DT_48[, .(value)], by = DT_48[, .(date_time)], FUN = sum, simplify = TRUE))

DT_48[, week := weekdays(date_time)]
DT_agg[, ':='(week = weekdays(date_time), date = as.Date(date_time))]

# now we have datasets with all needed features to build model for different days
n_ID <- unique(DT_48[, ID])
n_weekdays <- unique(DT_agg[, week])
n_date <- unique(DT_agg[, date])
period <- 48

setwd("C:\\Users\\Peter\\Downloads\\ProjektBD\\enernoc\\")
meta_data <- fread("meta\\all_sites.csv")
# prepare meta_data
meta_data[, ':='(TIME_ZONE = NULL, TZ_OFFSET = NULL)]
setnames(meta_data, "SITE_ID", "ID")
meta_data[, ID := as.character(meta_data[["ID"]])]

indus <- unique(meta_data[ID %in% n_ID, INDUSTRY])
DT_48 <- merge(DT_48, meta_data[, .(ID, INDUSTRY)], by = "ID")

DT_ind_com <- as.data.table(aggregate(DT_48[INDUSTRY == indus[1], .(value)], by = DT_48[INDUSTRY == indus[1], .(date_time)], FUN = sum, simplify = TRUE))
DT_ind_edu <- as.data.table(aggregate(DT_48[INDUSTRY == indus[2], .(value)], by = DT_48[INDUSTRY == indus[2], .(date_time)], FUN = sum, simplify = TRUE))
DT_ind_food <- as.data.table(aggregate(DT_48[INDUSTRY == indus[3], .(value)], by = DT_48[INDUSTRY == indus[3], .(date_time)], FUN = sum, simplify = TRUE))
DT_ind_ind <- as.data.table(aggregate(DT_48[INDUSTRY == indus[4], .(value)], by = DT_48[INDUSTRY == indus[4], .(date_time)], FUN = sum, simplify = TRUE))
DT_ind_ind[, ':='(week = weekdays(date_time), date = as.Date(date_time))]
DT_ind_food[, ':='(week = weekdays(date_time), date = as.Date(date_time))]
DT_ind_edu[, ':='(week = weekdays(date_time), date = as.Date(date_time))]
DT_ind_com[, ':='(week = weekdays(date_time), date = as.Date(date_time))]
DT_ind_com[, type := indus[1]]
DT_ind_edu[, type := indus[2]]
DT_ind_food[, type := indus[3]]
DT_ind_ind[, type := indus[4]]
DT_all_agg <- rbindlist(list(DT_ind_com, DT_ind_edu, DT_ind_food, DT_ind_ind))

write_feather(DT_all_agg, "DT_4_ind")

## Post from here ----
setwd("C:\\Users\\Peter\\Downloads\\ProjektBD\\enernoc\\")

library(feather)
library(data.table)
library(ggplot2)
# library(MASS)
library(plotly)
library(animation)
# library(mgcv)

# Use feather (fast to share data) to read data.table
DT <- as.data.table(read_feather("DT_4_ind"))

# Plot aggregated time series of consumption by industry
ggplot(data = DT, aes(x = date, y = value)) +
  geom_line() + 
  facet_grid(type ~ ., scales = "free_y") +
  theme(panel.border = element_blank(), panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 9, face = "bold")) +
  labs(x = "Date", y = "Load (kW)")
# Food Sales & Storage...it is interesting that there aren't changes during holidays


# prepare DT to work with a regression model
# tranform charactres of weekdays to integers
DT[, week_num := as.integer(as.factor(DT[, week]))]

# store information of the type of consumer, date, weekday and period
n_type <- unique(DT[, type])
n_weekdays <- unique(DT[, week])
n_date <- unique(DT[, date])
period <- 48

# Let's look at some data chunk of consumption and try do some regression analysis. Pick aggregate consumption of education (schools) buildings.
data_r <- DT[(type == n_type[2] & date %in% n_date[57:70])]

ggplot(data_r, aes(date_time, value)) +
  geom_line() +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Load (kW)")

# Multiple linear regression (form, assumptions). Like in the previous form, we want to forecast consumption one week ahead,
# so construction of seasonal features is necessery. Let's create daily and weekly seasonal dummy variables. Form like 10001...11110000.
# Compute features to model and store it in matrix_train.
N <- nrow(data_r)

window <- N / period # number of days in the train set
# 1, ..., period, 1, ..., period - and so on for the daily season 
# using feature "week_num" for the weekly season
matrix_train <- data.table(Load = data_r[, value],
                           Daily = as.factor(rep(1:period, window)),
                           Weekly = as.factor(data_r[, week_num]))

# Collinearity and singularity, so w7 isn't constructed. Names of features are needed due to clarity and class of object `lm`.
# Lets create our first multiple linear model (I will refer it as MLR) with function `lm`. Intercept is inappropiate in this sceniario, again due to collinearity and meaningfulness.

lm_m_1 <- lm(Load ~ 0 + ., data = matrix_train)

smmr_1 <- summary(lm_m_1)
paste("R-squared: ",
      round(smmr_1$r.squared, 3),
      ", p-value of F test: ",
      1-pf(smmr_1$fstatistic[1], smmr_1$fstatistic[2], smmr_1$fstatistic[3]))

# sm_1$adj.r.squared
# 1-pf(sm_1$fstatistic[1], sm_1$fstatistic[2], sm_1$fstatistic[3])

# summary seems not bad, R-squared high, F-statistic of goodness of fit is fine too.
# Let's look on fitted values and residuals.

datas <- rbindlist(list(data_r[, .(value, date_time)], data.table(value = lm_m_1$fitted.values, data_time = data_r[, date_time])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]

ggplot(data = datas, aes(date_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from MLR")

# Fit vs residuals - heteroscedasticity - non constant and nonnormal residuals (assumptions)
ggplot(data = data.table(Fitted_values = lm_m_1$fitted.values, Residuals = lm_m_1$residuals),
       aes(Fitted_values, Residuals)) +
  geom_point(size = 1.5) +
  geom_smooth() +
  geom_hline(yintercept = 0, color = "red", size = 0.8) +
  labs(title = "Fitted vs Residuals")

# g <- ggplotly(width = 600, height = 500)
# plotly_POST(x = g, filename = "test_1", fileopt = "overwrite",
#             sharing = c("public"))

# Function to create Normal Q-Q plot with qqline
ggQQ <- function(lm){
  # extract standardized residuals from the fit
  d <- data.frame(std.resid = rstandard(lm))
  # calculate 1Q/4Q line
  y <- quantile(d$std.resid[!is.na(d$std.resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  p <- ggplot(data = d, aes(sample = std.resid)) +
    stat_qq(shape = 1, size = 3) +         # open circles
    labs(title = "Normal Q-Q",             # plot title
         x = "Theoretical Quantiles",      # x-axis label
         y = "Standardized Residuals") +   # y-axis label
    geom_abline(slope = slope, intercept = int, linetype = "dashed", size = 1, col = "firebrick1")  # dashed reference line
  return(p)
}

# Absolutely not normal residuals
ggQQ(lm_m_1)

# What we can do now? Nonlinear regression? Machine learning methods? What about interactions between dummy day features and week features.
# Again take care about singularities, so omit d48...(d1+d2+...+d47):w1 + (d1+d2+...+d47):w2 + ... + (d1+d2+...+d47):w6.
# This could solve problem, what we have seen in the plot of fitted values.
# Define new formula to linear model:

lm_m_2 <- lm(Load ~ 0 + Daily + Weekly + Daily:Weekly, data = matrix_train)

# Because of long lenght of summary I will not print it, just quick view on most important values. First R^2^:
c(Previous = summary(lm_m_1)$r.squared, New = summary(lm_m_2)$r.squared)
# Boxplot of residuals (same as statistics in summary)
ggplot(data.table(Residuals = c(lm_m_1$residuals, lm_m_2$residuals),
                  Type = c(rep("MLR - simple", nrow(data_r)), rep("MLR with interactions", nrow(data_r)))), 
       aes(Type, Residuals, fill = Type)) +
  geom_boxplot()

ggplotly()

plotly_POST(x = last_plot(), filename = "mlr1_vs_mlr2", fileopt = "overwrite",
            sharing = c("public"))

# Much better then previous one, seems interactions working...prove it with next three plots, fitted values, fit vs residuals and qqplot
datas <- rbindlist(list(data_r[, .(value, date_time)], data.table(value = lm_m_2$fitted.values, data_time = data_r[, date_time])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]

ggplot(data = datas, aes(date_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from MLR")

# Fit vs residuals - much better
ggplot(data = data.table(Fitted_values = lm_m_2$fitted.values, Residuals = lm_m_2$residuals),
       aes(Fitted_values, Residuals)) +
  geom_point(size = 1.7) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  labs(title = "Fitted vs Residuals")

ggQQ(lm_m_2)

# Tried trend but not helped...so go ahead to forecast with this model.
# Put all to one function: arguments `data` and `set_of_date`. Function returns forecast to whole week.

predWeekReg <- function(data, set_of_date){
  
  # subsetting the dataset by dates
  data_train <- data[date %in% set_of_date]
  
  N <- nrow(data_train)
  window <- N / period # number of days in the train set
  # 1, ..., period, 1, ..., period - and so on for the daily season 
  # using feature "week_num" for the weekly season
  matrix_train <- data.table(Load = data_train[, value],
                             Daily = as.factor(rep(1:period, window)),
                             Weekly = as.factor(data_train[, week_num]))
  
  lm_m <- lm(Load ~ 0 + Daily + Weekly + Daily:Weekly, data = matrix_train)
  
  pred_week <- predict(lm_m, matrix_train[1:(7*period), -1, with = FALSE])
  
  return(as.vector(pred_week))
}

## Mean Absolute Percentage Error
mape <- function(real, pred){
  return(100 * mean(abs((real - pred)/real)))
}

# Training set of lenght of two weeks - experimentaly set. Make forecast to 50 weeks.

n_weeks <- floor(length(n_date)/7) - 2

lm_pred_weeks_1 <- sapply(0:(n_weeks-1), function(i)
  predWeekReg(DT[type == n_type[1]], n_date[((i*7)+1):((i*7)+7*2)]))

lm_pred_weeks_2 <- sapply(0:(n_weeks-1), function(i)
  predWeekReg(DT[type == n_type[2]], n_date[((i*7)+1):((i*7)+7*2)]))

lm_pred_weeks_3 <- sapply(0:(n_weeks-1), function(i)
  predWeekReg(DT[type == n_type[3]], n_date[((i*7)+1):((i*7)+7*2)]))

lm_pred_weeks_4 <- sapply(0:(n_weeks-1), function(i)
  predWeekReg(DT[type == n_type[4]], n_date[((i*7)+1):((i*7)+7*2)]))

lm_err_mape_1 <- sapply(0:(n_weeks-1), function(i)
  mape(DT[(type == n_type[1] & date %in% n_date[(15+(i*7)):(21+(i*7))]), value],
       lm_pred_weeks_1[, i+1]))

lm_err_mape_2 <- sapply(0:(n_weeks-1), function(i)
  mape(DT[(type == n_type[2] & date %in% n_date[(15+(i*7)):(21+(i*7))]), value],
       lm_pred_weeks_2[, i+1]))

lm_err_mape_3 <- sapply(0:(n_weeks-1), function(i)
  mape(DT[(type == n_type[3] & date %in% n_date[(15+(i*7)):(21+(i*7))]), value],
       lm_pred_weeks_3[, i+1]))

lm_err_mape_4 <- sapply(0:(n_weeks-1), function(i)
  mape(DT[(type == n_type[4] & date %in% n_date[(15+(i*7)):(21+(i*7))]), value],
       lm_pred_weeks_4[, i+1]))

# MAPE error for our forecasts.
summary(lm_err_mape_1) # n_type[1]
summary(lm_err_mape_2) # n_type[2]
summary(lm_err_mape_3) # n_type[3]
summary(lm_err_mape_4) # n_type[4]

# BoxPlot
ggplot(data.table(MAPE = c(lm_err_mape_1, lm_err_mape_2, lm_err_mape_3, lm_err_mape_4),
                  Industry = rep(n_type, each = length(lm_err_mape_1))), 
       aes(Industry, MAPE, fill = Industry)) + 
  geom_boxplot()

ggplotly()

# Visualization of forecast...one plot - nonsense..try animation!

datas <- data.table(value = c(as.vector(lm_pred_weeks_3),
                              DT[(type == n_type[3]) & (date %in% n_date[-c(1:14,365)]), value]),
                    date_time = c(rep(DT[-c(1:(14*48), (17473:nrow(DT))), date_time], 2)),
                    type = c(rep("MLR", nrow(lm_pred_weeks_1)*ncol(lm_pred_weeks_1)),
                             rep("Real", nrow(lm_pred_weeks_1)*ncol(lm_pred_weeks_1))),
                    week = c(rep(1:50, each = 336), rep(1:50, each = 336)))

saveGIF({
  oopt = ani.options(interval = 0.9, nmax = 50)
  for(i in 1:ani.options("nmax")){
    print(ggplot(data = datas[week == i], aes(date_time, value, group = type, colour = type)) +
            geom_line(size = 0.8) +
            scale_y_continuous(limits = c(min(datas[, value]), max(datas[, value]))) + 
            theme(panel.border = element_blank(), panel.background = element_blank(),
                  panel.grid.minor = element_line(colour = "grey90"),
                  panel.grid.major = element_line(colour = "grey90"),
                  panel.grid.major.x = element_line(colour = "grey90"),
                  title = element_text(size = 15),
                  axis.text = element_text(size = 10),
                  axis.title = element_text(size = 12, face = "bold")) +
            labs(x = "Time", y = "Load (kW)",
                 title = paste("Forecast of MLR (", n_type[3], "); ", "week: ", i, "; MAPE: ",
                               round(lm_err_mape_3[i], 2), "%", sep = "")))
    ani.pause()
  }}, movie.name = "industry_3.gif", ani.height = 450, ani.width = 750)

# Comparison with STL+ARIMA...MLR is better.

library(forecast)

stlARIMAPred <- function(Y, period = 48){
  ts_Y <- ts(Y, start = 0, freq = period)
  dekom <- stl(ts_Y, s.window="periodic", robust = T)
  arima <- forecast(dekom, h = period, method = "arima")
  return(as.vector(arima$mean))
}

predictWeek <- function(data, set_of_date, FUN, train_win = 6){
  
  for_mon <- FUN(data[(week == n_weekdays[1] & date %in% set_of_date), value])
  seq_tuethu <- data[(week %in% n_weekdays[2:4] & date %in% set_of_date), value]
  for_tuethu <- as.vector(sapply(2:0, function(j) FUN(seq_tuethu[(length(seq_tuethu)-(period*j)+1-(train_win*period)):(length(seq_tuethu)-(period*j))])))
  for_fri <- FUN(data[(week == n_weekdays[5] & date %in% set_of_date), value])
  for_sat <- FUN(data[(week == n_weekdays[6] & date %in% set_of_date), value])
  for_sun <- FUN(data[(week == n_weekdays[7] & date %in% set_of_date), value])
  
  return(c(for_mon, for_tuethu, for_fri, for_sat, for_sun))
}

n_weeks <- floor(length(n_date)/7) - 3


arima_pred_weeks_1 <- sapply(0:(n_weeks-1), function(i)
  predictWeek(DT[type == n_type[1]], n_date[((i*7)+1):((i*7)+7*3)], stlARIMAPred))

arima_err_mape_1 <- sapply(0:(n_weeks-1), function(i)
  mape(DT[(type == n_type[1] & date %in% n_date[(22+(i*7)):(28+(i*7))]), value],
       arima_pred_weeks_1[, i+1]))

arima_pred_weeks_2 <- sapply(0:(n_weeks-1), function(i)
  predictWeek(DT[type == n_type[2]], n_date[((i*7)+1):((i*7)+7*3)], stlARIMAPred))

arima_err_mape_2 <- sapply(0:(n_weeks-1), function(i)
  mape(DT[(type == n_type[2] & date %in% n_date[(22+(i*7)):(28+(i*7))]), value],
       arima_pred_weeks_2[, i+1]))

arima_pred_weeks_3 <- sapply(0:(n_weeks-1), function(i)
  predictWeek(DT[type == n_type[3]], n_date[((i*7)+1):((i*7)+7*3)], stlARIMAPred))

arima_err_mape_3 <- sapply(0:(n_weeks-1), function(i)
  mape(DT[(type == n_type[3] & date %in% n_date[(22+(i*7)):(28+(i*7))]), value],
       arima_pred_weeks_3[, i+1]))

arima_pred_weeks_4 <- sapply(0:(n_weeks-1), function(i)
  predictWeek(DT[type == n_type[4]], n_date[((i*7)+1):((i*7)+7*3)], stlARIMAPred))

arima_err_mape_4 <- sapply(0:(n_weeks-1), function(i)
  mape(DT[(type == n_type[4] & date %in% n_date[(22+(i*7)):(28+(i*7))]), value],
       arima_pred_weeks_4[, i+1]))

# BoxPlot
n_type <- c("Comm. Prop.", "Education", "Food Sales", "Light Ind.")
ggplot(data.table(MAPE = c(lm_err_mape_1, lm_err_mape_2, lm_err_mape_3, lm_err_mape_4,
                           arima_err_mape_1, arima_err_mape_2, arima_err_mape_3, arima_err_mape_4),
                  Industry = c(rep(n_type, each = length(lm_err_mape_1)),
                               rep(n_type, each = length(arima_err_mape_1))),
                  Method = c(rep("MLR", length(lm_err_mape_1)*4),
                             rep("STL+ARIMA", length(arima_err_mape_1)*4))), 
       aes(Industry, MAPE, fill = Industry)) +
  facet_grid(. ~ Method, labeller = "label_both") + 
  geom_boxplot() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"),
        strip.text = element_text(size = 12, face = "bold"))


ggplotly()

plotly_POST(x = last_plot(), filename = "mlr_vs_arima", fileopt = "overwrite",
                        sharing = c("public"))

# error separate for weekdays
err_days <- sapply(0:6, function(j)
        sapply(0:(n_weeks-1), function(i)
          mape(DT[(type == n_type[2] & date %in% n_date[(15+(i*7)):(21+(i*7))] & week == n_weekdays[j+1]), value],
               lm_pred_weeks_2[((period*j)+1):(period*(j+1)), i+1])))

colMeans(err_days)