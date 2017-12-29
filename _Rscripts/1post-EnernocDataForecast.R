# rm(list=ls())
# gc()

library(data.table)
library(lubridate)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(forecast)

## Meta data ----
# setwd("C:\\Users\\Peter\\Downloads\\ProjektBD\\enernoc\\")
meta_data <- fread("meta\\all_sites.csv")
str(meta_data)

# nice features to explore
meta_data[, .N, by = .(INDUSTRY, SUB_INDUSTRY)]
# Table of frequencies of industries and subindustries

# --- Graph 1 : If you want ONLY the table in your image :
# First I create an empty graph with absolutely nothing :
qplot(1:5, 1:5, geom = "blank") + theme_bw() + theme(line = element_blank(), text = element_blank()) +
  annotation_custom(grob = tableGrob(meta_data[, .N, by = .(INDUSTRY, SUB_INDUSTRY)]))

# Plot industries in map of USA
map <- get_map(location = "USA", zoom = 4) # c(lon = -125, lat = 22)
# ?get_map
ggmap(map) + geom_point(aes(x = LNG, y = LAT, color = INDUSTRY), size = 5, data = meta_data, alpha = .6) +
  theme(axis.title.x = element_text(colour = "white"), axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"), axis.text.y = element_text(colour = "white"))


# Histogram of sqft -> square meter
set(meta_data, j = "SQ_FT", value = meta_data[["SQ_FT"]] * 0.09290304)
setnames(meta_data, "SQ_FT", "SQ_M")
# par(bg="white", mar=c(5, 4, 3, 2), oma=c(0,0,0,0), xpd=FALSE,
#     xaxs="r", yaxs="r", mgp=c(2.8,0.3,0.5), col.lab="black", col.axis="black",
#     col.main="black", cex.main=1.1, cex.axis=1.1, cex.lab=1.1,
#     font.main=7, font.lab=7, font.axis=7, lend=1, tck=0)
# hist(meta_data$SQ_M, breaks=25, col = "dodgerblue2", border = "grey90", xlab = "SQ_M", main = "Histogram of SQ_M for all consumers")
ggplot(meta_data, aes(meta_data$SQ_M)) +
  geom_histogram(bins = 32,
                 col = "grey95",
                 fill = "dodgerblue2", 
                 alpha = .80) +
  labs(title = "Histogram of SQ_M for all consumers") +
  labs(x = "SQ_M", y = "Frequency") +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))


# By industries
ggplot(meta_data, aes(SQ_M, colour = INDUSTRY, fill = INDUSTRY)) + 
  geom_density(alpha=0.55) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))

## Prepare dataset to merge with DT
## Load all .csv files 
setwd("C:\\Users\\Peter\\Downloads\\ProjektBD\\enernoc\\csv\\")

files <- list.files(pattern = "*.csv")
DT <- rbindlist(lapply(files, function(x) cbind(fread(x), gsub(".csv", "", x))))
str(DT)
setnames(DT, c("dttm_utc", "V2"), c("date", "ID"))

# prepare meta_data
meta_data[, ':='(TIME_ZONE = NULL, TZ_OFFSET = NULL)]
setnames(meta_data, "SITE_ID", "ID")
meta_data[, ID := as.character(meta_data[["ID"]])]

# extract possible interesting features from ID
ID_stats <- DT[, .(Mean = mean(value),
                   Median = median(value),
                   Sum = sum(value)), .(ID)]

# merge and aggregate by sub_industry
data_m <- merge(ID_stats, meta_data, by = "ID")
sub_sum <- data_m[, .(mean(Mean)), .(SUB_INDUSTRY)]
setkey(sub_sum, V1)

# end_point <- 0.5 + nrow(sub_sum) + nrow(sub_sum) - 1
# par(bg="white", mar = c(7, 4, 2, 2) + 0.2, oma=c(0,0,0,0), xpd=FALSE,
#     xaxs="r", yaxs="r", mgp=c(2.8,0.3,0.5), col.lab="black", col.axis="black",
#     col.main="black", cex.main=1.2, cex.axis=1.1, cex.lab=1.1,
#     font.main=7, font.lab=7, font.axis=7, lend=1, tck=0)
# barplot(sub_sum[,V1], names.arg = "", las = 1, cex.names = 0.8, ylab = "Mean Load (kW)", xlab = "",
#         col = "dodgerblue2", space = 1, border = "grey90", main = "Mean load by subindustries")
# text(seq(1.5, end_point, by = 2), par("usr")[3]-0.25, 
#      srt = 60, adj = 1, xpd = TRUE,
#      labels = sub_sum[,SUB_INDUSTRY], cex = 0.70, font = 7)

ggplot(sub_sum, aes(x = reorder(SUB_INDUSTRY, V1), y = V1, fill = reorder(SUB_INDUSTRY, V1))) +
  geom_bar(stat = "identity", width = 0.8) +
  labs(x = "", y = "Mean Load (kW)",
       title = "Mean load by subindustries",
       fill = "SUB_INDUSTRY") +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Regression lines SQ_M vs Median Load
# library(MASS)
# par(bg="white", mar=c(5, 4, 3, 2), oma=c(0,0,0,0), xpd=FALSE,
#     xaxs="r", yaxs="r", mgp=c(2.8,0.3,0.5), col.lab="black", col.axis="black",
#     col.main="black", cex.main=1.1, cex.axis=1.1, cex.lab=1.1,
#     font.main=7, font.lab=7, font.axis=7, lend=1, tck=0)
# colW <- rainbow(4)[as.factor(data_m[,INDUSTRY])]
# pchW <- c(15,16,17,18)[as.factor(data_m[,INDUSTRY])]
# plot(data_m[,.(SQ_M, Median)], pch = pchW, col = colW, cex = 1.2, ylab = "Median Load (kW)")
# legend(113000, 370, unique(data_m[, INDUSTRY]), col = unique(colW), pch = unique(pchW),
#        cex=1.05, bty="n", pt.cex=1.5, text.font=7, ncol = 1)
# abline(rlm(Median ~ SQ_M, data = data_m[,.(SQ_M, Median)]), col = "yellow1", lwd = 2)
# abline(lm(Median ~ SQ_M, data = data_m[,.(SQ_M, Median)]), col = "salmon", lwd = 2)
# text(100000, 155, paste("LM"), font = 7)
# text(130000, 100, paste("Robust LM"), font = 7)

# Regression line SQ_M vs Median Load
ggplot(data_m[, .(SQ_M, Median, INDUSTRY)], aes(x = SQ_M, y = Median)) +
  geom_point(aes(colour = INDUSTRY, shape = INDUSTRY), size = 4, alpha = 0.8) +
  geom_smooth(method = lm, color = "yellow1", se = TRUE) +
  scale_shape_manual(values = c(15,16,17,18)) +
  scale_color_manual(values=c("salmon", "dodgerblue2", "springgreen3", "plum3")) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))

###########
# Transform characters to classical Date and Date&Time format (POSIxt)
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
table(num_date[, N])
DT <- DT[!date %in% num_date[c(1,367), date]] # first and last date has not all measurements - so remove them
unique(DT[,ID]) # our extracted (filtered) ID's
# Plot one
ggplot(DT[ID == 99, .(value, date)], aes(date, value)) +
  geom_line() +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Load (kW)")

# aggregate consumption to 48 measurments per day (every half hour) - due to reduction of dimensionality - 48/per day is good compromise
DT_48 <- DT[, .(value = sum(value), date, ID, date_time), by = (seq(nrow(DT)) - 1) %/% 6]
DT_48 <- DT_48[seq(1, nrow(DT_48), by = 6)]
DT_48[, seq := NULL]

## Some examples of consumers
# Primary/Secondary School
p1 <- ggplot(DT_48[ID == 213, .(value, date)], aes(date, value)) + geom_line()
# Grocer/Market
p2 <- ggplot(DT_48[ID == 401, .(value, date)], aes(date, value)) + geom_line()
# Corporate Office
p3 <- ggplot(DT_48[ID == 9, .(value, date)], aes(date, value)) + geom_line()
# Manufacturing
p4 <- ggplot(DT_48[ID == 832, .(value, date)], aes(date, value)) + geom_line()

# Plot typical representants of 4 groups of industries
ggplot(data = DT_48[ID %in% c(213, 401, 9, 832)], aes(x = date, y = value)) +
  geom_line() + 
  facet_grid(ID ~ ., scales = "free_y", labeller = "label_both") +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Load (kW)")

# Aggregate consumption of all consumers (43)
DT_agg <- as.data.table(aggregate(DT_48[, .(value)], by = DT_48[, .(date_time)], FUN = sum, simplify = TRUE))
ggplot(DT_agg, aes(date_time, value)) +
  geom_line() +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Load (kW)")

# Median daily profile of aggregate consumption with MAD (median absolute deviation)
Med_Mad <- DT_agg[, .(Med = median(value), Mad = mad(value)), by = (seq(nrow(DT_agg)) - 1) %% 48]
ggplot(Med_Mad, aes(x = seq, Med)) + 
  geom_line(size = 0.9) +
  geom_ribbon(data = Med_Mad, aes(ymin = Med - Mad, ymax = Med + Mad),
              fill = "firebrick2", alpha = 0.3) +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(title = "Median daily profile +- Deviation (MAD)") +
  labs(x = "Time", y = "Load (kW)")

# Median weekly profile of aggregate consumption with MAD (median absolute deviation)
Med_Mad_Week <- DT_agg[, .(Med = median(value), Mad = mad(value)), by = (seq(nrow(DT_agg)) - 1) %% (48*7)]
ggplot(Med_Mad_Week, aes(x = seq, Med)) + 
  geom_line(size = 0.9) + 
  geom_ribbon(data = Med_Mad_Week, aes(ymin = Med - Mad, ymax = Med + Mad),
              fill = "firebrick2", alpha = 0.3) +
  geom_vline(xintercept = c(47, 47+(48*3), 47+(48*4), 47+(48*5)), linetype = 2, size = 1) +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(title = "Median weekly profile +- Deviation (MAD)") +
  labs(x = "Time", y = "Load (kW)")

## Create forecast model for different days during the week ----

# add corresponding weekdays to date for datasets DT_48 and DT_agg
DT_48[, week := weekdays(date_time)]
DT_agg[, ':='(week = weekdays(date_time), date = as.Date(date_time))]

# now we have datasets with all needed features to build model for different days
n_ID <- unique(DT_48[, ID])
n_weekdays <- unique(DT_agg[, week])
n_date <- unique(DT_agg[, date])
period <- 48

DT_48[(ID == n_ID[1] & week == n_weekdays[1] & date %in% n_date[1:28]), value]
DT_agg[(week == n_weekdays[1] & date %in% n_date[7:35]), value]

## STL + ARIMA
stlARIMAPred <- function(Y, period = 48){
  ts_Y <- ts(Y, start = 0, freq = period) # Transform to ts
  dekom <- stl(ts_Y, s.window="periodic", robust = T) # STL decomposition
  arima <- forecast(dekom, h = period, method = "arima") # Predict decomposed time series with ARIMA
  return(as.vector(arima$mean))
}

## STL + EXP
stlEXPPred <- function(Y, period = 48){
  ts_Y <- ts(Y, start = 0, freq = period) # Transform to ts
  dekom <- stl(ts_Y, s.window = "periodic", robust = T) # STL decomposition
  expo <- forecast(dekom, h = period, method = "ets", etsmodel = "ZZN") # Predict decomposed time series with Exponential Smoothing
  return(as.vector(expo$mean))
}

## Optional other methods - HW and snaive:
## Holt-Winters ES
HWPred <- function(Y, period = 48){
  ts_Y <- ts(Y, start = 0, freq = period) # Transform to ts
  HW <- HoltWinters(ts_Y, beta = F, seasonal = "additive") # Holt-Winters ES , alpha = 0.15, gamma = 0.95
  HW_pred <- forecast(HW, period) # Predict
  return(as.vector(HW_pred$mean))
}

## Seasonal naive forecast
snaivePred <- function(Y, period = 48){
  ts_Y <- ts(Y, start = 0, freq = period)
  naive_pred <- snaive(ts_Y, period)
  return(as.vector(naive_pred$mean))
}

## Function to return forecast of length one week
predictWeek <- function(data, set_of_date, FUN, train_win = 6){

 for_mon <- FUN(data[(week == n_weekdays[1] & date %in% set_of_date), value])
 seq_tuethu <- data[(week %in% n_weekdays[2:4] & date %in% set_of_date), value]
 for_tuethu <- as.vector(sapply(2:0, function(j) FUN(seq_tuethu[(length(seq_tuethu)-(period*j)+1-(train_win*period)):(length(seq_tuethu)-(period*j))])))
 for_fri <- FUN(data[(week == n_weekdays[5] & date %in% set_of_date), value])
 for_sat <- FUN(data[(week == n_weekdays[6] & date %in% set_of_date), value])
 for_sun <- FUN(data[(week == n_weekdays[7] & date %in% set_of_date), value])

 return(c(for_mon, for_tuethu, for_fri, for_sat, for_sun))
}

## Mean Absolute Percentage Error
mape <- function(real, pred){
  return(100 * mean(abs((real - pred)/real)))
}

for_week_arima <- predictWeek(DT_agg, n_date[56:84], stlARIMAPred)
for_week_exp <- predictWeek(DT_agg, n_date[56:84], stlEXPPred)
for_week_naive <- predictWeek(DT_agg, n_date[56:84], snaivePred)
real_week <- DT_agg[date %in% n_date[85:91], value]
mape(real_week, for_week_arima)
mape(real_week, for_week_exp)
mape(real_week, for_week_naive)

sapply(0:6, function(i) mape(real_week[((i*period)+1):((i+1)*period)], for_week_arima[((i*period)+1):((i+1)*period)]))
sapply(0:6, function(i) mape(real_week[((i*period)+1):((i+1)*period)], for_week_exp[((i*period)+1):((i+1)*period)]))
sapply(0:6, function(i) mape(real_week[((i*period)+1):((i+1)*period)], for_week_naive[((i*period)+1):((i+1)*period)]))

# Plot forecast for one week
datas <- data.table(value = c(for_week_arima, for_week_exp, DT_agg[date %in% n_date[78:91], value]),
                    date = c(rep(DT_agg[date %in% n_date[85:91], date_time], 2), DT_agg[date %in% n_date[78:91], date_time]),
                    type = c(rep("ARIMA", period*7), rep("EXP", period*7), rep("REAL", period*14)))

ggplot(data = datas, aes(date, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Time", y = "Load (kW)",
       title = "Comparison of forecasts from two models")

# you can use too dshw or tbats..but slow (complex computation) and not as accurate as my solution

for_week_arima <- predictWeek(DT_48[ID == n_ID[40]], n_date[56:84], stlARIMAPred)
for_week_exp <- predictWeek(DT_48[ID == n_ID[40]], n_date[56:84], stlEXPPred)
real_week <- DT_48[ID == n_ID[40] & date %in% n_date[85:91], value]
mape(real_week, for_week_arima)
mape(real_week, for_week_exp)

sapply(0:6, function(i) mape(real_week[((i*period)+1):((i+1)*period)], for_week_arima[((i*period)+1):((i+1)*period)]))
sapply(0:6, function(i) mape(real_week[((i*period)+1):((i+1)*period)], for_week_exp[((i*period)+1):((i+1)*period)]))

# Plot forecast for one week
datas <- data.table(value = c(for_week_arima, for_week_exp, DT_48[ID == n_ID[40] & date %in% n_date[78:91], value]),
                    date = c(rep(DT_48[ID == n_ID[40] & date %in% n_date[85:91], date_time], 2), DT_48[ID == n_ID[40] & date %in% n_date[78:91], date_time]),
                    type = c(rep("ARIMA", period*7), rep("EXP", period*7), rep("REAL", period*14)))

ggplot(data = datas, aes(date, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Time", y = "Load (kW)",
       title = "Comparison of forecasts from two models")
