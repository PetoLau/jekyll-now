---
layout: post
title: Enernoc smart meter data and forecast with similar day approach
author: Peter Laurinec
published: true
status: publish
draft: false
tags: R, forecast
---
 
## Exploring meta data of consumers (ID)
 
Scan all needed packages.

{% highlight r %}
library(data.table)
library(lubridate)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(forecast)
{% endhighlight %}
 

{% highlight r %}
opts_knit$set(root.dir = "C:\\Users\\Peter\\Downloads\\ProjektBD\\enernoc\\csv\\")
{% endhighlight %}
 
Read meta data and show their structure.
 

{% highlight r %}
meta_data <- fread("meta\\all_sites.csv")
str(meta_data)
{% endhighlight %}



{% highlight text %}
## Classes 'data.table' and 'data.frame':	100 obs. of  8 variables:
##  $ SITE_ID     : int  6 8 9 10 12 13 14 21 22 25 ...
##  $ INDUSTRY    : chr  "Commercial Property" "Commercial Property" "Commercial Property" "Commercial Property" ...
##  $ SUB_INDUSTRY: chr  "Shopping Center/Shopping Mall" "Shopping Center/Shopping Mall" "Corporate Office" "Shopping Center/Shopping Mall" ...
##  $ SQ_FT       : int  161532 823966 169420 1029798 179665 185847 1675720 783982 318130 1807149 ...
##  $ LAT         : num  34.8 40.3 40.9 39.7 39.7 ...
##  $ LNG         : num  -106.9 -76.4 -74.7 -75 -74.9 ...
##  $ TIME_ZONE   : chr  "America/Denver" "America/New_York" "America/New_York" "America/New_York" ...
##  $ TZ_OFFSET   : chr  "-06:00" "-04:00" "-04:00" "-04:00" ...
##  - attr(*, ".internal.selfref")=<externalptr>
{% endhighlight %}
Nice features to explore...so frequency table of industries:

{% highlight r %}
qplot(1:5, 1:5, geom = "blank") + theme_bw() + theme(line = element_blank(), text = element_blank()) +
  annotation_custom(grob = tableGrob(meta_data[, .N, by = .(INDUSTRY, SUB_INDUSTRY)]))
{% endhighlight %}

![plot of chunk unnamed-chunk-3](/images/unnamed-chunk-3-1.png)
Map of USA of location of our consumers.

{% highlight r %}
map <- get_map(location = "USA", zoom = 4) # c(lon = -125, lat = 22)
ggmap(map) + geom_point(aes(x = LNG, y = LAT, color = INDUSTRY), size = 5, data = meta_data, alpha = .6) +
theme(axis.title.x = element_text(colour = "white"), axis.title.y = element_text(colour = "white"), axis.text.x = element_text(colour = "white"), axis.text.y = element_text(colour = "white"))
{% endhighlight %}

![plot of chunk unnamed-chunk-4](/images/unnamed-chunk-4-1.png)
 
Histogram of sqft -> square meter.

{% highlight r %}
set(meta_data, j = "SQ_FT", value = meta_data[["SQ_FT"]] * 0.09290304)
setnames(meta_data, "SQ_FT", "SQ_M")
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
{% endhighlight %}

![plot of chunk unnamed-chunk-5](/images/unnamed-chunk-5-1.png)
 
Density plot of industries and SQ_M.

{% highlight r %}
ggplot(meta_data, aes(SQ_M, colour = INDUSTRY, fill = INDUSTRY)) + 
  geom_density(alpha=0.55) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))
{% endhighlight %}

![plot of chunk unnamed-chunk-6](/images/unnamed-chunk-6-1.png)
 
Load all .csv files containing electricity consumption in one data.table by rbindlist and lapply. See structure and change column names.

{% highlight r %}
files <- list.files(pattern = "*.csv")
DT <- rbindlist(lapply(files, function(x) cbind(fread(x), gsub(".csv", "", x))))
str(DT)
{% endhighlight %}



{% highlight text %}
## Classes 'data.table' and 'data.frame':	10531288 obs. of  6 variables:
##  $ timestamp: int  1325376600 1325376900 1325377200 1325377500 1325377800 1325378100 1325378400 1325378700 1325379000 1325379300 ...
##  $ dttm_utc : chr  "2012-01-01 00:10:00" "2012-01-01 00:15:00" "2012-01-01 00:20:00" "2012-01-01 00:25:00" ...
##  $ value    : num  106 105 102 103 102 ...
##  $ estimated: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ anomaly  : chr  "" "" "" "" ...
##  $ V2       : chr  "10" "10" "10" "10" ...
##  - attr(*, ".internal.selfref")=<externalptr>
{% endhighlight %}



{% highlight r %}
setnames(DT, c("dttm_utc", "V2"), c("date", "ID"))
{% endhighlight %}
 
Prepare meta_data to merge with DT.

 
Extract possible interesting features from ID.

{% highlight r %}
ID_stats <- DT[, .(Mean = mean(value), Median = median(value), Sum = sum(value)), .(ID)]
{% endhighlight %}
 
Merge and aggregate by sub_industry.

{% highlight r %}
data_m <- merge(ID_stats, meta_data, by = "ID")
sub_sum <- data_m[, .(mean(Mean)), .(SUB_INDUSTRY)]
{% endhighlight %}
 
Bar plot of mean load by sub_industries.

{% highlight r %}
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
{% endhighlight %}

![plot of chunk unnamed-chunk-11](/images/unnamed-chunk-11-1.png)
 
Regression line SQ_M vs Median Load.

{% highlight r %}
ggplot(data_m[, .(SQ_M, Median, INDUSTRY)], aes(x = SQ_M, y = Median)) +
  geom_point(aes(colour = INDUSTRY, shape = INDUSTRY), size = 4, alpha = 0.8) +
  geom_smooth(method = lm, color = "yellow1", se = TRUE) +
  scale_shape_manual(values = c(15,16,17,18)) +
  scale_color_manual(values=c("salmon", "dodgerblue2", "springgreen3", "plum3")) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))
{% endhighlight %}

![plot of chunk unnamed-chunk-12](/images/unnamed-chunk-12-1.png)
 
Transform characters to classical Date and Date&Time format (POSIxt).

{% highlight r %}
DT[, date_time := ymd_hms(DT[["date"]])]
DT[, date := as.Date(DT[["date"]], "%Y-%m-%d")]
DT[, ':='(timestamp = NULL, estimated = NULL, anomaly = NULL)]
str(DT)
{% endhighlight %}



{% highlight text %}
## Classes 'data.table' and 'data.frame':	10531288 obs. of  4 variables:
##  $ date     : Date, format: "2012-01-01" "2012-01-01" ...
##  $ value    : num  106 105 102 103 102 ...
##  $ ID       : chr  "10" "10" "10" "10" ...
##  $ date_time: POSIXct, format: "2012-01-01 00:10:00" "2012-01-01 00:15:00" ...
##  - attr(*, ".internal.selfref")=<externalptr>
{% endhighlight %}
Extract ID's with an whole length (105408)

{% highlight r %}
count_ID <- DT[, .N, ID]
full <- count_ID[N == max(N), .(ID)]
DT <- DT[ID %in% full[, ID]]
{% endhighlight %}
 
Extract date's with an all measurements during the day (288).  First and last date has not all measurements - so remove them.

{% highlight r %}
num_date <- DT[ID == 100, .N, .(date)]
table(num_date[, N])
{% endhighlight %}



{% highlight text %}
## 
##   1 287 288 
##   1   1 365
{% endhighlight %}



{% highlight r %}
DT <- DT[!date %in% num_date[c(1,367), date]]
{% endhighlight %}
Our extracted (filtered) ID's.

{% highlight r %}
unique(DT[,ID])
{% endhighlight %}



{% highlight text %}
##  [1] "100" "101" "103" "116" "14"  "144" "153" "197" "213" "218" "228"
## [12] "236" "259" "275" "281" "285" "304" "31"  "339" "341" "366" "384"
## [23] "386" "391" "401" "455" "474" "475" "496" "512" "56"  "65"  "716"
## [34] "718" "731" "761" "765" "766" "832" "88"  "9"   "92"  "99"
{% endhighlight %}
 
Plot one (ID 99).

{% highlight r %}
ggplot(DT[ID == 99, .(value, date)], aes(date, value)) +
  geom_line() +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Load (kW)")
{% endhighlight %}

![plot of chunk unnamed-chunk-17](/images/unnamed-chunk-17-1.png)
 
Aggregate consumption to 48 measurements per day (every half hour) - due to reduction of dimensionality - 48/per day is good compromise.

{% highlight r %}
DT_48 <- DT[, .(value = sum(value), date, ID, date_time), by = (seq(nrow(DT)) - 1) %/% 6]
DT_48 <- DT_48[seq(1, nrow(DT_48), by = 6)]
DT_48[, seq := NULL]
{% endhighlight %}
 
Plot typical representants of 4 groups of industries.

{% highlight r %}
ggplot(data = DT_48[ID %in% c(213, 401, 9, 832)], aes(x = date, y = value)) +
  geom_line() + 
  facet_grid(ID ~ ., scales = "free_y", labeller = "label_both") +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Load (kW)")
{% endhighlight %}

![plot of chunk unnamed-chunk-19](/images/unnamed-chunk-19-1.png)
 
Aggregate consumption of all consumers (43).

{% highlight r %}
DT_agg <- as.data.table(aggregate(DT_48[, .(value)], by = DT_48[, .(date_time)], FUN = sum, simplify = TRUE))
ggplot(DT_agg, aes(date_time, value)) +
  geom_line() +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Load (kW)")
{% endhighlight %}

![plot of chunk unnamed-chunk-20](/images/unnamed-chunk-20-1.png)
 
Median daily profile of aggregate consumption with MAD (median absolute deviation).

{% highlight r %}
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
{% endhighlight %}

![plot of chunk unnamed-chunk-21](/images/unnamed-chunk-21-1.png)
 
Median weekly profile of aggregate consumption with MAD (median absolute deviation).

{% highlight r %}
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
{% endhighlight %}

![plot of chunk unnamed-chunk-22](/images/unnamed-chunk-22-1.png)
 
## Creation of forecast model for different days during the week (similar day approach)
 
Add corresponding weekdays to date for datasets DT_48 and DT_agg

{% highlight r %}
DT_48[, week := weekdays(date_time)]
DT_agg[, ':='(week = weekdays(date_time), date = as.Date(date_time))]
{% endhighlight %}
Now we have datasets with all needed features to build model for different days.
Extract date, ID, weekdays and period for better working with subsetting.

{% highlight r %}
n_ID <- unique(DT_48[, ID])
n_weekdays <- unique(DT_agg[, week])
n_date <- unique(DT_agg[, date])
period <- 48
{% endhighlight %}
 
Define forecast methods. STL+ARIMA and STL+EXP.

{% highlight r %}
# STL + ARIMA
stlARIMAPred <- function(Y, period = 48){
  ts_Y <- ts(Y, start = 0, freq = period)
  dekom <- stl(ts_Y, s.window="periodic", robust = T)
  arima <- forecast(dekom, h = period, method = "arima")
  return(as.vector(arima$mean))
}
# STL + EXP
stlEXPPred <- function(Y, period = 48){
  ts_Y <- ts(Y, start = 0, freq = period)
  dekom <- stl(ts_Y, s.window = "periodic", robust = T)
  expo <- forecast(dekom, h = period, method = "ets", etsmodel = "ZZN")
  return(as.vector(expo$mean))
}
{% endhighlight %}
 
Function to return forecast of the length one week.

{% highlight r %}
predictWeek <- function(data, set_of_date, FUN, train_win = 6){
 
 for_mon <- FUN(data[(week == n_weekdays[1] & date %in% set_of_date), value])
 seq_tuethu <- data[(week %in% n_weekdays[2:4] & date %in% set_of_date), value]
 for_tuethu <- as.vector(sapply(2:0, function(j) FUN(seq_tuethu[(length(seq_tuethu)-(period*j)+1-(train_win*period)):(length(seq_tuethu)-(period*j))])))
 for_fri <- FUN(data[(week == n_weekdays[5] & date %in% set_of_date), value])
 for_sat <- FUN(data[(week == n_weekdays[6] & date %in% set_of_date), value])
 for_sun <- FUN(data[(week == n_weekdays[7] & date %in% set_of_date), value])
 
 return(c(for_mon, for_tuethu, for_fri, for_sat, for_sun))
}
{% endhighlight %}
 
Define function to compute Mean Absolute Percentage Error.

{% highlight r %}
mape <- function(real, pred){
  return(100 * mean(abs((real - pred)/real)))
}
{% endhighlight %}
 
Run prediction and compute MAPE of both methods.

{% highlight r %}
for_week_arima <- predictWeek(DT_agg, n_date[56:84], stlARIMAPred)
for_week_exp <- predictWeek(DT_agg, n_date[56:84], stlEXPPred)
real_week <- DT_agg[date %in% n_date[85:91], value]
mape(real_week, for_week_arima)
{% endhighlight %}



{% highlight text %}
## [1] 3.796372
{% endhighlight %}



{% highlight r %}
mape(real_week, for_week_exp)
{% endhighlight %}



{% highlight text %}
## [1] 4.8135
{% endhighlight %}
 
Compute MAPE for every day of week separately.

{% highlight r %}
sapply(0:6, function(i) mape(real_week[((i*period)+1):((i+1)*period)], for_week_arima[((i*period)+1):((i+1)*period)]))
{% endhighlight %}



{% highlight text %}
## [1] 3.649075 2.869293 5.301543 4.731049 4.334487 2.584994 3.104161
{% endhighlight %}



{% highlight r %}
sapply(0:6, function(i) mape(real_week[((i*period)+1):((i+1)*period)], for_week_exp[((i*period)+1):((i+1)*period)]))
{% endhighlight %}



{% highlight text %}
## [1] 5.172147 3.931334 5.680406 7.224436 4.688619 3.281471 3.716090
{% endhighlight %}
 
Plot computed forecast of one week ahead.

{% highlight r %}
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
{% endhighlight %}

![plot of chunk unnamed-chunk-30](/images/unnamed-chunk-30-1.png)
 
THE END.
