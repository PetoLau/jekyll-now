---
layout: post
title: Enernoc smart meter data - forecast electricity consumption with similar day approach
author: Peter Laurinec
published: true
status: publish
draft: false
tags: R, forecast
---
 
Deployment of smart grids gives a space to emergence of new methods of machine learning and data analysis. Smart grids can contain of millions of smart meters, which produce large amount of data of electricity consumption (long time series). In addition to time series of electricity consumption, we can have extra information about consumer like ZIP code, type of consumer ([consumer vs. prosumer](http://wikidiff.com/consumer/prosumer)) and so on. These data can be used to support intelligent grid control, make accurate forecast or to detect anomalies. In this blog post I will focus on exploration of available open smart meter data and on creation of simple forecast model, which use similar day approach (will be drawn up in detail below).
 
Firstly, we must download smart meter data of electricity consumption. These data can be downloaded on this [link](https://open-enernoc-data.s3.amazonaws.com/anon/index.html). This dataset was produced by company [EnerNOC](https://www.enernoc.com/) and consists of 100 anonymized commercial buildings for the 2012 year. So download **all-data.tar.gz** file and go explore it what is in.
 
### Exploring meta data of consumers (IDs)
I will do every reading and filtering (cleaning) of dataset by awesome package `data.table`. As you maybe know, with [data.table](https://CRAN.R-project.org/package=data.table) you can change dataset by reference (`:=` or `set`), which is very effective. You can do similar things with package `dplyr`, but I prefer `data.table`, because of performance and memory usage. Interesting comparison between both packages can be seen on this stackoverflow [question](http://stackoverflow.com/questions/21435339/data-table-vs-dplyr-can-one-do-something-well-the-other-cant-or-does-poorly). To visualize interesting relations i will use package `ggplot2`. Manipulation with date and time can be done easily by package `lubridate`.
 
So...I hope everything needed was said, we can go to programming and explorative part of this post. Scan all needed packages.

{% highlight r %}
library(data.table)
library(lubridate)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(forecast)
{% endhighlight %}
 

 
Read meta data and show their structure. Of course, you must firstly set your working directory by `setwd("YOUR PATH")`, where smart meters are.
 

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
 
Nice features to explore...
 
We can do something interesting with features `INDUSTRY`, `SUB_INDUSTRY`, `SQ_FT`, `LAT` and `LNG`. So frequency table of industries and subindustries would be nice. This can be done by package `data.table` very effectively:

{% highlight r %}
meta_data[, .N, by = .(INDUSTRY, SUB_INDUSTRY)]
{% endhighlight %}
 
Plot to table:

{% highlight r %}
qplot(1:5, 1:5, geom = "blank") + theme_bw() + theme(line = element_blank(), text = element_blank()) +
  annotation_custom(grob = tableGrob(meta_data[, .N, by = .(INDUSTRY, SUB_INDUSTRY)]))
{% endhighlight %}

![plot of chunk unnamed-chunk-6](/images/unnamed-chunk-6-1.png)
 
By package `ggmap` it is easy to map location of our consumers to the map of USA. Lets split them by industries.

{% highlight r %}
map <- get_map(location = "USA", zoom = 4)
ggmap(map) + 
  geom_point(aes(x = LNG, y = LAT, color = INDUSTRY), size = 5, data = meta_data, alpha = .6) + 
  theme(axis.title.x = element_text(colour = "white"), axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"), axis.text.y = element_text(colour = "white"))
{% endhighlight %}

![plot of chunk unnamed-chunk-7](/images/unnamed-chunk-7-1.png)
 
Now look at feature `SQ_FT`. Firstly I transform square feets to square meters (I am an European...). Histogram of `SQ_M` of buildings.

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

![plot of chunk unnamed-chunk-8](/images/unnamed-chunk-8-1.png)
 
Looks like we have a majority of buildings under 20,000 m^2.
 
Let's do something similar, but now do density plot for our 4 industries separately.

{% highlight r %}
ggplot(meta_data, aes(SQ_M, colour = INDUSTRY, fill = INDUSTRY)) + 
  geom_density(alpha=0.55) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))
{% endhighlight %}

![plot of chunk unnamed-chunk-9](/images/unnamed-chunk-9-1.png)
 
Looks like Food Sales & Storage buildings have relatively small size, on other hand Commercial Property buildings have very variable size.
 
Now try combine meta data with real electricity consumption data and see interesting relations between them.
Load all `.csv` files containing electricity consumption in one `data.table` by functions `rbindlist` and `lapply`. See structure of these data and change column names. `V2` to `ID` and `dttm_utc` to `date`.

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
 
Prepare `meta_data` to merging with `DT`. Remove useless columns, change column name and unify class of column `ID`.

 
Lets extract possible interesting features from IDs - mean, median and sum of consumption.

{% highlight r %}
ID_stats <- DT[, .(Mean = mean(value), Median = median(value), Sum = sum(value)), .(ID)]
{% endhighlight %}
 
Merge it with `meta_data` and aggregate result by `SUB_INDUSTRY`.

{% highlight r %}
data_m <- merge(ID_stats, meta_data, by = "ID")
sub_sum <- data_m[, .(mean(Mean)), .(SUB_INDUSTRY)]
{% endhighlight %}
 
Bar plot of mean load by subindustries:

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

![plot of chunk unnamed-chunk-14](/images/unnamed-chunk-14-1.png)
 
Looks like biggest consumers in average are manufacturers, shopping centers and business service buildings. On the other hand lowest consumption have schools.
 
Look at possible (maybe obvious) dependence between amount of consumption and `SQ_M`. I will use median load and simple linear regression.

{% highlight r %}
ggplot(data_m[, .(SQ_M, Median, INDUSTRY)], aes(x = SQ_M, y = Median)) +
  geom_point(aes(colour = INDUSTRY, shape = INDUSTRY), size = 4, alpha = 0.8) +
  geom_smooth(method = lm, color = "yellow1", se = TRUE) +
  scale_shape_manual(values = c(15,16,17,18)) +
  scale_color_manual(values=c("salmon", "dodgerblue2", "springgreen3", "plum3")) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))
{% endhighlight %}

![plot of chunk unnamed-chunk-15](/images/unnamed-chunk-15-1.png)
 
There is evident correlation between median load and square meters of consumers.
 
### Prepare dataset to forecast and explore time series of load
Lets do the must changes to construct our forecast model. Transform characters of date to classical date and date&time format (POSIxt). Remove useless columns and look at structure of current dataset of full of time series.

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
 
Extract IDs with an whole length (105408). This is necessary to facilitate further work with time series .

{% highlight r %}
count_ID <- DT[, .N, ID]
full <- count_ID[N == max(N), .(ID)]
DT <- DT[ID %in% full[, ID]]
nrow(full) # number of extracted IDs
{% endhighlight %}



{% highlight text %}
## [1] 43
{% endhighlight %}
 
Our extracted (filtered) IDs:

{% highlight r %}
unique(DT[,ID])
{% endhighlight %}



{% highlight text %}
##  [1] "100" "101" "103" "116" "14"  "144" "153" "197" "213" "218" "228"
## [12] "236" "259" "275" "281" "285" "304" "31"  "339" "341" "366" "384"
## [23] "386" "391" "401" "455" "474" "475" "496" "512" "56"  "65"  "716"
## [34] "718" "731" "761" "765" "766" "832" "88"  "9"   "92"  "99"
{% endhighlight %}
 
Extract date with an all measurements during the day (288). First and last date has not all measurements - so remove them. So our day period (daily seasonality) is 288.

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
 
Let finally look at one ID - num. 99.

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

![plot of chunk unnamed-chunk-20](/images/unnamed-chunk-20-1.png)
 
There is strong dependence on time. Daily, weekly and monthly seasonalities are represented.
 
Aggregate consumption to 48 measurements per day (every half hour) is highly recommended - due to reduction of dimensionality. 48 measurements per day is good compromise. So do it:

{% highlight r %}
DT_48 <- DT[, .(value = sum(value), date, ID, date_time), by = (seq(nrow(DT)) - 1) %/% 6]
DT_48 <- DT_48[seq(1, nrow(DT_48), by = 6)]
DT_48[, seq := NULL]
{% endhighlight %}
 
Plot typical representants of 4 groups of industries. ID 213 is from the Primary/Secondary School segment, ID 401 is the Grocer/Market, ID 832 is the Corporate Office and ID 9 is the Manufactory.

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

![plot of chunk unnamed-chunk-22](/images/unnamed-chunk-22-1.png)
 
Forecast of electricity consumption is mainly done for some area of consumers. So aggregate consumption is used. Aggregate it for all our consumers (43) and plot it.

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

![plot of chunk unnamed-chunk-23](/images/unnamed-chunk-23-1.png)
 
For utility (distribution) companies is very helpful creation of daily profiles of consumers or daily profile for some area. It is characteristic behavior of consumer during the day. So lets create median daily profile of aggregate consumption with MAD (median absolute deviation). I use medians and MAD because of theirs robustness.

{% highlight r %}
Med_Mad <- DT_agg[, .(Med = median(value), Mad = mad(value)), by = (seq(nrow(DT_agg)) - 1) %% 48]
ggplot(Med_Mad, aes(x = seq, Med)) + 
  geom_line(size = 0.9) +
  geom_ribbon(data = Med_Mad, aes(ymin = Med - Mad, ymax = Med + Mad),
              fill = "firebrick2", alpha = 0.3) +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(title = "Median daily profile +- deviation (MAD)") +
  labs(x = "Time", y = "Load (kW)")
{% endhighlight %}

![plot of chunk unnamed-chunk-24](/images/unnamed-chunk-24-1.png)
 
Looks like biggest peak of load is during the evening.
 
Similarly, we can do this but with week pattern. So lets make median weekly profile of aggregate consumption with MAD.

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
  labs(title = "Median weekly profile +- deviation (MAD)") +
  labs(x = "Time", y = "Load (kW)")
{% endhighlight %}

![plot of chunk unnamed-chunk-25](/images/unnamed-chunk-25-1.png)
 
This is much more interesting plot then the previous one, isn't it? We can see 5 different patterns (separated by vertical lines) in behavior of consumers during the week. From Monday till Friday is consumption quite similar, but Monday starts with low consumption (because of weekend) so it's different than others. Friday has similar pattern, but consumption is much lower than at Thursday. It is obvious that weekend is absolutely different than workdays. Thereto Saturday and Sunday are different too.
 
### Creation of forecast model for different days during the week (similar day approach)
 
Ass we have seen in the previous plot (plots), forecast of time series of electricity consumption will be challenging task. We have 2 main seasonalities - daily and weekly. So it is necessary to adapt forecast model to this problem. One of the ideas to overcome this problem is to use similar day approach - separate forecast models for groups of days.
 
Lets prepare data and define all functions to achieve this goal. Add corresponding weekdays to date for datasets `DT_48` and `DT_agg`.

{% highlight r %}
DT_48[, week := weekdays(date_time)]
DT_agg[, ':='(week = weekdays(date_time), date = as.Date(date_time))]
unique(DT_agg[, week])
{% endhighlight %}



{% highlight text %}
## [1] "Monday"    "Tuesday"   "Wednesday" "Thursday"  "Friday"    "Saturday" 
## [7] "Sunday"
{% endhighlight %}
 
Now we have datasets with all needed features to build model for different days.
 
Extract date, ID, weekdays and period for further better working with subsetting.

{% highlight r %}
n_ID <- unique(DT_48[, ID])
n_weekdays <- unique(DT_agg[, week])
n_date <- unique(DT_agg[, date])
period <- 48
{% endhighlight %}
 
Lets define basic forecast methods - functions, which will produce forecasts. I am using two powerful methods, which are based on decomposition of time series. STL + ARIMA and STL + exponential smoothing. [STL](https://www.otexts.org/fpp/6/5) decomposition is widely used decomposition for seasonal time series, it is based on Loess regression. With [package](https://CRAN.R-project.org/package=forecast) `forecast` it can be combined to produce very accurate forecasts. We have two main possibilities of usage - with [ARIMA](https://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average) and with [exponential smoothing](https://en.wikipedia.org/wiki/Exponential_smoothing). We will use both to compare the performance (accuracy) of both. It should be added that the functions returns forecast of the length of one period (in this case 48 values).

{% highlight r %}
# STL + ARIMA
stlARIMAPred <- function(Y, period = 48){
  ts_Y <- ts(Y, start = 0, freq = period)
  dekom <- stl(ts_Y, s.window = "periodic", robust = TRUE)
  arima <- forecast(dekom, h = period, method = "arima")
  return(as.vector(arima$mean))
}
# STL + EXP
stlEXPPred <- function(Y, period = 48){
  ts_Y <- ts(Y, start = 0, freq = period)
  dekom <- stl(ts_Y, s.window = "periodic", robust = TRUE)
  expo <- forecast(dekom, h = period, method = "ets", etsmodel = "ZZN")
  return(as.vector(expo$mean))
}
{% endhighlight %}
 
Next it is a necessary to define metric, with which our forecasts will be compared. For simple comparison [MAPE](https://en.wikipedia.org/wiki/Mean_absolute_percentage_error) is used. Lets define function to compute Mean Absolute Percentage Error.  

{% highlight r %}
mape <- function(real, pred){
  return(100 * mean(abs((real - pred)/real)))
}
{% endhighlight %}
 
Now it's "simple" to define function, which will produce forecast for whole week. It's based on subsetting the given `data.table` by group of weekdays. We can simply change in it basic function of forecast (`FUN`), set of dates and length of training window (`train_win`).

{% highlight r %}
predictWeek <- function(data, set_of_date, FUN, train_win = 6){
 
 for_mon <- FUN(data[(week == n_weekdays[1] & date %in% set_of_date), value])
 seq_tuethu <- data[(week %in% n_weekdays[2:4] & date %in% set_of_date), value]
 for_tuethu <- as.vector(sapply(2:0, function(j)
   FUN(seq_tuethu[(length(seq_tuethu)-(period*j)+1-(train_win*period)):(length(seq_tuethu)-(period*j))])))
 for_fri <- FUN(data[(week == n_weekdays[5] & date %in% set_of_date), value])
 for_sat <- FUN(data[(week == n_weekdays[6] & date %in% set_of_date), value])
 for_sun <- FUN(data[(week == n_weekdays[7] & date %in% set_of_date), value])
 
 return(c(for_mon, for_tuethu, for_fri, for_sat, for_sun))
}
{% endhighlight %}
 
Lets do some examples of using `predictWeek` function. Run forecast for selection of dates on aggregated consumption and compute MAPE for both methods (STL+ARIMA and STL+EXP).

{% highlight r %}
for_week_arima <- predictWeek(DT_agg, n_date[56:84], stlARIMAPred)
for_week_exp <- predictWeek(DT_agg, n_date[56:84], stlEXPPred)
real_week <- DT_agg[date %in% n_date[85:91], value]
c(ARIMA = mape(real_week, for_week_arima), EXP = mape(real_week, for_week_exp))
{% endhighlight %}



{% highlight text %}
##    ARIMA      EXP 
## 3.796372 4.813500
{% endhighlight %}
 
Not so bad, actually very accurate.
 
Compute MAPE for every day of week separately - for better analysis.

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
 
And of course...plot computed forecast for one week ahead.

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

![plot of chunk unnamed-chunk-33](/images/unnamed-chunk-33-1.png)
 
Seems ARIMA can produce more accurate forecast on aggregated consumption than exponential smoothing.
 
Lets try it on dissagregated load, so on one consumer (ID).
 

{% highlight r %}
for_week_arima <- predictWeek(DT_48[ID == n_ID[40]], n_date[56:84], stlARIMAPred)
for_week_exp <- predictWeek(DT_48[ID == n_ID[40]], n_date[56:84], stlEXPPred)
real_week <- DT_48[ID == n_ID[40] & date %in% n_date[85:91], value]
c(ARIMA = mape(real_week, for_week_arima), EXP = mape(real_week, for_week_exp))
{% endhighlight %}



{% highlight text %}
##    ARIMA      EXP 
## 5.516613 7.131615
{% endhighlight %}
 
Similar results, but obviously not so accurate because of stochastic behavior of consumers.
 
Plot computed forecast for one week ahead.

{% highlight r %}
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
{% endhighlight %}

![plot of chunk unnamed-chunk-35](/images/unnamed-chunk-35-1.png)
 
Some words about forecast.
 
Like this you can make forecast for any consumer, any set of date and with your own forecast method.
 
On future posts I want mainly focus on regression methods for time series forecasting, because they can handle similar day approach much more easelly. So methods like multiple linear regression, generalized additive model, support vector regression, regression trees and forests and artificial neural networks will be shown.
