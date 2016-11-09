---
layout: post
title: Enernoc smart meter data and forecast with similar day approach
author: Peter Laurinec
published: true
status: publish
draft: false
tags: R, forecast
---
 
# Exploring meta data of consumers (ID)
 
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
knitr::opts_chunk$set(include = FALSE)
opts_knit$set(root.dir = "C:\\Users\\Peter\\Downloads\\ProjektBD\\enernoc\\csv\\")
{% endhighlight %}
 
Read meta data and show their structure.
 

 
Nice features to explore...so frequency table of industries:

 
Map of USA of location of our consumers.

 
Histogram of sqft -> square meter.

 
Density plot of industries and SQ_M.

 
Load all .csv files containing electricity consumption in one data.table by rbindlist and lapply. See structure and change column names.

 
Prepare meta_data to merge with DT.

 
Extract possible interesting features from ID.

 
Merge and aggregate by sub_industry.

 
Bar plot of mean load by sub_industries.

 
Regression line SQ_M vs Median Load.

 
# Prepare dataset to forecast and explore time series from smart meters
Transform characters to classical Date and Date&Time format (POSIxt).

Extract ID's with an whole length (105408)

 
Extract date's with an all measurements during the day (288).  First and last date has not all measurements - so remove them.

Our extracted (filtered) ID's.

 
Plot one (ID 99).

 
Aggregate consumption to 48 measurements per day (every half hour) - due to reduction of dimensionality - 48/per day is good compromise.

 
Plot typical representants of 4 groups of industries.

 
Aggregate consumption of all consumers (43).

 
Median daily profile of aggregate consumption with MAD (median absolute deviation).

 
Median weekly profile of aggregate consumption with MAD (median absolute deviation).

 
## Creation of forecast model for different days during the week (similar day approach)
 
Add corresponding weekdays to date for datasets DT_48 and DT_agg

Now we have datasets with all needed features to build model for different days.
Extract date, ID, weekdays and period for better working with subsetting.

 
Define forecast methods. STL+ARIMA and STL+EXP.

 
Function to return forecast of the length one week.

 
Define function to compute Mean Absolute Percentage Error.

 
Run prediction and compute MAPE of both methods.

 
Compute MAPE for every day of week separately.

 
Plot computed forecast of one week ahead.

 
THE END.
