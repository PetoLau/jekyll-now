---
layout: post
title: My first post
author: Peter Laurinec
published: true
status: publish
draft: false
tags: R
---
 
## Data Analysis
 
Generate some random numbers:
 

{% highlight r %}
rnorm(50)
{% endhighlight %}
 

{% highlight text %}
##  [1] -0.09908633 -0.03357058 -0.60778414 -0.77198704 -0.28020854
##  [6]  0.54725232 -0.07990724 -0.18639582  0.34952381  1.74911237
## [11] -1.62903895  0.74886671  1.21201765  0.51670520  1.18706508
## [16]  0.25144479  1.11295035 -0.44696164  0.95832188 -1.33602422
## [21] -0.15915637  0.96165292 -0.75254709  0.98230403  1.78300303
## [26] -0.50469664 -0.45972744 -0.28388731 -0.10902688 -0.18646227
## [31]  0.22861480 -0.51499551  0.16696061  0.02347941 -0.16399682
## [36]  0.23200253  1.41626886 -1.49010576 -0.79229925 -0.24205657
## [41] -0.04413988 -0.04958219  0.22183133 -1.11814828  0.17805408
## [46]  0.20143938 -0.45949441  1.03209873  0.97365609 -1.31126945
{% endhighlight %}
 
## Ploting time series
 
Plot random numbers like time series:
 

{% highlight r %}
plot(ts(timeseries_1, start = 0), ylab = "", xlab = "Time")
{% endhighlight %}

![plot of chunk plot](/images/plot-1.png)
