---
layout: post
title: Forecast double seasonal time series with multiple linear regression in R
author: Peter Laurinec
published: true
status: publish
tags: test
draft: false
---

Deployment of smart grids gives space to an occurrence of new methods of machine learning and data analysis. Smart grids can contain of millions of smart meters, which produce a large amount of data of electricity consumption (long time series). In addition to time series of electricity consumption, we can have extra information about the consumer like ZIP code, type of consumer ([consumer vs. prosumer](http://wikidiff.com/consumer/prosumer)) and so on. These data can be used to support intelligent grid control, make an accurate forecast or to detect anomalies. In this blog post, I will focus on the exploration of available open smart meter data and on the creation of a simple forecast model, which uses similar day approach (will be drawn up in detail below).

So...I hope I haven't forgotten something, go ahead to the programming and exploration part of this post. First step - scan all of the needed packages.

{% highlight r %}
library(feather)
library(data.table)
library(ggplot2)
library(plotly)
library(animation)
{% endhighlight %}
 

 
Use feather (fast to share data) to read `data.table`.

{% highlight r %}
DT <- as.data.table(read_feather("DT_4_ind"))
{% endhighlight %}
 
Plot aggregated time series of consumption by industry.

{% highlight r %}
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
{% endhighlight %}

![plot of chunk unnamed-chunk-4](/images/unnamed-chunk-4-1.png)
 
Prepare `DT` to work with a regression model. Tranform charactres of weekdays to integers.

{% highlight r %}
DT[, week_num := as.integer(as.factor(DT[, week]))]
{% endhighlight %}



{% highlight text %}
##                  date_time    value   week       date                type
##     1: 2012-01-02 00:00:00 1590.210 Monday 2012-01-02 Commercial Property
##     2: 2012-01-02 00:30:00 1563.772 Monday 2012-01-02 Commercial Property
##     3: 2012-01-02 01:00:00 1559.914 Monday 2012-01-02 Commercial Property
##     4: 2012-01-02 01:30:00 1584.671 Monday 2012-01-02 Commercial Property
##     5: 2012-01-02 02:00:00 1604.281 Monday 2012-01-02 Commercial Property
##    ---                                                                   
## 70076: 2012-12-31 21:30:00 3548.279 Monday 2012-12-31    Light Industrial
## 70077: 2012-12-31 22:00:00 3488.161 Monday 2012-12-31    Light Industrial
## 70078: 2012-12-31 22:30:00 3510.200 Monday 2012-12-31    Light Industrial
## 70079: 2012-12-31 23:00:00 3533.678 Monday 2012-12-31    Light Industrial
## 70080: 2012-12-31 23:30:00 3414.966 Monday 2012-12-31    Light Industrial
##        week_num
##     1:        2
##     2:        2
##     3:        2
##     4:        2
##     5:        2
##    ---         
## 70076:        2
## 70077:        2
## 70078:        2
## 70079:        2
## 70080:        2
{% endhighlight %}
 
Store information of the type of consumer, date, weekday and period.

{% highlight r %}
n_type <- unique(DT[, type])
n_weekdays <- unique(DT[, week])
n_date <- unique(DT[, date])
period <- 48
{% endhighlight %}
 
Let's look at some data chunk of consumption and try do some regression analysis. Pick aggregate consumption of education (schools) buildings.

{% highlight r %}
data_r <- DT[(type == n_type[2] & date %in% n_date[57:84])]
 
ggplot(data_r, aes(date_time, value)) +
  geom_line() +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Load (kW)")
{% endhighlight %}

![plot of chunk unnamed-chunk-7](/images/unnamed-chunk-7-1.png)
 
Multiple linear regression (form, assumptions). Like in the previous form, we want to forecast consumption one week ahead,
so construction of seasonal features is necessery. Let's create daily and weekly seasonal dummy variables. Form like 10001...11110000.
Compute features to model and store it in matrix_train.

{% highlight r %}
N <- nrow(data_r)
 
matrix_train <- matrix(0, nrow = N, ncol = period + 6)
for(j in 1:period){
  matrix_train[seq(j, N, by = period), j] <- 1
}
 
# using feature week_num
for(j in 1:6){
  matrix_train[data_r[, week_num] == j, period + j] <- 1
}
 
matrix_train <- as.data.frame(cbind(data_r[, value], matrix_train))
 
colnames(matrix_train) <- c("Load",
                            sapply(1:period, function(i) paste("d", i, sep = "")),
                            sapply(1:6, function(i) paste("w", i, sep = "")))
{% endhighlight %}
 
Collinearity and singularity, so w7 isn't constructed. Names of features are needed due to clarity and class of object `lm`.
Lets create our first multiple linear model (I will refer it as MLR) with function `lm`. Intercept is inappropiate in this sceniario, again due to collinearity and meaningfulness.
 

{% highlight r %}
lm_m_1 <- lm(Load ~ 0 + ., data = matrix_train)
{% endhighlight %}
 

{% highlight r %}
summary(lm_m_1)
{% endhighlight %}



{% highlight text %}
## 
## Call:
## lm(formula = Load ~ 0 + ., data = matrix_train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -642.44 -164.25  -31.86  206.48  532.68 
## 
## Coefficients:
##       Estimate Std. Error t value Pr(>|t|)    
## d1   949.85683   50.12225  18.951   <2e-16 ***
## d2   915.50394   50.12225  18.265   <2e-16 ***
## d3   873.23501   50.12225  17.422   <2e-16 ***
## d4   850.50417   50.12225  16.969   <2e-16 ***
## d5   827.43311   50.12225  16.508   <2e-16 ***
## d6   801.35372   50.12225  15.988   <2e-16 ***
## d7   780.00938   50.12225  15.562   <2e-16 ***
## d8   761.09518   50.12225  15.185   <2e-16 ***
## d9   749.58842   50.12225  14.955   <2e-16 ***
## d10  745.67667   50.12225  14.877   <2e-16 ***
## d11  740.87880   50.12225  14.781   <2e-16 ***
## d12  740.98697   50.12225  14.784   <2e-16 ***
## d13  747.08514   50.12225  14.905   <2e-16 ***
## d14  747.72917   50.12225  14.918   <2e-16 ***
## d15  755.09423   50.12225  15.065   <2e-16 ***
## d16  758.98207   50.12225  15.143   <2e-16 ***
## d17  762.94561   50.12225  15.222   <2e-16 ***
## d18  763.66924   50.12225  15.236   <2e-16 ***
## d19  789.75058   50.12225  15.756   <2e-16 ***
## d20  806.14430   50.12225  16.084   <2e-16 ***
## d21  878.28039   50.12225  17.523   <2e-16 ***
## d22  963.92253   50.12225  19.231   <2e-16 ***
## d23 1116.94015   50.12225  22.284   <2e-16 ***
## d24 1256.83117   50.12225  25.075   <2e-16 ***
## d25 1382.99782   50.12225  27.592   <2e-16 ***
## d26 1497.38460   50.12225  29.875   <2e-16 ***
## d27 1577.17166   50.12225  31.466   <2e-16 ***
## d28 1615.46187   50.12225  32.230   <2e-16 ***
## d29 1638.98732   50.12225  32.700   <2e-16 ***
## d30 1651.80317   50.12225  32.955   <2e-16 ***
## d31 1674.40373   50.12225  33.406   <2e-16 ***
## d32 1690.39292   50.12225  33.725   <2e-16 ***
## d33 1692.91796   50.12225  33.776   <2e-16 ***
## d34 1689.63837   50.12225  33.710   <2e-16 ***
## d35 1673.01962   50.12225  33.379   <2e-16 ***
## d36 1640.92279   50.12225  32.738   <2e-16 ***
## d37 1602.88803   50.12225  31.980   <2e-16 ***
## d38 1574.96860   50.12225  31.423   <2e-16 ***
## d39 1525.19045   50.12225  30.429   <2e-16 ***
## d40 1443.96607   50.12225  28.809   <2e-16 ***
## d41 1356.26893   50.12225  27.059   <2e-16 ***
## d42 1283.36918   50.12225  25.605   <2e-16 ***
## d43 1192.18675   50.12225  23.786   <2e-16 ***
## d44 1129.91725   50.12225  22.543   <2e-16 ***
## d45 1065.26136   50.12225  21.253   <2e-16 ***
## d46 1029.68688   50.12225  20.544   <2e-16 ***
## d47  986.04463   50.12225  19.673   <2e-16 ***
## d48  963.08927   50.12225  19.215   <2e-16 ***
## w1   -32.76478   25.52100  -1.284    0.199    
## w2     0.03753   25.52100   0.001    0.999    
## w3  -560.14912   25.52100 -21.949   <2e-16 ***
## w4  -583.32014   25.52100 -22.856   <2e-16 ***
## w5     7.39373   25.52100   0.290    0.772    
## w6     4.86152   25.52100   0.190    0.849    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 250.1 on 1290 degrees of freedom
## Multiple R-squared:   0.95,	Adjusted R-squared:  0.948 
## F-statistic: 454.3 on 54 and 1290 DF,  p-value: < 2.2e-16
{% endhighlight %}
 
Summary seems not bad, R-squared high, F-statistic of goodness of fit is fine too.
Let's look on fitted values and residuals.
 

{% highlight r %}
datas <- rbindlist(list(data_r[, .(value, date_time)], data.table(value = lm_m_1$fitted.values, data_time = data_r[, date_time])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]
 
ggplot(data = datas, aes(date_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from MLR")
{% endhighlight %}

![plot of chunk unnamed-chunk-11](/images/unnamed-chunk-11-1.png)
 
Fit vs residuals - heteroscedasticity - non constant and nonnormal residuals (assumptions).

<iframe width="500" height="400" frameborder="0" scrolling="no" src="//plot.ly/~PetoLau/0.embed"></iframe>

