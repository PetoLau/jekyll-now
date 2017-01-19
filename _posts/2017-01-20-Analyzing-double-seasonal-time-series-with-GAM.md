---
layout: post
title: Doing "magic" and analyzing double seasonal time series with GAM (Generalized Additive Model) in R
author: Peter Laurinec
published: true
status: publish
tags: test
draft: false
---
 
As I wrote in the previous post, I will continue in describing **regression** methods, which are suitable to double seasonal (or multi-seasonal) **time series**. In the previous post about [Multiple Linear Regression](https://petolau.github.io/Forecast-double-seasonal-time-series-with-multiple-linear-regression-in-R/), I showed how to use "simple" [**OLS**](https://en.wikipedia.org/wiki/Ordinary_least_squares) regression method to model **double seasonal time series** of electricity consumption and use it for accurate forecasting. Interactions between two seasonal variables were successfully used to achieve this goal. The issue of [forecasting time series from smart meters](https://petolau.github.io/Forecast-electricity-consumption-with-similar-day-approach-in-R/) was discussed in my first post.
 
In this post (tutorial), I will fully introduce "magical" [**Generalized Additive Model**](https://en.wikipedia.org/wiki/Generalized_additive_model) (GAM) to model time series of electricity consumption. You are maybe asking, why words "*magic*" or "*magical*" are mentioned? You will see in continuation of this post :wink:
 
Now I have to warn you that this blog post is very long. **GAM** is a very complex method and I wanted to go little bit deeply in order to introduce you this interesting regression method. I think it will be worth it and in the end of the post, I hope you will understand a theory behind **R** functions. In my opinion, **R** blogging (or any data mining or machine learning blogging) without theory interpretation of used methods has zero informative value for a reader.
 
So, let's look under the roof of the **GAM** regression method. I think you need some motivation to it, here it is!
 
![](/images/motivate.png)
 
### Theory behind Generalized Additive Model (GAM) 
 
What these three words (or letters) in the name of this method mean and where it comes from? I am sure that you know something about **Linear Model** (maybe because you had read my [previous post about MLR](https://petolau.github.io/Forecast-double-seasonal-time-series-with-multiple-linear-regression-in-R/) :relaxed:). It is simple regression method which model response (dependent) variable by independent variable(s). It's solved by OLS method. So we now know what is **M** in the name. When we want to linearly model response variable which is not from [normal Gaussian distribution](https://en.wikipedia.org/wiki/Normal_distribution), for example, it can be binary (logistic regression) or discrete (Poisson) variable, we can use a generalization of the linear model - [Generalized Linear Model](https://en.wikipedia.org/wiki/Generalized_linear_model) (GLM). It's solved by [iteratively reweighted least squares](https://en.wikipedia.org/wiki/Iteratively_reweighted_least_squares) (IRLS) method. **G** in the name is also solved. **A** letter, so [Additive Model](https://en.wikipedia.org/wiki/Additive_model), means that response variable depends linearly on unknown smooth functions. In the other words, the goal is to model response variable by independent variables, which are in the form of some smooth functions. Voilà, **GAM** is created.
 
The GAM can be formally written as:
 
$$g(E(y_i)) = \beta_0 + f_1(x_{i1}) + \dots + f_p(x_{ip}) + \varepsilon_i,$$
$$y_i \sim \mbox{some exponential family distribution,}$$
 
where \\( i = 1, \dots, N \\), \\( g \\) is a link function (identical, logarithmic or inverse), \\( y \\) is a response variable, \\( x_1, \dots, x_p \\) are independent variables, \\( \beta_0 \\) is an intercept, \\( f_1, \dots, f_p \\) are unknown smooth functions and \\( \varepsilon \\) is an i.i.d. random error.
 
The smooth function \\( f \\) is composed by sum of basis functions \\( b \\) and theirs corresponding regression coefficients \\( \beta \\), formally written:
 
$$f(x) = \sum_{i = 1}^q b_i(x)\beta_i,$$
$$\mbox{ where } q \mbox{ is basis dimension.}$$
 
Smooth functions are also called [splines](https://en.wikipedia.org/wiki/Spline_(mathematics)). [Smoothing splines](https://en.wikipedia.org/wiki/Smoothing_spline) are real functions that are piecewise-defined by polynomial functions (basis functions). The places where the polynomial pieces connect are called knots. In **GAMs**, penalized regression splines are used in order to regularize the smoothness of a spline.
 
It follows that model can be written in a linear way like this:
 
$$g(E(y)) = \beta\mathbf{X} + \varepsilon,$$
 
where \\( \mathbf{X} \\) is a model matrix and \\( \beta \\) is a vector of regression coefficients.
 
Than objective function to be minimized is:
 
$$\parallel y - \beta\mathbf{X}\parallel^2 + \lambda\int_0^1 [f^{''}(x)]^2dx,$$
 
where \\( \lambda \\) is smoothing parameter and the integral of squares of second derivatives can be written as:
 
$$\int_0^1 [f^{''}(x)]^2dx = \beta^T\mathbf{S}\beta,$$
 
where \\( \mathbf{S} \\) is matrix of known coefficients.
 
All this mathematical madness then implies that regression coefficients can be obtained (estimated) by the equation:
 
$$\hat{\beta} = (\mathbf{X}^T\mathbf{X} + \lambda\mathbf{S})^{-1}\mathbf{X}^Ty.$$
 
\\( \hat{\beta} \\) is called penalized least squares estimator in this case.
The method of obtaining the estimate of the \\( \beta \\) is called Penalized Iteratively Re-weighted Least Squares (P-IRLS).
 
Have you already enough of this post? I would no wonder...you can skip this section whenever you want, but there are next important theoretical questions to answer. For example, what kind of smoothing splines exists? Or, how to choose optimally smoothing parameter \\( \lambda \\)? How is set basis dimensions (or a number of knots)?
 
There are several smoothing bases (splines) which are suitable for regression:
 
* thin plate regression splines
* cubic regression spline
* cyclic cubic regression spline
* P-splines
 
You can then read more about them in references, which I will write up. In this post, for daily seasonality cubic regression spline, and for weekly seasonality P-splines will be used. Both types of splines are knot-based, so choosing a right number of knots will be important.
 
Next, an important procedure is to choose (estimate) optimal smoothing parameter \\( \lambda \\) and the number of basis dimensions (i.e. degrees of freedom). This can be done in **GAM** by Generalized Cross Validation score (GCV). It minimizes an equation:
 
$$\nu_g = \frac{n\sum_{i=1}^n (y_i - \hat{f}_i)^2}{[tr(\mathbf{I} - \mathbf{A})]^2},$$
 
where \\( \mathbf{A} \\) is the influence or hat matrix. It's obvious that when lambda is near 1 then spline will be over-smoothed, in opposite side when lambda is near zero than spline isn't penalized so the method behaves like classical OLS. With a number of basis dimensions (estimated degrees of freedom), it is opposite. Higher dimension implies that fit will be less smoothed (overfit), on the other side lower dimensions implies more smoothed behavior of fitted values.
 
#### Interactions
 
We are near the end of the explanation of **GAMs** theory, one more thing. As I showed in the previous blog post, **interactions** are very important part of the **regression** model for **double seasonal time series**. With **GAMs** there are four (!) main possibilities how to include them to the model. First is the most basic, like in **MLR**, the multiplication of two independent variables: \\( x_1\times x_2 \\). Second one is possibility to use smoothed function to one variable: \\( f_1(x_1)\times x_2 \\). Third one comes to use same smoothed function for both variables: \\( f(x_1, x_2) \\). Fourth one is the most complex, with GAM it is possible to use [tensor product](https://en.wikipedia.org/wiki/Tensor_product) interactions. So it is possible to use different smoothing bases for variables and penalize it in two (when we do interactions of two independent variables) different ways: \\( f_1(x_1)\otimes f_2(x_2)  \\). More nicely, tensor product interactions can be written as:
 
$$f_{12}(x_1, x_2) = \sum_{i=1}^I \sum_{j=1}^J \delta_{ij}b_{1i}(x_1)b_{2j}(x_2),$$
 
where \\( b_1 \\) and \\( b_2 \\) are basis functions, \\( I \\) and \\( J \\) are corresponding basis dimensions and \\( \delta \\) is vector of unknown coefficients.
 
This allows for an overall [anisotropic](https://en.wikipedia.org/wiki/Anisotropy) (different in each direction) penalty, so the overall shape of a tensor product smooth is invariant to a rescaling of its independent variables. This is a huge advantage in the comparison to usage of one smoothing function. Simply said, we have theoretically supported that it's allowed to use different metrics of variables in the interactions term.
 
#### Resources (references)
 
Phuuu...that's almost all of the difficult theory behind **GAM** method. I will add some more things in analytical part of this post. Honestly, it was difficult to me to find this information on the web. So if you want to read more about **GAM**, here is the list of useful links which were used in my post:
 
* Amazing book: Simon N. Wood: [Generalized Additive Models: an introduction with R](https://www.crcpress.com/Generalized-Additive-Models-An-Introduction-with-R/Wood/p/book/9781584884743), CRC Press, 2006.
* Great blog post by Gavin Simpson: [Modelling seasonal data with GAMs](http://www.fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/)
* Very helpful StackExchange (Cross Validated) [question about the intuition behind tensor product interactions](http://stats.stackexchange.com/questions/45446/intuition-behind-tensor-product-interactions-in-gams-mgcv-package-in-r)
* This [tutorial about mixed GAMs](http://www.sfs.uni-tuebingen.de/~jvanrij/Tutorial/GAMM.html) can be helpful too.
 
Go ahead to modeling and analyzing time series with **GAMs**
 
### Doing "magic" with GAMs for modeling time series
 
I have prepared a file with four aggregated time series of electricity consumption for an analysis. It can be found on [my GitHub repo](https://github.com/PetoLau/petolau.github.io/tree/master/_rmd), the name of the file is *DT_4_ind*. The file was created easily by the package `feather` ([CRAN link](https://CRAN.R-project.org/package=feather)). Data manipulations will be done then by `data.table` package.
 
GAM methods are implemented in **R** in the awesome package `mgcv` by Simon Wood ([link to `mgcv`](https://CRAN.R-project.org/package=mgcv)).
 
For visualizations packages `ggplot2`, `dygraphs`, `xts`, `grid` and `animation` will be used. One useful function from package `car` will be used too.
 
Let's scan all of the needed packages.

{% highlight r %}
library(feather)
library(data.table)
library(mgcv)
library(car)
library(ggplot2)
library(dygraphs)
library(xts)
library(grid)
library(animation)
{% endhighlight %}
 

 
Read the mentioned smart meter data by `read_feather` to one `data.table`.

{% highlight r %}
DT <- as.data.table(read_feather("DT_4_ind"))
{% endhighlight %}
 
Prepare `DT` to work with a **GAM** regression model. Transform the characters of weekdays to integers and use function `recode` from package `car` to recode weekdays as there are coming in the week: 1. Monday, ..., 7. Sunday.

{% highlight r %}
DT[, week_num := as.integer(as.factor(DT[, week]))]
DT[, week_num := recode(week_num, "1=5;2=1;3=6;4=7;5=4;6=2;7=3")]
{% endhighlight %}
 
Store informations in variables of the type of industry, date, weekday and period for simpler working.

{% highlight r %}
n_type <- unique(DT[, type])
n_date <- unique(DT[, date])
n_weekdays <- unique(DT[, week])
period <- 48
{% endhighlight %}
 
Let's look at some data chunk of electricity consumption and do analysis on it. I have picked aggregate consumption of commercial properties for two weeks. Store it in variable `data_r` and plot it.

{% highlight r %}
data_r <- DT[(type == n_type[1] & date %in% n_date[57:70])]
 
ggplot(data_r, aes(date_time, value)) +
  geom_line() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Load (kW)")
{% endhighlight %}

![plot of chunk unnamed-chunk-6](/images/unnamed-chunk-6-1.png)
 
As a big fan of a data visualizations, I'm thinking about to move from `ggplot2` graphs of time series to [`dygraphs`](https://rstudio.github.io/dygraphs/index.html). Biggest advantage of `dygraphs` its their rich interactivity and little bit simplier syntax than with `ggplot2`. Let's do a `dygraph` version of previous plot.

{% highlight r %}
dygraph(data_r[, .(date_time, value)]) %>%
  dyOptions(fillGraph = TRUE, fillAlpha = 0.5,
            strokeWidth = 2) %>%
  dyRangeSelector()
{% endhighlight %}

![plot of chunk unnamed-chunk-7](/images/unnamed-chunk-7-1.png)

<div id="htmlwidget-9252" style="width:864px;height:432px;" class="dygraphs html-widget"></div>
<script type="application/json" data-for="htmlwidget-9252">{"x":{"attrs":{"labels":["minute","value"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60,"drawAxis":true},"y":{"drawAxis":true}},"stackedGraph":false,"fillGraph":true,"fillAlpha":0.5,"stepPlot":false,"drawPoints":false,"pointSize":1,"drawGapEdgePoints":false,"connectSeparatedPoints":false,"strokeWidth":2,"strokeBorderColor":"white","colorValue":0.5,"colorSaturation":1,"includeZero":false,"drawAxesAtZero":false,"logscale":false,"axisTickSize":3,"axisLineColor":"black","axisLineWidth":0.3,"axisLabelColor":"black","axisLabelFontSize":14,"axisLabelWidth":60,"drawGrid":true,"gridLineWidth":0.3,"rightGap":5,"digitsAfterDecimal":2,"labelsKMB":false,"labelsKMG2":false,"labelsUTC":false,"maxNumberWidth":6,"animatedZooms":false,"mobileDisableYTouch":true,"showRangeSelector":true,"rangeSelectorHeight":40,"rangeSelectorPlotFillColor":" #A7B1C4","rangeSelectorPlotStrokeColor":"#808FAB","interactionModel":"Dygraph.Interaction.defaultModel"},"scale":"minute","annotations":[],"shadings":[],"events":[],"format":"date","data":[["2012-02-27T00:00:00Z","2012-02-27T00:30:00Z","2012-02-27T01:00:00Z","2012-02-27T01:30:00Z","2012-02-27T02:00:00Z","2012-02-27T02:30:00Z","2012-02-27T03:00:00Z","2012-02-27T03:30:00Z","2012-02-27T04:00:00Z","2012-02-27T04:30:00Z","2012-02-27T05:00:00Z","2012-02-27T05:30:00Z","2012-02-27T06:00:00Z","2012-02-27T06:30:00Z","2012-02-27T07:00:00Z","2012-02-27T07:30:00Z","2012-02-27T08:00:00Z","2012-02-27T08:30:00Z","2012-02-27T09:00:00Z","2012-02-27T09:30:00Z","2012-02-27T10:00:00Z","2012-02-27T10:30:00Z","2012-02-27T11:00:00Z","2012-02-27T11:30:00Z","2012-02-27T12:00:00Z","2012-02-27T12:30:00Z","2012-02-27T13:00:00Z","2012-02-27T13:30:00Z","2012-02-27T14:00:00Z","2012-02-27T14:30:00Z","2012-02-27T15:00:00Z","2012-02-27T15:30:00Z","2012-02-27T16:00:00Z","2012-02-27T16:30:00Z","2012-02-27T17:00:00Z","2012-02-27T17:30:00Z","2012-02-27T18:00:00Z","2012-02-27T18:30:00Z","2012-02-27T19:00:00Z","2012-02-27T19:30:00Z","2012-02-27T20:00:00Z","2012-02-27T20:30:00Z","2012-02-27T21:00:00Z","2012-02-27T21:30:00Z","2012-02-27T22:00:00Z","2012-02-27T22:30:00Z","2012-02-27T23:00:00Z","2012-02-27T23:30:00Z","2012-02-28T00:00:00Z","2012-02-28T00:30:00Z","2012-02-28T01:00:00Z","2012-02-28T01:30:00Z","2012-02-28T02:00:00Z","2012-02-28T02:30:00Z","2012-02-28T03:00:00Z","2012-02-28T03:30:00Z","2012-02-28T04:00:00Z","2012-02-28T04:30:00Z","2012-02-28T05:00:00Z","2012-02-28T05:30:00Z","2012-02-28T06:00:00Z","2012-02-28T06:30:00Z","2012-02-28T07:00:00Z","2012-02-28T07:30:00Z","2012-02-28T08:00:00Z","2012-02-28T08:30:00Z","2012-02-28T09:00:00Z","2012-02-28T09:30:00Z","2012-02-28T10:00:00Z","2012-02-28T10:30:00Z","2012-02-28T11:00:00Z","2012-02-28T11:30:00Z","2012-02-28T12:00:00Z","2012-02-28T12:30:00Z","2012-02-28T13:00:00Z","2012-02-28T13:30:00Z","2012-02-28T14:00:00Z","2012-02-28T14:30:00Z","2012-02-28T15:00:00Z","2012-02-28T15:30:00Z","2012-02-28T16:00:00Z","2012-02-28T16:30:00Z","2012-02-28T17:00:00Z","2012-02-28T17:30:00Z","2012-02-28T18:00:00Z","2012-02-28T18:30:00Z","2012-02-28T19:00:00Z","2012-02-28T19:30:00Z","2012-02-28T20:00:00Z","2012-02-28T20:30:00Z","2012-02-28T21:00:00Z","2012-02-28T21:30:00Z","2012-02-28T22:00:00Z","2012-02-28T22:30:00Z","2012-02-28T23:00:00Z","2012-02-28T23:30:00Z","2012-02-29T00:00:00Z","2012-02-29T00:30:00Z","2012-02-29T01:00:00Z","2012-02-29T01:30:00Z","2012-02-29T02:00:00Z","2012-02-29T02:30:00Z","2012-02-29T03:00:00Z","2012-02-29T03:30:00Z","2012-02-29T04:00:00Z","2012-02-29T04:30:00Z","2012-02-29T05:00:00Z","2012-02-29T05:30:00Z","2012-02-29T06:00:00Z","2012-02-29T06:30:00Z","2012-02-29T07:00:00Z","2012-02-29T07:30:00Z","2012-02-29T08:00:00Z","2012-02-29T08:30:00Z","2012-02-29T09:00:00Z","2012-02-29T09:30:00Z","2012-02-29T10:00:00Z","2012-02-29T10:30:00Z","2012-02-29T11:00:00Z","2012-02-29T11:30:00Z","2012-02-29T12:00:00Z","2012-02-29T12:30:00Z","2012-02-29T13:00:00Z","2012-02-29T13:30:00Z","2012-02-29T14:00:00Z","2012-02-29T14:30:00Z","2012-02-29T15:00:00Z","2012-02-29T15:30:00Z","2012-02-29T16:00:00Z","2012-02-29T16:30:00Z","2012-02-29T17:00:00Z","2012-02-29T17:30:00Z","2012-02-29T18:00:00Z","2012-02-29T18:30:00Z","2012-02-29T19:00:00Z","2012-02-29T19:30:00Z","2012-02-29T20:00:00Z","2012-02-29T20:30:00Z","2012-02-29T21:00:00Z","2012-02-29T21:30:00Z","2012-02-29T22:00:00Z","2012-02-29T22:30:00Z","2012-02-29T23:00:00Z","2012-02-29T23:30:00Z","2012-03-01T00:00:00Z","2012-03-01T00:30:00Z","2012-03-01T01:00:00Z","2012-03-01T01:30:00Z","2012-03-01T02:00:00Z","2012-03-01T02:30:00Z","2012-03-01T03:00:00Z","2012-03-01T03:30:00Z","2012-03-01T04:00:00Z","2012-03-01T04:30:00Z","2012-03-01T05:00:00Z","2012-03-01T05:30:00Z","2012-03-01T06:00:00Z","2012-03-01T06:30:00Z","2012-03-01T07:00:00Z","2012-03-01T07:30:00Z","2012-03-01T08:00:00Z","2012-03-01T08:30:00Z","2012-03-01T09:00:00Z","2012-03-01T09:30:00Z","2012-03-01T10:00:00Z","2012-03-01T10:30:00Z","2012-03-01T11:00:00Z","2012-03-01T11:30:00Z","2012-03-01T12:00:00Z","2012-03-01T12:30:00Z","2012-03-01T13:00:00Z","2012-03-01T13:30:00Z","2012-03-01T14:00:00Z","2012-03-01T14:30:00Z","2012-03-01T15:00:00Z","2012-03-01T15:30:00Z","2012-03-01T16:00:00Z","2012-03-01T16:30:00Z","2012-03-01T17:00:00Z","2012-03-01T17:30:00Z","2012-03-01T18:00:00Z","2012-03-01T18:30:00Z","2012-03-01T19:00:00Z","2012-03-01T19:30:00Z","2012-03-01T20:00:00Z","2012-03-01T20:30:00Z","2012-03-01T21:00:00Z","2012-03-01T21:30:00Z","2012-03-01T22:00:00Z","2012-03-01T22:30:00Z","2012-03-01T23:00:00Z","2012-03-01T23:30:00Z","2012-03-02T00:00:00Z","2012-03-02T00:30:00Z","2012-03-02T01:00:00Z","2012-03-02T01:30:00Z","2012-03-02T02:00:00Z","2012-03-02T02:30:00Z","2012-03-02T03:00:00Z","2012-03-02T03:30:00Z","2012-03-02T04:00:00Z","2012-03-02T04:30:00Z","2012-03-02T05:00:00Z","2012-03-02T05:30:00Z","2012-03-02T06:00:00Z","2012-03-02T06:30:00Z","2012-03-02T07:00:00Z","2012-03-02T07:30:00Z","2012-03-02T08:00:00Z","2012-03-02T08:30:00Z","2012-03-02T09:00:00Z","2012-03-02T09:30:00Z","2012-03-02T10:00:00Z","2012-03-02T10:30:00Z","2012-03-02T11:00:00Z","2012-03-02T11:30:00Z","2012-03-02T12:00:00Z","2012-03-02T12:30:00Z","2012-03-02T13:00:00Z","2012-03-02T13:30:00Z","2012-03-02T14:00:00Z","2012-03-02T14:30:00Z","2012-03-02T15:00:00Z","2012-03-02T15:30:00Z","2012-03-02T16:00:00Z","2012-03-02T16:30:00Z","2012-03-02T17:00:00Z","2012-03-02T17:30:00Z","2012-03-02T18:00:00Z","2012-03-02T18:30:00Z","2012-03-02T19:00:00Z","2012-03-02T19:30:00Z","2012-03-02T20:00:00Z","2012-03-02T20:30:00Z","2012-03-02T21:00:00Z","2012-03-02T21:30:00Z","2012-03-02T22:00:00Z","2012-03-02T22:30:00Z","2012-03-02T23:00:00Z","2012-03-02T23:30:00Z","2012-03-03T00:00:00Z","2012-03-03T00:30:00Z","2012-03-03T01:00:00Z","2012-03-03T01:30:00Z","2012-03-03T02:00:00Z","2012-03-03T02:30:00Z","2012-03-03T03:00:00Z","2012-03-03T03:30:00Z","2012-03-03T04:00:00Z","2012-03-03T04:30:00Z","2012-03-03T05:00:00Z","2012-03-03T05:30:00Z","2012-03-03T06:00:00Z","2012-03-03T06:30:00Z","2012-03-03T07:00:00Z","2012-03-03T07:30:00Z","2012-03-03T08:00:00Z","2012-03-03T08:30:00Z","2012-03-03T09:00:00Z","2012-03-03T09:30:00Z","2012-03-03T10:00:00Z","2012-03-03T10:30:00Z","2012-03-03T11:00:00Z","2012-03-03T11:30:00Z","2012-03-03T12:00:00Z","2012-03-03T12:30:00Z","2012-03-03T13:00:00Z","2012-03-03T13:30:00Z","2012-03-03T14:00:00Z","2012-03-03T14:30:00Z","2012-03-03T15:00:00Z","2012-03-03T15:30:00Z","2012-03-03T16:00:00Z","2012-03-03T16:30:00Z","2012-03-03T17:00:00Z","2012-03-03T17:30:00Z","2012-03-03T18:00:00Z","2012-03-03T18:30:00Z","2012-03-03T19:00:00Z","2012-03-03T19:30:00Z","2012-03-03T20:00:00Z","2012-03-03T20:30:00Z","2012-03-03T21:00:00Z","2012-03-03T21:30:00Z","2012-03-03T22:00:00Z","2012-03-03T22:30:00Z","2012-03-03T23:00:00Z","2012-03-03T23:30:00Z","2012-03-04T00:00:00Z","2012-03-04T00:30:00Z","2012-03-04T01:00:00Z","2012-03-04T01:30:00Z","2012-03-04T02:00:00Z","2012-03-04T02:30:00Z","2012-03-04T03:00:00Z","2012-03-04T03:30:00Z","2012-03-04T04:00:00Z","2012-03-04T04:30:00Z","2012-03-04T05:00:00Z","2012-03-04T05:30:00Z","2012-03-04T06:00:00Z","2012-03-04T06:30:00Z","2012-03-04T07:00:00Z","2012-03-04T07:30:00Z","2012-03-04T08:00:00Z","2012-03-04T08:30:00Z","2012-03-04T09:00:00Z","2012-03-04T09:30:00Z","2012-03-04T10:00:00Z","2012-03-04T10:30:00Z","2012-03-04T11:00:00Z","2012-03-04T11:30:00Z","2012-03-04T12:00:00Z","2012-03-04T12:30:00Z","2012-03-04T13:00:00Z","2012-03-04T13:30:00Z","2012-03-04T14:00:00Z","2012-03-04T14:30:00Z","2012-03-04T15:00:00Z","2012-03-04T15:30:00Z","2012-03-04T16:00:00Z","2012-03-04T16:30:00Z","2012-03-04T17:00:00Z","2012-03-04T17:30:00Z","2012-03-04T18:00:00Z","2012-03-04T18:30:00Z","2012-03-04T19:00:00Z","2012-03-04T19:30:00Z","2012-03-04T20:00:00Z","2012-03-04T20:30:00Z","2012-03-04T21:00:00Z","2012-03-04T21:30:00Z","2012-03-04T22:00:00Z","2012-03-04T22:30:00Z","2012-03-04T23:00:00Z","2012-03-04T23:30:00Z","2012-03-05T00:00:00Z","2012-03-05T00:30:00Z","2012-03-05T01:00:00Z","2012-03-05T01:30:00Z","2012-03-05T02:00:00Z","2012-03-05T02:30:00Z","2012-03-05T03:00:00Z","2012-03-05T03:30:00Z","2012-03-05T04:00:00Z","2012-03-05T04:30:00Z","2012-03-05T05:00:00Z","2012-03-05T05:30:00Z","2012-03-05T06:00:00Z","2012-03-05T06:30:00Z","2012-03-05T07:00:00Z","2012-03-05T07:30:00Z","2012-03-05T08:00:00Z","2012-03-05T08:30:00Z","2012-03-05T09:00:00Z","2012-03-05T09:30:00Z","2012-03-05T10:00:00Z","2012-03-05T10:30:00Z","2012-03-05T11:00:00Z","2012-03-05T11:30:00Z","2012-03-05T12:00:00Z","2012-03-05T12:30:00Z","2012-03-05T13:00:00Z","2012-03-05T13:30:00Z","2012-03-05T14:00:00Z","2012-03-05T14:30:00Z","2012-03-05T15:00:00Z","2012-03-05T15:30:00Z","2012-03-05T16:00:00Z","2012-03-05T16:30:00Z","2012-03-05T17:00:00Z","2012-03-05T17:30:00Z","2012-03-05T18:00:00Z","2012-03-05T18:30:00Z","2012-03-05T19:00:00Z","2012-03-05T19:30:00Z","2012-03-05T20:00:00Z","2012-03-05T20:30:00Z","2012-03-05T21:00:00Z","2012-03-05T21:30:00Z","2012-03-05T22:00:00Z","2012-03-05T22:30:00Z","2012-03-05T23:00:00Z","2012-03-05T23:30:00Z","2012-03-06T00:00:00Z","2012-03-06T00:30:00Z","2012-03-06T01:00:00Z","2012-03-06T01:30:00Z","2012-03-06T02:00:00Z","2012-03-06T02:30:00Z","2012-03-06T03:00:00Z","2012-03-06T03:30:00Z","2012-03-06T04:00:00Z","2012-03-06T04:30:00Z","2012-03-06T05:00:00Z","2012-03-06T05:30:00Z","2012-03-06T06:00:00Z","2012-03-06T06:30:00Z","2012-03-06T07:00:00Z","2012-03-06T07:30:00Z","2012-03-06T08:00:00Z","2012-03-06T08:30:00Z","2012-03-06T09:00:00Z","2012-03-06T09:30:00Z","2012-03-06T10:00:00Z","2012-03-06T10:30:00Z","2012-03-06T11:00:00Z","2012-03-06T11:30:00Z","2012-03-06T12:00:00Z","2012-03-06T12:30:00Z","2012-03-06T13:00:00Z","2012-03-06T13:30:00Z","2012-03-06T14:00:00Z","2012-03-06T14:30:00Z","2012-03-06T15:00:00Z","2012-03-06T15:30:00Z","2012-03-06T16:00:00Z","2012-03-06T16:30:00Z","2012-03-06T17:00:00Z","2012-03-06T17:30:00Z","2012-03-06T18:00:00Z","2012-03-06T18:30:00Z","2012-03-06T19:00:00Z","2012-03-06T19:30:00Z","2012-03-06T20:00:00Z","2012-03-06T20:30:00Z","2012-03-06T21:00:00Z","2012-03-06T21:30:00Z","2012-03-06T22:00:00Z","2012-03-06T22:30:00Z","2012-03-06T23:00:00Z","2012-03-06T23:30:00Z","2012-03-07T00:00:00Z","2012-03-07T00:30:00Z","2012-03-07T01:00:00Z","2012-03-07T01:30:00Z","2012-03-07T02:00:00Z","2012-03-07T02:30:00Z","2012-03-07T03:00:00Z","2012-03-07T03:30:00Z","2012-03-07T04:00:00Z","2012-03-07T04:30:00Z","2012-03-07T05:00:00Z","2012-03-07T05:30:00Z","2012-03-07T06:00:00Z","2012-03-07T06:30:00Z","2012-03-07T07:00:00Z","2012-03-07T07:30:00Z","2012-03-07T08:00:00Z","2012-03-07T08:30:00Z","2012-03-07T09:00:00Z","2012-03-07T09:30:00Z","2012-03-07T10:00:00Z","2012-03-07T10:30:00Z","2012-03-07T11:00:00Z","2012-03-07T11:30:00Z","2012-03-07T12:00:00Z","2012-03-07T12:30:00Z","2012-03-07T13:00:00Z","2012-03-07T13:30:00Z","2012-03-07T14:00:00Z","2012-03-07T14:30:00Z","2012-03-07T15:00:00Z","2012-03-07T15:30:00Z","2012-03-07T16:00:00Z","2012-03-07T16:30:00Z","2012-03-07T17:00:00Z","2012-03-07T17:30:00Z","2012-03-07T18:00:00Z","2012-03-07T18:30:00Z","2012-03-07T19:00:00Z","2012-03-07T19:30:00Z","2012-03-07T20:00:00Z","2012-03-07T20:30:00Z","2012-03-07T21:00:00Z","2012-03-07T21:30:00Z","2012-03-07T22:00:00Z","2012-03-07T22:30:00Z","2012-03-07T23:00:00Z","2012-03-07T23:30:00Z","2012-03-08T00:00:00Z","2012-03-08T00:30:00Z","2012-03-08T01:00:00Z","2012-03-08T01:30:00Z","2012-03-08T02:00:00Z","2012-03-08T02:30:00Z","2012-03-08T03:00:00Z","2012-03-08T03:30:00Z","2012-03-08T04:00:00Z","2012-03-08T04:30:00Z","2012-03-08T05:00:00Z","2012-03-08T05:30:00Z","2012-03-08T06:00:00Z","2012-03-08T06:30:00Z","2012-03-08T07:00:00Z","2012-03-08T07:30:00Z","2012-03-08T08:00:00Z","2012-03-08T08:30:00Z","2012-03-08T09:00:00Z","2012-03-08T09:30:00Z","2012-03-08T10:00:00Z","2012-03-08T10:30:00Z","2012-03-08T11:00:00Z","2012-03-08T11:30:00Z","2012-03-08T12:00:00Z","2012-03-08T12:30:00Z","2012-03-08T13:00:00Z","2012-03-08T13:30:00Z","2012-03-08T14:00:00Z","2012-03-08T14:30:00Z","2012-03-08T15:00:00Z","2012-03-08T15:30:00Z","2012-03-08T16:00:00Z","2012-03-08T16:30:00Z","2012-03-08T17:00:00Z","2012-03-08T17:30:00Z","2012-03-08T18:00:00Z","2012-03-08T18:30:00Z","2012-03-08T19:00:00Z","2012-03-08T19:30:00Z","2012-03-08T20:00:00Z","2012-03-08T20:30:00Z","2012-03-08T21:00:00Z","2012-03-08T21:30:00Z","2012-03-08T22:00:00Z","2012-03-08T22:30:00Z","2012-03-08T23:00:00Z","2012-03-08T23:30:00Z","2012-03-09T00:00:00Z","2012-03-09T00:30:00Z","2012-03-09T01:00:00Z","2012-03-09T01:30:00Z","2012-03-09T02:00:00Z","2012-03-09T02:30:00Z","2012-03-09T03:00:00Z","2012-03-09T03:30:00Z","2012-03-09T04:00:00Z","2012-03-09T04:30:00Z","2012-03-09T05:00:00Z","2012-03-09T05:30:00Z","2012-03-09T06:00:00Z","2012-03-09T06:30:00Z","2012-03-09T07:00:00Z","2012-03-09T07:30:00Z","2012-03-09T08:00:00Z","2012-03-09T08:30:00Z","2012-03-09T09:00:00Z","2012-03-09T09:30:00Z","2012-03-09T10:00:00Z","2012-03-09T10:30:00Z","2012-03-09T11:00:00Z","2012-03-09T11:30:00Z","2012-03-09T12:00:00Z","2012-03-09T12:30:00Z","2012-03-09T13:00:00Z","2012-03-09T13:30:00Z","2012-03-09T14:00:00Z","2012-03-09T14:30:00Z","2012-03-09T15:00:00Z","2012-03-09T15:30:00Z","2012-03-09T16:00:00Z","2012-03-09T16:30:00Z","2012-03-09T17:00:00Z","2012-03-09T17:30:00Z","2012-03-09T18:00:00Z","2012-03-09T18:30:00Z","2012-03-09T19:00:00Z","2012-03-09T19:30:00Z","2012-03-09T20:00:00Z","2012-03-09T20:30:00Z","2012-03-09T21:00:00Z","2012-03-09T21:30:00Z","2012-03-09T22:00:00Z","2012-03-09T22:30:00Z","2012-03-09T23:00:00Z","2012-03-09T23:30:00Z","2012-03-10T00:00:00Z","2012-03-10T00:30:00Z","2012-03-10T01:00:00Z","2012-03-10T01:30:00Z","2012-03-10T02:00:00Z","2012-03-10T02:30:00Z","2012-03-10T03:00:00Z","2012-03-10T03:30:00Z","2012-03-10T04:00:00Z","2012-03-10T04:30:00Z","2012-03-10T05:00:00Z","2012-03-10T05:30:00Z","2012-03-10T06:00:00Z","2012-03-10T06:30:00Z","2012-03-10T07:00:00Z","2012-03-10T07:30:00Z","2012-03-10T08:00:00Z","2012-03-10T08:30:00Z","2012-03-10T09:00:00Z","2012-03-10T09:30:00Z","2012-03-10T10:00:00Z","2012-03-10T10:30:00Z","2012-03-10T11:00:00Z","2012-03-10T11:30:00Z","2012-03-10T12:00:00Z","2012-03-10T12:30:00Z","2012-03-10T13:00:00Z","2012-03-10T13:30:00Z","2012-03-10T14:00:00Z","2012-03-10T14:30:00Z","2012-03-10T15:00:00Z","2012-03-10T15:30:00Z","2012-03-10T16:00:00Z","2012-03-10T16:30:00Z","2012-03-10T17:00:00Z","2012-03-10T17:30:00Z","2012-03-10T18:00:00Z","2012-03-10T18:30:00Z","2012-03-10T19:00:00Z","2012-03-10T19:30:00Z","2012-03-10T20:00:00Z","2012-03-10T20:30:00Z","2012-03-10T21:00:00Z","2012-03-10T21:30:00Z","2012-03-10T22:00:00Z","2012-03-10T22:30:00Z","2012-03-10T23:00:00Z","2012-03-10T23:30:00Z","2012-03-11T00:00:00Z","2012-03-11T00:30:00Z","2012-03-11T01:00:00Z","2012-03-11T01:30:00Z","2012-03-11T02:00:00Z","2012-03-11T02:30:00Z","2012-03-11T03:00:00Z","2012-03-11T03:30:00Z","2012-03-11T04:00:00Z","2012-03-11T04:30:00Z","2012-03-11T05:00:00Z","2012-03-11T05:30:00Z","2012-03-11T06:00:00Z","2012-03-11T06:30:00Z","2012-03-11T07:00:00Z","2012-03-11T07:30:00Z","2012-03-11T08:00:00Z","2012-03-11T08:30:00Z","2012-03-11T09:00:00Z","2012-03-11T09:30:00Z","2012-03-11T10:00:00Z","2012-03-11T10:30:00Z","2012-03-11T11:00:00Z","2012-03-11T11:30:00Z","2012-03-11T12:00:00Z","2012-03-11T12:30:00Z","2012-03-11T13:00:00Z","2012-03-11T13:30:00Z","2012-03-11T14:00:00Z","2012-03-11T14:30:00Z","2012-03-11T15:00:00Z","2012-03-11T15:30:00Z","2012-03-11T16:00:00Z","2012-03-11T16:30:00Z","2012-03-11T17:00:00Z","2012-03-11T17:30:00Z","2012-03-11T18:00:00Z","2012-03-11T18:30:00Z","2012-03-11T19:00:00Z","2012-03-11T19:30:00Z","2012-03-11T20:00:00Z","2012-03-11T20:30:00Z","2012-03-11T21:00:00Z","2012-03-11T21:30:00Z","2012-03-11T22:00:00Z","2012-03-11T22:30:00Z","2012-03-11T23:00:00Z","2012-03-11T23:30:00Z"],[1630.8752,1611.201,1657.1605,1653.0418,1702.3163,1648.4114,1645.5635,1698.4048,1743.0209,1695.5348,1707.3264,1705.9747,1743.1928,1716.1124,1695.0281,1716.8066,2348.9557,2474.5494,2716.5215,3448.259,2546.1375,2530.432,2920.7967,3612.3866,4340.5284,4507.1623,4633.7347,4706.3007,5158.577,5834.8887,4999.1508,5207.0161,4919.2831,4048.3027,4004.4922,3999.3961,3961.7111,3981.1979,3916.753,3973.4146,3997.1501,3955.6261,3960.8076,3906.081,3943.0109,3853.0698,3849.4963,3856.5072,3598.0664,3355.0794,2924.0033,2704.0346,2352.8494,2037.3473,1868.1192,1860.1816,1748.3676,1677.2737,1665.6009,1660.6378,1638.2591,1597.3354,1614.1049,1627.1606,1855.8744,2096.977,2082.1541,2264.2344,2324.2237,2475.8319,2478.879,3010.7483,3595.3856,3631.129,3432.7116,3834.1277,5076.3031,5592.3986,5274.024,4101.1732,3981.4822,3863.7599,3908.764,3947.3492,3942.1136,3810.4087,3845.4132,3859.635,3836.6911,3832.4501,3832.9676,3770.7882,3777.7938,3785.924,3713.1144,3680.6326,3552.3065,3105.22,2838.6917,2748.313,2394.9011,2149.3138,1962.4761,1851.6471,1786.5705,1741.0032,1699.3002,1724.7199,1655.7074,1652.2491,1730.2691,1731.5488,1852.556,2078.6337,2063.0313,2267.4713,2474.6717,2458.4805,2437.0928,2780.8493,3161.0278,3332.4802,3646.328,4449.7544,5245.0773,5425.2277,4319.762,4175.8418,4083.2191,4234.2906,4487.1249,4368.9253,4105.1608,4092.3061,4065.393,4223.0757,4133.9317,3929.4067,3930.5325,3930.8454,3905.9702,3849.9307,3813.7583,3718.1376,3456.9582,3175.3209,2895.5818,2777.1645,2636.1814,2550.6003,2241.2415,2061.9732,1782.4095,1920.9028,1762.4113,1730.1298,1696.1096,1653.0432,1682.1637,1684.8595,1908.4396,2088.5618,2380.2413,2392.7694,2247.677,2147.7398,2410.5703,2936.8138,3308.2186,3225.164,3509.0473,4374.7118,4206.6611,4483.6888,4588.0586,4505.7937,4434.5029,4361.4116,4340.9519,3933.756,3857.2679,3866.3665,3927.6512,3879.6892,3878.6336,3878.367,3906.2862,3940.7518,3868.8018,3788.8904,3726.0784,3610.2391,3411.0805,3149.8439,2811.1865,2689.6654,2421.111,2147.5688,1952.7157,1860.4215,1762.029,1724.6089,1721.7869,1716.5386,1695.6569,1633.2642,1658.3656,1651.3275,1810.5157,2033.4281,2127.4466,2146.892,2197.9924,2167.1197,2557.0698,2778.0764,2866.0369,3148.4971,3470.1839,3903.6258,3660.2657,4130.2718,3872.4977,3787.4021,3778.7982,3789.4866,3763.3064,3738.5668,3757.9212,3866.7041,3894.4752,3879.7599,3861.659,3881.37,3838.2127,3838.7326,3803.969,3767.9221,3689.8099,3635.335,3436.6781,3170.816,2746.0458,2637.922,2277.9401,2061.8414,1874.739,1800.37,1747.7552,1709.6637,1705.3777,1679.2485,1640.294,1643.3287,1650.7238,1626.5306,1806.9316,2029.2869,2121.6502,1581.41,1597.7524,2128.4169,2189.2551,2215.8331,2471.9959,2360.7618,2322.4399,2501.8526,2487.5391,2506.9568,2553.6802,2648.7735,2732.7232,2708.8979,2716.4896,2634.4557,2562.4152,2522.1519,2514.5181,2469.3136,2315.413,2223.4028,2072.9167,2057.6423,1936.7585,1738.3341,1691.7642,1671.8481,1700.6828,1650.8691,1677.9687,1660.2098,1650.0049,1665.2813,1652.7523,1634.2725,1633.7894,1611.1241,1598.3108,1607.3703,1576.2298,1560.4282,1566.5573,1559.0205,1702.9941,1984.8842,1838.1206,1562.188,1547.4921,1557.6075,1827.6983,1892.6698,1631.7523,1677.0426,1844.2166,1737.5257,1645.7986,1640.5369,1689.68,1728.5907,1696.7742,1669.9428,1671.1818,1690.6003,1725.3105,1666.9524,1673.1529,1672.0271,1718.1362,1626.0605,1616.969,1632.653,1604.7288,1588.5076,1640.5632,1639.5844,1615.55,1647.8858,1611.5975,1610.6009,1603.9048,1661.4874,1641.6899,1608.0317,1618.1619,1590.6766,1573.7938,1620.7177,1603.1866,1602.6278,1618.7809,1607.846,1829.3338,2108.4396,2321.5189,2281.6328,2444.5642,2648.9187,2582.0779,2887.9131,3269.7823,3398.963,3651.6184,4228.78,4077.0953,4978.2472,5045.0481,4176.792,4139.6792,4175.9052,4135.8135,4105.4895,4069.56,3997.4117,3946.3082,3907.3841,3943.045,3988.4379,4027.1852,4012.3096,4026.8768,3895.8485,3860.44,3782.4791,3548.74,3360.8763,2969.0948,2841.0881,2524.037,2158.2145,1995.8978,1916.5209,1833.783,1767.8779,1756.9488,1769.366,1731.983,1709.2056,1725.217,1732.9856,1929.6783,2190.4567,2433.0876,2643.1685,2500.4248,2401.2138,2732.522,3002.7629,3240.4545,3518.7985,3775.3051,4394.5922,4260.2256,5038.3176,4566.3284,4177.3376,4077.1837,4010.3748,3992.7904,3982.1423,3938.9589,3967.2815,3987.7543,3960.6394,3971.7212,3971.5548,3982.9459,3963.6043,3981.6572,3926.2192,3984.7768,3963.7997,3664.1221,3302.3995,2895.4707,2711.5817,2370.4208,2090.7572,1934.7665,1890.1622,1791.8606,1760.7503,1782.3542,1765.5407,1709.6361,1698.4601,1729.0317,1671.1174,1798.1186,2064.4228,1973.4321,2296.0324,2597.5509,2414.2691,2575.4875,2827.2113,3269.0973,3319.7481,3576.5598,4213.1617,4076.6753,4360.7554,4060.9716,3880.5216,3863.6823,3895.1449,3903.7878,3940.813,4071.0436,4094.9967,4181.8841,4096.7977,4089.6703,4032.1765,4018.0864,3991.8118,3974.1014,3878.8856,3857.4423,3579.0926,3506.5457,3260.1282,2831.114,2677.8919,2321.278,2052.0719,1901.558,1811.6888,1730.899,1674.5183,1711.0265,1682.4991,1646.4156,1579.059,1638.0314,1616.1404,1675.024,1622.4338,1888.9063,2126.6773,2141.0886,2073.2929,2174.5879,2605.3772,2911.1786,3121.3119,3288.1125,3884.855,3616.701,4130.0431,3960.8985,3878.1205,3951.5794,3944.0891,3946.7626,3951.643,3932.9702,3954.8035,4000.6016,4015.882,4031.8657,4074.1147,4014.6258,4047.8943,4014.4056,3909.5439,3813.8354,3742.0507,3551.6267,3375.1048,3009.4366,2812.8269,2360.8938,2105.4573,1870.6872,1822.3363,1747.3302,1694.4468,1687.1148,1661.6016,1615.1955,1617.1831,1614.9969,1612.9696,1617.1889,1566.0062,1640.0407,1901.0388,1949.4867,1963.9285,2060.0237,2334.1404,2630.0931,2676.1289,3014.5919,3475.2485,3362.2797,3472.8644,3692.7968,3651.2529,3630.3999,3708.4516,3751.2728,3742.7682,3765.0997,3746.1112,3759.0907,3728.9856,3730.1931,3687.9697,3681.9849,3668.3845,3685.7345,3577.6914,3564.5678,3549.0118,3332.707,3047.8015,2711.1932,2573.5851,2256.093,2044.0106,1912.9129,1849.149,1743.8538,1702.9448,1761.2025,1709.8613,1678.1744,1665.9788,1652.4483,1640.3571,1702.7575,1725.769,1632.4432,1729.835,1715.455,2329.3658,2405.3669,2355.5389,2295.0706,2270.7069,2448.5081,2577.9307,2478.0834,2490.8034,2573.3251,2622.2154,2778.3981,2729.3765,2698.8012,2646.2039,2618.2619,2642.2227,2591.3165,2476.6875,2329.8768,2271.5383,2126.2581,2117.3362,1913.3092,1762.857,1764.6878,1740.0429,1706.8833,1690.8104,1691.0764,1704.9521,1684.6397,1661.4673,1659.0838,1656.939,1695.3939,1645.0128,1663.5692,1656.7976,1666.4946,1620.5457,1630.9448,1647.6098,1805.1419,2035.4547,1945.3898,1657.7217,1637.2996,1893.2518,1727.0135,1800.3427,1678.2005,1661.9636,1715.9497,1735.0079,1741.6388,1791.3632,1761.9376,1783.3724,1805.1896,1783.939,1756.8993,1756.9202,1750.4033,1759.3238,1750.4815,1733.0281,1716.712,1739.8043,1751.1887,1772.5539,1725.1604,1723.5189,1705.7739,1679.5403]],"fixedtz":false,"tzone":"UTC"},"evals":["attrs.interactionModel"],"jsHooks":[]}</script>

We have two nice interactive options here: on the graph itself and panel under it for zooming. Ok, let's do now some more serious analysis.
 
There are possible to see two main seasonalities in plotted time series: daily and weekly. We have 48 measurements during the day and 7 days during the week so that will be our independent variables to model response variable - electricity load. Let's construct it:

{% highlight r %}
N <- nrow(data_r) # number of observations in the train set
window <- N / period # number of days in the train set
matrix_gam <- data.table(Load = data_r[, value],
                         Daily = rep(1:period, window),
                         Weekly = data_r[, week_num])
{% endhighlight %}
 
Here we are! Train our first **GAM** with function `gam`. Independent variables are modeled by smoothing function `s`, for daily seasonality cubic regression spline is used, for weekly seasonality, P-splines is used, a number of knots are logically set to the number of unique values. Let's do it.

{% highlight r %}
gam_1 <- gam(Load ~ s(Daily, bs = "cr", k = period) +
               s(Weekly, bs = "ps", k = 7),
             data = matrix_gam,
             family = gaussian)
{% endhighlight %}
 
Package `mgcv` have many advantages and nice features. First is its visualization capabilities. Let's try it:

{% highlight r %}
layout(matrix(1:2, nrow = 1))
plot(gam_1, shade = TRUE)
{% endhighlight %}

![plot of chunk unnamed-chunk-10](/images/unnamed-chunk-10-1.png)
 
That looks nice, right? We can see here the influence of variables to electricity load. In the left plot, the peak of the load is around 3 p.m. during the day. In the right plot, we can see that during weekends consumption logically decreases.
 
Let's use `summary` function to do the diagnostic of our first model.

{% highlight r %}
summary(gam_1)
{% endhighlight %}



{% highlight text %}
## 
## Family: gaussian 
## Link function: identity 
## 
## Formula:
## Load ~ s(Daily, bs = "cr", k = period) + s(Weekly, bs = "ps", 
##     k = 7)
## 
## Parametric coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2731.67      18.88   144.7   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##              edf Ref.df     F p-value    
## s(Daily)  10.159 12.688 119.8  <2e-16 ***
## s(Weekly)  5.311  5.758 130.3  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.772   Deviance explained = 77.7%
## GCV = 2.4554e+05  Scale est. = 2.3953e+05  n = 672
{% endhighlight %}
 
What to look at here? EDF: estimated degrees of freedom - can be interpreted like how much given variable is smoothed (higher EDF value implies more complex splines). P-values: statistical significance of given variable to response variable, tested by F-test (lower is better). \\( R^2 \\) - adjusted R-squared (higher is better). GCV: GCV score (mentioned above).
In the summary, we can see that the R-sq. (adj) value is a little bit low...
 
Let's plot fitted values:

{% highlight r %}
datas <- rbindlist(list(data_r[, .(value, date_time)],
                        data.table(value = gam_1$fitted.values,
                                   data_time = data_r[, date_time])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]
 
ggplot(data = datas, aes(date_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.1")
{% endhighlight %}

![plot of chunk unnamed-chunk-12](/images/unnamed-chunk-12-1.png)
 
That's, of course, horrible result. We need to include interactions of two independent variables to the model.
 
One more thing before it...what is hiding behind stored `gam_1` object and used optimizer?

{% highlight r %}
gam_1$optimizer
{% endhighlight %}



{% highlight text %}
## [1] "magic"
{% endhighlight %}
 
Here is that "magic"! :smile:
 
The first type of interactions will be the third one mentioned in the section about interactions, so one smoothing function to both variables is used. In the term `s(Daily, Weekly)` are also automatically included simple smoothing terms `s(Daily)` and `s(Weekly)`. Let's do it.
 

{% highlight r %}
gam_2 <- gam(Load ~ s(Daily, Weekly),
             data = matrix_gam,
             family = gaussian)
 
summary(gam_2)$r.sq
{% endhighlight %}



{% highlight text %}
## [1] 0.9352108
{% endhighlight %}
 
R-squared value suggests that result is much better. Look at the smooth term:

{% highlight r %}
summary(gam_2)$s.table
{% endhighlight %}



{% highlight text %}
##                     edf   Ref.df        F p-value
## s(Daily,Weekly) 28.7008 28.99423 334.2963       0
{% endhighlight %}
 
Seems good too. Plot of fitted values:

{% highlight r %}
datas <- rbindlist(list(data_r[, .(value, date_time)],
                        data.table(value = gam_2$fitted.values,
                                   data_time = data_r[, date_time])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]
 
ggplot(data = datas, aes(date_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.2")
{% endhighlight %}

![plot of chunk unnamed-chunk-16](/images/unnamed-chunk-16-1.png)
 
It's not bad, but it can be better. Now, let's try above mentioned tensor product interactions. That can be done by function `te`, basis functions can be defined as well.

{% highlight r %}
gam_3 <- gam(Load ~ te(Daily, Weekly,
                       bs = c("cr", "ps")),
             data = matrix_gam,
             family = gaussian)
 
summary(gam_3)$r.sq
{% endhighlight %}



{% highlight text %}
## [1] 0.9268452
{% endhighlight %}
 
Similar to the previous model `gam_2`. Look at the smooth term:

{% highlight r %}
summary(gam_3)$s.table
{% endhighlight %}



{% highlight text %}
##                       edf   Ref.df        F p-value
## te(Daily,Weekly) 23.65709 23.98741 354.5856       0
{% endhighlight %}
 
Again very similar results. Let's look at fitted values:

{% highlight r %}
datas <- rbindlist(list(data_r[, .(value, date_time)],
                        data.table(value = gam_3$fitted.values,
                                   data_time = data_r[, date_time])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]
 
ggplot(data = datas, aes(date_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.3")
{% endhighlight %}

![plot of chunk unnamed-chunk-19](/images/unnamed-chunk-19-1.png)
 
Just a little differences in comparison to the `gam_2` model, looks like with `te` fit is more smoothed. Are we something missing? Of course! Function `te` has a default for a number of knots \\( 5^d \\), where d is a number of dimensions (variables), which is in our case little bit small. Now, I will set a number of knots to the maximal possible value `k = c(period, 7)`, what means that upper boundary for EDF (Estimated Degrees of Freedom) will be 48*7 - 1 = 335.

{% highlight r %}
gam_4 <- gam(Load ~ te(Daily, Weekly,
                        k = c(period, 7),
                        bs = c("cr", "ps")),
              data = matrix_gam,
              family = gaussian)
 
summary(gam_4)$r.sq
{% endhighlight %}



{% highlight text %}
## [1] 0.9727604
{% endhighlight %}



{% highlight r %}
summary(gam_4)$sp.criterion
{% endhighlight %}



{% highlight text %}
##   GCV.Cp 
## 34839.46
{% endhighlight %}



{% highlight r %}
summary(gam_4)$s.table
{% endhighlight %}



{% highlight text %}
##                       edf   Ref.df        F p-value
## te(Daily,Weekly) 119.4117 149.6528 160.2065       0
{% endhighlight %}
 
We can see here that R-squared jumped little bit up and edf value has increased five times (!) in comparison to the model `gam_3`.
Let's plot fitted values:

{% highlight r %}
datas <- rbindlist(list(data_r[, .(value, date_time)],
                        data.table(value = gam_4$fitted.values,
                                   data_time = data_r[, date_time])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]
 
ggplot(data = datas, aes(date_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.4")
{% endhighlight %}

![plot of chunk unnamed-chunk-21](/images/unnamed-chunk-21-1.png)
 
This seems much better than it was with model `gam_3`.
 
Now I will prove my statement about the upper boundary for EDF. There is possibility to fix number of smooth basis (i.e. EDF) in smooth terms. Just use an argument `fx = TRUE`.

{% highlight r %}
gam_4_fx <- gam(Load ~ te(Daily, Weekly,
                        k = c(period, 7),
                        bs = c("cr", "ps"),
                        fx = TRUE),
              data = matrix_gam,
              family = gaussian)
 
summary(gam_4_fx)$r.sq
{% endhighlight %}



{% highlight text %}
## [1] 0.965618
{% endhighlight %}



{% highlight r %}
summary(gam_4_fx)$s.table
{% endhighlight %}



{% highlight text %}
##                  edf Ref.df        F p-value
## te(Daily,Weekly) 335    335 57.25389       0
{% endhighlight %}
 
EDF = 335, here we are. We can see that R-squared is lower than with the model `gam_4`, it is due to that 335 is too high and we [overfitted](https://en.wikipedia.org/wiki/Overfitting) the model. It is prove to that GCV procedure is working properly. You can read more about the optimal setting (choosing) of `k` argument at [`?choose.k`](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/choose.k.html) and of course in the book from Simon Wood.
 
With package `mgcv` we have two more opportunities, methods, how to include tensor product interactions term - with functions `ti` and `t2`. `ti` produces a tensor product interaction, appropriate when the main effects (and any lower interactions) are also present, while `te` produces a full tensor product smooth. `t2` is an alternative function to `te` and uses different penalization method. You can read more about it in a documentation of the package `mgcv`: use `?ti` and `?t2` to see more.
 
So, let's try both methods in the our case (model). First, let's use `ti`:

{% highlight r %}
gam_5 <- gam(Load ~ s(Daily, bs = "cr", k = period) +
                    s(Weekly, bs = "ps", k = 7) +
                    ti(Daily, Weekly,
                       k = c(period, 7),
                       bs = c("cr", "ps")),
             data = matrix_gam,
             family = gaussian)
 
summary(gam_5)$r.sq
{% endhighlight %}



{% highlight text %}
## [1] 0.9717469
{% endhighlight %}



{% highlight r %}
summary(gam_5)$sp.criterion
{% endhighlight %}



{% highlight text %}
##   GCV.Cp 
## 35772.35
{% endhighlight %}



{% highlight r %}
summary(gam_5)$s.table
{% endhighlight %}



{% highlight text %}
##                        edf     Ref.df          F p-value
## s(Daily)         22.583649  27.964970  444.19962       0
## s(Weekly)         5.914531   5.995934 1014.72482       0
## ti(Daily,Weekly) 85.310314 110.828814   41.22288       0
{% endhighlight %}
 
Then let's use `t2`. I set argument `full = TRUE` because it gives strict invariance to penalties.

{% highlight r %}
gam_6 <- gam(Load ~ t2(Daily, Weekly,
                       k = c(period, 7),
                       bs = c("cr", "ps"),
                       full = TRUE),
             data = matrix_gam,
             family = gaussian)
 
summary(gam_6)$r.sq
{% endhighlight %}



{% highlight text %}
## [1] 0.9738273
{% endhighlight %}



{% highlight r %}
summary(gam_6)$sp.criterion
{% endhighlight %}



{% highlight text %}
##   GCV.Cp 
## 32230.68
{% endhighlight %}



{% highlight r %}
summary(gam_6)$s.table
{% endhighlight %}



{% highlight text %}
##                       edf   Ref.df       F p-value
## t2(Daily,Weekly) 98.12005 120.2345 84.4022       0
{% endhighlight %}
 
I printed also the GCV score value for the last three models, which is also a good criterion to choose optimal model among a set of fitted models. We can see that with `t2` term and corresponding model `gam_6` is GCV value lowest. It may be indicating that `gam_6` is our best model so far.
 
Other model selection criterion, which is widely used in statistics, is AIC ([Akaike Information Criterion](https://en.wikipedia.org/wiki/Akaike_information_criterion)). Criterion has equation \\( AIC = 2k - 2\ln (\hat{L}) \\), where k is the number of parameters to be estimated and \\(\hat{L}\\) is the maximized value of the likelihood function of the model. So lower values are better for AIC. Let's look at them for our three models:

{% highlight r %}
AIC(gam_4, gam_5, gam_6)
{% endhighlight %}



{% highlight text %}
##             df      AIC
## gam_4 121.4117 8912.611
## gam_5 115.8085 8932.746
## gam_6 100.1200 8868.628
{% endhighlight %}
 
The lowest value is in the `gam_6` model, so again it's winning model. Let's again look at fitted values to be sure that everything is OK.

{% highlight r %}
datas <- rbindlist(list(data_r[, .(value, date_time)],
                        data.table(value = gam_6$fitted.values,
                                   data_time = data_r[, date_time])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]
 
ggplot(data = datas, aes(date_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.6")
{% endhighlight %}

![plot of chunk unnamed-chunk-26](/images/unnamed-chunk-26-1.png)
 
Seems OK. We can see that fitted values for models `gam_4` and `gam_6` are very similar. It's very difficult to choose which model is better. It's possible to use more visualization and model diagnostic capabilities of package `mgcv` to compare these two models.
 
The first one is function `gam.check`, which makes four plots: QQ-plot of residuals, linear predictor vs. residuals, the histogram of residuals and the plot of fitted values vs. response. Let's make them for models `gam_4` and `gam_6`.

{% highlight r %}
gam.check(gam_4)
{% endhighlight %}

![plot of chunk unnamed-chunk-27](/images/unnamed-chunk-27-1.png)

{% highlight text %}
## 
## Method: GCV   Optimizer: magic
## Smoothing parameter selection converged after 7 iterations.
## The RMS GCV score gradiant at convergence was 0.2833304 .
## The Hessian was positive definite.
## The estimated model rank was 336 (maximum possible: 336)
## Model rank =  336 / 336 
## 
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
## 
##                      k'    edf k-index p-value
## te(Daily,Weekly) 335.00 119.41    1.23       1
{% endhighlight %}
 

{% highlight r %}
gam.check(gam_6)
{% endhighlight %}

![plot of chunk unnamed-chunk-28](/images/unnamed-chunk-28-1.png)

{% highlight text %}
## 
## Method: GCV   Optimizer: magic
## Smoothing parameter selection converged after 9 iterations.
## The RMS GCV score gradiant at convergence was 0.05208856 .
## The Hessian was positive definite.
## The estimated model rank was 336 (maximum possible: 336)
## Model rank =  336 / 336 
## 
## Basis dimension (k) checking results. Low p-value (k-index<1) may
## indicate that k is too low, especially if edf is close to k'.
## 
##                      k'    edf k-index p-value
## t2(Daily,Weekly) 335.00  98.12    1.16       1
{% endhighlight %}
 
The function `gam.check` makes also output to the console of more useful information. We can see again that models are very similar, just in histograms can be seen some differences, but it's also insignificant.
 
We didn't use so far default `plot` function to `gam` object for models with tensor product interactions. Let's use it and explore what it brings us.

{% highlight r %}
layout(matrix(1:2, nrow = 1))
plot(gam_4, rug = FALSE, se = FALSE, n2 = 80, main = "gam n.4 with te()")
plot(gam_6, rug = FALSE, se = FALSE, n2 = 80, main = "gam n.6 with t2()")
{% endhighlight %}

![plot of chunk unnamed-chunk-29](/images/unnamed-chunk-29-1.png)
 
It's nice contour line plot. On axes are our independent variables, contour lines and corresponding numbers on them represents effects of ind. variables to the response variable. Now is possible to see some little differences. The model `gam_6` with `t2` have more "wavy" contours. So it implies that it more adapts to response variable and smoothing factor is lower. In the next parts of the post, the model `gam_6` will be used for the analysis.
 
Another great feature of the package `mgcv` is the plotting function `vis.gam`. It makes 3D view (or 2D) of surface of fitted values according to independent variables. Let's look at it.

{% highlight r %}
vis.gam(gam_6, theta = 35, phi = 32, ticktype = "detailed",
        color = "topo", n.grid = 50, main = "t2(D, W)", zlab = "")
{% endhighlight %}

![plot of chunk unnamed-chunk-30](/images/unnamed-chunk-30-1.png)
 
We can see that highest peak is when the Daily variable has values near 30 and the Weekly variable has value 1 (it is a Monday).
 
2D view, contour lines type of plot, can be visualized too. Just set argument `plot.type = "contour"`.

{% highlight r %}
vis.gam(gam_6, plot.type = "contour", color = "terrain",
        lwd = 2, main = "t2(D, W)")
{% endhighlight %}

![plot of chunk unnamed-chunk-31](/images/unnamed-chunk-31-1.png)
 
Again we can see that highest value of electricity load is on Monday at 3:00 pm, it is very similar till Thursday, then load decreasing (weekends).
 
Another interesting contribution to the **time series regression model** can be an inclusion of [**autoregressive model**](https://en.wikipedia.org/wiki/Autoregressive_model) for serially correlated errors. Let's look at it.
 
### Autoregressive models and GAMS
 
**GAM** or **MLR** have assumptions in a model that errors (residuals) are identically and independently distributed (i.i.d.). In the case of the time series regression, it is very strong assumption, which is here logically not fulfilled. Present time series values are highly correlated with past values, so errors of the model will be correlated too. This phenomenon is called an [autocorrelation](https://en.wikipedia.org/wiki/Autocorrelation). This implies that estimated regression coefficients and residuals of a model might be negatively biased, which also implies that previously computed p-values of statistical tests or confidence intervals are wrong.
 
How can be this situation handled? By inclusion of autoregressive model (AR) for errors in our model. So we have the
model with the term for errors like this:
 
$$y_i = \beta\mathbf{X} + \varepsilon_i, \hspace{0.2cm} \varepsilon_i = \phi\varepsilon_{i-1} + v_i,$$
 
where the second equation is a classical AR(1) process and \\( \phi \\) is an unknown autoregressive coefficient to be estimated. Errors can be also nested within a week, which is in our case more appropriate, because of the double seasonal character of our time series. You can read more about estimation and modeling of this kind of models in an excellent book by [Box, Jenkins, and Reinsel: Time Series Analysis](http://onlinelibrary.wiley.com/book/10.1002/9781118619193).
 
It's possible to add correlation term for errors with function `gamm`, which stands for **GAM** mixture models. It calls `lme` function from package `nlme`. Now, train basic model with function `gamm` and a another model with AR(1) process nested within a week - just add this argument to `gamm` function: `correlation = corARMA(form = ~ 1|Weekly, p = 1)`.
 

{% highlight r %}
gam_6_ar0 <- gamm(Load ~ t2(Daily, Weekly,
                            k = c(period, 7),
                            bs = c("cr", "ps"),
                            full = TRUE),
                  data = matrix_gam,
                  family = gaussian,
                  method = "REML")
 
gam_6_ar1 <- gamm(Load ~ t2(Daily, Weekly,
                            k = c(period, 7),
                            bs = c("cr", "ps"),
                            full = TRUE),
             data = matrix_gam,
             family = gaussian,
             correlation = corARMA(form = ~ 1|Weekly, p = 1),
             method = "REML")
{% endhighlight %}
 
 
We can use an [ANOVA](https://en.wikipedia.org/wiki/Analysis_of_variance) to compare these two models and pick the right one.

{% highlight r %}
anova(gam_6_ar0$lme, gam_6_ar1$lme)
{% endhighlight %}



{% highlight text %}
##               Model df      AIC      BIC    logLik   Test  L.Ratio p-value
## gam_6_ar0$lme     1 10 9070.525 9115.568 -4525.263                        
## gam_6_ar1$lme     2 11 8751.361 8800.908 -4364.680 1 vs 2 321.1644  <.0001
{% endhighlight %}
 
AIC value of the model with AR(1) term is lower, so seems better, also p-value is very low which indicates that second model is better to choose.
 
Estimated \\( \phi \\) coefficient of AR(1) process can be seen here:

{% highlight r %}
intervals(gam_6_ar1$lme, which = "var-cov")$corStruct
{% endhighlight %}



{% highlight text %}
##         lower      est.     upper
## Phi 0.6363059 0.7107914 0.7721463
## attr(,"label")
## [1] "Correlation structure:"
{% endhighlight %}
 
Value 0.71 is pretty high, which indicates a strong dependency on previous values of errors (lags).
 
Let's look now on another diagnostic for these two models. Plot of values of a [partial autocorrelation function](https://en.wikipedia.org/wiki/Partial_autocorrelation_function):

{% highlight r %}
layout(matrix(1:2, ncol = 2))
pacf(resid(gam_6_ar0$lme), lag.max = 48, main = "pACF of gam n.6")
pacf(resid(gam_6_ar1$lme), lag.max = 48, main = "pACF of gam n.6 with AR(1)")
{% endhighlight %}

![plot of chunk unnamed-chunk-35](/images/unnamed-chunk-35-1.png)
 
Am I blind, or I can't see a significant difference between these two plots and corresponding values of pACF. Optimal values of pACF should be under dashed blue lines, so it isn't this scenario.
 
Let's make another useful visualization of residuals of two models to confirm uncertainty about correctness of model with AR(1).

{% highlight r %}
datas <- data.table(Fitted_values = c(gam_6_ar0$gam$fitted.values,
                                      gam_6_ar1$gam$fitted.values),
                    Residuals = c(gam_6_ar0$gam$residuals,
                                  gam_6_ar1$gam$residuals),
                    Model = rep(c("Gam n.6", "Gam n.6 with AR(1)"), each = nrow(data_r)))
 
ggplot(data = datas,
       aes(Fitted_values, Residuals)) +
  facet_grid(Model~., switch = "y") +
  geom_point(size = 1.7) +
  geom_smooth(method = "loess") +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 13, face = "bold"),
        strip.background = element_rect(color = "black")) +
  labs(title = "Fitted values vs Residuals of two models")
{% endhighlight %}

![plot of chunk unnamed-chunk-36](/images/unnamed-chunk-36-1.png)
 
Here we are. The model with an AR(1) process has residuals at low fitted values somehow correlated, which is not good. What now? I have to be honest, I can't explain why this interesting thing happened. So, I would be very happy when someone guides me to the right direction and writes some notes to the discussion, it would really help. One more note about the inclusion of AR(1) model to **GAM** or **MLR**. It didn't help in the meaning of forecast accuracy, I made a lot of experiments, but MAPE was higher than with models without AR(1). 
 
### Animations and conclusions
 
I will end this post with a visualization analysis of the model `gam_6`. I will try to do some animated dashboards of main characteristics of the model thru time. It should be very interesting how electricity load changes and how the model can adapt to these changes. In the first two animations you can see three plots on the one figure:
 
1. Fitted values by GAM and real response values of electricity consumption.
2. Fitted values vs. residuals of GAM.
3. Forecast for one week ahead vs. real consumption plus forecast accuracy in MAPE.
 
Animations were created by functions from package `grid` to layout ggplots to one figure, and function `saveGIF` from package `animation` to create final GIF. Here there are:
 
![](/images/industry_1_dashboard.gif)
 
![](/images/industry_4_dashboard.gif)
 
We can see the behavior of **GAMs** on two times series from two different types of industries: commercial property and light industrial. Notice (look), that fitted values and forecasts are smoother than where were with [Multiple linear regression from the previous post](https://petolau.github.io/Forecast-double-seasonal-time-series-with-multiple-linear-regression-in-R/). It's good information to know, it implies that our model isn't overfitted. On the other hand, I also compared forecast performance of **GAM** and **MLR**, but GAM was slightly worse than MLR, which can be a little bit surprising for somebody. Seems that usage of penalized least squares (GAM) for forecasting time series doesn't bring improvement against classical OLS (MLR). But there are other advantages, which I will point out in the end.
 
Another animation, which I created, is a 3D visualization of fitted values like a surface. As I showed you before, it can be done very easily by function `vis.gam`.
 
![](/images/industry_1_vis_3D.gif)
 
We can compare it with the first dashboard because again it's time series from commercial properties. EDF is also printed to the title of GIF for reasons to see how it changes depending on the behavior of electricity consumption.
 
With this, I would like to end main part of this tutorial and conclude it with some remarks.
 
In the beginning of the post, motivation and the theory behind GAM method was introduced. Next, the analytical (**magical**) part continued with explaining various features of the package `mgcv`. Different types of **interactions** were showed and analyzed. Next, consideration about the correctness of an autoregressive model for errors was discussed. In the end, I tried to make a big view of a behavior of **GAMs** by animations of its performance on **double seasonal time series**.
 
Everything that was said implies these advantages and disadvantages of using **GAM** for your problem:
 
Advantages:
 
* Many possibilities how to model your independent variables (many types of smooth functions).
* Model response variable by all known distribution families (Normal, Gamma, Poisson etc.).
* Define interactions by tensor product, which means usage of different basis functions for interacted variables.
* Test statistical significance of a non-linear relationship of the independent variable to dependent.
 
Disadvantages:
 
* Complex method, difficult to learn.
* It's sometimes difficult to interpret results.
* Several parameters to tune, which have to be set carefully.
 
In the future post, I want to focus on some machine learning method like Support Vector Regression or Regression Trees with alongside of grid search for tuning parameters.
 
The script for a creation of the whole tutorial is located on [my GitHub repository](https://github.com/PetoLau/petolau.github.io/tree/master/_Rscripts). I would like to encourage you to add any feedback to the discussion below, thank you for reading this long post.
