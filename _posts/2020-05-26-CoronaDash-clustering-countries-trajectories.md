---
title: "CoronaDash app use case - Clustering countries' COVID-19 active cases trajectories"
author: "Peter Laurinec"
layout: post
published: true
status: publish
tags: test clustering shiny opendata
draft: no
---
 

 
COVID-19 disease spread hit the World really globally and also field of mathematicians/ statisticians/ machine learning researchers and related.
These experts want to help to understand for example future trends (forecast) of corona virus spread.
My motivation in this case was to create **interactive dashboard** about COVID-19 to inform about various scenarios in every country in the World through **data mining methods**.
 
I created **CoronaDash** `shinydashboard` application that is hosted on [**petolau.shinyapps.io** RStudio platform](https://petolau.shinyapps.io/coronadash/).
The dashboard provides various data mining/ visualization techniques for **comparing countries' COVID-19 data statistics** as:
 
  * extrapolating total confirmed cases by exponential smoothing model,
  * trajectories of cases/ deaths spread,
  * multidimensional clustering of countries' data/ statistics - with dendogram and table of clusters averages,
  * aggregated views for the whole World,
  * hierarchical clustering of countries' trajectories based on DTW distance and preprocessing by SMA (+ normalization), for fast comparison of large number of countries' COVID-19 magnitudes and trends.
 
The blog post will be about the last bullet of the above list - **clustering of countries' trajectories**.
This use case is challenging because of **clustering time series with different lengths**.
 
#### CovidR contest
 
I submitted my shiny application also to interesting initiative of eRum 2020 organizers - [**CovidR Contest**](https://milano-r.github.io/erum2020-covidr-contest/index.html).
If you like my contribution, please vote for me at [**erum2020-covidr-contest/laurinec-CoronaDash**](https://milano-r.github.io/erum2020-covidr-contest/laurinec-CoronaDash.html) gallery post :)
 
## Preprocessing of COVID-19 open-data
 
Firstly, load all the needed packages.

{% highlight r %}
library(data.table) # data handling
library(TSrepr) # time series representation, pls use dev version devtools::install_github("PetoLau/TSrepr")
library(ggplot2) # visualisations
library(dtwclust) # clustering using DTW distance
library(dygraphs) # interactive visualizations
library(ggrepel) # nice labels in ggplot
library(dendextend) # dendrograms
library(DT) # nice datatable
{% endhighlight %}
 
Data are coming from [Johns Hopkins CSSE GitHub repository](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series), [GitHub repository by ulklc](https://github.com/ulklc/covid19-timeseries), and tests data are coming from [COVID19 API](https://github.com/ChrisMichaelPerezSantiago/covid19).
 
Read prepared data at *2020-05-24* snapshot.

{% highlight r %}
data_covid_ts <- fread("data_covid_time_series_2020-05-24.csv")
{% endhighlight %}



{% highlight text %}
## Error in fread("data_covid_time_series_2020-05-24.csv"): File 'data_covid_time_series_2020-05-24.csv' does not exist or is non-readable. getwd()=='C:/Users/PeterLaurinec/Downloads/petolau.github.io'
{% endhighlight %}



{% highlight r %}
data_covid_ts[, DateRep := as.Date(DateRep)] # transform character to Date class
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'data_covid_ts' not found
{% endhighlight %}



{% highlight r %}
# what we got
str(data_covid_ts)
{% endhighlight %}



{% highlight text %}
## Error in str(data_covid_ts): object 'data_covid_ts' not found
{% endhighlight %}
 
Different interesting statistics also computed "per 1 million population" for better comparison.
 
Transform data for 'since first 100th case' countries trajectories with same lengths:

{% highlight r %}
n_cases <- 100 # you can vary
statistic <- 'Total active cases per 1 million population'
 
# Cases greater than threshold
data_covid_100_cases <- copy(data_covid_ts[,
                                           .SD[DateRep >= .SD[Cases_cumsum >= n_cases,
                                                              min(DateRep,
                                                                  na.rm = T)]],
                                            by = .(Country)])
{% endhighlight %}



{% highlight text %}
## Error in copy(data_covid_ts[, .SD[DateRep >= .SD[Cases_cumsum >= n_cases, : object 'data_covid_ts' not found
{% endhighlight %}



{% highlight r %}
setorder(data_covid_100_cases,
         Country,
         DateRep)
{% endhighlight %}



{% highlight text %}
## Error in is.data.frame(x): object 'data_covid_100_cases' not found
{% endhighlight %}



{% highlight r %}
# Create 'days since' column
data_covid_100_cases[, (paste0("Days_since_first_",
                               n_cases, "_case")) := 1:.N,
                     by = .(Country)]
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'data_covid_100_cases' not found
{% endhighlight %}



{% highlight r %}
# top N countries selection for analysis
data_cases_order <- copy(data_covid_100_cases[,
                                      .SD[DateRep == max(DateRep),
                                          get(statistic)],
                                      by = .(Country)
                                      ])
{% endhighlight %}



{% highlight text %}
## Error in copy(data_covid_100_cases[, .SD[DateRep == max(DateRep), get(statistic)], : object 'data_covid_100_cases' not found
{% endhighlight %}



{% highlight r %}
setnames(data_cases_order, "V1", statistic)
{% endhighlight %}



{% highlight text %}
## Error in is.data.frame(x): object 'data_cases_order' not found
{% endhighlight %}



{% highlight r %}
setorderv(data_cases_order, statistic, -1)
{% endhighlight %}



{% highlight text %}
## Error in is.data.frame(x): object 'data_cases_order' not found
{% endhighlight %}



{% highlight r %}
# subset data based on selected parameter -  top 82 countries and analyzed columns
data_covid_cases_sub <- copy(data_covid_100_cases[.(c(data_cases_order[1:82][!is.na(Country), Country],
                                                      "Slovakia")),
                                                  on = .(Country),
                                                 .SD,
                                                 .SDcols = c("Country",
                                                             paste0("Days_since_first_", n_cases, "_case"),
                                                             statistic)
                                                 ])
{% endhighlight %}



{% highlight text %}
## Error in copy(data_covid_100_cases[.(c(data_cases_order[1:82][!is.na(Country), : object 'data_covid_100_cases' not found
{% endhighlight %}



{% highlight r %}
# Make same length time series from countries data
data_covid_trajectories <- dcast(data_covid_cases_sub,
                                 get(paste0("Days_since_first_", n_cases, "_case")) ~ Country,
                                 value.var = statistic)
{% endhighlight %}



{% highlight text %}
## Error in is.data.table(data): object 'data_covid_cases_sub' not found
{% endhighlight %}



{% highlight r %}
setnames(data_covid_trajectories,
         colnames(data_covid_trajectories)[1],
         paste0("Days_since_first_", n_cases, "_case"))
{% endhighlight %}



{% highlight text %}
## Error in is.data.frame(x): object 'data_covid_trajectories' not found
{% endhighlight %}



{% highlight r %}
DT::datatable(data_covid_trajectories,
              class = "compact",
              extensions = 'Scroller',
              options = list(
                dom = 't',
                deferRender = TRUE,
                scrollY = 270,
                scroller = TRUE,
                scrollX = TRUE
              ))
{% endhighlight %}



{% highlight text %}
## Error in crosstalk::is.SharedData(data): object 'data_covid_trajectories' not found
{% endhighlight %}
 
Prepare trajectories' data for clustering:

{% highlight r %}
q_sma <- 3
 
# save stats of dt
n_col <- ncol(data_covid_trajectories)
{% endhighlight %}



{% highlight text %}
## Error in ncol(data_covid_trajectories): object 'data_covid_trajectories' not found
{% endhighlight %}



{% highlight r %}
n_row <- nrow(data_covid_trajectories)
{% endhighlight %}



{% highlight text %}
## Error in nrow(data_covid_trajectories): object 'data_covid_trajectories' not found
{% endhighlight %}



{% highlight r %}
n_row_na <- rowSums(data_covid_trajectories[, lapply(.SD, is.na)])
{% endhighlight %}



{% highlight text %}
## Error in is.data.frame(x): object 'data_covid_trajectories' not found
{% endhighlight %}



{% highlight r %}
n_col_na <- colSums(data_covid_trajectories[, lapply(.SD, is.na)])
{% endhighlight %}



{% highlight text %}
## Error in is.data.frame(x): object 'data_covid_trajectories' not found
{% endhighlight %}



{% highlight r %}
# remove all NA rows and cols - for sure
if (length(which(n_row_na %in% n_col)) != 0) {
 
  data_covid_trajectories <- copy(data_covid_trajectories[-which(n_row_na == n_col)])
      
}
{% endhighlight %}



{% highlight text %}
## Error in n_row_na %in% n_col: object 'n_row_na' not found
{% endhighlight %}



{% highlight r %}
if (length(which(n_col_na %in% n_row)) != 0) {
      
  data_covid_trajectories <- copy(data_covid_trajectories[, -which(n_col_na %in% n_row), with = FALSE])
      
}
{% endhighlight %}



{% highlight text %}
## Error in n_col_na %in% n_row: object 'n_col_na' not found
{% endhighlight %}



{% highlight r %}
# use SMA for preprocessing time series from my TSrepr package
data_covid_trajectories[,
                        (colnames(data_covid_trajectories)[-1]) := lapply(.SD, function(i)
                        c(rep(NA, q_sma - 1),
                        ceiling(repr_sma(i, q_sma))
                          )),
                        .SDcols = colnames(data_covid_trajectories)[-1]]
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'data_covid_trajectories' not found
{% endhighlight %}



{% highlight r %}
DT::datatable(data_covid_trajectories,
              class = "compact",
              extensions = 'Scroller',
              options = list(
                dom = 't',
                deferRender = TRUE,
                scrollY = 270,
                scroller = TRUE,
                scrollX = TRUE
              ))
{% endhighlight %}



{% highlight text %}
## Error in crosstalk::is.SharedData(data): object 'data_covid_trajectories' not found
{% endhighlight %}
 
 
Define clustering function with DTW distance with additional data preprocessing necessary for dtwclust package.

{% highlight r %}
cluster_trajectories <- function(data, k, normalize = FALSE) {
  
  # transpose data for clustering
  data_trajectories_trans <- t(data[, .SD,
                                    .SDcols = colnames(data)[-1]])
  
  data_trajectories_trans_list <- lapply(1:nrow(data_trajectories_trans), function(i)
    na.omit(data_trajectories_trans[i,]))
  names(data_trajectories_trans_list) <- colnames(data)[-1]
 
  n_list <- sapply(1:length(data_trajectories_trans_list), function(i)
    length(data_trajectories_trans_list[[i]]))
  names(n_list) <- names(data_trajectories_trans_list)
 
  if (length(which(n_list %in% 0:1)) != 0) {
    
    data_trajectories_trans_list <- data_trajectories_trans_list[-which(n_list %in% 0:1)]
    
  }
  
  list_names <- names(data_trajectories_trans_list)
 
  # normalization
  if (normalize) {
    
    data_trajectories_trans_list <- lapply(names(data_trajectories_trans_list),
                                                function(i)
                                                  norm_z(data_trajectories_trans_list[[i]])
                                                 )
    names(data_trajectories_trans_list) <- list_names
    
  }
 
  hc_res <- tsclust(data_trajectories_trans_list,
                    type = "hierarchical",
                    k = k,
                    distance = "dtw_basic", # "dtw", "dtw_basic", "dtw2", "sdtw"
                    centroid = dba, # dba, sdtw_cent
                    trace = FALSE,
                    seed = 54321,
                    control = hierarchical_control(method = "ward.D2"),
                    args = tsclust_args(dist = list(norm = "L2"))
                    )
  
  return(hc_res)
  
}
{% endhighlight %}
 
Execute clustering

{% highlight r %}
clust_res <- cluster_trajectories(data = data_covid_trajectories,
                                  k = 14,
                                  normalize = TRUE)
{% endhighlight %}



{% highlight text %}
## Error in t(data[, .SD, .SDcols = colnames(data)[-1]]): object 'data_covid_trajectories' not found
{% endhighlight %}



{% highlight r %}
clust_res
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'clust_res' not found
{% endhighlight %}
 
Prepare data for plotting:

{% highlight r %}
# prepare time series
data_clust_id <- data.table(Cluster = clust_res@cluster,
                            Country = names(clust_res@cluster))
{% endhighlight %}



{% highlight text %}
## Error in data.table(Cluster = clust_res@cluster, Country = names(clust_res@cluster)): object 'clust_res' not found
{% endhighlight %}



{% highlight r %}
data_plot <- melt(data_covid_trajectories,
                  id.vars = colnames(data_covid_trajectories)[1],
                  variable.name = "Country",
                  variable.factor = FALSE,
                  value.name = statistic,
                  value.factor = FALSE
                  )
{% endhighlight %}



{% highlight text %}
## Error in is.data.table(data): object 'data_covid_trajectories' not found
{% endhighlight %}



{% highlight r %}
data_plot <- copy(data_plot[.(data_clust_id$Country), on = .(Country)])
{% endhighlight %}



{% highlight text %}
## Error in copy(data_plot[.(data_clust_id$Country), on = .(Country)]): object 'data_plot' not found
{% endhighlight %}



{% highlight r %}
data_plot[data_clust_id,
          on = .(Country),
          Cluster := i.Cluster]
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'data_plot' not found
{% endhighlight %}



{% highlight r %}
DT::datatable(data_plot,
              class = "compact",
              extensions = 'Scroller',
              filter = "top",
              options = list(
                dom = 't',
                deferRender = TRUE,
                scrollY = 270,
                scroller = TRUE,
                scrollX = TRUE
              ))
{% endhighlight %}



{% highlight text %}
## Error in crosstalk::is.SharedData(data): object 'data_plot' not found
{% endhighlight %}
 
Plot of cluster members....

{% highlight r %}
theme_my <- theme(panel.border = element_rect(fill = NA,
                                                  colour = "grey10"),
                      panel.background = element_blank(),
                      panel.grid.minor = element_line(colour = "grey85"),
                      panel.grid.major = element_line(colour = "grey85"),
                      panel.grid.major.x = element_line(colour = "grey85"),
                      axis.text = element_text(size = 12, face = "bold"),
                      axis.title = element_text(size = 13, face = "bold"),
                      plot.title = element_text(size = 16, face = "bold"),
                      strip.text = element_text(size = 12, face = "bold"),
                      strip.background = element_rect(colour = "black"),
                      legend.text = element_text(size = 14),
                      legend.title = element_text(size = 15, face = "bold"),
                      legend.background = element_rect(fill = "white"),
                      legend.key = element_rect(fill = "white"),
                      legend.position="bottom")
 
ggplot(data_plot,
       aes(get(colnames(data_plot)[1]),
           get(statistic),
           group = Country)) +
      facet_wrap(~Cluster,
                 ncol = ceiling(data_plot[, sqrt(uniqueN(Cluster))]),
                 scales = "free_y") +
      geom_line(color = "grey10",
                alpha = 0.75,
                size = 0.8) +
      scale_y_continuous(trans = 'log10') +
      labs(x = colnames(data_plot)[1],
           y = statistic) +
      theme_my
{% endhighlight %}



{% highlight text %}
## Error in ggplot(data_plot, aes(get(colnames(data_plot)[1]), get(statistic), : object 'data_plot' not found
{% endhighlight %}
 
Check some clusters interactively with dygraphs:

{% highlight r %}
data_clust_focus <- dcast(data_plot[.(c(2,6)), on = .(Cluster)],
                          Days_since_first_100_case ~ Country,
                          value.var = statistic)
{% endhighlight %}



{% highlight text %}
## Error in is.data.table(data): object 'data_plot' not found
{% endhighlight %}



{% highlight r %}
dygraph(data_clust_focus,
        main = "Clusters n. 2 and 6") %>%
      dyAxis("x", label = colnames(data_clust_focus)[1]) %>%
      dyAxis("y", label = statistic) %>%
      dyOptions(strokeWidth = 2,
                drawPoints = F,
                pointSize = 3,
                pointShape = "circle",
                logscale = TRUE,
                colors = RColorBrewer::brewer.pal(ncol(data_clust_focus)-1, "Set2")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5,
                                             pointSize = 4)) %>%
      dyLegend(width = 150, show = "follow",
               hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
{% endhighlight %}



{% highlight text %}
## Error in dygraph(data_clust_focus, main = "Clusters n. 2 and 6"): object 'data_clust_focus' not found
{% endhighlight %}
 
Check some clusters interactively with dygraphs:

{% highlight r %}
data_clust_focus <- dcast(data_plot[.(7), on = .(Cluster)],
                          Days_since_first_100_case ~ Country,
                          value.var = statistic)
{% endhighlight %}



{% highlight text %}
## Error in is.data.table(data): object 'data_plot' not found
{% endhighlight %}



{% highlight r %}
dygraph(data_clust_focus,
        main = "Cluster n.7") %>%
      dyAxis("x", label = colnames(data_clust_focus)[1]) %>%
      dyAxis("y", label = statistic) %>%
      dyOptions(strokeWidth = 2,
                drawPoints = F,
                pointSize = 3,
                pointShape = "circle",
                logscale = TRUE,
                colors = RColorBrewer::brewer.pal(ncol(data_clust_focus)-1, "Set2")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5,
                                             pointSize = 4)) %>%
      dyLegend(width = 150, show = "follow",
               hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
{% endhighlight %}



{% highlight text %}
## Error in dygraph(data_clust_focus, main = "Cluster n.7"): object 'data_clust_focus' not found
{% endhighlight %}
 
Check some clusters interactively with dygraphs:

{% highlight r %}
data_clust_focus <- dcast(data_plot[.(1), on = .(Cluster)],
                          Days_since_first_100_case ~ Country,
                          value.var = statistic)
{% endhighlight %}



{% highlight text %}
## Error in is.data.table(data): object 'data_plot' not found
{% endhighlight %}



{% highlight r %}
dygraph(data_clust_focus,
        main = "Cluster n. 1") %>%
      dyAxis("x", label = colnames(data_clust_focus)[1]) %>%
      dyAxis("y", label = statistic) %>%
      dyOptions(strokeWidth = 2,
                drawPoints = F,
                pointSize = 3,
                pointShape = "circle",
                logscale = TRUE,
                colors = RColorBrewer::brewer.pal(ncol(data_clust_focus)-1, "Set2")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5,
                                             pointSize = 4)) %>%
      dyLegend(width = 150, show = "follow",
               hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
{% endhighlight %}



{% highlight text %}
## Error in dygraph(data_clust_focus, main = "Cluster n. 1"): object 'data_clust_focus' not found
{% endhighlight %}
 
Dendrogram to see nicer connections between countries in a tree:

{% highlight r %}
dend <- as.dendrogram(clust_res)
{% endhighlight %}



{% highlight text %}
## Error in as.dendrogram(clust_res): object 'clust_res' not found
{% endhighlight %}



{% highlight r %}
dend <- dend %>%
  color_branches(k = 14) %>%
  color_labels(k = 14) %>%
  set("branches_lwd", 1) %>%
  set("labels_cex", 0.8)
{% endhighlight %}



{% highlight text %}
## Error in eval(lhs, parent, parent): object 'dend' not found
{% endhighlight %}



{% highlight r %}
ggd1 <- as.ggdend(dend)
{% endhighlight %}



{% highlight text %}
## Error in as.ggdend(dend): object 'dend' not found
{% endhighlight %}



{% highlight r %}
ggplot(ggd1,
       horiz = T)
{% endhighlight %}



{% highlight text %}
## Error in ggplot(ggd1, horiz = T): object 'ggd1' not found
{% endhighlight %}
 
MDS 2D plot - we can use simply DTW distances.

{% highlight r %}
mds_classical <- cmdscale(clust_res@distmat, eig = FALSE, k = 2)
{% endhighlight %}



{% highlight text %}
## Error in cmdscale(clust_res@distmat, eig = FALSE, k = 2): object 'clust_res' not found
{% endhighlight %}



{% highlight r %}
data_plot <- data.table(mds_classical,
                        Country = row.names(mds_classical),
                        Cluster = clust_res@cluster)
{% endhighlight %}



{% highlight text %}
## Error in data.table(mds_classical, Country = row.names(mds_classical), : object 'mds_classical' not found
{% endhighlight %}



{% highlight r %}
ggplot(data_plot, aes(x = get("V1"),
                      y = get("V2"),
                      label = Country,
                      color = as.factor(Cluster))) +
      geom_label_repel(size = 4.2,
                       alpha = 0.95,
                       segment.alpha = 0.35,
                       label.r = 0.1,
                       box.padding = 0.25,
                       label.padding = 0.3,
                       label.size = 0.35,
                       max.iter = 2500) +
      scale_color_manual(values = colorspace::rainbow_hcl(14, c = 90, l = 50)) +
      labs(x = NULL, y = NULL, color = NULL) +
      guides(color = FALSE) +
      theme_my
{% endhighlight %}



{% highlight text %}
## Error in ggplot(data_plot, aes(x = get("V1"), y = get("V2"), label = Country, : object 'data_plot' not found
{% endhighlight %}
 
## Recap
 
DTW hierarchical clustering of active cases.
CovidR contest, shinyapps.io, GitHub source code petolau + CoronaDash
