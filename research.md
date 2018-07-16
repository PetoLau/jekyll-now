---
title: "Research and Presentations"
permalink: /research/
layout: page
---

## PhD. thesis
I defended my dissertation (i.e. PhD. thesis) at the [Faculty of Informatics and Information Technologies](http://www.fiit.stuba.sk/en.html?page_id=749), Slovak University of Technology in Bratislava, in July 2018. My supervisor was associate professor [Mária Lucká](https://scholar.google.sk/citations?user=1bQwDSgAAAAJ&hl=sk&oi=ao). The theme of the thesis was about improving forecasting accuracy of electricity load through the cluster analysis of consumers (or prosumers) and time series representations (see extended abstract of my thesis: [Improving Forecasting Accuracy Through the Influence of Time Series Representations and Clustering](http://acmbulletin.fiit.stuba.sk/abstracts/laurinec2018.pdf)). I had focused on three interesting areas of data mining:

  * **Time series analysis**
  * **Clustering**
  * **Forecasting and regression**

The area of **time series analysis** consists of a research in (and also proposals of new) time series **representations**, specifically efficient dimensionality reduction of time series of electricity consumption that will input to a clustering algorithm. I developed my own **R** package called [**TSrepr**](https://CRAN.R-project.org/package=TSrepr) that involves various representations methods and is available on my GitHub repository: [github.com/PetoLau/TSrepr](https://github.com/PetoLau/TSrepr).

The **clustering** task is about classification (clustering) consumers into more predictable (forecastable) groups of consumers. The challenge is to develop an algorithm that will be adaptable to a behavior of **multiple data streams** of electricity load. Results of clustering are then used in statistical time series analysis and regression methods to improve forecasting accuracy of aggregate (global) or individual (end-consumer) electricity load. Results of clustering can be also used for smart grid monitoring, anomaly (outlier) detection, and an extraction of typical patterns of electricity consumption.

The research scope of the **forecasting and regression** part focuses on methods that will benefit the most from clustering of consumers. Forecasting and regression methods have to incorporate to a model a seasonality and a trend, and they have to be adaptable to a concept drift appearance. Here is a promising approach – **ensemble learning** that combines multiple forecasts from various forecasting and regression methods.

#### Research papers
You can read my research papers related to the thesis on [**Google Scholar**](https://scholar.google.sk/citations?user=1fEwHTkAAAAJ&hl=en) and [**ResearchGate**](https://www.researchgate.net/profile/Peter_Laurinec) (as is also mentioned in the [About me section](https://petolau.github.io/about)).

## Presented works
Works (papers) that were presented by me at a conference, a workshop or a meetup are listed below.

<hr>
##### **Time Series Representations for Better Data Mining**
**Conference:** [eRum'2018](http://2018.erum.io)

**Date:** 15.5.2018

**Where:** Budapest, Hungary

<a href="/presentations/erum_laurinec.pdf" target="_blank">Download the presentation (PDF)</a>

<hr>
##### **New Clustering-based Forecasting Method for Disaggregated End-consumer Electricity Load Using Smart Grid Data**
**Conference:** [Informatics'2017](https://informatics.kpi.fei.tuke.sk/)

**Date:** 14.11.2017

**Where:** Poprad, Slovakia

<a href="/presentations/informatics_laurinec.pdf" target="_blank">Download the presentation (PDF)</a>

<hr>
##### **Is Unsupervised Ensemble Learning Useful for Aggregated or Clustered Load Forecasting?**
**Workshop:** [ECML-PKDD NFMCP'2017](http://www.di.uniba.it/~loglisci/NFmcp17/program.html)

**Date:** 22.9.2017

**Where:** Skopje, Macedonia

<a href="/presentations/ecmlpkdd_laurinec.pdf" target="_blank">Download the presentation (PDF)</a>

<hr>
##### **Using Clustering Of Electricity Consumers To Produce More Accurate Predictions**
**Meetup:** [R<-Slovakia](https://petolau.github.io/First-R-Slovakia-meetup/)

**Date:** 22.3.2017

**Where:** Bratislava, Slovakia

<a href="/presentations/RSlovakia_laurinec.pdf" target="_blank">Download the presentation (PDF)</a>

<hr>
##### **Adaptive Time Series Forecasting of Energy Consumption using Optimized Cluster Analysis**
**Workshop:** [ICDM DaMEMO'2016](http://www.covic.otago.ac.nz/DaMEMO16/index.html)

**Date:** 12.12.2016

**Where:** Barcelona, Spain

<a href="/presentations/icdm_laurinec.pdf" target="_blank">Download the presentation (PDF)</a>

<hr>
##### **Comparison of Representations of Time Series for Clustering Smart Meter Data**
**Conference:** [WCECS ICMLDA'2016](http://www.iaeng.org/publication/WCECS2016/)

**Date:** 21.10.2016

**Where:** San Francisco, California, U.S.A.

<a href="/presentations/icmlda_laurinec.pdf" target="_blank">Download the presentation (PDF)</a>

<hr>

{% include share-page.html share-page_identifier=page.share-page_identifier %}
{% include disqus.html disqus_identifier=page.disqus_identifier %}
