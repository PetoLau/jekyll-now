# Header ----
library(data.table)
library(parallel)
library(cluster)
library(clusterCrit)
library(TSrepr)
library(kSamples)
library(OpenML)
library(ggplot2)
library(animation)

# Post specification and motivation ----
# title: Multiple Data (Time Series) Streams Clustering in R
# you can do data stream clustering by "stream" package, (BUT!) there are methods only for one stream (not multiple streams)!
# multiple data streams clustering - attribute based approach - multiple attributes for multiple streams
# I created clustering method that is adapted for time series streams - ClipStream (ref.)
# You can read more about it here and here (pdf link).
# I will show you use case on smart meter data - electricty consumption time series (consumers) from London (openly available)
# Motivation: 1) extract typical patterns of consumption, 2) detect outliers, 3) unsupervised classification of consumers,
# 4) create more forecastable groups of time series, 5) monitor changes and behaviour

# ClipStream - steps: 1.) computing data streams representation for every stream (dimensionality reduction) - ie synopsis
# 2.) detect outlier (anomal) streams directly by (from) extracted representations,
# 3.) Cluster non-outlier representations by K-medoids,
# 4.) Assign outlier streams to nearest clusters (medoids)
# 5.) Aggregate time series by clusters
# 6.) Detect changes in aggregated time series

# In this post, I will mainly focus on first 4 parts of my approach -
# since there are aplicable for various task - not only on smart meter data and its forecasting
# So for representation, clustering and outlier detection of time series streams - all in unsupervised fashion (yey!).

# Firstly, read the London smart meter data, which compromise more than 4000 consumers...

# Read the data ----
data <- OpenML::getOMLDataSet(data.id = 41060)
data <- data.matrix(data$data)

# subset first 1000 consumers for more simple operations
data_cons <- data[1:1000,]

period <- 48

# plot random consumer for better imagination
theme_ts <- theme(panel.border = element_rect(fill = NA, 
                                              colour = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.minor = element_line(colour = "grey85"),
                  panel.grid.major = element_line(colour = "grey85"),
                  panel.grid.major.x = element_line(colour = "grey85"),
                  axis.text = element_text(size = 13, face = "bold"),
                  axis.title = element_text(size = 15, face = "bold"),
                  plot.title = element_text(size = 16, face = "bold"),
                  strip.text = element_text(size = 16, face = "bold"),
                  strip.background = element_rect(colour = "black"),
                  legend.text = element_text(size = 15),
                  legend.title = element_text(size = 16, face = "bold"),
                  legend.background = element_rect(fill = "white"),
                  legend.key = element_rect(fill = "white"),
                  legend.position="bottom")

# we can see stochastic behaviour, but also some pattern
ggplot(data.table(Time = 1:ncol(data_cons),
                  Value = data_cons[200,])) +
  geom_line(aes(Time, Value)) +
  labs(y = "Consumption (kWh)") +
  theme_ts

# we can plot for example its daily or weekly profile
# by TSrepr function repr_seas_profile

ggplot(data.table(Time = 1:period,
                  Value = repr_seas_profile(data_cons[200,],
                                            freq = period,
                                            func = mean))) +
  geom_line(aes(Time, Value)) +
  geom_point(aes(Time, Value), alpha = 0.8, size = 2.5) +
  scale_x_continuous(breaks = seq(1, 48, 6),
                     labels = paste(seq(0, 23, by = 3), "h", sep = "")) +
  labs(y = "Consumption (kWh)") +
  theme_ts

# peak on start of a day...

# now weekly pattern
ggplot(data.table(Time = 1:(period*7),
                  Value = repr_seas_profile(data_cons[200,],
                                            freq = period*7,
                                            func = mean))) +
  geom_line(aes(Time, Value), size = 0.8) +
  geom_vline(xintercept = (0:7)*period, color = "dodgerblue2",
             size = 1.2, alpha = 0.7, linetype = 2) +
  labs(y = "Consumption (kWh)") +
  theme_ts
# there is some change in during week...

# Clustering TS ----
# Explore whole consumer base..how when we have more than 1000 TS?

# we can cluster TS and just plot its daily patterns for example
# we will reduce length of the visualized TS and also number of TS in one plot

# extract average daily patterns first - by repr_matrix function from TSrepr
# normalisation of every consumer TS - row-wise by z-score!
# You can use your own norm. func (z-score and min-max methods are implemented)
data_ave_prof <- repr_matrix(data_cons,
                             func = repr_seas_profile,
                             args = list(freq = period,
                                         func = median),
                             normalise = TRUE,
                             func_norm = norm_z)

# cluster it for example by K-means and 12 clusters
res_clust <- kmeans(data_ave_prof, 12, nstart = 20)

# preprocess results and plot them
data_plot <- data.table(melt(data_ave_prof))
data_plot[, Clust := rep(res_clust$cluster, ncol(data_ave_prof))]

data_centroid <- data.table(melt(res_clust$centers))
data_centroid[, Clust := Var1]

str(data_plot)

ggplot(data_plot) +
  facet_wrap(~Clust, scales = "free", ncol = 3) +
  geom_line(aes(Var2, value, group = Var1), alpha = 0.7) +
  geom_line(data = data_centroid, aes(Var2, value, group = Var1),
            alpha = 0.8, color = "red", size = 1.2) +
  scale_x_continuous(breaks = c(1,seq(12, 48, 12)),
                     labels = paste(seq(0, 24, by = 6), "h", sep = "")) +
  theme_ts

# various interesting patterns of daily consumption

# BUT! (Motivation)
# when we would like to cluster just most recent data (for example 2-3 weeks)...
# and update it regurarly every day (or eventought half-hour)...
# automaticly detect changes in TS and detect anomalies (outlier consumers)...
# detect automaticly number of clusters!...
# and do it as quickly as possible...
# We can not do it like in previous case...because of computational and memory load and also accuracy...
# Here comes on screen more sophisticated multiple data streams clustering methods...
# I developed one - ClipStream that uses FeaClip time series streams representation
# representation can be computed incrementally, clustering by batches, detect outliers straight from repr...

# FeaClip representation for interpretability of results
ts_feaclip <- repr_feaclip(data_cons[1, 1:48])

library(grid)
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
}

gg1 <- ggplot(data.table(Consumption = data_cons[1, 1:48],
                         Time = 1:48)) +
  geom_line(aes(Time, Consumption)) +
  geom_point(aes(Time, Consumption), size = 2, alpha = 0.8) +
  geom_hline(yintercept = mean(data_cons[1, 1:48]), color = "red", size = 0.8, linetype = 7) +
  theme_ts

gg2 <- ggplot(data.table(Count = ts_feaclip,
                         Time = 1:8)) +
  geom_line(aes(Time, Count)) +
  geom_point(aes(Time, Count), size = 2, alpha = 0.8) +
  scale_x_continuous(breaks = 1:8, labels = names(ts_feaclip)) +
  theme_ts

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))
print(gg1, vp = define_region(1, 1))
print(gg2, vp = define_region(2, 1))

# compute it for consumption of length 2 weeks - by windows ----
ts_feaclip_long <- repr_windowing(data_cons[1, 1:(period*14)], func = repr_feaclip, win_size = period)

win <- 14
means <- sapply(0:(win-1), function(i) mean(data_cons[1, ((period*i)+1):(period*(i+1))]))

means <- data.table(Consumption = rep(means, each = period),
                    Time = 1:length(data_cons[1, 1:(period*14)]))

gg1 <- ggplot(data.table(Consumption = data_cons[1, 1:(period*14)],
                         Time = 1:(period*14))) +
  geom_line(aes(Time, Consumption), alpha = 0.95) +
  geom_vline(xintercept = seq(from=period,to=(period*14)-period, by=period), alpha = 0.75, size = 1.0, col = "dodgerblue2", linetype = 2) +
  geom_line(data = means, aes(Time, Consumption), color = "firebrick2", alpha = 0.8, size = 1.2, linetype = 5) +
  labs(y = "Consumption (kWh)", title = NULL, x = NULL) +
  scale_x_continuous(breaks=seq(from=period/2, to=(period*14), by = period),
                     labels=c(1:14)) +
  theme_ts

gg2 <- ggplot(data.table(Count = ts_feaclip_long,
                         Time = 1:length(ts_feaclip_long))) +
  geom_line(aes(Time, Count)) +
  geom_vline(xintercept = seq(from=8,to=(8*14)-8, by=8), alpha = 0.75, size = 1.0, col = "dodgerblue2", linetype = 2) +
  # geom_point(aes(Time, Count), size = 2, alpha = 0.8) +
  scale_x_continuous(breaks=seq(from=8/2, to=(8*14), by = 8),
                     labels=c(1:14)) +
  labs(title = NULL, x = "Day") +
  theme_ts

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))
print(gg1, vp = define_region(1, 1))
print(gg2, vp = define_region(2, 1))

# we can see typical pattern during working days and some changes when consumption goes high -
# the representation adapts to these changes, but not drastically - because of average of window (normalization)

# through time ----
data_feaclip_all <- repr_windowing(data_cons[1,], func = repr_feaclip, win_size = period)
win <- 14
win_size <- 14*8
n_win <- length(data_feaclip_all)/8 - win
data_feaclip_all_anim <- sapply(0:(n_win - 1), function(i) data_feaclip_all[((i*8)+1):((i*8) + win_size)])
data_feaclip_all_anim <- data.table(melt(data_feaclip_all_anim))
data_feaclip_all_anim[, Type := "Representation - FeaClip"]

win <- 14
win_size <- 14*period
n_win <- ncol(data_cons)/period - win
data_cons_anim <- sapply(0:(n_win - 1), function(i) data_cons[1, ((i*period)+1):((i*period) + win_size)])
row.names(data_cons_anim) <- NULL
data_cons_anim <- data.table(melt(data_cons_anim))
data_cons_anim[, Type := "Original TS"]

data_plot <- rbindlist(list(data_cons_anim,
                            data_feaclip_all_anim))

setnames(data_plot, "Var2", "Window")

data_vline <- data.table(Var1 = c(seq(from=period,to=(period*14)-period, by=period),
                                  seq(from=8,to=(8*14)-8, by=8)),
                         Type = rep(c("Original TS", "Representation - FeaClip"), each = 13)
                         )

library(gganimate)

gg_anim <- ggplot(data_plot) +
  facet_wrap(~Type, scales = "free", ncol = 1) +
  geom_line(aes(Var1, value)) +
  geom_vline(data = data_vline, aes(xintercept = Var1),
             alpha = 0.75, size = 0.8, col = "dodgerblue2", linetype = 2) +
  labs(x = "Length", y = NULL, title = "Time series and its representation, Window: {frame_time}") +
  theme_ts +
  # transition_states(Window, transition_length = 4, state_length = 2) +
  transition_time(Window)

library(av)

animate(gg_anim, fps = 1.2, nframes = n_win, width = 900, height = 500)
gganimate::animate(gg_anim, fps = 2, nframes = n_win, width = 900, height = 500,
        res = 120,
        renderer = av_renderer('animation.mp4'))
utils::browseURL('animation.mp4')

# we can see how FeaClip representation adapts on different behaviour of electricity consumption
# (when it's low around zero, or it gets high etc.) - drastic noise reduction, extracted only key features from TS.

## Multiple Data Streams
# we already got picture of one consumers stream
# let's again play with whole customer base (in our playing scenario with 1000 consumers)
# I would like to make nice animations of outlier detection and clustering phase

# First iteration ----
i = 0
k.min <- 8
k.max <- 15

data_win <- data_cons[, ((i*period)+1):((i+win)*period)]

data_clip <- repr_matrix(data_win, func = repr_feaclip,
                        windowing = T, win_size = period)

# detect outliers
outliers <- detectOutliersIQR(data_clip, treshold = 1.5)

# clustering
clip_filtered <- data_clip[-outliers$outliers,]
clus_res <- clusterOptimKmedoidsDB(clip_filtered, k.min, k.max, 4, criterium = "Davies_Bouldin")

clustering <- outliers$class
clustering[which(clustering == 1)] <- clus_res$clustering

# Assign outliers to clusters (to nearest medoid)
out_clust <- sapply(seq_along(outliers$outliers),
                    function(z) my_knn(clus_res$medoids, as.numeric(data_clip[outliers$outliers[z],])))

clustering[which(clustering == 0)] <- out_clust

# outlier detection anim (PCA or t-SNE)
# clustering anim - average daily (weekly) FeaClip

pc_clip <- prcomp(data_clip, center = T, scale. = T)$x[,1:2]
pc_clip <- data.table(pc_clip,
                      Outlier = factor(outliers$class))
levels(pc_clip$Outlier) <- c("Outlier", "Normal")

gg1 <- ggplot(pc_clip) +
  geom_point(aes(PC1, PC2, fill = Outlier, shape = Outlier),
             size = 2.8, alpha = 0.75) +
  scale_shape_manual(values = c(24, 21)) +
  scale_fill_manual(values = c("salmon", "dodgerblue3")) +
  labs(title = paste("N.of.Outliers: " , nrow(pc_clip[Outlier == "Outlier"]))) +
  theme_ts

# which one is that lowest along PC2 axes? OR lowest on PC1 axes?

which(pc_clip$PC2 == max(pc_clip$PC2))
which(pc_clip$PC1 == min(pc_clip$PC1)) # zeros

plot(ts(data_win[597,]))
plot(ts(data_win[562,])) # zeros

ave_clip <- repr_matrix(data_clip, func = repr_seas_profile, args = list(freq = 8,
                                                                        func = mean))
data_plot <- data.table(melt(ave_clip))
data_plot[, Clust := rep(clustering, ncol(ave_clip))]
pc_clip[,Var1 := 1:.N]
data_plot[pc_clip[, .(Var1, Outlier)], Outlier := i.Outlier, on = .(Var1)]

medoids <- data.table(melt(repr_matrix(clus_res$medoids, func = repr_seas_profile, args = list(freq = 8,
                                                                       func = mean))))
medoids[, Clust := Var1]

gg2 <- ggplot(data_plot) +
  facet_wrap(~Clust, scales = "free", ncol = 3) +
  geom_line(aes(Var2, value, group = Var1, color = Outlier), alpha = 0.7) +
  geom_line(data = medoids, aes(Var2, value, group = Var1),
            alpha = 0.9, color = "dodgerblue2", size = 1) +
  labs(title = "Clusters of average daily FeaClip representations", y = "Count", x = NULL) +
  scale_x_continuous(breaks = 1:8, labels = c("m_1", "s_1", "m_0", "cr.", names(ts_feaclip)[5:8])) +
  scale_color_manual(values = c("salmon", "black")) +
  theme_ts

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(gg1, vp = define_region(1, 1))
print(gg2, vp = define_region(1, 2))

# Streaming ----
n_win # I will do only 50 windows
# i = 1

saveGIF({
  oopt = ani.options(interval = 2.5, nmax = 50)
for(i in 1:(50)) {
  
  # new data from new day
  data_new <- data_cons[, (((i+(win-1))*period)+1):((i+win)*period)]
  
  # sliding window of represention
  clip_new <- repr_matrix(data_new, func = repr_feaclip)
  data_clip <- cbind(data_clip[, -(1:ncol(clip_new))], clip_new)
  
  # detect outliers
  outliers <- detectOutliersIQR(data_clip, treshold = 1.5)
  
  # clustering
  if(max(clustering) >= k.min + 2){
    k_min <- max(clustering) - 2
  } else k_min <- k.min
  
  if(max(clustering) <= k.max - 2){
    k_max <- max(clustering) + 2
  } else k_max <- k.max
  
  clip_filtered <- data_clip[-outliers$outliers,]
  clus_res <- clusterOptimKmedoidsDB(clip_filtered, k_min, k_max, 4, criterium = "Davies_Bouldin")
  
  clustering <- outliers$class
  clustering[which(clustering == 1)] <- clus_res$clustering
  
  # Assign outliers to clusters (to nearest medoid)
  out_clust <- sapply(seq_along(outliers$outliers),
                      function(z) my_knn(clus_res$medoids, as.numeric(data_clip[outliers$outliers[z],])))
  
  clustering[which(clustering == 0)] <- out_clust
  
  # outlier detection anim (PCA or t-SNE)
  # clustering anim - average daily (weekly) FeaClip
  
  pc_clip <- prcomp(data_clip, center = T, scale. = T)$x[,1:2]
  pc_clip <- data.table(pc_clip,
                        Outlier = factor(outliers$class))
  levels(pc_clip$Outlier) <- c("Outlier", "Normal")
  
  gg1 <- ggplot(pc_clip) +
    geom_point(aes(PC1, PC2, fill = Outlier, shape = Outlier),
               size = 2.8, alpha = 0.75) +
    scale_shape_manual(values = c(24, 21)) +
    scale_fill_manual(values = c("salmon", "dodgerblue3")) +
    labs(title = paste("Window: ", i+1, "; N.of.Outliers: " , nrow(pc_clip[Outlier == "Outlier"]))) +
    theme_ts
  
  ave_clip <- repr_matrix(data_clip, func = repr_seas_profile, args = list(freq = 8,
                                                                           func = mean))
  data_plot <- data.table(melt(ave_clip))
  data_plot[, Clust := rep(clustering, ncol(ave_clip))]
  pc_clip[, Var1 := 1:.N]
  data_plot[pc_clip[, .(Var1, Outlier)], Outlier := i.Outlier, on = .(Var1)]
  
  medoids <- data.table(melt(repr_matrix(clus_res$medoids, func = repr_seas_profile, args = list(freq = 8,
                                                                                                 func = mean))))
  medoids[, Clust := Var1]
  
  gg2 <- ggplot(data_plot) +
    facet_wrap(~Clust, scales = "free", ncol = 3) +
    geom_line(aes(Var2, value, group = Var1, color = Outlier), alpha = 0.7) +
    geom_line(data = medoids, aes(Var2, value, group = Var1),
              alpha = 0.9, color = "dodgerblue2", size = 1) +
    labs(title = "Clusters of average daily FeaClip representations", y = "Count", x = NULL) +
    scale_x_continuous(breaks = 1:8, labels = c("m_1", "s_1", "m_0", "cr.", names(ts_feaclip)[5:8])) +
    scale_color_manual(values = c("salmon", "black")) +
    theme_ts
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 2)))
  print(gg1, vp = define_region(1, 1))
  print(gg2, vp = define_region(1, 2))
  ani.pause()  
}
}, movie.name = "clipstream_1.gif", ani.height = 748, ani.width = 1690)

# End
# on using clustering for improving forecasting of aggrageted consumption maybe next time...or read my publications
