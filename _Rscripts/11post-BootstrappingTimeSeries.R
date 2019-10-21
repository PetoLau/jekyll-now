# bootstrapping time series ----
library(clusterCrit)

# Moving Block Bootstrap ----
# source code from *forecast* package - MBB
MBB <- function(x, window_size) {
  
  bx <- array(0, (floor(length(x)/window_size)+2)*window_size)
  for (i in 1:(floor(length(x)/window_size)+2)) {
    c <- sample(1:(length(x)-window_size+1),1)
    bx[((i-1)*window_size+1):(i*window_size)] <- x[c:(c+window_size-1)]
  }
  start_from <- sample(0:(window_size-1),1) + 1
  bx[start_from:(start_from+length(x)-1)]
}

# My smoothed bootstrap version of previous bld.mbb.bootstrap function ----
smo.bootstrap <- function(x, num, block_size=NULL, alpha = 0.05)
{
  freq <- frequency(x)
  if(is.null(block_size))
  {
    block_size <- ifelse(freq > 1, 2*freq, min(8, floor(length(x)/ 2)))
  }
  
  xs <- list()
  xs[[1]] <- x # the first series is the original one
  
  if (num>1) {
    # Box-Cox transformation
    lambda <- BoxCox.lambda(x, lower=0.01, upper=1)
    x.bc <- BoxCox(x, lambda)
    
    if (freq>1) {
      # STL decomposition
      x.stl <- stl(ts(x.bc, frequency=freq), "per", robust = TRUE)$time.series
      seasonal <- x.stl[,1]
      trend <- x.stl[,2]
      # Smoothing the remainder part by H-W ES
      remainder <- ts(c(0, HoltWinters(x.stl[,3], alpha = alpha, beta = FALSE, gamma = FALSE)$fitted[,1]), freq = freq)
      
    } else {
      # Loess
      trend <- 1:length(x)
      suppressWarnings(x.loess <- loess(x.bc ~ trend, span=6/length(x), degree=1))
      seasonal <- rep(0, length(x))
      trend <- x.loess$fitted
      remainder <- x.loess$residuals
    }
    
    # Bootstrap some series, using MBB
    for (i in 2:num) {
      xs[[i]] <- InvBoxCox(trend + seasonal + MBB(remainder, block_size), lambda)
    }
  }
  
  xs
}

# K-means based bootstrap ----
# automatic K-means - searches for optimal K
clusterOptimKmeans <- function(matrixOOM, k_low, k_high){
  
  mat <- data.matrix(matrixOOM)
  
  clusterings <- sapply(c(k_low:k_high), function(x) kmeans(mat, x, nstart = 10, iter.max = 20)$cluster)
  
  DB_values <- sapply(1:ncol(clusterings), function(x) intCriteria(mat, as.integer(clusterings[,x]), c("Davies_Bouldin")))
  
  return(as.integer(clusterings[, which.min(DB_values)]))
}

KMboot <- function(x, num = 100, k_range = c(12, 20)) {
  
  freq <- frequency(x)
  x_clust <- data.matrix(x)
  
  if (sd(x) == 0) {
    km_res <- rep(1, length(x))
  } else {
    km_res <- clusterOptimKmeans(x_clust, k_range[1], k_range[2])
  }
  
  xs <- list()
  xs[[1]] <- ts(x, freq = freq)
  
  for(j in 2:num) {
    xs[[j]] <- vector(length = length(x))
    for(i in 1:length(x)) {
      xs[[j]][i] <- sample(x[km_res %in% km_res[i]], 1)
    }
    xs[[j]] <- ts(xs[[j]], freq = freq)
  }
  
  return(xs)
}

# K-means based bootstrap combined with random number generation from Normal dist. ----
KMboot.norm <- function(x, num = 100, k_range = c(12, 20)) {
  
  freq <- frequency(x)
  x_clust <- data.matrix(x)
  
  if (sd(x) == 0) {
    km_res <- rep(1, length(x))
  } else {
    km_res <- clusterOptimKmeans(x_clust, k_range[1], k_range[2])
  }
  
  clus_means <- sapply(sort(unique(km_res)), function(i) mean(x[km_res == i]))
  clus_sd <- sapply(sort(unique(km_res)), function(i) sd(x[km_res == i]))
  
  xs <- list()
  xs[[1]] <- ts(x, freq = freq)
  
  for(j in 2:num) {
    xs[[j]] <- vector(length = length(x))
    for(i in 1:length(x)) {
      xs[[j]][i] <- rnorm(1, mean = clus_means[km_res[i]], sd = clus_sd[km_res[i]])
    }
    xs[[j]] <- ts(xs[[j]], freq = freq)
  }
  
  return(xs)
}

# motivation ----
# generating new training data for time series forecasting methods like ARIMA or triple-ES (H-W) to improve foreasting accuracy.
# It is called bootstrapping, and after applying forecasting method on each new time series,
# forecasts are aggregated by average or median - so it is bagging - bootstrapp aggregating.
# It is proofed by multiple methods, e.g. in regression, that bagging helps improve accuracy - 
# like classical bagging, random forests, boosting and similar.
# The bagging methods for time series forecasting were used also in the latest M4 competion (link).
# For residential electricity consumption time series (as used in my previous blog posts),
# I proposed three new bootstrapping methods for time series forecasting methods (one enhacament of original ... link on article and two clustering based).
# I also combined classical bagging for regression trees and time series bagging to create ensemble forecasts - I will cover it in the next post.
# Link for paper.
# In this blog post, I will cover introduction to bootstrapping time series and my new methods will be introduced as well.
# Extensive experiments with 7 forecasting methods and 4 bootstrapping methods will be described and analysed on M4 competition dataset.

# read the M4comp data ----
# first install the package with data
devtools::install_github("carlanetto/M4comp2018")
library(M4comp2018) # M4 data
library(data.table) # manipulating the data
library(TSrepr) # forecasting error measures
library(forecast) # forecasting methods
library(smooth) # forecasting methods
library(forecTheta) # forecasting methods
library(ggplot2) # graphics
library(ggsci) # colours
library(clusterCrit) # int.validity indices

# load the data to memory
data(M4)

# hourly_M4 <- Filter(function(l) l$period == "Weekly", M4) # 4227 (Daily) + 359 (Weekly) + 414 (Hourly) = 5000

# I will use only hourly data - time series (data with the highest frequency),
# because I use at work also data with double-seasonality (quarter-hourly or half-hourly data).
# The high (or double) period (frequency) also means higher complexity and challenge,
# therefore it is great for this use case :)
hourly_M4 <- Filter(function(l) l$period == "Hourly", M4)

# plot random time series
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

ggplot(data.table(Time = 1:length(hourly_M4[[1]]$x),
                  Value = as.numeric(hourly_M4[[1]]$x))) +
  geom_line(aes(Time, Value)) +
  labs(title = hourly_M4[[1]]$st) +
  theme_ts

# Description of bootstrapping methods for time series ----
# Bootstrap aggregating (bagging) is an ensemble meta-algorithm (introduced by Breiman in 1996),
# which creates multiple versions of a learning set to produce a multiple number of
# predictors. These predictors are then aggregated, for example by arithmetic mean.

# For time dependent data, the classical bagging can't be used - so sampling (bootstrapping) with replacement.
# We have to sample data more sophisticated - based on seasonality or similar.
# One of the used bootstrapping method is Moving Block Bootstrap (MBB) that uses block (defined by seasonality for example) for creating new series.
# However, we don't use whole time series as it is, but we bootstrapp only its remainder part from STL decomposition (this method was proposed by Bergmeir et al. in 2016).

# ref Bergmeir C, Hyndman RJ, Benítez JM (2016)
# Bagging exponential smoothing methods using stl decomposition and box–cox transformation.
# International Journal of Forecasting, 32(2):303–312

# This method is implemented in the forecast package as `bld.mbb.bootstrap` function, let's use it on one time series from M4 comp dataset:

period <- 24*7 # weekly period
data_ts <- as.numeric(hourly_M4[[1]]$x)
data_boot_mbb <- bld.mbb.bootstrap(ts(data_ts, freq = period), 100)
data_plot <- data.table(Value = unlist(data_boot_mbb),
                        ID = rep(1:100, each = length(data_ts)),
                        Time = rep(1:length(data_ts), 100)
                        )

ggplot(data_plot) +
  geom_line(aes(Time, Value, group = ID), alpha = 0.5) +
  geom_line(data = data_plot[.(1), on = .(ID)], aes(Time, Value),
            color = "firebrick1", alpha = 0.9, size = 0.8) +
  theme_ts

# We can see that where values of the time series are low, there bld.mbb method fails to bootstrapp new values around original ones (red line).
# Notice, that variance of bootstrapped time series is very high and values flies somewhere more than 30% againts original value

# For this reason, I proposed smoothed version of bld.mbb method for reducing variance.
# I smoothed remainder part from STL decomposition by exponential smoothing, so extreme noise was removed.
# Let's use it:
data_boot_smbb <- smo.bootstrap(ts(data_ts, freq = period), 100)
data_plot <- data.table(Value = unlist(data_boot_smbb),
                        ID = rep(1:100, each = length(data_ts)),
                        Time = rep(1:length(data_ts), 100)
                        )

ggplot(data_plot) +
  geom_line(aes(Time, Value, group = ID), alpha = 0.5) +
  geom_line(data = data_plot[.(1), on = .(ID)], aes(Time, Value),
            color = "firebrick1", alpha = 0.9, size = 0.8) +
  theme_ts

# We can see that the variance was nicely lowered, but the final time series are sometimes really different from the original.
# It is not good when we want to use them for forecasting.
# Therefore, I developed (designed) another two bootstrapping methods based on K-means clustering.
# First step of the two methods are identical - automatic clustering of univariate time series -
# where automatic means that it estimates the number of clusters from defined range by Davies-Bouldin index.

# The first method, after clustering, samples new values from cluster members, so it isn't creating new values. Let's plot results.
data_boot_km <- KMboot(ts(data_ts, freq = period), 100, k_range = c(8, 10))
data_plot <- data.table(Value = unlist(data_boot_km),
                        ID = rep(1:100, each = length(data_ts)),
                        Time = rep(1:length(data_ts), 100)
                        )

ggplot(data_plot) +
  geom_line(aes(Time, Value, group = ID), alpha = 0.5) +
  geom_line(data = data_plot[.(1), on = .(ID)], aes(Time, Value),
            color = "firebrick1", alpha = 0.9, size = 0.8) +
  theme_ts

# we can, of course, change the range of the number of clusters to be selected
# to lower or to increase a variance of bootstrtapped time series.
# Let's try increase n. of clusters to decrease variance:
data_boot_km <- KMboot(ts(data_ts, freq = period), 100, k_range = c(14, 20))
data_plot <- data.table(Value = unlist(data_boot_km),
                        ID = rep(1:100, each = length(data_ts)),
                        Time = rep(1:length(data_ts), 100)
                        )

ggplot(data_plot) +
  geom_line(aes(Time, Value, group = ID), alpha = 0.5) +
  geom_line(data = data_plot[.(1), on = .(ID)], aes(Time, Value),
            color = "firebrick1", alpha = 0.9, size = 0.8) +
  theme_ts

# we can see that the variance is much lower against MBB based methods.
# But we have still power to increase it if we change the range of n. of clusters.

# The second K-means based method is sampling new values randomly from Gaussian distribution based on parameters of clusters (mean and variance).
data_boot_km.norm <- KMboot.norm(ts(data_ts, freq = period), 100, k_range = c(12, 20))
data_plot <- data.table(Value = unlist(data_boot_km.norm),
                        ID = rep(1:100, each = length(data_ts)),
                        Time = rep(1:length(data_ts), 100)
                        )

ggplot(data_plot) +
  geom_line(aes(Time, Value, group = ID), alpha = 0.5) +
  geom_line(data = data_plot[.(1), on = .(ID)], aes(Time, Value),
            color = "firebrick1", alpha = 0.9, size = 0.8) +
  theme_ts

# we can see nicely distributed values around the original time series, but will be it beneficial for forecasting?

# We can also check all four bootstrapping methods in one plot by wrapper around above calls:

# show bootstrapped TS ----
# bootstraping
print_boot_series <- function(data, ntimes = 100, k_range = c(12, 20)) {
  
  data_boot_1 <- bld.mbb.bootstrap(data, ntimes)
  data_boot_2 <- smo.bootstrap(data, ntimes)
  data_boot_3 <- KMboot(data, ntimes, k_range = k_range)
  data_boot_4 <- KMboot.norm(data, ntimes, k_range = k_range)
  
  datas_all <- data.table(Value = c(unlist(data_boot_1), unlist(data_boot_2), unlist(data_boot_3), unlist(data_boot_4)),
                          ID = rep(rep(1:ntimes, each = length(data)), 4),
                          Time = rep(rep(1:length(data), ntimes), 4),
                          Method = factor(rep(c("MBB", "S.MBB", "KM", "KM.boot"), each = ntimes*length(data)))
  )
  
  datas_all[, Method := factor(Method, levels(Method)[c(3,4,1,2)])]
  
  print(ggplot(datas_all) +
          facet_wrap(~Method, ncol = 2) +
          geom_line(aes(Time, Value, group = ID), alpha = 0.5) +
          geom_line(data = datas_all[.(1), on = .(ID)], aes(Time, Value),
                    color = "firebrick1", alpha = 0.9, size = 0.8) +
          theme_ts)
  
}

print_boot_series(ts(hourly_M4[[200]]$x, freq = period))

# when the time series is obviously non-stationary, the clustering bootstr. methods can oscillate little-bit badly.
# The clustering methods could be enhanced by for example differencing to be competetive (applicable)
# for non-stationary time series with strong trend line.

# forecasting with all the methods at time -----

# Forecasting with all specified methods with selected bootstrapping method
forec_boot_spec <- function(data_ts, boot_method = KMboot, freq = 24*7, h = 48, ntimes = 100) {
  
  data_ts <- ts(data_ts, freq = freq)
  
  # bootstraping
  data_boot <- boot_method(data_ts, ntimes)
  
  # check the correcteness of created TS
  res.inf <- sapply(1:ntimes, function(i) sum(as.integer(is.infinite(data_boot[[i]]) | is.na(data_boot[[i]]))))
  ind.good <- which(res.inf == 0)
  
  # forecasting with STL+ES
  mat_pred_ets <- sapply(ind.good, function(i) {
    stl_ets <- forecast::stlm(data_boot[[i]],
                              s.window = "periodic", robust = T, method = c("ets"))
    forecast::forecast(stl_ets, h, level = F)$mean
  }
  )
  
  # forecasting with STL+ARIMA
  mat_pred_arima <- sapply(ind.good, function(i) {
    stl_arima <- forecast::stlm(data_boot[[i]],
                                s.window = "periodic", robust = T, method = c("arima"))
    forecast::forecast(stl_arima, h, level = F)$mean
  }
  )
  
  # forecasting with H-W
  mat_pred_hw <- sapply(ind.good, function(i) {
    tryCatch({
      hw <- HoltWinters(data_boot[[i]])
      forecast::forecast(hw, h = h, level = F)$mean}, error = function(e) rep(NA, h))
  }
  )
  
  # forecasting with Theta
  mat_pred_theta <- sapply(ind.good, function(i) {
    tryCatch({
      forecast::thetaf(data_boot[[i]], h = h, level = F)$mean
    }, error = function(e) rep(NA, h))
  }
  )
  
  # forecasting with DOTM
  mat_pred_dtheta <- sapply(ind.good, function(i) {
    tryCatch({
      forecTheta::dotm(data_boot[[i]], h = h, s = "additive")$mean
    }, error = function(e) rep(NA, h))
  }
  )
  
  # forecasting with ES
  mat_pred_es <- sapply(ind.good, function(i)
    smooth::es(data_boot[[i]], model = "AAdA",
               h = h, cfType = "MAE", intervals = "none", silent = "all")$forecast
  )
  
  # bind all forecasts
  res <- rbindlist(list(data.table(melt(t(mat_pred_ets)))[, Method := "_STL+ETS"],
                        data.table(melt(t(mat_pred_arima)))[, Method := "_STL+ARIMA"],
                        data.table(melt(t(mat_pred_hw)))[, Method := "_HW"],
                        data.table(melt(t(mat_pred_theta)))[, Method := "_Theta"],
                        data.table(melt(t(mat_pred_dtheta)))[, Method := "_DOTM"],
                        data.table(melt(t(mat_pred_es)))[, Method := "_ES_AAdA"]
  )
  )
  
  # return forecasts
  return(res)
  
}

# Forecasting with all testing methods at once
forec_all_spec <- function(data, freq = 24*7, h = 48) {
  
  # ts object with defined seasonality
  data_ts <- ts(data, freq = freq)
  
  # forecasting with all bootstrapping methods at once
  # bld.mbb
  orig_res <- forec_boot_spec(data_ts,
                              boot_method = bld.mbb.bootstrap,
                              freq = freq, h = h)[, Method := paste0("MBB", Method)]
  
  # s.MBB
  smo_res <- forec_boot_spec(data_ts, boot_method = smo.bootstrap,
                             freq = freq, h = h)[, Method := paste0("S.MBB", Method)]
  # KM.boot
  km_res <- forec_boot_spec(data_ts, freq = freq, h = h)[, Method := paste0("KM", Method)]
  # KM.norm boot
  km.norm_res <- forec_boot_spec(data_ts, boot_method = KMboot.norm,
                                 freq = freq, h = h)[, Method := paste0("KM.norm", Method)]
  
  # sNAIVE
  naive_res <- snaive(data_ts, h = h)$mean
  
  # ES (AAdA)
  es_res <- es(data_ts, model = "AAdA",
               h = h, cfType = "MAE", intervals = "none", silent = "all")$forecast
  
  # STL+ARIMA
  stl_arima <- stlm(data_ts, s.window = "periodic", robust = T, method = c("arima"))
  stl_atima_forec <- forecast(stl_arima, h, level = F)$mean
  
  # STL+ES
  stl_ets <- forecast::stlm(data_ts,
                            s.window = "periodic", robust = T, method = c("ets"))
  stl_ets_forec <- forecast::forecast(stl_ets, h, level = F)$mean
  
  # Holt-Winters
  hw_forec <- tryCatch({
    hw <- HoltWinters(data_ts)
    forecast::forecast(hw, h = h, level = F)$mean}, error = function(e) rep(NA, h))
  
  # Theta
  theta_forec <- tryCatch({
    forecast::thetaf(data_ts, h = h, level = F)$mean
  }, error = function(e) rep(NA, h))
  
  # DOTM
  dtheta_forec <- tryCatch({
    forecTheta::dotm(data_ts, h = h, s = "additive")$mean
  }, error = function(e) rep(NA, h))
  
  # Var1 Var2 value Method
  return(rbindlist(list(
    orig_res,
    smo_res,
    km_res,
    km.norm_res,
    data.table(Var1 = 1,
               Var2 = rep(1:h, 7),
               value = c(naive_res, es_res, stl_atima_forec,
                         stl_ets_forec, hw_forec, theta_forec, dtheta_forec),
               Method = rep(c("NAIVE", "ES_AAdA", "STL+ARIMA",
                              "STL+ETS", "HW", "Theta", "DOTM"), each = h)
    )
  )
  )
  )
  
}

library(parallel)
cl <- parallel::makeCluster(8)
parallel::clusterExport(cl, varlist = c("hourly_M4", "forec_all_spec", "forec_boot_spec",
                                        "KMboot.norm", "KMboot", "MBB", "smo.bootstrap", "clusterOptimKmeans"), envir = environment())
clusterCall(cl, function() {library(data.table);setDTthreads(1);library(smooth);library(forecast);library(clusterCrit)})
data_forec_all <- parallel::parLapply(cl, 1:length(hourly_M4), function(i) forec_all_spec(hourly_M4[[i]]$x)[, Dataset := hourly_M4[[i]]$st])
if(!is.null(cl)) {
  parallel::stopCluster(cl)
  cl <- c()
}
gc()

data_forec_all <- rbindlist(data_forec_all, use.names = T)

fwrite(data_forec_all, "hourly_forecasts_boot_all-various.csv", col.names = T, row.names = F, quote = F)

# analysis of results ----
data_forec_all_whole <- fread("_Rscripts/hourly_forecasts_boot_all-various.csv")
data_forec_all_whole[, Method := factor(Method)]

data_forec_all_whole_median <- copy(data_forec_all_whole[, .(Value = median(value)),
                                                         by = .(Dataset, Method, Var2)])

library(ggforce)

ggplot(data_forec_all[.(M4datasets[1]), on = .(Dataset)]) +
  geom_line(aes(Time, Value, color = Method), alpha = 0.7, size = 0.8) +
  theme_bw()

# orig test forec ----
orig_forec <- rbindlist(lapply(1:length(hourly_M4), function(i) data.table(Time = 1:length(hourly_M4[[i]]$xx),
                                                                           Value = as.numeric(hourly_M4[[i]]$xx),
                                                                           Method = "Original",
                                                                           Dataset = hourly_M4[[i]]$st))
                        )

# compute errors
mapes_all <- copy(data_forec_all_whole_median[, .(sMAPE = smape(orig_forec[.(.BY$Dataset), on = .(Dataset), Value],
                                                                Value),
                                                  MAPE = mape(orig_forec[.(.BY$Dataset), on = .(Dataset), Value],
                                                              Value),
                                                  MAAPE = maape(orig_forec[.(.BY$Dataset), on = .(Dataset), Value],
                                                                Value)),
                                              by = .(Dataset, Method)])

mapes_all[is.na(sMAPE)] # we got some NAs

setorder(mapes_all, Dataset, sMAPE)

res_mapes_arima <- mapes_all[.(c(unique(grep(x = Method, pattern = "STL\\+ARIMA", value = T)), "NAIVE")), on = .(Method)]
res_mapes_ets <- mapes_all[.(c(unique(grep(x = Method, pattern = "STL\\+ETS", value = T)), "NAIVE")), on = .(Method)]
res_mapes_dotm <- mapes_all[.(c(unique(grep(x = Method, pattern = "DOTM", value = T)), "NAIVE")), on = .(Method)]
res_mapes_theta <- mapes_all[.(c(unique(grep(x = Method, pattern = "Theta", value = T)), "NAIVE")), on = .(Method)]
res_mapes_es <- mapes_all[.(c(unique(grep(x = Method, pattern = "ES", value = T)), "NAIVE")), on = .(Method)]
res_mapes_hw <- mapes_all[.(c(unique(grep(x = Method, pattern = "HW", value = T)), "NAIVE")), on = .(Method)]

setorder(res_mapes_arima, Dataset, sMAPE)
res_mapes_arima[!is.na(sMAPE), Rank := 1:uniqueN(Method), by = .(Dataset)]
setorder(res_mapes_ets, Dataset, sMAPE)
res_mapes_ets[!is.na(sMAPE), Rank := 1:uniqueN(Method), by = .(Dataset)]
setorder(res_mapes_dotm, Dataset, sMAPE)
res_mapes_dotm[!is.na(sMAPE), Rank := 1:uniqueN(Method), by = .(Dataset)]
setorder(res_mapes_theta, Dataset, sMAPE)
res_mapes_theta[!is.na(sMAPE), Rank := 1:uniqueN(Method), by = .(Dataset)]
setorder(res_mapes_es, Dataset, sMAPE)
res_mapes_es[!is.na(sMAPE), Rank := 1:uniqueN(Method), by = .(Dataset)]
setorder(res_mapes_hw, Dataset, sMAPE)
res_mapes_hw[!is.na(sMAPE), Rank := 1:uniqueN(Method), by = .(Dataset)]

# For evaluating four presented bootstrapping methods for time series (to see which is the most competetive in general),
# experiments with 6 statistical forecasting methods were performed on 414 hourly time series. 
# Forecasts from bootstrapped time series were aggregated by median. Also simple base methods were evaluated alongside seasonal naive forecast.
# The six chosen statistical base foreasting methods were: STL+ARIMA, STL+ETS (forecast package), triple exponential smoothing with damped trend (smooth package - ES (AAdA)),
# Holt-Winters exponential smoothing (stats package), dynamic optimized theta model (forecTheta package), and standard theta model (forecast package).

res_all <- rbindlist(list(cbind(res_mapes_arima, Base_method = "STL+ARIMA"),
                          cbind(res_mapes_ets, Base_method = "STL+ETS"),
                          cbind(res_mapes_es, Base_method = "ES (AAdA)"),
                          cbind(res_mapes_hw, Base_method = "Holt-Winters"),
                          cbind(res_mapes_theta, Base_method = "Theta"),
                          cbind(res_mapes_dotm, Base_method = "DOTM")))

res_all[, Boot_method := sub("\\_.*", "", Method)]
boots <- unique(res_all$Boot_method)
res_all[!Boot_method %in% boots[c(2:6)], Boot_method := "Base-learner"]
res_all[Boot_method %in% "NAIVE", Boot_method := "NAIVE"]
res_all[, Boot_method := factor(Boot_method)]
res_all[, Base_method := factor(Base_method)]
res_all[, Base_method := factor(Base_method, levels(Base_method)[c(4,5,2,3,1,6)])]
res_all[, Boot_method := factor(Boot_method, levels(Boot_method)[c(1,4,6,2,3,5)])]

ggplot(res_all[, .(sMAPE = mean(sMAPE, na.rm = TRUE)), by = .(Base_method, Boot_method)]) +
  facet_wrap(~Base_method, ncol = 3, scales = "fix") +
  geom_bar(aes(Boot_method, sMAPE, fill = Boot_method, color = Boot_method), alpha = 0.8, stat = "identity") +
  geom_text(aes(Boot_method, y = sMAPE + 1, label = paste(round(sMAPE, 2)))) +
  scale_fill_d3() +
  scale_color_d3() +
  theme_ts
# best methods based on average sMAPE are STL+ETS with s.MBB and DOTM with s.MBB
# the our proposed s.MBB boot. method wons 5 times from 6 based on average sMPAE.

# boxplot of Ranks
ggplot(res_all) +
  facet_wrap(~Base_method, ncol = 3, scales = "fix") +
  geom_boxplot(aes(Boot_method, Rank, fill = Boot_method), alpha = 0.8) +
  scale_fill_d3() +
  theme_ts
# we can see very tight results amongst base learner, MBB and s.MBB methods

# Let's see average Rank instead
ggplot(res_all[, .(Rank = mean(Rank, na.rm = TRUE)), by = .(Base_method, Boot_method)]) +
  facet_wrap(~Base_method, ncol = 3, scales = "fix") +
  geom_bar(aes(Boot_method, Rank, fill = Boot_method, color = Boot_method), alpha = 0.8, stat = "identity") +
  geom_text(aes(Boot_method, y = Rank + 0.22, label = paste(round(Rank, 2))), size = 5) +
  scale_fill_d3() +
  scale_color_d3() +
  theme_ts
# as was seen in the previous plot, it changes and it dependes on a forecasting method 
# but very slightly better results has original MBB method over my s.MBB, but both beats base learner 5 times from 6
# KM-based bootstrapps failed to beat base learner 4 times from 6

# Let's be more curious about distribution of Ranks...
# Let's plot counts of Ranks how many times each method had 
ggplot(res_all[, .(N = .N), by = .(Base_method, Boot_method, Rank)]
       [, Rank := factor(Rank)],
       aes(Boot_method, N)) +
  facet_wrap(~Base_method, ncol = 3, scales = "free") +
  geom_bar(aes(fill = Rank,
               color = Rank),
           alpha = 0.75, position = "dodge", stat = "identity") +
  scale_fill_d3() +
  scale_color_d3() +
  theme_ts
# we can see that KM-based boots. can be first or second too!
# around 25-times for each forecasting method were these 2 boot. methods first, and around 2-times more second..not bad
# Also seasonal naive forecast can be on some types of time series best against more sophisticated statistical forecasting methods...
# However the MBB and s.MBB methods are best and the most of the times are in the first two ranks.
