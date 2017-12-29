# rm(list=ls())
# gc()

## Post from here ----
# setwd("C:\\Users\\Peter\\Downloads\\ProjektBD\\enernoc\\")

library(feather)
library(data.table)
library(mgcv)
library(car)
library(ggplot2)
library(grid)
library(animation)

# Use feather (fast to share data) to read data.table
DT <- as.data.table(read_feather("DT_4_ind"))

# prepare DT to work with a regression model
# tranform charactres of weekdays to integers
# DT[, week_num := as.integer(as.factor(DT[, week]))]
# DT[, week_num := recode(week_num, "1=5;2=1;3=6;4=7;5=4;6=2;7=3")] #recode

# alternative:
DT[, week_num := as.integer(car::recode(
  week, "'Monday'='1';'Tuesday'='2';'Wednesday'='3';'Thursday'='4';'Friday'='5';'Saturday'='6';'Sunday'='7'"
))]

# store information of the type of consumer, date, weekday and period
n_type <- unique(DT[, type])
n_weekdays <- unique(DT[, week])
n_date <- unique(DT[, date])
period <- 48

# Let's look at some data chunk of consumption and try do some regression analysis.
data_r <- DT[(type == n_type[1] & date %in% n_date[57:70])]

ggplot(data_r, aes(date_time, value)) +
  geom_line() +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Load (kW)")

# theory: GLM: Iteratively Re-weighted Least Squares (IRLS)
# theory: GAM: Penalized Iteratively Re-weighted Least Squares (P-IRLS)
# k -> (knots) is upper boundery for EDF...how smooth fitted value will be (more knots more overfit (under smoothed), less more smooth) 
# bs -> basis function..type of smoothing function
# dimension can be fixed by fx = TRUE
# EDF (trace of influence matrix) and lambda (smoothing factor) is estimated (tuned) by GCV, UBRE or REML, we will use the default GCV (Generalized Cross Validation) (more in Woods)
# basis function: I will use "cr" - cubic regression spline or "ps", which is P-spline (more in Woods).
# More options: "cc" cyclic cubic regression spline (good too with our problem), default is "tp" thin plane spline
# family - how response is fitted -> gaussian, log_norm, gamma, log_gamma is our possibilities, but gaussian is most variable in practice because gamma distibution must have only positive values
# gamm - possibility to add autocorrelation for errors

N <- nrow(data_r)
window <- N / period # number of days in the train set

matrix_gam <- data.table(Load = data_r[, value],
                         Daily = rep(1:period, window),
                         Weekly = data_r[, week_num])

gam_1 <- gam(Load ~ s(Daily, bs = "cr", k = period) +
               s(Weekly, bs = "ps", k = 7),
             data = matrix_gam,
             family = gaussian)

layout(matrix(1:2, nrow = 1))
plot(gam_1)
dev.off()

# what to look at: summary: EDF, p-values, R^2, GCV; AIC, magic
summary(gam_1)
# Parametric coefficients:
summary(gam_1)$p.table
# smooth terms:
summary(gam_1)$s.table
# R^2:
summary(gam_1)$r.sq
# GCV:
summary(gam_1)$sp.criterion
# AIC:
gam_1$aic
# BIC:
BIC(gam_1)
# Here is that "magic":
gam_1$optimizer

datas <- rbindlist(list(data_r[, .(value, date_time)],
                        data.table(value = gam_1$fitted.values, data_time = data_r[, date_time])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]

ggplot(data = datas, aes(date_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.1")

gam_2 <- gam(Load ~ s(Daily, Weekly),
             data = matrix_gam,
             family = gaussian)

summary(gam_2)

datas <- rbindlist(list(data_r[, .(value, date_time)],
                        data.table(value = gam_2$fitted.values, data_time = data_r[, date_time])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]

ggplot(data = datas, aes(date_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.2")

gam_3 <- gam(Load ~ te(Daily, Weekly,
                       bs = c("cr", "ps")),
             data = matrix_gam,
             family = gaussian)

summary(gam_3)

datas <- rbindlist(list(data_r[, .(value, date_time)],
                        data.table(value = gam_3$fitted.values, data_time = data_r[, date_time])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]

ggplot(data = datas, aes(date_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.3")

AIC(gam_2, gam_3)

gam_4 <- gam(Load ~ te(Daily, Weekly,
                        k = c(period, 7),
                        bs = c("cr", "ps")),
              data = matrix_gam,
              family = gaussian)

plot(gam_4, se = FALSE, rug = FALSE)
summary(gam_4)
# default number of knots makes fit more smoother,
# manual set of knots makes fit more overfit
# AIC = 2*k - 2*ln(L)

datas <- rbindlist(list(data_r[, .(value, date_time)],
                        data.table(value = gam_4$fitted.values, data_time = data_r[, date_time])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]

ggplot(data = datas, aes(date_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.4")

gam_5 <- gam(Load ~ s(Daily, bs = "cr", k = period) +
               s(Weekly, bs = "ps", k = 7) +
               ti(Daily, Weekly,
                  k = c(period, 7),
                  bs = c("cr", "ps")),
             data = matrix_gam,
             family = gaussian)

# choose k: https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/choose.k.html
# methods for smoothing parameter estimation "GCV.Cp", "REML" or "P-REML".
# te() produces a full tensor product smooth
# ti() produces a tensor product interaction, appropriate when the main effects (and any lower interactions) are also present.

summary(gam_5)

datas <- rbindlist(list(data_r[, .(value, date_time)],
                        data.table(value = gam_5$fitted.values, data_time = data_r[, date_time])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]

ggplot(data = datas, aes(date_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.5")

gam_6 <- gam(Load ~ t2(Daily, Weekly,
                       k = c(period, 7),
                       bs = c("cr", "ps"),
                       full = TRUE),
             data = matrix_gam,
             family = gaussian)

summary(gam_6)

datas <- rbindlist(list(data_r[, .(value, date_time)],
                        data.table(value = gam_6$fitted.values, data_time = data_r[, date_time])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]

ggplot(data = datas, aes(date_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.6")

AIC(gam_4, gam_5, gam_6)

# Seems gam_6 with t2 is best
# Look at autocorrelation of residuals

layout(matrix(1:2, ncol = 2))
acf(resid(gam_6), lag.max = 48, main = "ACF")
pacf(resid(gam_6), lag.max = 48, main = "pACF")
dev.off()

# within-group correlation structure

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

library(forecast)

arima_1 <- auto.arima(resid(gam_6_ar0$lme, type = "normalized"),
                      stationary = TRUE, seasonal = FALSE)
summary(arima_1)
arima_1$model$theta
arima_1$model$phi
arima_1$coef

gam_6_arma <- gamm(Load ~ t2(Daily, Weekly,
                            k = c(period, 7),
                            bs = c("cr", "ps"),
                            full = TRUE),
                  data = matrix_gam,
                  family = gaussian,
                  correlation = corARMA(form = ~ 1|Weekly, p = 1, q = 2),
                  method = "REML")

layout(matrix(1:2, ncol = 2))
acf(resid(gam_6_ar1$lme, type = "normalized"), lag.max = 48, main = "ACF")
pacf(resid(gam_6_ar1$lme, type = "normalized"), lag.max = 48, main = "pACF")
dev.off()

layout(matrix(1:2, ncol = 2))
acf(resid(gam_6_arma$lme, type = "normalized"), lag.max = 48, main = "ACF")
pacf(resid(gam_6_arma$lme, type = "normalized"), lag.max = 48, main = "pACF")
dev.off()

anova(gam_6_ar0$lme, gam_6_ar1$lme, gam_6_arma$lme)
anova(gam_6_ar0$lme, gam_6_ar1$lme)
# gam_6 with AR(1) correlated errors seems better than simple model, ARMA is slightly better than AR

# Different method of estimation of parameters
summary(gam_6_arma$gam)
summary(gam_6_ar1$gam)
summary(gam_6_ar0$gam)
summary(gam_6)

# Estimated \phi coefficient of AR(1) process
intervals(gam_6_ar1$lme, which = "var-cov")$corStruct
intervals(gam_6_arma$lme, which = "var-cov")$corStruct

datas <- rbindlist(list(data_r[, .(value, date_time)],
                        data.table(value = gam_6_arma$gam$fitted.values, data_time = data_r[, date_time])))
datas[, type := c(rep("Real", nrow(data_r)), rep("Fitted", nrow(data_r)))]

ggplot(data = datas, aes(date_time, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Time", y = "Load (kW)",
       title = "Fit from GAM n.6 with ARMA(1,2)")

# We are happy that we chosen the best model..lets make more exploratory analysis!

## Visualizations ----

# looks gam_6 is better than gam_6_ar1
# lets make more detailed view of residuals of these two models

summary(gam_6_arma$gam)
summary(gam_6_ar1$gam)

datas <- data.table(Fitted_values = c(gam_6_ar0$gam$fitted.values, gam_6_arma$gam$fitted.values),
                    Residuals = c(gam_6_ar0$gam$residuals, gam_6_arma$gam$residuals),
                    Model = rep(c("Gam n.6", "Gam n.6 with ARMA)"), each = nrow(data_r)))

# datas <- data.table(Fitted_values = c(gam_6_ar0$gam$fitted.values, gam_6_ar1$gam$fitted.values),
#                     Residuals = c(gam_6_ar0$gam$residuals, gam_6_ar1$gam$residuals),
#                     Model = rep(c("Gam n.6", "Gam n.6 with AR(1)"), each = nrow(data_r)))

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

# We can see some bias in gam_6_ar1 model at begging of the plot, it is not homoscedastic
# So we will stay in gam_6 model in further analysis

# two types of view on fitted surface, we can see that highets peak is when Day variable has values near 30 and Week variable has value 1 (it is monday)
vis.gam(gam_6, n.grid = 50, theta = 35, phi = 32, zlab = "",
        ticktype = "detailed", color = "topo", main = "t2(D, W)")

# 2D - topological view (contour lines), again highest value of electricity load is monday at 3:00 pm, it is very similar till thursday, than load changes alot
vis.gam(gam_6, main = "t2(D, W)", plot.type = "contour",
        color = "terrain", contour.col = "black", lwd = 2)

# plot option does very similar job, but this time is ploted effects (estimated coeffcients) of variables Daily and Weekly
plot(gam_6, rug=FALSE, se = F, n2 = 80)

# I will try to do some animated dashboard of main characteristics of model thru time.
# It should be very interesting how electricity load changes and how model can adapte to these changes.

predWeekGAM <- function(data, set_of_date){
  
  # subsetting the dataset by dates
  data_train <- data[date %in% set_of_date]
  
  # creation of training dataset
  N <- nrow(data_train)
  window <- N / period # number of days in the train set
  matrix_train <- data.table(Load = data_train[, value],
                             Daily = rep(1:period, window),
                             Weekly = data_train[, week_num])
  
  # model fit
  gam_m <- gam(Load ~ t2(Daily, Weekly,
                         k = c(period, 7),
                         bs = c("cr", "ps"),
                         full = TRUE),
               data = matrix_train,
               family = gaussian)
  
  # new data and prediction
  new.data <- matrix_train[1:(48*7), .(Daily, Weekly)]
  pred_week <- predict(gam_m, newdata = new.data, type = "response", se.fit = FALSE)
  
  return(list(GAMobj = gam_m, Pred = as.vector(pred_week)))
}

mape <- function(real, pred){
  return(100 * mean(abs((real - pred)/real))) # MAPE - Mean Absolute Percentage Error
}

n_weeks <- floor(length(n_date)/7) - 2

define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
}

saveGIF({
  oopt = ani.options(interval = 2, nmax = 50)
  for(i in 0:(n_weeks-1)){
    
    gam_pred_weeks <- predWeekGAM(DT[type == n_type[1]], n_date[((i*7)+1):((i*7)+7*2)])
    
    gam_err_mape <- mape(DT[(type == n_type[1] & date %in% n_date[(15+(i*7)):(21+(i*7))]), value],
                         gam_pred_weeks$Pred)
    
    # 1. plot of fitted values
    g1 <- ggplot(data = data.table(value = c(gam_pred_weeks$GAMobj$fitted.values,
                                             DT[(type == n_type[1]) & (date %in% n_date[(1+(i*7)):(14+(i*7))]), value]),
                                   date_time = rep(DT[(type == n_type[1]) & (date %in% n_date[(1+(i*7)):(14+(i*7))]), date_time], 2),
                                   type = rep(c("Fit", "Real"), each = length(gam_pred_weeks$GAMobj$fitted.values))),
                 aes(date_time, value, group = type, colour = type)) +
      geom_line(size = 0.8) +
      theme(panel.border = element_blank(), panel.background = element_blank(),
            panel.grid.minor = element_line(colour = "grey90"),
            panel.grid.major = element_line(colour = "grey90"),
            panel.grid.major.x = element_line(colour = "grey90"),
            title = element_text(size = 15),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12, face = "bold")) +
      labs(x = "Time", y = "Load (kW)",
           title = paste("Fit from GAM (", n_type[1], "); ", "week: ", i+1, "-", i+2, sep = ""))
    
    # 2. plot of fitted values vs. residuals
    g2 <- ggplot(data = data.table(Fitted_values = gam_pred_weeks$GAMobj$fitted.values,
                                   Residuals = gam_pred_weeks$GAMobj$residuals),
                 aes(Fitted_values, Residuals)) +
      geom_point(size = 1.7) +
      geom_smooth(method = "loess") +
      geom_hline(yintercept = 0, color = "red", size = 1) +
      theme(title = element_text(size = 14),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12, face = "bold")) +
      labs(title = "Fitted values vs Residuals")
    
    # 3. plot of forecasts
    g3 <- ggplot(data = data.table(value = c(gam_pred_weeks$Pred,
                                             DT[(type == n_type[1]) & (date %in% n_date[(15+(i*7)):(21+(i*7))]), value]),
                                   date_time = rep(DT[(type == n_type[1]) & (date %in% n_date[(15+(i*7)):(21+(i*7))]), date_time], 2),
                                   type = rep(c("GAM", "Real"), each = length(gam_pred_weeks$Pred))),
                 aes(date_time, value, group = type, colour = type)) +
      geom_line(size = 0.8) +
      theme(panel.border = element_blank(), panel.background = element_blank(),
            panel.grid.minor = element_line(colour = "grey90"),
            panel.grid.major = element_line(colour = "grey90"),
            panel.grid.major.x = element_line(colour = "grey90"),
            title = element_text(size = 15),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12, face = "bold")) +
      labs(x = "Time", y = "Load (kW)",
           title = paste("Forecast of GAM; ", "week: ", i+3, "; MAPE: ",
                         round(gam_err_mape, 2), "%", sep = ""))
    
    grid.newpage()
    # Create layout : nrow = 2, ncol = 2
    pushViewport(viewport(layout = grid.layout(2, 2)))
    # Arrange the plots
    print(g1, vp = define_region(1, 1:2))
    print(g2, vp = define_region(2, 1))
    print(g3, vp = define_region(2, 2))
    
    ani.pause()
  }}, movie.name = "industry_1_dashboard_v2.gif", ani.height = 750, ani.width = 950)


saveGIF({
  oopt = ani.options(interval = 1.1, nmax = 50)
  for(i in 0:(n_weeks-1)){
    
    data_train <- DT[type == n_type[1] & date %in% n_date[((i*7)+1):((i*7)+7*2)]]
    
    # creation of training dataset
    N <- nrow(data_train)
    window <- N / period # number of days in the train set
    matrix_train <- data.table(Load = data_train[, value],
                               Daily = rep(1:period, window),
                               Weekly = data_train[, week_num])
    
    gam_m <- gam(Load ~ t2(Daily, Weekly,
                           k = c(period, 7),
                           bs = c("cr", "ps"),
                           full  = TRUE),
                 data = matrix_train,
                 family = gaussian)
    
    sum_gam <- summary(gam_m)
    
    par(bg="white", mar=c(5, 4, 3, 2), oma=c(0,0,0,0), xpd=FALSE,
        xaxs="r", yaxs="r", mgp=c(2.8,0.3,0.5), col.lab="black", col.axis="black",
        col.main="black", cex.main=1.3, cex.axis=1.2, cex.lab=1.1,
        font.main=7, font.lab=7, font.axis=7, lend=1, tck=0)
    
    vis.gam(gam_m, main = paste(n_type[1], "; t2(D, W); EDF = ", round(sum_gam$edf, 2),
                                "; Week:", i+1, "-", i+2, sep = ""), plot.type = "contour",
            color = "terrain", contour.col = "black", lwd = 2, nCol = 150)
    
    ani.pause()
  }}, movie.name = "industry_1_vis.gif", ani.height = 450, ani.width = 550)

saveGIF({
  oopt = ani.options(interval = 1.5, nmax = 50)
  for(i in 0:(n_weeks-1)){
    
    data_train <- DT[type == n_type[1] & date %in% n_date[((i*7)+1):((i*7)+7*2)]]
    
    # creation of training dataset
    N <- nrow(data_train)
    window <- N / period # number of days in the train set
    matrix_train <- data.table(Load = data_train[, value],
                               Daily = rep(1:period, window),
                               Weekly = data_train[, week_num])
    
    gam_m <- gam(Load ~ t2(Daily, Weekly,
                           k = c(period, 7),
                           bs = c("cr", "ps"),
                           full  = TRUE),
                 data = matrix_train,
                 family = gaussian)
    
    sum_gam <- summary(gam_m)
    
    par(bg="white", mar=c(5, 4, 3, 2), oma=c(0,0,0,0), xpd=FALSE,
        xaxs="r", yaxs="r", mgp=c(2.8,0.3,0.5), col.lab="black", col.axis="black",
        col.main="black", cex.main=1.3, cex.axis=1.2, cex.lab=1.1,
        font.main=7, font.lab=7, font.axis=7, lend=1, tck=0)
    
    vis.gam(gam_m, main = paste(n_type[1], "; t2(D, W); EDF = ", round(sum_gam$edf, 2),
                                "; Week:", i+1, "-", i+2, sep = ""), ticktype = "detailed", 
            color = "topo", contour.col = "black", n.grid = 50, theta = 35, phi = 32, zlab = "",
            zlim = c(min(DT[type == n_type[1], value]) - 100, max(DT[type == n_type[1], value])-1000))
    
    ani.pause()
  }}, movie.name = "industry_1_vis_3D_v4.gif", ani.height = 520, ani.width = 550)

# interpretation of plot.gam - effect (estimated coefficients) of variables on target variable
# interpretation of vis.gam - response in values of selected variables

## Resources ----
# http://stats.stackexchange.com/questions/45446/intuition-behind-tensor-product-interactions-in-gams-mgcv-package-in-r
# http://www.fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
# http://www.sfs.uni-tuebingen.de/~jvanrij/Tutorial/GAMM.html
# Wood, S.N. (2006) Generalized Additive Models: an introduction with R, CRC

# You can see that with GAM you have many possibilities to model your target variable. This was purpose of this post to fully introduce this complex method.
# Main advantage is that you can model target variable with non linear function.
# You can statistically test significance of independent variable to target variable, which is modeled not linearly (like in MLR), so test if ind. variable has significant non linear behaviour. (Big advantage)
# Tensor product interactions - another big advantage - so you can make interactions of two variables with two differnt smoothing functions.
# For monitoring behavior thru time, for me, GAM is perfect method. Monitor significance, smoothness, EDF etc.

## Evaluation of time complexity ----
library(microbenchmark)

res1 <- microbenchmark(GAM = gam(Load ~ t2(Daily, Weekly,
                                           k = c(period, 7),
                                           bs = c("cr", "ps"),
                                           full = TRUE),
                                 data = matrix_gam,
                                 family = gaussian),
                       MLR = lm(Load ~ 0 + Daily + Weekly + Daily:Weekly, data = matrix_train),
                       times = 50L)

plot(res1)
autoplot(res1)
