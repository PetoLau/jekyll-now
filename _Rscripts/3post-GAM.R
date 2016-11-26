## Post from here ----
setwd("C:\\Users\\Peter\\Downloads\\ProjektBD\\enernoc\\")

library(feather)
library(data.table)
library(ggplot2)
# library(MASS)
library(plotly)
library(animation)
# library(mgcv)

# Use feather (fast to share data) to read data.table
DT <- as.data.table(read_feather("DT_4_ind"))

# Plot aggregated time series of consumption by industry
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
# Food Sales & Storage...it is interesting that there aren't changes during holidays


# prepare DT to work with a regression model
# tranform charactres of weekdays to integers
DT[, week_num := as.integer(as.factor(DT[, week]))]

# store information of the type of consumer, date, weekday and period
n_type <- unique(DT[, type])
n_weekdays <- unique(DT[, week])
n_date <- unique(DT[, date])
period <- 48

# Let's look at some data chunk of consumption and try do some regression analysis. Pick aggregate consumption of education (schools) buildings.
data_r <- DT[(type == n_type[2] & date %in% n_date[57:70])]

ggplot(data_r, aes(date_time, value)) +
  geom_line() +
  theme(panel.border = element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"), panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Load (kW)")

# library(smooth)
# exp.f <- es(ts(data_r[week_num == 3, value], freq = 48, start = 0), model = "ANA", h = 48, cfType = "MAE", holdout = TRUE)
# ges.f <- ges(ts(data_r[week_num == 3, value], freq = 48, start = 0), orders=c(2,1), h = 48, cfType = "MAE", holdout = TRUE)
# arima.f <- auto.ssarima(ts(data_r[week_num == 3, value], freq = 48, start = 0), h = 48, cfType = "MAE", holdout = TRUE)
# arima.f
# 
# as.vector(exp.f$forecast[,1])
# koef1 <- exp.f$initialSeason
# koef2 <- exp.f$initialSeason
# exp.f$states
# HW.f <- HoltWinters(ts(data_r[week_num == 3, value], freq = 48, start = 0), beta = F)
# HW.f$coefficients[-1]
# plot(ts(HW.f$coefficients[-1]))
# lines(ts(exp.f$initialSeason), col = "red")
# lines(ts(exp.f$states[241:288,2]), col = "blue")

# Multiple linear regression (form, assumptions). Like in the previous form, we want to forecast consumption one week ahead,
# so construction of seasonal features is necessery. Let's create daily and weekly seasonal dummy variables. Form like 10001...11110000.
# Compute features to model and store it in matrix_train.

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

m_interactions <- sapply(50:55, function(x) paste("(", paste(colnames(matrix_train)[2:48], sep = "", collapse = " + "), "):",
                                                  colnames(matrix_train)[x], sep = ""))
m_interactions <- paste(m_interactions, collapse = " + ")

# Whole formula together:
frmla <- as.formula(paste(colnames(matrix_train)[1], "~", "0 +",
                          paste(colnames(matrix_train)[2:ncol(matrix_train)], sep = "", collapse = " + "),
                          "+", m_interactions))

lm_m_1 <- lm(Load ~ 0 + ., data = matrix_train)
lm_m_2 <- lm(frmla, data = matrix_train)

summary(lm_m_1)
summary(lm_m_2)
lm_m_2$terms
lm_m_2$coefficients
pred_week_2 <- predict(lm_m_2, matrix_train[1:(7*period), -1])

library(car)
ncvTest(lm_m_2) # Homoscedasticity test
qqnorm(residuals(lm_m_2), ylab="Residuals")
qqline(residuals(lm_m_2), col = "red", lwd = 2, lty = 2)
shapiro.test(residuals(lm_m_2))
plot(fitted(lm_m_2), residuals(lm_m_2))
abline(h = 0, col = "red") # Heteroscedasticity
plot(fitted(lm_m_2), abs(residuals(lm_m_2)))

# Heteroscedasticity, nonnormal variance => box-cox transform
boxcox(lm_m_2, plotit = TRUE)
lambda.y <- which.max(boxCox(lm_m_2, family="yjPower", plotit = F)$y)
lambda <- boxCox(lm_m_2, family="yjPower", plotit = F)$x[lambda.y]
depvar.transformed <- yjPower(matrix_train$Load, lambda)
plot(ts(matrix_train$Load, freq = period))
plot(ts(depvar.transformed, freq = period))

# fit model with transformed dependent variable
matrix_train$Load <- depvar.transformed
lm_m_3 <- lm(frmla, data = matrix_train)
summary(lm_m_3)

reverseYJ <- function(Y, lambda){
  return(exp(log((lambda*Y) + 1)/lambda) - 1)
}

# Diagnostics
ncvTest(lm_m_3) # Homoscedasticity test
qqnorm(residuals(lm_m_3), ylab="Residuals")
qqline(residuals(lm_m_3), col = "red", lwd = 2, lty = 2)
shapiro.test(residuals(lm_m_3))
plot(fitted(lm_m_3), residuals(lm_m_3))
abline(h = 0, col = "red") # Heteroscedasticity
plot(fitted(lm_m_3), abs(residuals(lm_m_3)))
layout(matrix(1:2, nrow = 2))
layout(1)

plot(ts(matrix_train$Load, freq = period, start = 0))
lines(ts(lm_m_3$fitted.values, freq = period, start = 0), col = "red")

pred_week_2 <- predict(lm_m_2, matrix_train[1:(7*period), -1])
pred_week_3 <- predict(lm_m_3, matrix_train[1:(7*period), -1])
plot(ts(data[, value], freq = period, start = 0), xlim = c(0, 28+7))
lines(ts(pred_week_2, freq = period, start = 28), col = "blue")
lines(ts(reverseYJ(pred_week_3, lambda), freq = period, start = 28), col = "red")

plot(ts(matrix_train$Load, freq = period, start = 0), xlim = c(0, 28+7))
lines(ts(pred_week_3, freq = period, start = 28), col = "blue")

rlm_m <- rlm(frmla, data = matrix_train, method="M", psi=psi.huber, k = 1)
summary(rlm_m)
pred_week_4 <- predict(rlm_m, matrix_train[1:(7*period), -1])
lines(ts(reverseYJ(pred_week_4, lambda), freq = period, start = 28), col = "green")

# library(kernlab)
# svr_m <- ksvm(Load ~ ., data = matrix_train, type = "eps-svr", kernel = "rbfdot", eps = 0.1, C = 10)
# plot(ts(data[, value], freq = period, start = 0))
# plot(ts(attributes(svr_m)$fitted, freq = period, start = 0), col = "red")
# 
# pred_week <- predict(svr_m, matrix_train[1:(7*period), -1])
# plot(ts(data[, value], freq = period, start = 0), xlim = c(0, 28+7))
# lines(ts(pred_week, freq = period, start = 28), col = "red")

## GAM
library(mgcv)
matrix_gam <- data.frame(Load = data[, value], Season1 = rep(1:period, window), Season2 = data[, week_num])
mod_gam <- gamm(Load ~ s(Season1, bs = "cc", k = period) + s(Season2, bs = "cc", k = 7), data = matrix_gam)
mod_gam_2 <- gamm(Load ~ s(Season1, bs = "cc", k = period) + s(Season2, bs = "cc", k = 7) +
                    s(Season1, bs = "cc", by = Season2, k = period, m = 1), data = matrix_gam)
mod_gam_3 <- gamm(Load ~ s(Season1, bs = "cc", k = period) + s(Season2, bs = "cc", k = 7) +
                    s(Season1, bs = "cc", by = Season2, k = period, m = 1) +
                    s(Season2, bs = "cc", by = Season1, k = 7, m = 1), data = matrix_gam)
mod_gam_4 <- gamm(Load ~ s(Season1, bs = "cc", k = period) +
                    s(Season2, bs = "cc", k = 7) +
                    s(Season2, bs = "cc", by = Season1, k = 7, m = 1) +
                    te(Season1, Season2, bs = rep("cc",2)), data = matrix_gam)

mod_gam_4$gam$model

summary(mod_gam$gam)
summary(mod_gam_2$gam)
summary(mod_gam_3$gam)
summary(mod_gam_4$gam)

plot(ts(matrix_gam$Load, freq = period, start = 0))
lines(ts(mod_gam$gam$fitted.values, freq = period, start = 0), col = "blue")
lines(ts(mod_gam_3$gam$fitted.values, freq = period, start = 0), col = "green")
lines(ts(mod_gam_4$gam$fitted.values, freq = period, start = 0), col = "red")

layout(matrix(1:2, ncol = 2))
acf(resid(mod_gam_4$lme), lag.max = 48*7, main = "ACF")
pacf(resid(mod_gam_4$lme), lag.max = 48*7, main = "pACF")
layout(1)

ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")
mod_gam_5 <- gamm(Load ~ s(Season1, bs = "cc", k = period) +
                    s(Season2, bs = "cc", k = 7) +
                    s(Season2, bs = "cc", by = Season1, k = 7, m = 1) +
                    te(Season1, Season2, bs = rep("cc",2)),
                  data = matrix_gam,
                  correlation = corARMA(form = ~ 1|Season2, p = 1),
                  control = ctrl)

# Comparison of GAMs
anova(mod_gam$lme, mod_gam_3$lme, mod_gam_4$lme, mod_gam_5$lme)

summary(mod_gam_5$gam)
layout(matrix(1:2, ncol = 2))
res <- resid(mod_gam_5$lme, type = "normalized")
acf(res, lag.max = 48*7, main = "ACF - AR(1) errors")
pacf(res, lag.max = 48*7, main = "pACF- AR(1) errors")

qqnorm(resid(mod_gam_5$lme, type = "normalized"), ylab="Residuals")
qqline(resid(mod_gam_5$lme, type = "normalized"), col = "red", lwd = 2, lty = 2)
shapiro.test(resid(mod_gam_5$lme, type = "normalized"))
plot(mod_gam_5$gam$fitted.values, resid(mod_gam_5$lme, type = "normalized"))
abline(h = 0, col = "red") # Heteroscedasticity

layout(matrix(1:2, ncol = 2))
plot(mod_gam_5$gam, scale = 0)
layout(1)

plot(ts(data[, value], freq = period, start = 0))
lines(ts(mod_gam_4$gam$fitted.values, freq = period, start = 0), col = "blue")
lines(ts(mod_gam_5$gam$fitted.values, freq = period, start = 0), col = "green")
lines(ts(reverseYJ(mod_gam_6$gam$fitted.values, lambda), freq = period, start = 0), col = "red")

new.data <- matrix_gam[1:(48*7), -1]
pred <- predict(mod_gam_4$gam,  newdata = new.data, type = "response", se.fit = FALSE)
lines(ts(as.vector(pred), freq= period))
plot(ts(reverseYJ(as.vector(pred),lambda), freq= period), col = "red")
lines(ts(as.vector(pred), freq= period), col = "blue")

plot(ts(data[, value], freq = period, start = 0), xlim = c(0, 28+7))
lines(ts(as.vector(pred), freq = period, start = 28), col = "blue")

# Box-Cox (YJ) with GAM
matrix_gam$Load <- depvar.transformed
mod_gam_6 <- gamm(Load ~ s(Season1, bs = "cc", k = period) +
                    s(Season2, bs = "cc", k = 7) +
                    s(Season2, bs = "cc", by = Season1, k = 7, m = 1) +
                    te(Season1, Season2, bs = rep("cc",2)),
                  data = matrix_gam,
                  correlation = corARMA(form = ~ 1|Season2, p = 1),
                  control = ctrl)

summary(mod_gam_6$gam)
layout(matrix(1:2, ncol = 1))
plot(mod_gam_6$gam$fitted.values, resid(mod_gam_6$lme, type = "normalized"))
abline(h = 0, col = "red") # Heteroscedasticity
layout(1)

qqnorm(resid(mod_gam_6$lme, type = "normalized"), ylab="Residuals")
qqline(resid(mod_gam_6$lme, type = "normalized"), col = "red", lwd = 2, lty = 2)
shapiro.test(resid(mod_gam_6$lme, type = "normalized"))