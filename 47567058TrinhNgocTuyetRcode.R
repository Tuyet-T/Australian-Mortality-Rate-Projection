#install.packages("StMoMo")
library("StMoMo")
library(demography)
require(StMoMo)
Mort <- hmd.mx(country = "AUS", username = "",password = "")
#View(Mort)
Mort$age <- as.numeric(gsub("\\+","",Mort$age))
#Part a
#Latest year 2020 plotting
latest_year <- max(Mort$year)
latest_data <- extract.years(Mort,latest_year)
plot(latest_data$age,log(latest_data$rate$total),main = "Plot of mortality rates in 2020", xlab = "Age", ylab = "Log of total rate", type = "l", col = "darkgreen", lwd = 2)

# Fit the Natural cubic spline
#install.packages("splines")
library(splines)
calibration <- extract.years(Mort,2018)
validation <- extract.years(Mort,2019)

fit_spline <- function(knots, data) {
  natural_fit <- lm(calibration$rate$total~ns(calibration$age, knots = knots), weights = calibration$pop$total)
  return(natural_fit)
}
new_knots <- c(5,15,25,35,45,55,65,75,85,95)
for (i in 1:(2^length(new_knots))) {
  knots <- as.numeric(intToBits(i - 1))
  knots <- new_knots[knots == 1] }
natural_fit <- fit_spline(knots, calibration)
test_19 <- predict(natural_fit,validation)
mse <- mean((calibration$rate$total-test_19)^2)
result <- list()
result[[paste(knots, collapse = "_")]] <- list(
  Knots = knots,
  best_mse = mse,
  Test_19 = test_19
)
best_model_test <- which.min(sapply(result, function(x) x$best_mse))
best_model <- result[[best_model_test]]
best_model

#Plotting
natural_spline_log <- lm(log(calibration$rate$total)~ns(calibration$age, knots = best_model$Knots, Boundary.knots = c(4,96)), weights = calibration$pop$total) 
plot(calibration$age,log(calibration$rate$total), main = "Plot of mortality rates 2018", xlab = "Age", ylab = "Log of total rate", type = "l", col ="red", lwd=2)
lines(calibration$age, fitted(natural_spline_log), col = "blue", lwd=2)
legend(x = "topleft", legend = c("Crude rate", "Natural cubic spline"), fill = c("red","blue"))

# Smoothing
lambdas <- seq(0.0001, 2.0, by = 0.01)
best_lambda <- NULL
best_mse <- Inf
for (lambda in lambdas) {
  smooth_model <- smooth.spline(calibration$age,calibration$rate$total, spar = lambda)
  pred_smooth <- predict(smooth_model, newdata = data.frame(Age = validation$age))
  mse <- mean((validation$rate$total-pred_smooth$y)^2)
  if (mse < best_mse) {
    best_lambda <- lambda
    best_mse <- mse
  }
}
best_lambda
#Plotting
smooth_model_log <- smooth.spline(calibration$age,log(calibration$rate$total), spar = best_lambda)
plot(calibration$age,log(calibration$rate$total), main = "Plot of mortality rates 2018", xlab = "Age", ylab = "Log of total rate", type = "l", col ="red", lwd=2)
lines(calibration$age, fitted(smooth_model_log), col = "darkgreen", lwd=2)
legend(x = "topleft", legend = c("Crude rate", "Smooth spline"), fill = c("red","darkgreen"))

#Test the two splines model in the 2020 dataset
validation_20 = extract.years(Mort,2020)
natural_spline <- lm(calibration$rate$total ~ns(calibration$age, knots = best_model$Knots), weights = calibration$pop$total)
pred_20_natural <- predict(natural_spline, validation_20)
smooth_spline <- smooth.spline(calibration$age,calibration$rate$total, spar = best_lambda)
pred_20_smooth <- predict(smooth_spline, newdata = data.frame(Age = validation_20$age))

mse_natural <- mean((validation_20$rate$total - pred_20_natural)^2)
mse_smooth <-mean((validation_20$rate$total -pred_20_smooth$y)^2)
mse_natural
mse_smooth

#plotting
natural_spline_log <- lm(log(calibration$rate$total) ~ns(calibration$age, knots = best_model$Knots), weights = calibration$pop$total) 
smooth_model_log <- smooth.spline(calibration$age,log(calibration$rate$total), spar = best_lambda)
plot(validation_20$age,log(validation_20$rate$total), main = "Plot of mortality rates 2020", xlab = "Age", ylab = "Log of total rate", type = "l", col ="red", lwd=1.5)
lines(calibration$age, fitted(natural_spline_log), col = "blue", lwd = 1.5)
lines(calibration$age, fitted(smooth_model_log), col = "darkgreen", lwd = 1.5)
legend(x = "topleft", legend = c("Crude rate", "Natural cubic spline","Smoothing spline"), fill = c("red","blue","darkgreen"))

###II.d) Applying the statistical test
data_19 <- validation
##Chi-squared test 
observed <- data_19$pop$total*data_19$rate$total
pred_19_smooth <- predict(smooth_spline, newdata = data.frame(Age = data_19$age))
expected <- pred_19_smooth$y*data_19$pop$total

#chi-square test statistic
chi_squared <- sum((observed - expected)^2 / expected)
chi_squared
#degree of freefom
df <- smooth_spline$df
df
alpha <- 0.05
#critical value
critical_value <- qchisq(1 - alpha, df)
critical_value
if (critical_value > chi_squared) {
  cat("Do not reject the null hypothesis")
} else {
  cat("Reject the null hypothesis")
}
critical_value
##Standardised deviations test 
z <- length(observed)

for (i in 1:length(observed)) {
  z[i] <- (observed[i] - expected[i]) / sqrt(expected[i])
}
z
intervals <- c(-Inf,-5,-1,0,1,5,Inf)
actual <- table(cut(z, breaks = intervals, right = FALSE))
actual
probs <- diff(pnorm(intervals))
probs
z_new <-z[!is.na(z)]
expected_1 <- length(z_new) * probs
expected_1
# Perform the chi-squared test
chi_test <- sum((actual-expected_1)^2/expected_1)
chi_test
#Conclusion
if (critical_value > chi_test) {
  cat("Do not reject the null hypothesis")
} else {
  cat("Reject the null hypothesis")
}

##The cumulative deviation test 
z_CD <- (sum(observed)-sum(expected))/sqrt(sum(expected))
z_CD
#Since it is 2 sided test
if (z_CD < -1.96 && z_CD > 1.96) {
  print("Reject the null hypothesis")
} else {
  print("Do not reject the null hypothesis")
}

#Signs test
pos <- sum(z_new>0)
neg <- sum(z_new<0)
m <- length(z_new)
k <- 0
cum_prob <- 0

while (cum_prob < 0.025) {
  cum_prob <- 0
  for (j in 0:k) {
    cum_prob <- cum_prob + choose(m, j) * 0.5^m
  }
  k <- k + 1
}
if (k <= pos && pos <= m-k) {
  cat("Do not reject the null hypothesis\n")
} else {
  cat("Reject the null hypothesis\n")
}
k
#Grouping signs test
change <- sign(z_new)
G <- sum(diff(change) == -2)
k_G <- 1
cum_G <- 0

while (cum_G < 0.05) {
  cum_G <- 0 
  for (j in 1:k_G) {
    cum_G <- cum_G + (choose(pos-1, G-1)*choose(neg+1,G))/choose(m,pos)
  }
  k_G <- k_G +1
}

if (k_G < G) {
  cat("Do not reject the null hypothesis")
} else {
  cat ("Reject the null hypothesis")
}
k_G

#The serial correlations test - NOT APPLICABLE
z_bar <- sum(z_new)/m
z_bar
z_1 <- sum((z_new - z_bar)^2)/m
z_1
z_2 <- head(z_new, -1)
z_3 <- tail(z_new,-1)
z_4 <- sum((z_2 - z_bar)*(z_3-z_bar))/(m-1)
z_4
r_1 <- z_4/z_1
r_1

r_1_m <- r_1*sqrt(m)
r_1_m
#Since it is one-side test
if (r_1_m < 1.64) {
  cat("Do not reject the null hypothesis")
} else {
  cat("Reject the null hypothesis")
}

###LC & APC modelling
##Fit models
Data <- extract.years(Mort, 1921:2019)
Momo <- StMoMoData(data=Data, series = "total", type ="central")
Momo_ini <- central2initial(Momo)
# LC
LC <- lc(link = "log")
wxt <- genWeightMat(ages = 0:110, years = Momo_ini$year,clip = 3)
LC_fit <- fit(LC, data = Momo_ini, ages.fit = 0:110 , wxt = wxt)
plot(LC_fit)
#APC
APC <- apc(link = "log")
APC_fit <- fit(APC, data = Momo_ini, ages.fit = 0:110 , wxt = wxt)
plot(APC_fit)

##Test error in 2020
Data_20 <- extract.years(Mort,2020)
#View(Data_20)
LC_20 <- forecast(LC_fit,h=1)
mse_LC = mean((Data_20$rate$total - LC_20$rates)^2)
mse_LC
APC_20 <- forecast(APC_fit,h=1)
mse_APC = mean((Data_20$rate$total - APC_20$rates)^2)
mse_APC

###Model Comparision
#LC
LC_33 <- forecast(LC_fit, h = 14)
plot(LC_33)
LC_53 <- forecast(LC_fit, h = 34)
plot(LC_53)
LC_73 <- forecast(LC_fit, h = 54)
plot(LC_73)
#APC
APC_33 <- forecast(APC_fit, h = 14)
plot(APC_33)
APC_53 <- forecast(APC_fit, h = 34)
plot(APC_53)
APC_73 <- forecast(APC_fit, h = 54)
plot(APC_73)
