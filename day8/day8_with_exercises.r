setwd('~/Dropbox/R_course/day8') # set working directory

pikes <- read.table('pike-perch.csv', header=T, sep=',', dec=".")
head(pikes)
attach(pikes)

cor(length,weight) # return correlation coefficient only
cor.test(length,weight,method="pearson", conf.level = 0.95)

cor(length,weight,method="spearman") # return correlation coefficient only
cor.test(length,weight,method="spearman", conf.level = 0.95)

cor(length,weight,method="kendall") # return correlation coefficient only
cor.test(length,weight,method="kendall", conf.level = 0.95)

##---------------------------------------

## plot data
plot(weight~length,  # plot(length, weight)
     ylab="weight [g]", xlab="length [mm]", main="Sander lucioperca")
mean(weight)
# points(mean(length),mean(weight),pch=3,cex=2)
abline(mean(weight),0)
arrows(500,500,480,mean(weight)-20, lwd=2)# draw an arrow: arrows(x1,y1,x2,y2)
text(530,500, "mean weight")

# calculate total sums of squares (SST)
segments(length,mean(weight),length, weight) # show residuals
SST <- sum((weight-mean(weight))^2)


## linear regression model
plot(weight~length,  # plot(length, weight)
     ylab="weight [g]", xlab="length [mm]", main="Sander lucioperca")
lmodel <-lm(weight~length)
abline(lmodel) # add regression line

# calculate error sums of squares (SSE)
segments(length,fitted(lmodel),length, weight) # show residuals
SSE <- sum(residuals(lmodel)^2)

# calculate regression sums of squares (SSR),
# variation that is explained by the model
SSR <- sum((mean(weight)-predict(lmodel))^2)

c(SST, SSE+SSR) # verify that SST=SSE+SSR

# perform ANOVA to test whether our slope 
# is significantly different from 0!
F <- (SSR/1)/(SSE/lmodel$df) # regression variance/error variance
F

# F-Value: 
# return critical F-Value at significance level:
# qf(significance-level, Effect df, Error df)
qf(0.95, 1, length(weight)-2)

# sqrt(F) should be the (absolute) value of the t-statistic
sqrt(F)

# all in one solution:
summary(lmodel)
# Residuals, Coefficients & Correlation coefficients with statistics
# t-tests:
# H0: coeffcients == 0
# if p-value < 0.05: reject H0, else: keep H0

##---------------------------------------

# how to get data from summary
summary(summary(lmodel))

# receive intercept and slope from model summary
summary(lmodel)["coefficients"]
coef(lmodel)

intercept <- round(coef(lmodel)[1],2)
slope <- round(coef(lmodel)[2],2)

# replot data
plot(weight~length, xlim=c(0,1000), ylim=c(-3000,3000)) # plot(length, weight)
abline(lmodel) # add regression line
text(550,-2000, paste("weight = ", intercept, "+ ", slope, "*length", sep=""))

# summary(summary(lmodel))
r2 <- as.numeric(summary(lmodel)["r.squared"])
r2  # same as SSR/SST:
SSR/SST # percentage of total variation that can be explained by the model
sqrt(r2) # same as pearson-correlation coefficient:
cor(length,weight) # return correlation coefficient only
cor.test(length,weight,method="pearson", conf.level = 0.95)


## prediction bands
pred.interval <- predict(object = lmodel, interval = "prediction", level=0.95)
matlines(pred.interval)

## confidence bands
conf.interval <- predict(object = lmodel, interval = "confidence", level=0.95)
matlines(conf.interval)
# prediction bands enclose area with 95% of all data points
# confidence bands enclose area of regression line

##---------------------------------------

# testing initial assumptions
resid.values <- residuals(lmodel) # same as resid(lmodel)

# 1.) graphical way
boxplot(resid.values, main="Boxplot of Residuals", ylab="Residuals")
qqnorm(resid.values); qqline(resid.values, col="red")

par(mfrow=c(2,2))
plot(lmodel)
# a) Fitted values vs Residuals (Is there a trend)
# b) Normal Q-Qplot, testing normal distribution (all values on a straight line?)
# c) Scale location plots: constant variance vs. trend?
# d) Leverage & Cook's distance:
#   how influential individual data points were in estimating
#   the parameters of the regression line?
#   a) Leverage statistics: 0:1 (0: no influence)
#                        values > 0.5 = outliers
#   Influential points (outliers=extreme values) are marked
#   with their index number
#
#   b) Cook's distance:
#   dashed lines mark critical distance

# 2.) 
## Levene's test for homogeneity of variance across groups
# H0: variances are equal (homogeneity of variance)
# H1: variances differ
# install.packages('car') # install required package
library(car)            # load package
leveneTest(resid.values, pikes$weight)
# if Pr(>F) < 0.05: reject H0, else: keep H0

## Kolmogorov-Smirnov tests for Normal Distribution
# H0: normal distributed data
ks.test(resid.values, "pnorm", mean = mean(resid.values), sd = sqrt(var(resid.values)))
# if p-value < 0.05: reject H0, else: keep H0

# ## Shapiro-Wilk test for Normal Distribution
# # better for small sampling sizes (<50)
# # H0: normal distributed data
# shapiro.test(resid.values)
# # if p-value < 0.05: reject H0, else: keep H0

##---------------------------------------

## Exercise
detach(pikes)

par(mfrow=c(1,2))
for(i in 1:2)
{
  if (i == 1)
  {dataset <- subset(pikes, length < 500)
    }else{dataset <- subset(pikes, length > 500)}
  plot(weight~length, data=dataset) # plot(length, weight)
  print(paste("testing subsample", i))
  lmodel <-lm(weight~length, data=dataset)
  abline(lmodel) # add regression line
  
  print(summary(lmodel))
  resid.values <- residuals(lmodel) # same as resid(lmodel)
  print(leveneTest(resid.values, dataset$weight))
  print(ks.test(resid.values, "pnorm", mean = mean(resid.values), sd = sqrt(var(resid.values))))
  print(shapiro.test(resid.values)) 
}

##---------------------------------------
## Exercise
cod <- read.table('cod.csv', header=T, sep=',', dec=".")
head(cod)
attach(cod)