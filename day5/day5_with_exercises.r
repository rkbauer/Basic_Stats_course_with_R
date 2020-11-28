##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# t-test
setwd('~/Dropbox/R_course/day5') # set working directory

file <- "Clone1.csv"
datasheet <- read.table(file, header=T, sep=',', dec=".") # load dataframe
head(datasheet)
# Student's t-test
# H0: mean == 0
t.test(datasheet$growth.rate)
# if p-value < 0.05: reject H0, else: keep H0

# H0: mean == 2
t.test(datasheet$growth.rate, mu=2)
t.test(datasheet$growth.rate, mu=2, alternative="two.sided")

# H0: mean <= 2
t.test(datasheet$growth.rate, mu=2, alternative="greater")

# H0: mean >= 2
t.test(datasheet$growth.rate, mu=2, alternative="less")

# H0: mean == 3
t.test(datasheet$growth.rate, mu=3)

# changing the confidence interval
t.test(datasheet$growth.rate, mu=3, conf.level=0.99)
# more precise information cause larger intervals!

# accessing test results
results <- t.test(datasheet$growth.rate, mu=3,
                       conf.level=0.99)
names(results) # return names of test output
results$conf.int # call confidence intervals

##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Two sample t-Test

# compare means between sexes
Data = datasheet
Data$sex <- factor(Data$sex)
formula <- growth.rate~sex
ylabel <- "growth rate"
xlabel <- "sex"

boxplot(formula, data=Data, ylab=ylabel, xlab=xlabel)

# Shapiro-Wilk test for Normal Distribution
# better for small sampling sizes (<50)
# H0: normal distributed data
shapiro.test(Data$growth.rate[Data$sex == "f"])
shapiro.test(Data$growth.rate[Data$sex == "m"])
# if p-value < 0.05: reject H0, else: keep H0

# Levene's test for homogeneity of variance across groups
# H0: variances are equal (homogeneity of variance)
# H1: variances differ
install.packages('car') # install required package
library(car)            # load package
leveneTest(formula, data=Data)
# if Pr(>F) < 0.05: reject H0, else: keep H0

# Two-Sample t-test
# H0: average growth rates are equal between sexes
# H1: average growth rates differ between sexes
t.test(formula, alternative="two.sided",
       paired=F, var.equal=T, data=Data)
# if p-value < 0.05: reject H0, else: keep H0
##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# One-Way ANOVA
# H0: average growth rates are equal between sexes
anova(lm(formula, data=Data))    # version 1
summary(aov(formula, data=Data)) # version 2
# if Pr(>F) < 0.05: reject H0, else: keep H0

##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# combining results from 3 Clones
Clone.levels <- 1:3
for(i in Clone.levels){
  # load dataframe
  file <- paste("Clone", i, ".csv", sep="")
  datasheet <- read.table(file, header=T, sep=',', dec=".")
  
  # add column consisting Clone number information
  datasheet <- data.frame(datasheet, 
                          Clone=rep(i, dim(datasheet)[1]))
  
  # add all subtables to new data frame
  if(i == 1){
    daphnia <- datasheet
  }else{
    daphnia <- rbind(daphnia, datasheet) 
  }
}

summary(daphnia)
daphnia$Clone <- factor(daphnia$Clone)
summary(daphnia)

# Perform an ANOVA to compare means between sexes
Data <- daphnia
formula <- growth.rate~sex
ylabel <- "growth rate"
xlabel <- "sex"

# boxplot
boxplot(formula, data=Data, ylab=ylabel, xlab=xlabel)

# Shapiro-Wilk test for Normal Distribution
# better for small sampling sizes (<50)
# H0: normal distributed data
shapiro.test(Data$growth.rate[Data$sex == "f"])
shapiro.test(Data$growth.rate[Data$sex == "m"])
# if p-value < 0.05: reject H0, else: keep H0

# test homogeneity of variances
leveneTest(formula, data=Data)
# if Pr(>F) < 0.05: reject H0! (H0: Variances are equal)

# One-way ANOVA
# H0: average growth rates are equal between sexes
summary(aov(formula, data=Data))
# if Pr(>F) < 0.05: reject H0, else: keep H0


##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Exercises
# 1. Compare growth rates between sexes, but for each Clone separately
formula <- growth.rate~sex
ylabel <- "growth rate"
xlabel <- "sex"

Clone.levels <- 1:3
par(mfrow=c(2,2))
for(i in Clone.levels){
  # option 1: subset data
  Data <- subset(daphnia, daphnia$Clone == i)
  
  # option 2: reload data
  # file <- paste("Clone", i, ".csv", sep="")
  # Data <- read.table(file, header=T, sep=',', dec=".")
  
  Title <- paste("Clone: ", i, sep="")
  boxplot(formula, data=Data,
          main= Title, ylab=ylabel, xlab=xlabel)
}

Clone.levels <- 1:3
for(i in Clone.levels){
  # option 1: subset data
  Data <- subset(daphnia, daphnia$Clone == i)
  Title <- paste("Clone: ", i, sep="")
  print(Title)
  
  # Shapiro-Wilk test for Normal Distribution
  # better for small sampling sizes (<50)
  # H0: normal distributed data
  print(shapiro.test(Data$growth.rate[Data$sex == "f"]))
  print(shapiro.test(Data$growth.rate[Data$sex == "m"]))
  # if p-value < 0.05: reject H0, else: keep H0
  
  # test homogeneity of variances
  print(leveneTest(formula, data=Data))
  # if Pr(>F) < 0.05: reject H0! (H0: Variances are equal)
  
  # One-way ANOVA
  # H0: average growth rates are equal between sexes
  print(summary(aov(formula, data=Data)))
  # if Pr(>F) < 0.05: reject H0 (growth rates are equal)
}

##---------------------------------------
# 2. Compare growth rates between Clones, disregarding sexes
Data <- daphnia
formula <- growth.rate~Clone
ylabel <- "growth rate"
xlabel <- "Clones"

# boxplot
par(mfrow=c(1,1))
boxplot(formula, data=Data, ylab=ylabel, xlab=xlabel)

# Shapiro-Wilk test for Normal Distribution
# better for small sampling sizes (<50)
# H0: normal distributed data
shapiro.test(Data$growth.rate[Data$Clone == "1"])
shapiro.test(Data$growth.rate[Data$Clone == "2"])
shapiro.test(Data$growth.rate[Data$Clone == "3"])
# if p-value < 0.05: reject H0, else: keep H0

# test homogeneity of variances
leveneTest(formula, data=Data)
# if Pr(>F) < 0.05: reject H0! (H0: Variances are equal)

# One-way ANOVA
# H0: average growth rates are equal between Clones
summary(aov(formula, data=Data))# if Pr(>F) < 0.05: reject H0

##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# multiple pairwise comparisons
ANOVA <- aov(formula, data=Data)
ANOVA[]
plot(ANOVA)

# I. post-hoc
# a) Scheffe Test
install.packages('agricolae')
library(agricolae)
scheffe.test(ANOVA, "Clone")

# b) Tukey Test
# version 1
TukeyHSD(ANOVA) # p adj:
plot(TukeyHSD(ANOVA))

# version 2 - "General Linear Hypotheses"
library(multcomp)
TUKEY <- glht(ANOVA, linfct=mcp(Clone="Tukey"),
              interaction_average=TRUE)

summary(TUKEY)
summary(TUKEY, test=adjusted("none"))

plot(TUKEY)
confint(TUKEY) # show the confidence intervals
##---------------------------------------

# II. a priori
# a) Dunnett Contrasts, treating group 1 as control group
DUNNET <- glht(ANOVA, linfct=mcp(Clone="Dunnett"))
summary(DUNNET)
plot(DUNNET)
confint(DUNNET) # show the confidence intervals

# b) User-defined Contrasts
contrast <- rbind(c(-1,1,0),
                  c(-1,0,1))

contrast <- rbind("2 - 1"=c(-1,1,0),
                  "3 - 1"=c(-1,0,1))

USER <- glht(ANOVA, linfct=mcp(Clone=contrast))
summary(USER)

plot(USER)
confint(USER) # show the confidence intervals

results <-summary(DUNNET)
results <- summary(USER)
results # return summary
results[] # return all results as list
results$linfct # return contrast matrix
results$test$pvalues # return pvalues of comparisons

##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# creating barplots
ylabel <- "growth rate"
xlabel <- "Clones"
attach(daphnia)

# calculate average, sd, sampling size and standard error of growth rates
Means <- aggregate(growth.rate, list(Clone), mean)$x
sd <- aggregate(growth.rate, list(Clone), sd)$x
sampling_size <- aggregate(growth.rate, list(Clone), length)$x
Error <- sd/sqrt(sampling_size)

# start plotting procedure
BARPLOT <- barplot(Means, ylab=ylabel, xlab=xlabel, names=1:3, ylim=c(0,max(Means+Error)))
# assignment returns x-coordinates of barplots
BARPLOT

# draw error bars
arrows(BARPLOT, Means+Error, # starting coordinates (x,y) of arrows
       BARPLOT, Means,       # end coordinates (x,y) of arrows
       angle=90,             # angle between the arrow shaft and the arrow head
       code=1,               # arrow type
       length=0.1)           # length of arrow head

# add labels from variance analysis
text(BARPLOT, Means+1, # x and y-coordinates
     xpd = TRUE,       # allow text placement outside ylim
     c("a", "b", "b"), # text to plot
     font=2)           # font type
# 1=plain, 2=bold, 3=italic, 4=bold italic