##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# One sample tests
setwd('~/Dropbox/R_course/day5') # set working directory

# load dataframe
file <- "Clone1.csv"
datasheet <- read.table(file, header=T, sep=',', dec=".")
head(datasheet)

#### H0: mean == 0
# a) Student's t-test
t.test(datasheet$growth.rate)

# b) Wilcoxon Rank Sum Test
wilcox.test(datasheet$growth.rate) 
# if p-value < 0.05: reject H0, else: keep H0


#### H0: mean == 2
# a) Student's t-test
t.test(datasheet$growth.rate, mu=2)
t.test(datasheet$growth.rate, mu=2, alternative="two.sided")
t.test(datasheet$growth.rate, mu=2, alternative="t")

# b) Wilcoxon Rank Sum Test
wilcox.test(datasheet$growth.rate, mu=2)
wilcox.test(datasheet$growth.rate, mu=2, alternative="t")


#### H0: mean <= 2
# a) Student's t-test
t.test(datasheet$growth.rate, mu=2, alternative="g")

# b) Wilcoxon Rank Sum Test
wilcox.test(datasheet$growth.rate, mu=2, alternative="g")


#### H0: mean >= 2
# a) Student's t-test
t.test(datasheet$growth.rate, mu=2, alternative="l")

# b) Wilcoxon Rank Sum Test
wilcox.test(datasheet$growth.rate, mu=2, alternative="l")


#### H0: mean == 3
# a) Student's t-test
t.test(datasheet$growth.rate, mu=3)

# b) Wilcoxon Rank Sum Test
wilcox.test(datasheet$growth.rate, mu=3)


#### calculate confidence intervals and pseudo-median
wilcox.test(datasheet$growth.rate, mu=3, conf.int=TRUE) 


##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Two sample comparisons

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

##---------------------------------------

#### parametric tests
# a) Two-Sample t-test
# H0: average growth rates are equal between sexes
# H1: average growth rates differ between sexes
t.test(formula, alternative="two.sided",
       paired=F, var.equal=T, data=Data)
# if p-value < 0.05: reject H0, else: keep H0

# b) One-Way ANOVA
# H0: average growth rates are equal between sexes
anova(lm(formula, data=Data))    # version 1
summary(aov(formula, data=Data)) # version 2
# if Pr(>F) < 0.05: reject H0, else: keep H0

##---------------------------------------

#### non-parametric tests
# a) Two sample Wilcoxon Rank Sum Test (= Mann-Whitney U-Test)
# H0: average growth rates are equal between sexes
# H1: average growth rates differ between sexes
wilcox.test(formula, alternative="two.sided",
       paired=F, var.equal=T, data=Data, conf.int=T)
# if p-value < 0.05: reject H0, else: keep H0

# b) Kruskal-Wallis Rank Sum Test (One-Way non-parametric ANOVA)
# H0: average growth rates are equal between sexes
kruskal.test(formula, data=Data)
# if Pr(>F) < 0.05: reject H0, else: keep H0

##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Exercise

setwd('~/Dropbox/R_course/day6') # set working directory
# load dataframe
file <- "soybean1.csv"
datasheet <- read.table(file, header=T, sep=',', dec=".")
head(datasheet)
attach(datasheet)
formula = Height~Light

boxplot(formula, ylab="Height", xlab="Light")

# Shapiro-Wilk test for Normal Distribution
# better for small sampling sizes (<50)
# H0: normal distributed data
shapiro.test(subset(Height, Light == "Green"))
shapiro.test(subset(Height, Light == "Red"))
# if p-value < 0.05: reject H0, else: keep H0

# Levene's test for homogeneity of variance across groups
# H0: variances are equal (homogeneity of variance)
# H1: variances differ
install.packages('car') # install required package
library(car)            # load package
leveneTest(formula, data=datasheet)
# if Pr(>F) < 0.05: reject H0, else: keep H0

wilcox.test(formula, alternative="two.sided",
            paired=F, var.equal=T, data=Data, conf.int=T)

install.packages('exactRankTests')
library(exactRankTests)

wilcox.exact(formula, alternative="t",
            paired=F, var.equal=T, data=Data, conf.int=T)

##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd('~/Dropbox/R_course/day6') # set working directory
# load dataframe
file <- "soybean2.csv"
datasheet <- read.table(file, header=T, sep=',', dec=".")
head(datasheet)
attach(datasheet)
formula = Height~Light

boxplot(formula, ylab="Height", xlab="Light")

# Shapiro-Wilk test for Normal Distribution
# H0: normal distributed data
shapiro.test(subset(Height, Light == "Green"))
shapiro.test(subset(Height, Light == "Red"))
shapiro.test(subset(Height, Light == "Yellow"))
# if p-value < 0.05: reject H0, else: keep H0

# Levene's test for homogeneity of variance across groups
# H0: variances are equal (homogeneity of variance)
# H1: variances differ
install.packages('car') # install required package
library(car)            # load package
leveneTest(formula, data=datasheet)
# if Pr(>F) < 0.05: reject H0, else: keep H0

# Kruskal-Wallis Rank Sum Test (non-parametric ANOVA)
# H0: average growth rates are equal between sexes
# option 1:
kruskal.test(formula, data=datasheet)

# option 2:
Red <- subset(Height, Light == "Red")
Green <- subset(Height, Light == "Green")
Yellow <- subset(Height, Light == "Yellow")

kruskal.test(list(Red, Green, Yellow))
# if Pr(>F) < 0.05: reject H0, else: keep H0

##---------------------------------------
# post hoc tests
# non parametric tests

# method a)
wilcox.test(Red, Green)
wilcox.test(Red, Yellow)
wilcox.test(Yellow, Green)

pvalues <- rep(NA, 3)
pvalues[1] <- wilcox.test(Red, Green)$p.value
pvalues[2] <- wilcox.test(Yellow, Green)$p.value
pvalues[3] <- wilcox.test(Yellow, Red)$p.value

p.adjust(pvalues, method="bonf")

# method b)
pairwise.wilcox.test(Height, Light, p.adj = "bonf")

# ties: One or more equal values or sets of equal values in the data set.

# method c)
pvalues <- rep(NA, 3)
pvalues[1] <- wilcox.exact(Red, Green, paired=F)$p.value
pvalues[2] <- wilcox.exact(Yellow, Green)$p.value
pvalues[3] <- wilcox.exact(Yellow, Red)$p.value

p.adjust(pvalues, method="bonf")

# further options:
# wilcox_test from the "coin"-package

##---------------------------------------

# recall options for parametric post hoc tests
ANOVA <- aov(formula, data=datasheet)
summary(ANOVA)

# option 1: Tukey comparisons
# method a)
library(multcomp)
TUKEY <- glht(ANOVA, linfct=mcp(Light="Tukey"),
              interaction_average=TRUE)
summary(TUKEY)

# method b)
TukeyHSD(ANOVA)

# option 2: pairwise t-tests
pairwise.t.test(Height, Light, p.adj = "bonf")

##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# repeated measures analyses
# Example 1: Sea urchins

setwd('~/Dropbox/R_course/day6') # set working directory
file <- "Seeigel_abhaengigeStichproben.csv"
datasheet <- read.csv(file, header=T, dec=".", sep=',')
head(datasheet)

Data <- data.frame(grass_t0=datasheet$grass_t0, grass_1month=datasheet$grass_1month)
Data <- stack(Data) # rearrange dataframe

# comparing two repeated samplings: paired t-test
formula = values~ind
t.test(formula, alternative="two.sided",
       paired=T, var.equal=T, data=Data)

wilcox.exact(formula, alternative="two.sided",
             paired=T, var.equal=T, data=Data, conf.int=T)

##---------------------------------------
# comparing three samples: repeated measures ANOVA
Data2 <- data.frame(datasheet$grass_t0, datasheet$grass_1month, datasheet$grass_1year)
Data2 <- stack(Data2) # rearrange dataframe

attach(Data2)
Gebiet <- rep(1:length(datasheet$Gebiet),3)

# a) parametric repeated measures ANOVA
ANOVA.rep <- aov(values~ind + Error(Gebiet/ind))
summary(ANOVA.rep)

pairwise.t.test(values, ind, p.adj = "bonf", paired=T)

# b) non parametric repeated measures ANOVA
friedman.test(values ~ ind | Gebiet)

pairwise.wilcox.test(values, ind, p.adj = "bonf", paired=T)

##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# repeated measures analyses
# Exercise: Coral Coverage

setwd('~/Dropbox/R_course/day6') # set working directory
file <- "Coral_Coverage.csv"
datasheet <- read.table(file, header=T, sep=',', dec=".") # load dataframe

head(datasheet)
datasheet <- stack(datasheet)
head(datasheet)

transect <- rep(1:50,9)

attach(datasheet)

boxplot(values~ind, data=datasheet, names=1996:2004,
        ylab="coverage [%]", xlab="year")

# Levene's test for homogeneity of variance across groups
# H0: variances are equal (homogeneity of variance)
# H1: variances differ
install.packages('car') # install required package
library(car)            # load package
leveneTest(values~ind, data=datasheet)
# if Pr(>F) < 0.05: reject H0, else: keep H0


# a) parametric repeated measures ANOVA
ANOVA.rep <- aov(values ~ ind + Error(transect/ind), data=datasheet)
summary(ANOVA.rep)

pairwise.t.test(values, ind, p.adj = "bonf", paired=T, data=datasheet)

# b) non parametric repeated measures ANOVA
friedman.test(values ~ ind | transect, data=datasheet)

pairwise.wilcox.test(values, ind, p.adj = "bonf", paired=T, data=datasheet)

##---------------------------------------
# self made comparisons
library(multcomp)
values <- datasheet$values
groups <- factor(datasheet$ind)
group.levels <- levels(groups)

n <- 1:length(group.levels)
names(n) <- group.levels

contrasts <- contrMat(n, type = "Sequen") # Sequen contrasts
pvalues <- rep(NA, length(contrasts[,1]))  # creates vector for pvalue assignment

for (i in 1:length(pvalues))
{
  x <- subset(values, groups == group.levels[which(contrasts[i,] == 1)])
  y <- subset(values, groups == group.levels[which(contrasts[i,] == -1)])
  post.hoc.test.results <- t.test(y,x,alternative="two.sided", paired=T)
  pvalues[i] <- as.numeric(post.hoc.test.results$p.value)
}

pvalues <- p.adjust(pvalues, method="bonf")
cbind(contrasts,pvalues)