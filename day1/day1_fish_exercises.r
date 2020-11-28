#                             fish exercise - day 1
# 1. Import "Fish.csv" (mind not available values!)
setwd('~/Dropbox/R_course/day1')
fish <- read.table("Fish.csv", header=T, sep=',', dec=".") # load dataframe
head(fish)
attach(fish)

# 2. Which was the maximum, which was the minimum size of each caught species?
range(total.length[species == "dab"])
range(total.length[species == "cod"])

# 3. Create box-plots of the length distribution of both species and both sexes
boxplot(total.length[species == "dab"]~sex[species == "dab"], data = fish, las=1, names = c("male", "female"), ylab="total length [mm]", main = "dab")

boxplot(total.length[species == "cod"]~sex[species == "cod"], data = fish, las=1, names = c("male", "female", "juvenile"), ylab="total length [mm]", main = "cod")

# 4. Create species specific box-plots of the full, empty and liver weight
boxplot(full.weight[species == "dab"], empty.weight[species == "dab"], liver.weight[species == "dab"], data = fish, las=1, names = c("full", "empty", "liver"), ylab="weight [g]", main = "dab")

boxplot(full.weight[species == "cod"], empty.weight[species == "cod"], liver.weight[species == "cod"], data = fish, las=1, names = c("full", "empty", "liver"), ylab="weight [g]", main = "cod")

# 5. Define a new vector: Hepta Somatic Index
HSI <- liver.weight*100/full.weight

# 6. What is the median & the range of the HSI per species?
median(HSI[species == "dab"], na.rm=T)
median(HSI[species == "cod"], na.rm=T)

range(HSI[species == "dab"], na.rm=T)
range(HSI[species == "cod"], na.rm=T)