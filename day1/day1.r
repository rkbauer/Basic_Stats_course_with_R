seagrass <- read.table("path_to_file/seagrass.csv", header=T, sep=',', dec=".") # load dataframe

setwd("~/Dropbox/R_course/day1") # set working directory
seagrass <- read.table("seagrass.csv", header=T, sep=',', dec=".") # load dataframe

# accsess data:
seagrass # return entire table in console
View(seagrass) # open table in editor
head(seagrass) # show first 10 rows incl. header

colnames(seagrass) <- c("area", "n", "urchins") # rename columns
head(seagrass)

seagrass[,1] # selecting first column
seagrass$area # selecting column name
attach(seagrass) # load each column of dataframe as vector 
area

# subsetting data
seagrass$n[seagrass$urchins == "ja"]
n[urchins == "ja"] # seagrass density at areas with sea urchins
subset(n, urchins == "ja")
subset(seagrass, urchins == "ja" & area > 40)
which(n > 40) # index of areas where n > 40

# start plotting procedure:
boxplot(seagrass$n[seagrass$urchins == "ja"], seagrass$n[seagrass$urchins == "nein"])
attach(seagrass) # load each column of dataframe as vector
boxplot(n[urchins == "ja"], n[urchins == "nein"], data = seagrass) # boxplot of specified categories

boxplot(n~urchins, data = seagrass) # boxplot of all categories given in urchins

boxplot(n~urchins, data = seagrass, las=1) # rotate y-axis values by 90 degrees

boxplot(n~urchins, data = seagrass, las=1, xlab="sea urchins", ylab="seagrass density", names=c("available", "not available")) # set axes labels & name boxplot categories
