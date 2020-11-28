# CTD

# load data
setwd("~/Dropbox/Programming & Stats/R_course/day2") # set working directory
CTD <- read.table("CTD.csv", header=T, sep=',', dec=".") # load dataframe
head(CTD)
colnames(CTD) <- c(colnames(CTD)[1:5], "Pressure", "Conductivity", "Temp",  "Sal",  "OxySat")
head(CTD)
attach(CTD)

Depth <- Pressure -1

# 1:3 plot the profiles of temperature, salinity, conductivity and oxygen in a 2x2 plotting window
ylabel <- "depth [m]"
ylimits <- c(max(Depth),0)

par(mfrow=c(2,2))

# plot temperature profile
plot(Temp, Depth, ylim=ylimits, xlim=c(5,20),  type="l", ylab=ylabel, xlab="", axes=F)
axis(2, pos=15) # plot y-axis to the left
axis(3, pos=0) # plot x-axis above the plot
mtext("temperature [degrees C]", side=1, line=2, cex=0.8)

plot(Temp, Depth, ylim=ylimits, xlim=c(5,20),  type="l", ylab=ylabel, xlab="")


# plot salinity profile
plot(Sal, Depth, ylim=ylimits, xlim=c(10,30), type="l", ylab=ylabel, xlab="", axes=F)
axis(2, pos=10) # plot y-axis to the left
axis(3, pos=0) # plot x-axis above the plot
mtext("salinity", side=3, line=2, cex=0.8)

# plot conductivity
plot(Conductivity, Depth, ylim=ylimits, xlim=c(15,30) , type="l", ylab=ylabel, xlab="", axes=F)
axis(2, pos=15) # plot y-axis to the left
axis(3, pos=0) # plot x-axis above the plot
mtext("conductivity", side=3, line=2, cex=0.8)




# Parasite

# 1. Import Parasite.csv
setwd('~/Dropbox/R_course/day2') # Import "Fish.csv" from day1
Parasite <- read.table("Parasite.csv", header=T, sep=',', dec=".") # load dataframe
head(Parasite)
attach(Parasite)


# 2. plot the age distribution of both sexes as 2 histograms but in one window
par(mfrow=c(2,1))
hist(age[sex == "female"], freq = F, breaks = seq(0,250,25), xlab = "", main = "female")
hist(age[sex == "male"], freq = F,breaks = seq(0,250,25), xlab = "age [days]", main = "male")


# 3. redraw the age distribution as colored lines using the density and plot function
par(mfrow=c(1,1))
age.female <- density(age[sex == "female"], from=0)
age.male <- density(age[sex == "male"], from=0)

plot(age.female, type="l", xlab = "age [days]", main = "age distribution")
lines(age.male, col = "blue")


# 4. add a legend
legend("topright", lty=c(1,1), legend = c("female", "male"), col=c("black", "blue"))


# 5. create boxplots of the weight of infected and non infected organisms and both sexes
par(mfrow=c(2,1))
ylabel="weight [g]"
boxplot(weight~infection, data = subset(fish, sex == "female"), names=c("",""), main = "female", ylab=ylabel, ylim=c(0,20))
boxplot(weight~infection, data = subset(fish, sex == "male"), main="male", xlab ="infection", ylab=ylabel, ylim=c(0,20))