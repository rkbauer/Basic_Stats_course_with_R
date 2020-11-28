setwd('~/Dropbox/R_course/day1') # Import "Fish.csv" from day1
fish <- read.table("Fish.csv", header=T, sep=',', dec=".") # load dataframe
head(fish)
attach(fish)

# histograms
hist(total.length[species == "dab"], xlab = "total length [mm]", main="length distribution of dab") # absolute frequencies (counts)

hist(total.length[species == "dab"], freq = F, xlab = "total length [mm]", main="length distribution of dab") # relative frequencies (counts)

hist(total.length[species == "dab"], breaks=10, xlab = "total length [mm]", main="length distribution of dab") # plot histogram with 10 breaks

hist(total.length[species == "dab"], breaks=seq(80, 560, 5), xlab = "total length [mm]", main="length distribution of dab") # plot histogram with breaks from 80 till 560 mm (breaks width = 5mm)
box(bty="l") # plot L-like box to align axes

par(mfrow=c(3,1)) # plot rowvise by deviding the display in a "c(rows, columns)" matrix
hist(total.length[species == "dab"], xlab = "total length [mm]", main="length distribution of dab") # first histogram

hist(total.length[species == "cod"], xlab = "total length [mm]", main="length distribution of cod") # second histogram

# the aggregate function
aggregate(total.length, by = list(species=species, sex=sex), FUN="max", na.rm=T)

max.lengths <- aggregate(total.length, by = list(species=species, sex=sex), FUN="max", na.rm=T)

write.table(max.lengths, "max.lengths.csv", sep=',', row.names=F)


# the plot function
# load CTD data
setwd("~/Dropbox/R_course/day2") # set working directory
CTD <- read.table("CTD.csv", header=T, sep=',', dec=".") # load dataframe
head(CTD)
colnames(CTD) <- c(colnames(CTD)[1:5], "Pressure", "Conductivity", "Temp",  "Sal",  "OxySat")
head(CTD)
attach(CTD)

Depth <- Pressure -1

par(mfrow=c(1,1)) # single plot window
plot(Temp, Depth, ylab="depth [m]", xlab="temperature [degrees C]")

plot(Temp, Depth, type="l", ylab="depth [m]", xlab="temperature [degrees C]") # plot line instead of dots

plot(Temp, Depth, xlim = c(0, 25), ylim=c(8, 0), type="l", ylab="depth [m]", xlab="temperature [degrees C]") # plot inverse y-axis

xlabel <- "temperature [degrees C]"
ylabel <- "depth [m]"
plot(Temp, Depth, ylim=c(max(Depth), 0), type="l", xlab=xlabel, ylab=ylabel)

plot(Temp, Depth, ylim=c(max(Depth), 0), type="l", xlab=xlabel, ylab=ylabel, axes=F) # plot no axes
axis(2) # plot y-axis to the left
# axis(4) # plot y-axis to theright
# axis(1) # plot x-axis at the bottom
axis(3) # plot x-axis above the plot

text(10, 5, "text") # add text
points(15, 5, cex=3.5, col="red", pch=16)    # add a point (cex: point size; pch: point type)
lines(0:20,rep(15,21), lwd=2, lty=1) # add line
lines(0:20,rep(Depth[Temp == min(Temp)],21), lwd=2, lty=2, col="blue") # add line

# plot legend
plot(Temp, Depth, ylim=c(max(Depth), 0), type="l", xlab=xlabel, ylab=ylabel, axes=F) # plot inverse y-axis
axis(2) # plot y-axis to the left
axis(3) # plot x-axis above the plot

legend("bottom", legend = c("refline 1", "refline 2"), col=c("black", "blue"), lwd = 2)
