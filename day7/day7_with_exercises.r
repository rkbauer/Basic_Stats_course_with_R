##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd('~/Dropbox/R_course/day7') # set working directory

# install.packages("climatol")
require(climatol)
data("windfr")
rosavent(windfr,4,4,ang=-3*pi/16,main="Annual windrose")
names(windfr)

u <- windfr
names(u) <- 1:16 # seq(0,360-22.5, length.out=16)
rosavent(windfr,4,4,ang=-3*pi/16,main="Annual windrose")

## Exercise
data <- read.table('wave_height_frequencies.csv')
rosavent(data, uni="wave height [m]", start = 0.53, end = 0.2)
?rosavent

text(0.2,0.2,"WSW")
text(0.28,0.05,"SW")
text(0.42,0,"SW",xpd=T)
mtext(0.1,0.21,"FG")
##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## matrices and arrays

## defining matrices
matrix(1:10,nrow=5,ncol=2) # matrix(data,rows,columns)
matrix(1:10,5,2)           # option 2
matrix(1:10,5)             # option 3
dim(m)                     # checking matrix dimensions

# matrices with uniform values
matrix(0,5,2)              # 5x2 matrix of zeros
matrix(1,5,2)              # 5x2 matrix of ones
matrix(NA,5,2)             # 5x2 matrix of NAs

matrix(0,5)                # single column matrix

matrix(1:10,ncol=5,nrow=2) # changing arguments order
matrix(1:10,5,2,byrow=T)   # fill matrix by rows; default: byrow=F

## other options:
## a) aligning vectors
cbind(rep(0,5),rep(0,5))   # 5x2 matrix of zeros

## b) converting vectors
m <- 1:10
dim(m) <- c(5,2)

## c) arrays
array(1:10,dim=c(5,2))     # arrays
array(0, dim=c(5,2))       # 5x2 matrix of zeros
array(1:10, dim=c(5,2,3))  # multiple dimensions

##---------------------------------------

## matrix operations
m <- matrix(1:10,5,2)      # matrix(data,rows,columns)
m

## 1) changing values
## a) accessing specific elements
i <- c(2,4,8,10)
m[i]
m[i] <- 0
m

m[3,2] <- NA
m
m[8]
m[8] <- 100
m

## b) matrix wide operations
m <- matrix(rnorm(10),5,2) # matrix(data,rows,columns)
m
m <- round(m)
m

m*2 
m*m  # element by element product

## applying functions
# apply(matrix, margin, fun, ...)
# margin = 1; --> apply functions per row
# margin = 2; --> apply functions per column
# margin = c(1,2); --> apply functions per row & column
apply(m, 2, mean)
apply(m, 2, max)
apply(m, 2, sort) # sorting values

## 2) transpose matrix (changing rows and columns)
t(m)

## 3) extend matrix
cbind(m,1:5)
n <- 1:5
cbind(m,n)

## 4) naming columns and vectors
colnames(m) <- paste("col",1:2, sep="")
rownames(m) <- paste("row",1:5, sep="")
m

# caution when accessing data!
m$col1 # works only on data frames
m[,1]
m <- data.frame(m)
m
str(m) # check structure
m$col1 # works only on data frames

# converting dataframes
m <- as.matrix(m)
str(m) # check structure

## 4) plotting matrices
m
dim(m) # 5x2 matrix
image(m)

image(1:5,1:2,m) # change axes tick marks
# attention: 
# plot from the lower left margin
# rows and columns are switched!

# show indices
text(c(row(m)), c(col(m))-.25,  
     paste("[",c(row(m)), ",",c(col(m)),"]", sep=""))

# show values
text(c(row(m)), c(col(m)), m)


##---------------------------------------

# set figure margins
par(mar=c(10,5,5,6)) # mar=c(bottom, left, top, right); default:c(5, 4, 4, 2) + 0.1.

# start plotting procedure
image(1:5,1:2,m, axes=FALSE) # plot know axes!

# add axes & box
axis(1,at=1:5,lab=2:6)
axis(2,at=1:2,lab=6:5)
box()

# show indices
text(c(row(m)), c(col(m))-.25,  
     paste("[",c(row(m)), ",",c(col(m)),"]", sep=""))

# show values
text(c(row(m)), c(col(m)), m)

# add colorbar
# install.packages("fields") 
library(fields)
image.plot(m, legend.only=TRUE, col = heat.colors(12))


##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------

datasheet <- read.table('SST_data.csv', header=F, sep=',', dec=".")
head(datasheet)
z <- t(as.matrix(datasheet))
x <- 1:dim(z)[1]
y <- 1:dim(z)[2]
colorbar.colors <- tim.colors(64)

# set figure margins
par(mar=c(10,5,5,6)) # mar=c(bottom, left, top, right); default:c(5, 4, 4, 2) + 0.1.

# start plotting procedure
image(x, y, z[,180:1], col=colorbar.colors,
      xlab='time', ylab='latitude', main="average monthly SST from 2000-2010 at 335°E", axes=F)

dates <- paste(7, '/', 2000:2010, sep="")
axis(1,seq(7,length(x),12),dates)
axis(2,seq(1,180,44.5),c(-90,45,0,45,90))
box()
image.plot(zlim=range(z), legend.only=TRUE, col=colorbar.colors)

# add contour lines
# contour(x, y, z, add = TRUE, lty = 2, nlev = 20, method = "simple", labcex=1)


## Exercises
# 1. Calculate the min, mean, median, max SST of each latitude
apply(z,2,min)
apply(z,2,mean)
apply(z,2,median)
apply(z,2,max)
apply(z,2,range)

colMeans(z)
apply(z,2,quantile, probs = c(0.25, 0.5, 0.75))

##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Exercise

## image plot of wind rose data
# install.packages("fields") 
library(fields)

x <- 1:16; y <- 1:16; z <- as.matrix(data)
zrange <- range(z)
colorbar.colors <- tim.colors(64)

# set figure margins
par(mar=c(10,5,5,6)) # mar=c(bottom, left, top, right); default:c(5, 4, 4, 2) + 0.1.

# start plotting procedure
image(x, y, z, zlim=zrange, col=colorbar.colors,
      xlab='wave height', ylab='wind direction', axes=F)

# add axes & box
axis(1,x,seq(0.25,4,.25))
axis(2,y,names(data))
box()

# add colorbar
image.plot(zlim=zrange, legend.only=TRUE, col=colorbar.colors)

# add contour lines
contour(x, y, z, add = TRUE, lty = 2, nlev = 10, method = "simple")



