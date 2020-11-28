n <- 100                           # sample size
x <- rnorm(n, mean=24, sd=4)       # normal distribution

Range <- seq(10,40,by=1)           # define groups of values
f <- dnorm(Range, mean=24, sd=4)   # prob. density function
sum(f)                             # sum of f = 1

# start plotting procedure
# prepare data for plotting
f <- f*n  # multiply densities by number of oberservations

hist(x, breaks = Range, col="grey")
# add red line showing the probability density function
lines(10:40, f, col="red", lwd = 2)


Y <- seq(0,12,0.05)    # define range/groups of values

# genreate different probability density functions (PDF)
A <- dnorm(Y,mean=4,sd=1) 
B <- dnorm(Y,mean=8,sd=1)
C <- dnorm(Y,mean=8,sd=0.5)

plot(Y, A,                         # plot first PDF
     type="l",                     # plotting line
     lwd = 2,                      # line width (default=1)
     ylim=c(0,1),                  # Range for the y-axis
     ylab = "relative frequency",  # label for the y-axis
     xlab = "Y",                   # label for the x-axis
     font.lab = 3)                 # font type
# font type: 1=plain text, 2=bold, 3=italic, 4=bold italic

lines(Y, B, lwd = 2, lty = 2)      # add second PDF                         
lines(Y, C, lwd = 2, lty = 3)      # add third PDF
# Line type: 0=blank, 1=solid, 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash
           
# add PHD label
text(3,0.38, "A", font = 2)        
text(10,0.2, "B", font = 2)
text(9,0.8,  "C", font = 2)
# font type: 1=plain text, 2=bold, 3=italic, 4=bold italic

# add legend
legend("topleft",
       legend = c("A", "B", "C"),# Legend labels 
       text.width = 1,           # legend width (default=1)
       lwd = 2,                  # line width (default=1)
       lty=c(1,2,3),             # Line types
       bty="n") # box type: "n" no box; "o" show box             
# Line types: 0=blank, 1=solid, 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash

# add PHD information
fontsize = 0.9
text(4, 0.5,                 # text coordinates (x,y)
     "mean = 4 ; sigma = 1", # text to be written
     col="red",              # text color
     cex=fontsize,           # text size (default=1)
     font= 2)                # font type
# font types: 1=plain text, 2=bold, 3=italic, 4=bold italic

# text(4,0.5, expression(paste("µ = 4 ; ", sigma, " = 1")), col="red", cex=fontsize, font=2)

text(11, 0.35, "mean = 8 ; sigma = 1", col="red", cex=fontsize, font= 2)
text(10.5, 0.65, "mean = 8 ; sigma = 0.5", col="red", cex=fontsize, font= 2)


# saving plots
setwd("path/where/to/save/the plots")

# save files as pixel graphic
bmp('plot.bmp',
    res = 300,     # plot resolution in dpi
    width = 2220,  # plot width according to defined units
    height = 1220, # plot height according to defined units
    units = "px")  # units of width and height; 
# default units="px" (pixels);
# other options: "in" (inches), "cm" or "mm"

plot(1:10, 51:60)  # dummy plot
dev.off()          # closes graphic device

# other pixel graphic formats:
# jpeg('plot.jpeg', ..)
# png('plot.png', ..)
# tiff('plot.tiff', ..)


# save files as vector graphic
setEPS()  #  set defaults appropriate for publication
postscript("plot.eps",
           width = 7,  # plot width in inches (default=7)
           height = 7) # plot height in inches (default=7)

plot(1:10, 51:60)      # dummy plot
dev.off()              # closes graphic device


# Kolmogorov-Smirnov test
## ks.test(x, y, ...,
##        alternative = c("two.sided", "less", "greater"),
##        exact = NULL)

## y:  either a numeric vector of data values, or a character string naming
## a cumulative distribution function or
## an actual cumulative distribution function such as pnorm.

## The possible values "two.sided", "less" and "greater" of alternative
## specify the null hypothesis that the true distribution function of x
## is equal to, not less than or not greater than the hypothesized
## distribution function (one-sample case) or the distribution function
## of y (two-sample case), respectively. 

n <- 1000      # sample size
x <- rnorm(n)  # normally distributed numbers

# Kolmogorov-Smirnov test for normal distribution
ks.test(x, "pnorm", mean = mean(x), sd = sqrt(var(x)))

n <- 1000      # sample size
y <- runif(n, min = -3, max = 3) # uniformly distributed numbers

# Kolmogorov-Smirnov test for uniform distribution
ks.test(y, "punif", min = -3, max = 3)

# two-samples of the same distribution?
par(mfrow=c(2,1))
hist(x, main="normally distributed values", xlim=c(-3,3))
hist(y, main="uniformly distributed values", xlim=c(-3,3))

ks.test(x, y) # reject HO (data from the same distribution) if p-value < 0.05


# other tests
# Shapiro-Wilk test
# H0: normal distributed data
shapiro.test(x)     # better for small sampling size (<50)
# if p-value < 0.05: reject H0, else: keep H0
