# 1. generate random Poisson-distributed numbers
n <- 100    # sample size
lambda <- c(0.1, 1, 2, 3, 10)

x <- rpois(n, lambda[1]) # rpois(n, lambda)

##---------------------------------------
# 2. plot associated probability density functions (PDF)
plot(0:20, dpois(0:20, lambda[1]), type="l", lwd=2, axes=F, xlim=c(0,20), ylim=c(0,1),
     xlab="Number of rare events per sample", ylab="Relative expected frequency")
axis(1,pos=0)
axis(2,pos=0)
lines(0:20, dpois(0:20, lambda[2]), lwd=2, lty=2)
lines(0:20, dpois(0:20, lambda[3]), lwd=2, lty=3)
lines(0:20, dpois(0:20, lambda[4]), lwd=2, lty=4)
lines(0:20, dpois(0:20, lambda[5]), lwd=2, lty=5)

# 3. add a legend
legend("topright", bty="n", lwd=2, lty=c(1,2,3,4,5),
       legend = c("lambda = 0.1", "lambda = 1", "lambda = 2", "lambda = 3", "lambda = 10"))

##---------------------------------------
# 4. perform Kolmogorov-Smirnov tests for Normal Distribution
n <- 100    # sample size
lambda <- c(0.1, 1, 2, 3, 10)

x <- rpois(n, lambda[5]) # rpois(n, lambda)

# H0: normal distributed data
ks.test(x, "pnorm", mean = mean(x), sd = sqrt(var(x)))
# if p-value < 0.05: reject H0, else: keep H0

# result: Poisson random numbers with lambda = 10 appear to be normal distributed!
