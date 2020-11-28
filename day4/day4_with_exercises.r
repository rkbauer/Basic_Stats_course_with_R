# 1.) for loop
for(i in 1:5) i         # no output shown

for(i in 1:5) print(i)  # return index

for(i in 5:1) print(i)  # inverse index

index <- c(9,2,21,3,4)  # specified index
for(i in index) print(i)

for(i in index) # multiple operations using {}
{
  x <- rnorm(i)
  print(x)
  print(sum(x^2))
}

# 2.) while loop
i <- 1
while(i <= 13){
  print(i)
  i <- i+1 
}

# repeat loop
x <- 0
repeat{
  x <- x+1
  if(x == 7) next
  print(x)
  # if condition, necessary to end (break) loop
  if(x >= 10) break
}

# Conditional Statements
x <- 5
if(x <= 99) print("x <= 99")

x <- 100
if(x <= 99){
  print("x <= 99")
}else{
  print("x > 99")
}


ifelse(x<=99, print("x <= 99"), print("x > 99"))
ifelse(x<=99, "x <= 99", "x > 99")

x <- 3
switch(x, 2+2, mean(1:10), rnorm(5))
switch(2, 2+2, mean(1:10), rnorm(5))

y <- "fruit"
switch(y,fruit="banana",vegetable="broccoli","Neither")

k <- switch(y,fruit="banana",vegetable="broccoli","Neither")
k

##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# combining loops!
setwd("~/Dropbox/R_course/day4") # set working directory
file = "DataC13.csv"
data <- read.csv(file, header=T, dec=".", sep=',') # load dataframe
head(data)
attach(data)

levels.depth <- as.numeric(levels(factor(depth)))
levels.species <- levels(data$species)
max.values <- aggregate(data$atom13C, list(species=data$species), FUN=max, na.rm=T)

for(s in levels.species)
{ 
  par(mfrow = c(2,2))
  for(d in 1:length(levels.depth))
  {
    # select data to plot
    plot.data <-subset(data,species == s & depth == levels.depth[d])
    
    # define title and y-axis limits
    Title <- paste('depth: ', levels.depth[d], ' m')
    ylimits <- c(1.06, max.values$x[max.values$species == s])
    
    # create boxplots
    boxplot(atom13C ~ regime, data = plot.data, main=Title,
            ylim = ylimits, xlab = 'nutrient regime', ylab = 'atom percentage 13C')
    # add species information
    mtext(s, NORTH<-3, line=0.2)
  }
}

# 1. don't show x-axis label on upper plots and
# 2. don't show y-axis label on right plots
# option 1. if-else
if(d > 2)
{
  if(d == 3){
    xlabel <- 'nutrient regime'
    ylabel <- 'atom percentage 13C'
  }else{
    xlabel <- 'nutrient regime'
    ylabel <- ''
  }
}else{
  if(d == 1){
    xlabel <- ''
    ylabel <- 'atom percentage 13C'
  }else{
    xlabel <- ''
    ylabel <- ''
  }
}

boxplot(atom13C ~ regime, data = plot.data,
        ylim = ylimits, xlab = xlabel, ylab = ylabel)

# option 2. vector based
xlabel <- c("", "", 'nutrient regime', 'nutrient regime')
ylabel <- c('atom percentage 13C', "", 'atom percentage 13C', "")

boxplot(atom13C ~ regime, data = plot.data,  ylim = ylimits,
        xlab = xlabel[d], ylab = ylabel[d])

# option 3. switch
xlabel <- switch(d, "", "", 'nutrient regime', 'nutrient regime')
ylabel <- switch(d,'atom percentage 13C', "", 'atom percentage 13C', "")

boxplot(atom13C ~ regime, data = plot.data,  ylim = ylimits,
        xlab = xlabel, ylab = ylabel)

boxplot(atom13C ~ regime, names=rep("",4))
mtext(1, at = 1:4, text = c("1\nn=100", "1\nn=89", "Long\nLabel 3"), line = 2)

