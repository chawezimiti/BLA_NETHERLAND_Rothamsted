
data <- read.csv("maize.csv")
head(data)

summastat(data$N)

summastat(log(data$Yield)) 



x<-data$N
y<-data$Yield
#expl.boundary(x,y,10,100) # may take a bit more time to run.

# 4. Fitting boundary model to the data using the Binning methodology. The function
#    BOLIDES() will be used. For more information on the function run ?BOLIDES()

bolides(x,y,model = "explore", pch=16)
startValues("lp",5) # Run this twice to get the upper and lower linear components

theta<-c(-11,112,9.2) # vector of three consists of intercept, slope, plateau yield, 
# intercept2 and slope2

bolides(x,y, theta = theta,model = "lp",xlab=expression("N/ %"), 
        ylab=expression("Yield/ t ha"^-1), pch=1)




head(data)
rnorm(20,2,0.7)
plot(data$N, data$Yield)

# 1. soil N

data <- read.csv("maize.csv")
set.seed(127) # 125 and 127 is good
N <- rnorm(35,0.22,0.1)
N2 <-  rnorm(45,0.22,0.1)

data$N[48:82] <- abs(rnorm(35,0.22,0.1))

data$N[103:148] <- abs(rnorm(46,0.22,0.1))


x<-data$N
y<-data$Yield


bolides(x,y,model = "explore", pch=16)

# 2. Soil P

x<-data$pH
y<-data$Yield
#expl.boundary(x,y,10,100) # may take a bit more time to run.

# 4. Fitting boundary model to the data using the Binning methodology. The function
#    BOLIDES() will be used. For more information on the function run ?BOLIDES()

bolides(x,y,model = "explore", pch=16, xlab="pH")


## All plots

bolides(data$K,data$Yield,model = "explore", pch=16, xlab="K", ylab="yield")
bolides(data$N,data$Yield,model = "explore", pch=16, xlab="N", ylab="yield")
bolides(data$pH,data$Yield,model = "explore", pch=16, xlab="pH", ylab="yield")

### adding other points
set.seed(127)

pH <- c(data$pH, runif(40, min = 5, max = 6.5))
N <- c(data$N, runif(40, min = 0.28, max = 0.4))
K <- c(data$K, runif(40, min = 1.5, max = 2.8))
Yield <- c(data$Yield, runif(40, min = 1, max = 7.8))
dat <- data.frame(N,K,pH,Yield)

bolides(K,Yield, model = "explore", pch=16, xlab="K", ylab="yield")
bolides(N,Yield, model = "explore", pch=16, xlab="N", ylab="yield")
bolides(pH,Yield, model = "explore", pch=16, xlab="pH", ylab="yield")

#write.csv(dat, "maize_data.csv", row.names = F)

summastat(N)
summastat(K)
summastat(pH)
summastat(Yield)

maize <- read.csv("https://raw.githubusercontent.com/chawezimiti/BLA_NETHERLAND_Rothamsted/refs/heads/main/maize_data.csv")

head(maize)







