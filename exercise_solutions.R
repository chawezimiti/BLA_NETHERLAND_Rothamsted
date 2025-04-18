
########################### EXERCISE SOLUTION ############################################


# Load in packages

library(aplpack)
library(BLA)

# Read in the data into your R session

maize <- read.csv("https://raw.githubusercontent.com/chawezimiti/BLA_NETHERLAND_Rothamsted/refs/heads/main/maize_data.csv")

head(maize)


### 1. NITROGEN ##########################################################################

x<- maize$N
y <- maize$Yield

### 1. Normality test---------------------------------------------------------------------

summastat(x)
summastat(y)

### 2. Outliers---------------------------------------------------------------------------

dat <- data.frame(x,y)

dat[which(is.na(dat$x)==T),1]<-mean(dat$x,na.rm=T) 

bag <- bagplot(dat)

new_data <- rbind(bag$pxy.bag, bag$pxy.outer)

x <- new_data[,1]
y <- new_data[,2]

### 3. Test for evidence of boundary------------------------------------------------------

expl_boundary(x,y)

### 4. Fit the boundary line--------------------------------------------------------------

### 4.1 Bolides===========================================================================

bolides(x,y, model = "explore")

# a) linear-plateau model

startValues("lp")
start = c(-0.04, 50,9)
bolides(x,y, model = "lp", start = start, xmax = 0.45)

# b) logistic model

bolides(x,y, model = "explore")

startValues("logistic",8)
start = c(0.16, 0.3,8.9)
bolides(x,y, model = "logistic", start = start, xmax = 0.45)


### 4.2 Binning---------------------------------------------------------------------------

bins <- c(min(x), max(x), (max(x) - min(x))/10)

blbin(x,y, bins = bins, tau = 0.99, model = "explore")

# a) linear-Plateau model

startValues("lp")
start = c(2.95, 32.06, 8.6)
blbin(x,y, bins = bins, tau = 0.99, model = "lp", start = start, xmax = 0.45)

# b) logistic model

blbin(x,y, bins = bins, tau = 0.99)

startValues("logistic",8)
start = c(0.17, 0.3,8.5)
blbin(x,y, bins = bins, tau = 0.99, model = "logistic", start = start, xmax = 0.45)


### 4.3 Quantile regression---------------------------------------------------------------

# a) linear-plateau model

plot(x,y)
startValues("lp")
start = c(-0.50, 52.97,8.55)

blqr(x,y, tau =0.99, model = "lp", start = start)


# b) logistic model

plot(x,y)
startValues("logistic",8)
start = c(0.17, 0.3,8.5)

blqr(x,y, tau =0.95, model = "logistic", start = start)


### 4.4 Censored model====================================================================

# a) linear-plateau model

data <- data.frame(x,y)

plot(data)

startValues("lp")

start <- c(-2.3,52,9.5, 0.25,  4.03,  0.08,  2.30,  0.22)

start <- c(-0.41,49.35,8.33, 0.25,  4.03,  0.08,  2.30,  0.22)

start<-c(-0.88, 54.6, 8.22, 0.25,  4.03,  0.08,  2.30,  0.22)

start<-c(-0.74, 53.15, 8.56, 0.25,  4.03,  0.08,  2.30,  0.22)

cbvn(data, model="lp", start = start, sigh = 0.3, optim.method = "Nelder-Mead")


#### 2. POTASIUM ###########################################################################


x<- maize$K
y <- maize$Yield

plot(x,y)

### 1. Normality test=====================================================================

summastat(x)
summastat(y)

### 2. Outliers===========================================================================

dat <- data.frame(x,y)

dat[which(is.na(dat$x)==T),1]<-mean(dat$x,na.rm=T) 

bag <- bagplot(dat)

new_data <- rbind(bag$pxy.bag, bag$pxy.outer)

x <- new_data[,1]
y <- new_data[,2]

### 3. Test for evidence of boundary======================================================

expl_boundary(x,y)

### 4. Fit the boundary line==============================================================

### 4.1 Bolides---------------------------------------------------------------------------

bolides(x,y, model = "explore")

# a) linear-plateau model

startValues("lp")
start = c(4.4, 6.20,8.9)
bolides(x,y, model = "lp", start = start, xmax = 3)

# b) logistic model

bolides(x,y, model = "explore")

startValues("logistic",8)
start = c(0.87, 1.5,9.23)
start = c(1.17, 2, 9.02)

bolides(x,y, model = "logistic", start = start, xmax = 3)


### 4.2 Binning---------------------------------------------------------------------------

bins <- c(min(x), max(x)+0.1, (max(x) - min(x)+0.1)/10)

# a) linear-plateau model

blbin(x,y, bins = bins, tau = 0.95)

startValues("lp")
start = c(4.09, 6.03,8.5)
blbin(x,y, bins = bins, tau = 0.99, model = "lp", start = start, xmax = 3)

# b) logistic model

blbin(x,y, bins = bins, tau = 0.95)

startValues("logistic",8)
start = c(0.87, 1.5,8.59)

blbin(x,y, bins = bins, tau = 0.99, model = "logistic", start = start, xmax = 3)

blbin(x,y, bins = bins, tau = 0.95, model = "logistic", start = start, xmax = 3)

### 4.3 Quantile regression---------------------------------------------------------------

# a) linear-plateau model

plot(x,y)

startValues("lp")
start = c(3.86, 5.77, 8.83)
start = c(3.4, 4.5, 8.03)

blqr(x,y, tau = 0.99, model = "lp", start = start)

# b) logistic model

plot(x,y)

startValues("logistic",8)
start = c(0.95, 2,8.89)
blqr(x,y, tau =0.95, model = "logistic", start = start)


### 4.4 Censored model====================================================================

# a) linear-plateau model

data <- data.frame(x,y)

plot(data)

startValues("lp")

start <- c(2.92,7.5,8.6, mean(x),  mean(y),  sd(x),  sd(y),  cor(x,y))

start <- c(3.62,5.6,9.2, mean(x),  mean(y),  sd(x),  sd(y),  cor(x,y))

cbvn(data, model="lp", start = start, sigh = 0.3, optim.method = "Nelder-Mead")



#### 3. SOIL pH #############################################################################


x<- maize$pH
y <- maize$Yield

plot(x,y)

### 1. Normality test=====================================================================

summastat(x)
summastat(y)

### 2. Outliers

dat <- data.frame(x,y)

dat[which(is.na(dat$x)==T),1]<-mean(dat$x,na.rm=T) 

bag <- bagplot(dat)

new_data <- rbind(bag$pxy.bag, bag$pxy.outer)

x <- new_data[,1]
y <- new_data[,2]

### 3. Test for evidence of boundary======================================================

expl_boundary(x,y)

### 4. Fit the boundary line==============================================================

### 4.1 Bolides---------------------------------------------------------------------------

bolides(x,y, model = "explore")

# a) linear-plateau model

startValues("lp")
start = c(-23.38, 6.26,8.83)
bolides(x,y, model = "lp", start = start)

# b) logistic model

startValues("logistic",8)
start = c(5.08, 5.7,9.2)
start = c(5.11, 5.8, 9.23)

bolides(x,y, model = "logistic", start = start)


### 4.2 Binning---------------------------------------------------------------------------

bins <- c(min(x), max(x)+0.1, (max(x) - min(x)+0.1)/10)

# a) linear-plateau model

blbin(x,y, bins = bins, tau = 0.99)

startValues("lp")
start = c(-21.77, 5.95,8.6)
blbin(x,y, bins = bins, tau = 0.99, model = "lp", start = start)

# b) logistic model

startValues("logistic",8)
start = c(5.22, 5.5,8.65)
blbin(x,y, bins = bins, tau = 0.99, model = "logistic", start = start)

blbin(x,y, bins = bins, tau = 0.95, model = "logistic", start = start)

### 4.3 Quantile regression---------------------------------------------------------------

# a) linear-plateau model

plot(x,y)

startValues("lp")
start = c(-25.45, 6.60, 8.73)
start = c(-27.13, 7.03, 8.86)

blqr(x,y, tau = 0.99, model = "lp", start = start)


# b) logistic model

startValues("logistic",8)
start = c(0.95, 2,8.89)
blqr(x,y, tau =0.95, model = "logistic", start = start)

### 4.4 Censored model====================================================================

# a) linear-plateau model

data <- data.frame(x,y)

plot(data)

startValues("lp")

start <- c(-27.93,7.18,9.06, mean(x),  mean(y),  sd(x),  sd(y),  cor(x,y))

start <- c(-26.40,7.02,9.06, mean(x),  mean(y),  sd(x),  sd(y),  cor(x,y))

start <- c(-29.29,7.48,8.9, mean(x),  mean(y),  sd(x),  sd(y),  cor(x,y))



cbvn(data, model="lp", start = start, sigh = 0.3, optim.method = "Nelder-Mead")












