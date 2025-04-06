
# 1. INTRODUCTION

# 2. Libraries

library(BLA)
library(aplpack)

# 3. Load data

# 4. Test Assumptions

# 4.1. Normality check for the soil P-----------------------------------------------------

# When the censored bivariate normal model is used, the variables x and y should plausibly 
# be drawn from an underlying bivariate normal distribution albeit with some censoring of 
# the y variable. Here we check this assumption using the summastat() function.
#

# 4.1.1. Soil P

summastat(soil$P) # From results, P can not be assumed to be from a normal distribution and
# so we try a log transformation.

summastat(log(soil$P))# The log-transformed P can be assumed to be from a normal distribution

# 4.1.2 Wheat yield

summastat(soil$yield)# Wheat yield can be assumed to be from a normal distribution

# 4.2. Outlier detection using bagplot() function ----------------------------------------

# The boundary line model is highly sensitive to outliers and for this reason, bivariate 
# outliers are identified and removed from the data. This is achieved using the bagplot 
# method. The bag plot is a bivariate equivalent of the univariate boxplot. It is composed 
# of a depth median, bag and loop. The depth median describes the center of the data cloud 
# and is equivalent to the median value in the univariate boxplot, the bag contains 50% of 
# the data and the loop contains data outside the bag which are not outliers. Here we identify 
# and remove outliers from the dataset using the bagplot() function from the aplpack package.

dat<-data.frame(x=log(soil$P), y=soil$yield) #Input for the bagplot() is a dataframe of x and y. 

out<-bagplot(dat,show.whiskers = F)# Figure 2 in article

legend("bottomright", legend = c("Bag","Loop","Depth median", "outlier"), #adds legend
       pch = c(15,15,8,3), col = c(adjustcolor( "#7799ff", alpha.f = 0.7),
                                   "#aaccff","red","red"))

# Creating a new data set without bivariate outliers i.e points in the bag and loop only.

vals<-rbind(out$pxy.bag,out$pxy.outer) 

head(vals)

# 4.3. Testing evidence for presence of boundary in dataset---------------------------------

# Fitting boundary line models to data works on the assumption that data has boundary structure
# at the upper edges of the data. This assumption can be assessed using expl_boundary() function.
# The inputs are the x and y variables.

x<-vals[,1]
y<-vals[,2]

expl_boundary(x,y,shells=10,simulations=1000) 

# Results indicate that there is strong evidence of bounding structures in both the right 
# and left sections.


