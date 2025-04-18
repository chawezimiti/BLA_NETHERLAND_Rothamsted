
install.packages("devtools")
library(devtools)
#install.packages("BLA")
install_github("chawezimiti/BLA") # install the development version of BLA from Github
install.packages("aplpack")

library(BLA) 
library(aplpack)

summastat(soil$P) # From results, P can not be assumed to be from a normal distribution and so we try a log transformation.

summastat(log(soil$P))


summastat(soil$yield)


dat <- data.frame(x=log(soil$P), y=soil$yield) #Input for the bagplot() is a dataframe of x and y.

dat[which(is.na(dat$x)==T),1] <- mean(dat$x,na.rm=T)
dat[which(is.na(dat$y)==T),2] <- mean(dat$y,na.rm=T)

out <- bagplot(dat,show.whiskers = F)

legend("bottomright", legend = c("Bag","Loop","Depth median", "outlier"), 
       pch = c(15,15,8,3), col = c(adjustcolor( "#7799ff", alpha.f = 0.7), "#aaccff","red","red"))



vals <- rbind(out$pxy.bag,out$pxy.outer)

head(vals)


x <- vals[,1] 
y <- vals[,2]

expl_boundary(x=x,y=y,shells=10,simulations=1000)

# bolides
?bolides

bolides(x=x,y=y,model = "explore", pch=16, col="grey")

?startValues()

startValues("trapezium")


start <- c(4,3,14,104,-22) # start values is a vector of five consists of intercept, slope, plateau yield, intercept2 and slope2. 

model1 <- bolides(x=x,y=y, start = start,model = "trapezium",
                  xlab=expression("Phosphorus/ln(mg L"^-1*")"), 
                  ylab=expression("Yield/ t ha"^-1), pch=16, 
                  col="grey", bp_col="red")

model1



P <- c(4.5, 7.4, 12.2, 20.1, 54.5)

P_log <- log(P)

Max_yield <- predictBL(model1, P_log) # the argument inputs are the boundary line model and the independent values (in this case P_log)

Max_yield


?blbin

c(1.61,4.74,0.313)

(1.61-4.74)/10

(max(x)-min(x))/10

c(min(x), max(x), (max(x)-min(x))/10)


bins <- c(min(x), max(x), (max(x)-min(x))/10)

blbin(x=x, y=y, bins=bins, model = "explore", tau=0.99, pch=16, col="grey")

start <- c(4.75, 3.23, 13.3, 24.87,-2.95 )

model2 <- blbin(x=x,y=y, bins, start = start, model = "trapezium", 
                tau=0.99, 
                ylab=expression("t ha"^-1), 
                xlab=expression("Phosphorus/ln(mg L"^-1*")"), 
                pch=16, col="grey", bp_col="grey")

model2

?blqr

start <- c(4,3,13.5,31,-4.5)

model3 <- blqr(x=x,y=y, start = start, model = "trapezium", 
               tau=0.99,
               xlab=expression("Phosphorus/ mg L"^-1),
               ylab=expression("Phosphorus/ln(mg L"^-1*")"),
               pch=16, col="grey") # may take a few seconds to ran

model3



?cbvn

plot(x,y,pch=16, col="grey", xlab="soil P", ylab = "Yield (t/ha)") 

start1 <- c(4.3,3.4,13.8,32.8,-4.9,mean(x),mean(y),sd(x),sd(y),cor(x,y))

start2<-list(
  c(4.3,3.4,13.8,32.8,-4.9,mean(x),mean(y),sd(x),sd(y),cor(x,y)),
  c(2.5,4.1,13.42,32,-4.8,mean(x),mean(y),sd(x),sd(y),cor(x,y)),
  c(3.5,3.7,13.35,47.7,-8.4,mean(x),mean(y),sd(x),sd(y),cor(x,y)),
  c(2.83,4.11,13.7,32.,-4.6,mean(x),mean(y),sd(x),sd(y),cor(x,y)),
  c(4.1,3.4,13.6,29,-4.1,mean(x),mean(y),sd(x),sd(y),cor(x,y))
)


sigh<-c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1) # we suggest sigh values ranging from 0.3 to 1 t/ha

ble_profile(data = vals, start = start1, sigh = sigh, model = "trapezium", plot = T)

# Note: This function will take a longer to run as it is evaluating each sigh value.



profile <- lapply(start2, function(start2) {
  
  lapply(sigh, function(sigh) {
    
    ble_profile(data = vals, start = start2, sigh = sigh, model = "trapezium", plot = F)
  })
})

# Extract log-likelihood and sigh (Merror) values from profile list

log_likelihood <- unlist(lapply(profile, function(sublist) {
  sapply(sublist, function(x) x[["log-likelihood"]])
}))

Merror<- unlist(lapply(profile, function(sublist) {
  sapply(sublist, function(x) x[["Merror"]])
}))

# The sigh with the smallest negative log-likelihood 

Merror[which(log_likelihood==max(log_likelihood, na.rm = T))]# profile maximized  at 0.4 t/ha



me_profile<-data.frame(x=Merror,y=log_likelihood)
me_profile<-me_profile[order(Merror),] # ordering data from smallest to largest Merror

vec<-vector()

for(i in unique(me_profile$x)){ 
  maxy<-max(me_profile[which(me_profile$x==i),]$y, na.rm = T)
  vec<-c(vec,maxy)
}

# ploting the largest value at each Merror

par(mar=c(5,5,4,4))

plot(unique(me_profile$x),vec,pch=16, 
     xlab=expression(bold(sigma[e]*"/t ha"^-1)),
     ylab=expression(bold(log-Likelihood)),
     cex.lab=1.8, cex.axis=1.8)
lines(unique(me_profile$x),vec, lty=5, lwd=1.5) 
abline(v=Merror[which(log_likelihood==max(log_likelihood, na.rm = T))],
       lty=5, col="red", lwd=1.3) # sigh with largest log-likelihood



model4 <- cbvn(data=vals, start = start1, sigh=0.4, model = "trapezium",
               optim.method = "Nelder-Mead",
               xlab=expression("Phosphorus/ mg L"^-1),
               ylab=expression("Yield/ t ha"^-1), pch=16, col="grey")


models <- lapply(start2, function(start2) {
  
  tryCatch(
    
    cbvn(data=vals, start = start2, sigh=0.4, model = "trapezium",
         optim.method = "Nelder-Mead",
         xlab=expression("Phosphorus/ mg L"^-1), 
         ylab=expression("Yield/ t ha"^-1), pch=16, col="grey"),
    
    error=function(e) NA) 
})


model4 <- models[[which.min(unlist(lapply(X=models,FUN = function(a){
  
  b<-tryCatch(a$AIC[2,1],error=function(e) NA)
  return(b)})))]]

model4


P_data <-log(soil$P) # extracting soil P from the data set

P_data[which(is.na(P_data)==T)]<-mean(P_data,na.rm=T) # replace missing values with mean soil P value

P<-predictBL(model4,P_data) # boundary yield for soil P


#### Soil pH=======================================================================

# 1. Test for Normailty-------------------------------------------------------------------

summastat(soil$pH) 

summastat(log(soil$pH)) # transformation doesnt improve results


# 2. Identify and remove outliers --------------------------------------------------------

dat2 <- data.frame(x=soil$pH, y=soil$yield) # Input for the bagplot() 

dat2[which(is.na(dat2$x) == T),1] <- mean(dat2$x, na.rm=T) #replace missing values with mean 
dat2[which(is.na(dat2$y) == T),2] <- mean(dat2$y, na.rm=T)

out2 <- bagplot(dat2,show.whiskers = F)

vals2 <- rbind(out2$pxy.bag, out2$pxy.outer) 

# 3. Select boundary model and initial starting values------------------------------------

plot(vals2, pch=16, col="grey", xlab="soil P", ylab = "Yield (t/ha)")

startValues("lp")

X <- vals2[,1]
Y <- vals2[,2]

start2 <- c(-6.07,3.05,13.53,mean(X),mean(Y),sd(X),sd(Y),cor(X,Y))

# 4. Fit the boundary model-------------------------------------------------------------

model5 <- cbvn(data=vals2, start = start2, sigh=0.4, model = "lp",
               optim.method = "Nelder-Mead",
               xlab=expression("Soil pH"),
               ylab=expression("Yield/ t ha"^-1), pch=16, col="grey")

model5

# Predict pH ----------------------------------------------------

pH_data <- soil$pH 

pH_data [which(is.na(pH_data )==T)]<-mean(pH_data ,na.rm=T)# replace missing values with mean

pH<-predictBL(model5,pH_data) # boundary yield for soil pH


### Limiting factor ===============================================================

Limiting_factor <- limfactor(P, pH)

Limiting_factors <- Limiting_factor[[1]]

head(Limiting_factors)

Attainable_yield <- Limiting_factor[[2]]


soil$yield[which(soil$yield > Attainable_yield)] <- Attainable_yield

plot(Limiting_factors$Rs, soil$yield,
     xlab="Predicted yield (ton/ha)",
     ylab="Actual yield (ton/ha)", pch=16, col="grey")

abline(h=Limiting_factor[[2]], col="blue", lty=5, lwd=1)
lines(c(min(Limiting_factors$Rs),max(Limiting_factors$Rs)), 
      c(min(Limiting_factors$Rs),max(Limiting_factors$Rs)), 
      col="red", lwd=2)

legend("bottomleft",legend = c("Att yield", "1:1 line"),
       lty=c(5,1), col=c("blue", "red"), lwd=c(1, 2), cex = 0.8)

#---Exp and  UnExp

points(9,5, pch=16, col="red")

arrows(x0=c(9,9),y0=c(5,9),x1=c(9,9),y1=c(9,Attainable_yield), code = 2, lwd = 1.4, lty = 5)

text(c(8,8),c(6,11), c("Unexplained Yg", "Explained Yg"), cex=0.8)









