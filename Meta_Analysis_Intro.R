## An Introduction to Meta-Analysis ##

# Install packages
install.packages("metafor",dependencies=TRUE,repos="https://cloud.r-project.org")

## The funnel plot ##
# 2.1 Simulating data 

slope <--0.25
intercept<-0
predictor<-rnorm(n=100,mean=10,sd=10) # this generates a random normal predictor and the residual
#rnorm is extremely useful for simulating data, need to specifit n, mean and sd
response<-intercept+slope*predictor+rnorm(n=100,mean=0,sd=40)
plot(predictor,response) # this is a scatter plot NOT a funnel plot

#this code above generates a fairly shallow slope thorugh the data

#Q: Try to run a simple LM with the data you generated. 
#How does the slope estimate compare to the slope you simulated? - A: As it's a large data set,  the slope estimate should be quite similar to the slope you used for the simulation

model <- lm(response~predictor)
summary(model)

# 2.2 Simulating many data sets 

#Use the above approach in a for loop 
store<-matrix(nrow=200,ncol=4)
#We need to create somewhere to store our data

for(x in 1:200){
  #we will simulate 200 different datasets 
  
  samplesize<-ceiling(exp(rnorm(1,4.5,1.5)))+3
  #we're using this code to select sample sizes at random from a log normal distribution, so that small sample sizes are common and large sample sizes are rare. And n is always > 3.                  
  
  
  predictor<-rnorm(n=samplesize,mean=10,sd=10)
  response<-intercept+predictor*slope+rnorm(n=samplesize,0,40)
  #predictor and response are just as we used before, except now n is given by samplesize rather than n = 100
  
  model<-lm(response~predictor)
  #the same linear model as we ran before
  
  store[x,]<-c(samplesize,summary(model)$coefficients[2,1:2],summary(model)$coefficients[2,4])
  #here we extract the model outputs we want and store them in our store matrix
  
  
}
store<-as.data.frame(store)
names(store)<-c("n","slope","standard.error","p.value")

# 2.3 Producing and interpreting a funnel plot
# this is a scatterplot where the x axis is the effect size and the y axis is the inverse of the sampling variance (e.g. sample size or standard error)

par(mfrow=c(1,2))
plot(store$slope,store$n,xlab="Slope",ylab="Sample size") #Funnel plot 1 (sampling size)
plot(store$slope,(1/store$standard.error),xlab="Slope",ylab="Precision, (1/se)") #Funnel plot 1 (precision)

#We can colour the slope estimates that are significant (P< 0.05) red, and indicate the slope that we used for the simulation with a vertical dashed line
sigslope<-which(store$p.value<0.05)
par(mfrow=c(1,2))
plot(store$slope,store$n,xlab="Slope",ylab="Sample size")
points(store$slope[sigslope],store$n[sigslope],pch=16,col="red")
abline(v=slope,lty=2)
plot(store$slope,(1/store$standard.error),xlab="Slope",ylab="Precision, (1/se)")
points(store$slope[sigslope],(1/store$standard.error[sigslope]),pch=16,col="red")
abline(v=slope,lty=2)

## A Basic Meta Analysis ##
# 3.1 Estimating mean effect size ignoring sampling variance
model2<-lm(slope~1,data=store)
#The ~1 tells the model to fit an intercept only.
summary(model2)

#Q. What is wrong with the analysis above?
#A. This analysis ignores sampling variance, and the fact that some slopes are estimated much more accurately than others

# 3.2 Estimating the mean effect size using metafor

#We will load the R package and then take a look at the main r function (use ?rma) we will use to run our meta-analysis.
library(metafor)

# If you look at the details about rma you will see that we have the two main bits of information to run this, as yi = slope and sei = standard.error.

meta <- rma(yi = slope,sei = standard.error, data=store)
meta

#Below is a summary of the key elements of the output: The mean estimate and the standard error of the mean and 95% confidence interval
#tau^2 - an estimate of the variance in true effect sizes among studies
#i^2 statistic - tells us what proportion of the total variance (sampling variance + among study heterogeneity in effect size) is due to among study heterogeneity in effect size, see here for more explanation
#Test for heterogeneity - this provides a test of whether there is significant heterogeneity in effect sizes

#We can also use some of metafor’s in-built plotting functions to generate a nice funnel plot and forest plot.

funnel(meta)

# Forest plot
forest(meta,cex.lab=0.8,cex.axis=0.8,addfit=TRUE,shade="zebra")

## A meta-analysis with moderators and random terms ##
# 4.1 Simulating a new meta-dataset

# Last time when we simulated data we used the same slope estimate (effect size) every time. 
# This time we will generate datasets such that the slope estimates (effect sizes) vary as a function of another variable

# let’s imagine that the slopes we are generating correspond to the effect of temperature on phenology (days/degree C), and that this slope becomes more negative with latitude.

latitude<-runif(100,0,90)
#we will randomly sample a latitude from 0,90 degree North
slope<-0+latitude*-0.1+rnorm(100,0,3)
plot(latitude,slope)

store2<-matrix(nrow=200,ncol=7)
#We need to create somewhere to store our data. We'll call this one store2 to distinguish it from the previous one. This time we also want to save the latitude and species that the slope estimate comes from. We will aslo save a unique ID for each observation - we can use this later to include a residual random effect

species<-rep(1:20,each=10)
specieseffect<-rep(rnorm(20,0,2),each=10)
#we will use this to generate our 20 species random effects

for(x in 1:200){
  #we will simulate 200 different datasets 
  
  latitude<-runif(1,0,90)
  
  slope<-0+specieseffect[x]+latitude*-0.1+rnorm(1,0,3)
  
  samplesize<-ceiling(exp(rnorm(1,4.5,1.5)))
  #we're using this code to select sample sizes at random from a log normal distribution, so that small sample sizes are common and large sample sizes are rare                    
  
  if(samplesize>3){
    #we included this if function so that we don't run an analyses on datasets that are too small
    predictor<-rnorm(n=samplesize,mean=10,sd=10)
    response<-intercept+predictor*slope+rnorm(n=samplesize,0,40)
    
    model<-lm(response~predictor)
    #the same linear model as we ran before
    
    store2[x,]<-c(samplesize,summary(model)$coefficients[2,1:2],summary(model)$coefficients[2,4],latitude,species[x],x)
    #here we extract the model outputs we want and store them in our store matrix
    
    
  }}
store2<-as.data.frame(store2)
names(store2)<-c("n","slope","standard.error","p.value","latitude","species","ID")

# 4.2 Funnel plot and simple Meta analysis

plot(store2$slope,(1/store2$standard.error),xlab="Slope",ylab="Precision, (1/se)")

meta2<-rma(yi=slope,sei=standard.error,data=store2)
meta2
funnel(meta2) # Question: Why doesn’t the slope estimate funnel in much this time?
#A. There is substantial heterogeneity in the slope, as is also reflected in the I^2 value and the test for heterogeneity

# 4.3 A meta-analysis controlling for latitude
# include latitude as a covaraite (fixed model)

meta3<-rma(yi=slope,sei=standard.error,mods=~latitude,data=store2)
meta3
funnel(meta3)

# 4.4 A meta-analysis with random terms
# We now need to shift to using the rma.mv function and can add a random term by adding the argument random=~1|yourterm.
# we’ve included the slope variance as the square of the standard error. We’ve also included an observation level random effect (to estimate the residual variance)
store2$se2<-store2$standard.error^2
store3<-store2[-which(is.na(store2$slope)==TRUE),]
#this function won't run with NAs, so we remove those rows
meta4<-rma.mv(yi=slope,V=se2,mods=~latitude,random=~1|species/ID,data=store3)
meta4


## Confronting a real dataset ##

#import the data into R
birdbroods<-read.csv("~/Documents/WorkingD/Meta-Analysis/birdbroods.csv",sep=",",header=TRUE)
birdbroods

plot(birdbroods$slope,(1/birdbroods$slope.SE),xlab="Slope",ylab="Precision, (1/se)")
birdbroods$se2<-birdbroods$slope.SE^2
meta5<-rma.mv(yi=slope,V=se2,random=~1|Species/id.pop,data=birdbroods)
meta5
funnel(meta5)

forest(meta5,cex.lab=0.8,cex.axis=0.8,addfit=TRUE,shade="zebra",order="obs")


#Q1. Has the brood size of the average bird species declined?
#A. Probably not, estimate is very minimally negative (-0.006) and a non-significant p-value (0.09)suggests the evidence is not significant
# there are also very small confidence intervals

#Q2. Is more of the variation in slope estimates distributed among or within species?
#A. within species? 
#sigma values (1 = among, 2 = within), estimate is greater within species

#Q3. Is trend in brood size more positive for populations in protected areas? (Clue: you will need to add an extra moderator to your meta-analysis)
#A. No it is not more positive, it is negative 
meta6<-rma.mv(yi=slope,V=se2,mods=~protected.area,random=~1|Species/id.pop,data=birdbroods)
meta6
#Q4. If the information was available, what other terms do you think it would be worth including as random effects?
#A. age, if its their first reproductive event, body size

## Publication bias ##
#To see the pattern that this publication bias leaves we’ll return to the first dataset that we simulated (called store). We’ll go through each row of data in turn and make it so that the probability of being published is as follows:
#significant, probability published = 1.
#non-significant and <= 30 observations, probability published = 0.25.
#non-significant and > 30 observations, probability published = 0.75

store<-store[is.na(store$slope)==FALSE,]
store$publish<-0
#
store$publish[store$p.value<=0.05]<-1
largesamplesize<-intersect(which(store$p.value>0.05),which(store$n>30))
retainlarge<-largesamplesize[as.logical(rbinom(length(largesamplesize),prob=0.75,size=1))]
store$publish[retainlarge]<-1
smallsamplesize<-intersect(which(store$p.value>0.05),which(store$n<=30))
retainsmall<-smallsamplesize[as.logical(rbinom(length(smallsamplesize),prob=0.25,size=1))]
store$publish[retainsmall]<-1

par(mfrow=c(1,2))
plot(store$slope,(1/store$standard.error),xlab="Slope",ylab="Precision, (1/se)",main="Before")
plot(store$slope[store$publish==1],(1/store$standard.error[store$publish==1]),xlab="Slope",ylab="Precision, (1/se)",main="After")

# Above we see what an extreme case of publication bias looks like - a scarcity of low precision results that are close to the null hypothesis
# For the sake of completeness we’ll apply a test of whether our final plot is asymmetric. Below we’ll conduct this test for the original store data and the published data

regtest(x=slope, sei=standard.error, data=store,
        model="rma", predictor="sei", ret.fit=FALSE)

regtest(x=slope, sei=standard.error, data=store,
        model="rma", subset=publish==1,predictor="sei", ret.fit=FALSE)

# if the asymmetry p value is significant then it suggests publication bias
