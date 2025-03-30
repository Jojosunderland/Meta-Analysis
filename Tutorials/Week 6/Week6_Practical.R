## Week 6: Selection Coefficients ##

## RED DEER BIRTH WEIGHT ##

# Load packages:
library(lme4) # mixed effect models
library(lattice) # graphics
install.packages('beeswarm')
library(beeswarm)
library(scales)

# Load data:
setwd("~/Documents/WorkingD/Meta-Analysis") # Set working directory
deer_data <-read.csv('deer data for selection analysis.csv', stringsAsFactors=TRUE)  # read in the data

head(deer_data)
str(deer_data)

# You need to tell R to treat Sex as a two-level factor rather than a number. Similarly, you need to tell it to treat BirthYear as a factor.
deer_data$Sex<-as.factor(deer_data$Sex)
deer_data$BirthYear<-as.factor(deer_data$BirthYear)

# Summarise the data in the column Birth Weight:
summary(deer_data$BirthWt)

# Linear model of Birth weight:

model1 <- lm(BirthWt ~ Sex, deer_data)
summary(model1)

# Visualise the sex difference in birth weight:
beeswarm(BirthWt~Sex,deer_data, method = "compactswarm", col=c("red","blue"), pch=19,cex=0.5)

# Linear mixed model of birth weight:

# Visualise the variation in birth weights across years and mothers
boxplot(BirthWt~BirthYear, deer_data,
        xlab="Year",
        ylab="Calf birth weight (kg)")

# create a vector of mother IDs sorted by mean mass of their offspring
sorted_mum <- names(sort(tapply(deer_data$BirthWt, deer_data$Mum, mean)))

boxplot(BirthWt~factor(deer_data$Mum, levels=sorted_mum), deer_data,
        xlab="Mother ID",
        ylab="Calf birth weight (kg)",
        xaxt="n")

# LMM adding random effects of birth year and mum 
lme1<-lmer(BirthWt ~ Sex + (1|BirthYear) + (1|Mum), data=deer_data)
summary(lme1)

# Check model diagnostics:
plot(lme1)
lattice::qqmath(lme1) # the q-q line is not perfect at the lower tail, but sufficient for this purpose


# Measuring fitness:
summary(deer_data$FemaleLBS)  # summarise female Lifetime Breeding Success (LBS)
plot(table(deer_data$FemaleLBS), xlab="Female Lifetime Breeding Success", ylab="Count")

#plot(table(...)) is a nice way to plot out integer numbers, as opposite to using a histogram
summary(deer_data$MaleLBS) # summarise male LBS
plot(table(deer_data$MaleLBS), xlab="Male Lifetime Breeding Success", ylab="Count")

# SELECTION VIA JUVENILE SURVIVAL #

# Create a juvenile survival column
deer_data$JuvSurv <- ifelse(deer_data$AgeAtDeath>2,1,0)
# use a quick summary table to check this has done what you want, i.e. coded those who died age 0-2 as 0, and those who lived longer as 1
table(deer_data$JuvSurv,deer_data$AgeAtDeath)

# look at difference in mean birth weight between those that survive and die
tapply(deer_data$BirthWt, deer_data$JuvSurv,mean)

# Plot the relationship between survival and birth weight:
plot(jitter(JuvSurv,0.07)~BirthWt,deer_data, cex=0.5, pch=19, col=alpha("black",0.1), xlab="Birth Weight", ylab="Survival")
# jitter moves points a little bit, so enable you to see the distribution a bit better when there are lots of overlaying points
# alpha from the scales package allows you to alter the transparency of the points

# group body size then plot the average survival:

# make a evenly spaced sequence long body size
bw_breaks<-with(deer_data,seq(min(BirthWt),max(BirthWt),length.out=11))
# calculate the midpoints of the intervals
bw_midpoints<- sapply(1:10,function(x) mean(c(bw_breaks[x],bw_breaks[x+1])))
# assign birthweight into these bands
bw_groups <- as.factor(findInterval(deer_data$BirthWt,bw_breaks,all.inside=TRUE))

# work out the mean survival per band 
js_means<-tapply(deer_data$JuvSurv, bw_groups, mean)
# work out the sample size per band 
js_n<-tapply(deer_data$JuvSurv, bw_groups, length)

plot(js_means~bw_midpoints,deer_data, cex=1, pch=19, col=1, ylim=c(0,1), xlab="Birth Weight", ylab="Survival")
text(js_means+0.05~bw_midpoints,labels=js_n)

# Estimating a selection differential #

# Calculate the mean birth weight of ALL calves and of those that survived

# mean before selection (all calves)
mean(deer_data$BirthWt)

# mean after selection (only for the survivors, i.e. for those rows where JuvSurv is 1)
# You have to tell R to ignore missing values: here, with na.rm=T (missing values are NA, and 'na.rm=TRUE' means ignore them)
mean(deer_data$BirthWt[deer_data$JuvSurv==1],na.rm=TRUE)

# selection differential: the difference before vs after selection
mean(deer_data$BirthWt[deer_data$JuvSurv==1],na.rm=TRUE) - mean(deer_data$BirthWt,na.rm=TRUE)

# Calculate the statistical covariance between the trait and relative fitness
deer_data$rel_JuvSurv <- deer_data$JuvSurv/mean(deer_data$JuvSurv)   # creates a new column of relative Juvenile Survival
CovJS<-cov(deer_data$rel_JuvSurv,deer_data$BirthWt,use="pairwise.complete.obs")    # estimates covariance with Birth Weight
CovJS

# Need to rescale by (N-1)/N to get population covariance
N<-length(deer_data$BirthWt)   # number of observations
CovJS * (N-1)/N

# Estimating a selection gradient #
# Following the Lande-Arnold method

# Estimate the gradient of linear regression of relative fitness on the trait
lm_juvsurv<-lm(rel_JuvSurv ~ Sex+BirthWt, data=deer_data)
summary(lm_juvsurv)

#Compare the selection gradient to the selection differential 

beta <- coef(lm_juvsurv)["BirthWt"]
S <- CovJS
## beta = S/V_P
beta
S/var(deer_data$BirthWt)

# Plot the survival data with the model predictions:
js_rel_means<-tapply(deer_data$rel_JuvSurv, as.factor(findInterval(deer_data$BirthWt,bw_breaks,all.inside=TRUE)), mean)
plot(js_rel_means~bw_midpoints,deer_data, cex=1, pch=19, col=1, ylab="w", ylim=c(0.2,1.8))
text(js_rel_means+0.08~bw_midpoints,labels=js_n)

# here I am adding half the sex effect to the intercept
abline(coef(lm_juvsurv)[1] + coef(lm_juvsurv)[2]/2,coef(lm_juvsurv)[3])


# SELECTION VIA ADULT BREEDING SUCCESS # 

# Define three new columns for adult breeding success:
deer_data$Fem_AdultLBS <- with(deer_data, ifelse(AgeAtDeath>2,FemaleLBS,NA)) 

deer_data$Male_AdultLBS <- with(deer_data, ifelse(AgeAtDeath>2,MaleLBS,NA))      # column with MaleLBS only for males that died age 3+  

## all abs
deer_data$AdultLBS <- with(deer_data, 
                           ifelse(AgeAtDeath>2 & !is.na(MaleLBS),MaleLBS,
                                  ifelse(AgeAtDeath>2 & !is.na(FemaleLBS),FemaleLBS,
                                         NA)))

plot(table(deer_data$Fem_AdultLBS), xlab="Female Adult LBS", ylab="Count")

plot(table(deer_data$Male_AdultLBS), xlab="Female Adult LBS", ylab="Count")

# Is breeding success related to birth weight?
plot(Fem_AdultLBS~BirthWt, deer_data,
     xlab="Birth weight (kg)",ylab="Female Adult LBS",
     pch=19, col=alpha(1,0.5))

plot(Male_AdultLBS~BirthWt, deer_data,
     xlab="Birth weight (kg)",ylab="Male Adult LBS",
     pch=19, col=alpha(1,0.5))

# define the relative fitness measure for LBS in each sex, then we need to estimate the covariance
deer_data$relFem_AdultLBS <- with(deer_data,Fem_AdultLBS/mean(Fem_AdultLBS,na.rm=T))  # divide Fem_AdultLBS by its mean
deer_data$relMale_AdultLBS <- with(deer_data,Male_AdultLBS/mean(Male_AdultLBS,na.rm=T))

# check that it's done what you expected
mean(deer_data$relFem_AdultLBS,na.rm=T)

mean(deer_data$relMale_AdultLBS,na.rm=T)

cov(deer_data$BirthWt,deer_data$relFem_AdultLBS,use="complete.obs")   # female selection differential
cov(deer_data$BirthWt,deer_data$relMale_AdultLBS,use="complete.obs")  # male selection differential

# Selection gradients #

# female selection gradient
lm_femLBS<-lm(relFem_AdultLBS~BirthWt,data=deer_data) 
# summary(lm_femLBS) would give the full output from the model, but we only want the parameter coefficient and its SE
summary(lm_femLBS)$coeff["BirthWt",1:2]      

# male selection gradient
lm_maleLBS<-lm(relMale_AdultLBS~BirthWt,data=deer_data) 
summary(lm_maleLBS)$coeff["BirthWt",1:2]      # male selection gradient

# Plot the model predictions
plot(relFem_AdultLBS~BirthWt, deer_data,
     xlab="Birth weight (kg)",ylab="Relative Female Adult LBS",
     pch=19, col=alpha(1,0.5))

abline(coef(lm_femLBS)["(Intercept)"] ,coef(lm_femLBS)["BirthWt"])

plot(relMale_AdultLBS~BirthWt, deer_data,
     xlab="Birth weight (kg)",ylab="Relative Male Adult LBS",
     pch=19, col=alpha(1,0.5))

abline(coef(lm_maleLBS)["(Intercept)"] ,coef(lm_maleLBS)["BirthWt"])


# STATISTICAL MODELS OF SELECTION #

# Juvenile survival
glmm_juvsurv<-glmer(JuvSurv ~ Sex+BirthWt+(1|Mum)+(1|BirthYear), data=deer_data, family='binomial')
summary(glmm_juvsurv)

# does the effect of birth weight differ between sexes
glmm_juvsurv_sex<-glmer(JuvSurv ~ Sex*BirthWt+(1|Mum)+(1|BirthYear), data=deer_data, family='binomial')
summary(glmm_juvsurv_sex)

# Adult breeding success
glmm_Fem_AdultLBS<-glmer(Fem_AdultLBS ~ BirthWt+(1|Mum)+(1|BirthYear)+(1|Code), data=deer_data, family='poisson')
summary(glmm_Fem_AdultLBS)

glmm_Male_AdultLBS<-glmer(Male_AdultLBS ~ BirthWt+(1|Mum)+(1|BirthYear )+ (1|Code), data=deer_data, family='poisson')
summary(glmm_Male_AdultLBS)
