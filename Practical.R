## Heritability practical ##

## Part 1: Set up ##
install.packages(c("nadiv","gremlin","pedtricks"))

library(pedtricks) # pedigree descriptions
library(nadiv)  # needed for creating inverse relatedness matrices
library(gremlin) # package for mixed models, and specifically animal models
library(scales)

# set WD
deer_data<-read.csv('deer_dat_gauzere.csv')  # read in the data
deer_ped<-read.csv('deer_ped_gauzere.csv')

str(deer_data)
str(deer_ped)

tail(deer_ped)

# Calculate unique mothers and fathers

#unique mothers and fathers
length(unique(deer_ped[,"Sire"]))
length(unique(deer_ped[,"Dam"]))
# missing mothers and fathers
sum(!is.na(deer_ped[,"Dam"]))
sum(!is.na(deer_ped[,"Sire"]))

# We can use the pedtricks package to summarise and display the pedigree.
ggpedigree(deer_ped) #The top row are the base population, those without known parents. As you can see there are a lot of long lineages.

# Summarise the different relationships in the pedigree:
stats <- ped_stats(deer_ped)
summary(stats)

## Part 2: Estimating heritability from parent-offspring regression ##
# create three datasets, one to do father-offspring regression, one to do mother-offspring regression and a third for midparent-offspring regression.
# To do this we need to create datasets with mean offspring value per parent, or parent offspring combination, along with (mid) parent birthweights

# to do with I have used functions in base R. You might be more familiar with other data manipulation packages.

## First we should add father ID to dataset from the pedigree
deer_data$Father <- deer_ped$Sire[match(deer_data$ID,deer_ped$ID)]

## then we can add both father and mother birthweight to the dataset. To do this we need to look them up by matching parents code to their record as an offspring
## add father birthweight to dataset
deer_data$fatherBirthWt <- deer_data$BirthWt[match(deer_data$Father,deer_data$ID)]

## add mother birthweight to dataset
deer_data$motherBirthWt <- deer_data$BirthWt[match(deer_data$Mother,deer_data$ID)]

## check what we have created
head(deer_data)

# then get the mean offspring weight per mother (and single value for mother birth weight):
offspring_means_M <- aggregate(cbind(BirthWt,motherBirthWt) ~ Mother, deer_data, mean)

# same for fathers
offspring_means_F <- aggregate(cbind(BirthWt,fatherBirthWt) ~ Father, deer_data, mean)

# and the same for parent pairs
offspring_means_mid <- aggregate(cbind(BirthWt,fatherBirthWt,motherBirthWt) ~ Mother+Father, deer_data, mean)
# and then average across parent phenotypes to get mid parent value
offspring_means_mid$midparentBirthWt <- (offspring_means_mid$motherBirthWt + offspring_means_mid$fatherBirthWt)/2

head(offspring_means_F)

head(offspring_means_mid)

#Lets check what we have created 
head(offspring_means_M)
par(mfrow=c(1,3))
plot(BirthWt~motherBirthWt,offspring_means_M, pch=19, col=alpha(1,0.5), xlab="Mother Birth Weight", ylab="Offspring Birth Weight")
plot(BirthWt~fatherBirthWt,offspring_means_F, pch=19, col=alpha(1,0.5), xlab="Father Birth Weight", ylab="Offspring Birth Weight")
plot(BirthWt~midparentBirthWt,offspring_means_mid, pch=19, col=alpha(1,0.5), xlab="Mid-parent Birth Weight", ylab="Offspring Birth Weight")

# Estimate the slope of the midparent-offspring regression using this formula and the values calculated above
v = var(offspring_means_mid$midparentBirthWt)
c = cov(offspring_means_mid$midparentBirthWt,offspring_means_mid$BirthWt)
c/v

# estimate the slope using a linear regression, and plot it with confidence intervals
lm_po <- lm(BirthWt~midparentBirthWt,offspring_means_mid)
summary(lm_po)

h2_po <- summary(lm_po)$coeff[2,1:2]
print(round(h2_po,3))

plot(BirthWt~midparentBirthWt,offspring_means_mid, pch=19, col=alpha(1,0.5), xlab="Mid-parent Birth Weight", ylab="Offspring Birth Weight")

# make data to predict from 
newdat <- data.frame(midparentBirthWt=seq(min(offspring_means_mid$midparentBirthWt),max(offspring_means_mid$midparentBirthWt),0.1))

#make predictions from model with confidence intervals
preds<-predict(lm_po, newdata=newdat,interval = "confidence")

# add lines
lines(newdat$midparentBirthWt,preds[,"fit"], lwd=2)
lines(newdat$midparentBirthWt,preds[,"lwr"], lty=2)
lines(newdat$midparentBirthWt,preds[,"upr"], lty=2)

h2_po <- summary(lm_po)$coeff[2,1:2]
print(round(h2_po,3))

# mother offspring
lm_mo <- lm(BirthWt~motherBirthWt,offspring_means_M)
summary(lm_mo)$coeff[2,1:2]*2

# father offspring
lm_fo <- lm(BirthWt~fatherBirthWt,offspring_means_F)
summary(lm_fo)$coeff[2,1:2]*2

## Part 3: Estimating heritability from a dam-sire model