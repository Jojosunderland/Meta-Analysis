library(tidyverse)

pines <- read.csv("pines.csv", sep=";")
pines
View(pines)

traits <- read.table('fieldtraits_v2.txt', head = TRUE)
View(traits)

yairclass <- cbind(pines, traits[pmatch(pines$id, traits$FieldCode), c("PopulationCode", "Family", "Block")])
View(yairclass)

library(lme4)
model <- lmer(dbh ~ 1 + (1|PopulationCode) + (1|Family) + (1 | group)+ (1|Block), yairclass)
summary(model)
