#
#
# LL
# February, 2022 - checked September 2022
# PCA


#clear
rm(list = ls())

library("data.table")  # data management
library("tidyverse")
library(psych)
library(MVA)
library(FactoMineR)
library(factoextra)
library(PCAmixdata)
library(tidyverse)
library(haven)
library(dplyr)
library(tidyr)
library(GPArotation)
library(ggplot2)
library(gridExtra)
library(pca3d)
library(devtools)
library(readstata13)



# 1. LOAD DATA -----------------------------------------------------------

#stata check
stata <- read.dta13("data/clean/EurobarometerStata_FINAL_2pc.dta")
stata
stata <-  subset(stata, select=c(Unique_ID, pc1, pc2))
stata <- na.omit(stata)
stata


#disregard the already created variables
dt <- readRDS("data/clean/cleaned_survey_data_12Item_Feb_22.rds")
dt

#useless to drop individuals with missing values as the analysis does it as default but
# drop individuals with missing values:
ddt <- na.omit(dt)
ddt



# 2.CHECK DISTRIBUTIONS --------------------------------

pairs.panels(ddt,
             gap = 0,
             pch=21)

hist(ddt$QA14_1)
hist(ddt$QA14_2)
hist(ddt$QA14_3)
hist(ddt$QA14_4)
hist(ddt$QA14_5)
hist(ddt$QA14_6)
hist(ddt$QA14_7)
hist(ddt$QA14_8)
hist(ddt$QA14_9)
hist(ddt$QA14_10)
hist(ddt$QA14_11)
hist(ddt$QA14_12)

# 3. PCA -------------------------------------------

### PCA package ### this one gives PCA as in Stata PCA. Correct ###################

# remove N.A.s
pca2 <- princomp((ddt %>% select(starts_with("QA14_"))), 
                 cor = TRUE) #cov matrix def

print(pca2)
summary(pca2)
loadings(pca2)
screeplot(pca2)
#biplot(pca2)


# Individual PCA Scores
ind <- get_pca_ind(pca2)
ind
# FactoMineR uses "coordinates" for standardised loadings.
print(ind$coord)
stata
#the first two components correspond
#extract the coord in a csv
write.csv(ind$coord, "data/clean/components_pca.csv")

# merge datasets with components and single items
coord <- read.csv("data/clean/components_Feb.csv")

dt.m <- merge(coord, ddt, by = "Unique_ID", all.x=T, all.y=T)
dt.m
stata
#slight difference but overall correspondence of scores between the two programs





saveRDS(dt.m, "data/clean/components&items_Feb.rds")








