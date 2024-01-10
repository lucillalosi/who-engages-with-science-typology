#
#
# LL
# January, 2022 - Checked September 2022
# Descriptives


#clear
rm(list = ls())


library("data.table")
library("tidyverse")
library("dplyr")
library("ggplot2")
library("psych")




# 1. LOAD DATA -----------------------------------------------------------


############################################!
####### DO NOT RUN EVERY TIME ----
############################################!


# Merge of two datasets, one with the 12 variables and one with the components & clusters

# open the ds with the variables
var <- readRDS("data/clean/cleaned_survey_data_12Item_Feb_22.rds")
var


# open the ds with the components and clusters
comp <- readRDS("data/clean/results_wide_Feb24_2rpca.rds")
comp

clus <- subset(comp, select=c(Unique_ID, Dim.1, Dim.2, clusters_5, clusters_4, clusters_3, clusters_2)) 
clus

# MERGE: var & clus by Unique_ID

class(var)
class(clus)

class(var$Unique_ID)
class(clus$Unique_ID)

clus$Unique_ID <- as.numeric(clus$Unique_ID)
class(clus$Unique_ID)

# merge keeping invalid cases out as are not useful for current analysis purpose
ddt <- merge(var, clus, by = "Unique_ID", all.x=F, all.y=T)
ddt
nrow(ddt) == nrow(var)
nrow(ddt) == nrow(clus)

#rename engagement items for easier analysis
names(ddt)[names(ddt)=="QA14_1"] <- "V1"
names(ddt)[names(ddt)=="QA14_2"] <- "V2"
names(ddt)[names(ddt)=="QA14_3"] <- "V3"
names(ddt)[names(ddt)=="QA14_4"] <- "V4"
names(ddt)[names(ddt)=="QA14_5"] <- "V5"
names(ddt)[names(ddt)=="QA14_6"] <- "V6"
names(ddt)[names(ddt)=="QA14_7"] <- "V7"
names(ddt)[names(ddt)=="QA14_8"] <- "V8"
names(ddt)[names(ddt)=="QA14_9"] <- "V9"
names(ddt)[names(ddt)=="QA14_10"] <- "V10"
names(ddt)[names(ddt)=="QA14_11"] <- "V11"
names(ddt)[names(ddt)=="QA14_12"] <- "V12"
ddt

saveRDS(ddt, "data/clean/results_wide_Feb_item&comp&clus.rds")



############################################!
####### RUN FROM HERE ----
############################################!

#clear
rm(list = ls())
library("data.table")
library("tidyverse")
library("dplyr")
library("ggplot2")
library("psych")

# open data

ddt <- readRDS("data/clean/results_wide_Feb_item&comp&clus.rds")
ddt

# convert to dataframe
setDF(ddt)
ddt
# Put Unique_ID as row name
row.names(ddt) <- ddt$Unique_ID
ddt$Unique_ID <- NULL
ddt

Vdata <- ddt


# 2. DESCRIPTIVES OF THE 12 ITEMS -------------------------------------------

# putting v12 at v10 place for visualization reasons

names(ddt)[names(ddt)=="V1"] <- "Ha"
names(ddt)[names(ddt)=="V2"] <- "Hb"
names(ddt)[names(ddt)=="V3"] <- "Hc"
names(ddt)[names(ddt)=="V4"] <- "Hd"
names(ddt)[names(ddt)=="V5"] <- "Va"
names(ddt)[names(ddt)=="V6"] <- "Vb"
names(ddt)[names(ddt)=="V7"] <- "Vc"
names(ddt)[names(ddt)=="V8"] <- "Vd"
names(ddt)[names(ddt)=="V9"] <- "Ca"
names(ddt)[names(ddt)=="V12"] <- "Cb"
names(ddt)[names(ddt)=="V10"] <- "Cc"
names(ddt)[names(ddt)=="V11"] <- "Cd"

sing <- subset(ddt, select=c(Ha:Hd, Va:Vd, Ca:Cb)) 
sing

require(dplyr)

sing <- sing %>% relocate(Cb, .before = Cc)
sing

## 1. descriptives table ----

descr <- psych::describe(sing) %>% dplyr::select(mean, sd, median)
print(descr)
write.csv(descr, "figures/final/items_descriptives.csv", row.names=T)


## 2.  correlation matrix ----

# full matrix
res <- cor(sing, method = "pearson")
write.csv(res, "figures/final/items_fullcorrelation.csv", row.names=T)

# half matrix
library(corrplot)
corrplot(res, order = "hclust", 
         tl.col = "black", tl.srt = 45)

cor(sing, method = "pearson", type = "upper")

source("http://www.sthda.com/upload/rquery_cormat.r")
correla <- rquery.cormat(sing)
correla$r
write.csv(correla$r, "figures/final/items_halfcorrelation.csv")



## 3. mean of the 12 variables across the 4 clusters (grouped subset) ----

#group means

#creating a grouped dataframe
# putting v12 at v10 place for visualization reasons
Vdata <- Vdata %>% relocate(12, .before = 10)
Vdata

# reordering clusters_4 by size
setDT(Vdata)
table(Vdata$clusters_4)
Vdata[clusters_4 == 1, clusters_4 := 5] 
Vdata[clusters_4 == 2, clusters_4 := 7]
Vdata[clusters_4 == 3, clusters_4 := 8]
Vdata[clusters_4 == 4, clusters_4 := 6]
table(Vdata$clusters_4)
Vdata[clusters_4 == 5, clusters_4 := 1] 
Vdata[clusters_4 == 6, clusters_4 := 2]
Vdata[clusters_4 == 7, clusters_4 := 3]
Vdata[clusters_4 == 8, clusters_4 := 4]
table(Vdata$clusters_4)


byclst4 <- Vdata %>% group_by(clusters_4)
byclst4



#asking to show the mean of each variable
means <- byclst4 %>% summarise(
  V1 = mean(V1),
  V2 = mean(V2),
  V3 = mean(V3),
  V4 = mean(V4),
  V5 = mean(V5),
  V6 = mean(V6),
  V7 = mean(V7),
  V8 = mean(V8),
  V9 = mean(V9),
  V12 = mean(V12),
  V10 = mean(V10),
  V11 = mean(V11)
)
means

write.csv(means, "figures/final/items_clustermeans.csv", row.names=F)


# and means of the components 

byclst4 %>% summarise(
  informative = mean(Dim.1),
  general = mean(Dim.2)
)




## 3. correlation of the 2 factors across the 4/3 clusters (grouped subset)
# by cluster
byclst4 %>% summarise(
  COR = cor(Dim.1, Dim.2, method = "pearson")
)
# remember overall corr between two comp is 0
# cor(Vdata$Dim.1, Vdata$Dim.2, method = "pearson")







