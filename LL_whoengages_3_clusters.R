#
#
# LL
# January, 2021
# Hierarchical cluster analysis on server
#

#clear
rm(list = ls())


library("data.table")
library("tidyverse")
library("ggplot2")
library("factoextra")
library("rgl")
library("NbClust")
library("cluster") 
library("fastcluster")
library("tidyverse")



######################################################
# this part done on the server (runs only on a server)
######################################################

# 1. LOAD DATA -----------------------------------------------------------

# components from R PCA - Sept 2022
dt <- readRDS("Data/clean_server/components&items_Feb.rds")
dt

# SUBSET to the vars used here

#PCA scores
ddt <- subset(dt, select=c(Unique_ID, Dim.1, Dim.2))

# drop individuals with missing values:
ddt <- na.omit(ddt)
ddt


# 2. HIERARCHICAL CLUSTERING -------------------------------------------

#2.1 DISTANCE MATRIX  -----------------

# convert to dataframe
setDF(ddt)
ddt
#put Unique_ID as row name
row.names(ddt) <- ddt$Unique_ID
ddt$Unique_ID <- NULL
ddt

# distance matrix
# euclidean distance

dist <- dist(ddt, method = "euclidean")


#2.2 CLUSTERING -----------------

# hierarchical, agglomerative
# Ward.D method as it is indicated as best in papers on cluster analysis with
# ordinal variables 

clust <- hclust(dist, method = "ward.D2", members = NULL)


#DENDOGRAM 
plot(clust, cex = 0.6) # plot tree
# with RECTANGLES delimiting clusters
rect.hclust(clust, k = 4, border = 2:5)


# 2.3 DETERMINING NUMBER OF CLUSTERS ------------------

# 2.3.1 STOPPING RULES ----------------------------


# SCREE PLOT
#scree1 <- fviz_nbclust(ddt, FUN = hcut, method = "wss")
#scree1 +  geom_vline(xintercept = 4, linetype = 2)

# the line connecting the dots flattens at 4 so the n of clust should be 3/4 
# !!! does not work with full sample


# ALL with NbClust 
# computation of different Indices indicating the best number of clusters

#NbClust(data = ddt, diss = dist, distance = NULL, min.nc = 2, max.nc = 15, 
#        method = "ward.D2", index = "alllong", alphaBeale = 0.1)

# !!! does not work with full sample



# 2.3.2 CUTTING CLUSTERS ----------------------------

#number to cut at to be chosen according to dataset and intent
clusters_5 <- cutree(clust, k = 5)
head(clusters_5)
table(clusters_5)

clusters_4 <- cutree(clust, k = 4)
head(clusters_4)
table(clusters_4)

clusters_3 <- cutree(clust, k = 3)
head(clusters_3)
table(clusters_3)

clusters_2 <- cutree(clust, k = 2)
head(clusters_2)
table(clusters_2)



#DENDOGRAM again with RECTANGLES delimiting clusters
plot(clust, cex = 0.6) # plot tree
rect.hclust(clust, k = 4, border = 2:5)



# 2.3.3 ADDING CLUSTERS COLUMN IN DATASET ----------------------------

results <- cbind(ddt, clusters_5, clusters_4, clusters_3, clusters_2)
results

#matching with the Unique_ID again
results$Unique_ID <- row.names(results)
setDT(results)

#check
results[clusters_3 == 1, ]

results

  
# SAVE DATA  ----------------

saveRDS(results, "~/Typology/Data/clean_server/results_wide_Feb24_2rpca.rds")

# to be used offline






############################################
# OPEN DATASET ON LAPTOP
############################################

# ALL THIS WAS COMPUTED ON THE UNI SERVER BECAUSE OF MEMORY ISSUES

dt <- readRDS("data/clean/results_wide_Feb24_2rpca.rds") 
dt


# PROPORTIONS ----------------


table(dt$clusters_2)

# FACTOR SCORES
#* 5 clusters solution:  1=6218    2=6664     3=3560   4=9789  5= 9682 
#* 4 clusters solution:  1=16007   2=6664     3=3560   4=9682  
#* 3 clusters solution:  1=16007   2=16346    3=3560   
#* 2 clusters solution:  1=16007   2=19906   


#subset
ddt <- subset(dt, select=c(Unique_ID, clusters_4, Dim.1, Dim.2))




################################################################################
#                 Simon code with pipes
################################################################################

#disregard the already created variables
dt <- readRDS("data/clean/cleaned_survey_data_12Item_Feb_22.rds")
dt



princomp(
  dt %>% 
    select(starts_with("QA14_")) %>%
    drop_na(), 
  cor = TRUE 
) -> pca


bind_cols(
  dt %>% drop_na(starts_with("QA14_")), 
  get_pca_ind(pca)$coord[,1:2]
) 


fastcluster::hclust(
  dist(
    get_pca_ind(pca)$coord[,1:2], 
    method = "euclidean"
  ), 
  method = "ward.D2", 
  members = NULL
) -> clust

#DENDOGRAM again with RECTANGLES delimiting clusters
plot(clust, cex = 2, labels = F) # plot tree
rect.hclust(clust, k = 4, border = 2:5)


