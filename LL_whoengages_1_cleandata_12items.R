#*
#*
#* LL
#* 22 November, 2021 - Checked September 2022
#* load and clean data
#* 
#* scales with original categories
#* "occasionally" and "regularly" not collapsed in 1 answer categories, but kept
#* separate
#* 
#* TO DO: RENAME THE SCALES WHEN MEANINGFUL NAME FOUND



rm(list = ls())

library(foreign)
library(data.table)


# 1. LOAD SPSS DATA  ----------------------------------------------

SPSS <- read.spss("data/raw/ebs_526_SPSS - Copy.sav", to.data.frame = T, use.value.labels = F)  
summary(SPSS[8:10]) 

#so data are loaded
#putting all the var label in a vector
SPSS
n <- names(SPSS)
n

# create a subset of data with only Q14 keeping the unique ID to be able
# to merge back other variables of interest in the future
# grepl search for matches in the argument and gives a T/F output

grepl("QA14", n)

# keeping only the TRUEs in the vector n
n <- n[grepl("QA14", n)]
n

# keep only those which do not have NET in their name
n <- n[!grepl("NET", n)]
n

# data subset associated with UniqueID
spss_qa14 <- SPSS[, c("Unique_ID", n)]
summary(spss_qa14[8:10]) 
summary(spss_qa14) 


# 2. REVERSE SCALES  ------------------------------------------------------------------------------

# invert the scales, so to higher values correspond more engagement
# I do not know how to loop, so i would have to go column by column
# I decide to melt the dataset so i get one col for the id, one for the questions and one for the corresp values

#melted dataset
setDT(spss_qa14) # transform the dataset in data.table
spss_melted <- melt(spss_qa14,
                    id.vars = c("Unique_ID"))
spss_melted

#check if everything worked with the values of the first obs
spss_melted[Unique_ID == 1, ] 
spss_qa14[Unique_ID == 1, ]


# INVERT the values
spss_melted[value == 1, value_inv := 3]
#check
spss_melted[value == 1]
spss_melted[value == 3]

#for the others
spss_melted[value == 2, value_inv := 2]
spss_melted[value == 3, value_inv := 1]
spss_melted[value == 4, value_inv := 0]
spss_melted[value == 5, value_inv := NA]

#check by showing only the unique combinations
unique(spss_melted[, .(value, value_inv)])

spss_melted


# 2.3 RE-CHANGE DATA FORMAT TO WIDE -------------------------------------------

spss_wide <- dcast(spss_melted,
                   Unique_ID  ~ variable,
                   value.var = "value_inv")

spss_wide

spss_wide[duplicated(Unique_ID), ]
# this should return 0 rows

# descriptives
summary(spss_wide) 


# 2.1. SAVE ONLY 12 ITEMS------------------------------------------------------------------------------


saveRDS(spss_wide, "data/clean/cleaned_survey_data_12Item_Feb_22.rds")






