#
#
# LL
# 29 November, 2021 - Checked September 2022
# Socio Demo Attitude on clusters
# Data cleaning
# Creating dataset: sull
# Creating dataset: socdem clean

#clear
rm(list = ls())


library("data.table")
library("tidyverse")
library("foreign")




# 1. LOAD DATA -----------------------------------------------------------


############################################!
####### DO NOT RUN EVERY TIME ----
############################################!

dt <- readRDS("data/clean/results_wide_Feb_item&comp&clus.rds")
dt

attr <- read.spss("data/raw/ebs_526_SPSS - Copy.sav", to.data.frame = T, use.value.labels = F) 
attr


# 2. MERGE DATA ---------------------------

# variables type have to match

setDT(dt)
setDT(attr)
class(dt)
class(attr)

# check if merging variables are of the same type
class(dt$Unique_ID)
class(attr$Unique_ID)

# Merging in data.table

dt.m <- merge(dt, attr, by = "Unique_ID", all.x=T, all.y=T)
dt.m
nrow(dt.m) == nrow(dt)
#FALSE is correct bcz dt does not have missing values

is.na(dt.m$Dim.1)

# merge on tydiverse
#df.m <- left_join(dt, attr, by = "Unique_ID")
#df.m



# 3. SAVE DATASET ---------------------------

saveRDS(dt.m, "data/clean/full_Feb_stata&comp&clust.rds")



############################################!
####### RUN FROM HERE ----
############################################!

# 4. EXPLORE DATASET ---------------------------
#clear
rm(list = ls())


library("data.table")
library("tidyverse")
library("foreign")


# explore dataset through the questionnaire
#subsetting to the variables needed

dt <- readRDS("data/clean/full_Feb_stata&comp&clust.rds")
dt

#* variables needed for profiling:
#* SOCIODEMO: D90.1 (=_v135) how religious or spiritual 1-10 (recode: 1-3, 4-7, 8-10)
#*            D90.2 (=_v137) religious denominations
#*            D92A highest level edu mother
#*            D92B highest level edu father --> compute edupar
#*            SD5R age in age intervals
#*            D1 left-right scale 
#*            D7 marital status --> to recode (consider the kids)
#*            D8 age when stopped full time education --> to recode
#*            D10 gender
#*            C14 employment recoded by Eurobarometer
#*            D25 area of living
#*            D60 difficulties in paying bills
#*            D62_1 D62_2 D62_3 D62_4 internet use --> recode into a summation index
#*            D63 self assessed position in society
#*            D70 life satisfaction
#*            SD18 satisfaction with democracy in country 
#*            B country of fieldwork
#* 
#* ATTITUDINAL: QA16.01:QA16.11 impediments to engagemnet
#*              QA7 appropriate level of public engagement
#*              QA2_1:QA2_6 interest in medicine/science/sport/arts/politics
#*              Q13A:Q13F decision on scitech based on
#*              QA17_1:QA17_8 exclusion / science elite faireness perception
#*              QA20_1:QA20_11 + QA20T science literacy
#*              D93.1:D93.4 family members close to science or themselves
#*            

  # 4.1 SUBSET DATASET ---------------------------

try <- subset(dt, select = c(Unique_ID, V1, V2, V3, V4, V5, V6, V7, V8, V9, V12, V10, V11, 
                             Dim.1, Dim.2, clusters_2, clusters_3, clusters_4, clusters_5, D90.1, D90.1NET, D90.2, D92A, D92ANET, D92B, D92BNET, SD5R, 
                             D1, D7, D8, D8R, D10, C14, D25, D1.3, D60, D62_1, D62_2, D62_3, D62_4, D62R.1, D62R.2, D62R.3, D62R.4, D63, D70,
                             SD18, B, QA16.01, QA16.02, QA16.03, QA16.04, QA16.05, QA16.06, QA16.07,
                             QA16.08, QA16.09, QA16.10, QA16.11, QA7, QA2_1, QA2_2, QA2_3, QA2_4, QA2_5,
                             QA2_6, QA17_1, QA17_2, QA17_3, QA17_4, QA17_5, QA17_6, QA17_7, QA17_8,
                             QA20_1, QA20_2, QA20_3, QA20_4, QA20_5, QA20_6, QA20_7, QA20_8, QA20_9,
                             QA20_10, QA20_11, QA20T, D93.1, D93.2, D93.3, D93.4,
                             QA13A, QA13B, QA13C, QA13D, QA13E, QA13F))
try 



#put Unique_ID as row name
row.names(try) <- try$Unique_ID
try$Unique_ID <- NULL
try


# 4.1 RECODE SUBSET ---------------------------

#### SOCIODEMO ----

# RELIGIOSITY ----

# D90.1NET already recoded
# 1= not very reli / 2=not / 3=very / 4=dont know / 5=refusal
table (try$D90.1NET)
try[D90.1NET == 4, D90.1NET := NA]
try[D90.1NET == 5, D90.1NET := NA]
table (try$D90.1NET, useNA = "always")
names(try)[names(try)=="D90.1NET"] <- "relicat"
table (try$relicat, useNA = "always")


# EDUPARENTS  ----
# 1=low edu / 2=secondary / 3=higher / 4 dont know 5 refusal
## D92ANET ## --> edumoth (highest edulelvel mother)
table (try$D92ANET)
try[D92ANET == 4, D92ANET := 0]
try[D92ANET == 5, D92ANET := NA]
table (try$D92ANET, useNA = "always")
names(try)[names(try)=="D92ANET"] <- "edumoth"
table (try$edumoth, useNA = "always")

## D92B ## --> edufath (highest edulelvel father)
table (try$D92BNET)
try[D92BNET == 4, D92BNET := 0]
try[D92BNET == 5, D92BNET := NA]
table (try$D92BNET, useNA = "always")
names(try)[names(try)=="D92BNET"] <- "edufath"
table (try$edufath, useNA = "always")

## edupar (highest edulelvel parents)##  @@@@ and as.factor
# constructed considering the highest edulvl between the two
# those who have 1NA in either parent have NA in edupar
try$edupar <- pmax(try$edumoth, try$edufath)
unique(try[, .(edufath, edumoth, edupar)])
table (try$edupar, useNA = "always")
try[edupar == 0, edupar := NA]
table (try$edupar, useNA = "always")

# AGE  ----
## SD5R ## --> age (how old are you in age intervals)  @@@@ should be as.factor
# 1= 15-24 / 2=25-34 / 3=35-44 / 4=45-54 / 5= 55-64 / 6=65+
# 1= 15-34 / 2=35-54 / 3=55+
table (try$SD5R)
try[SD5R == 1, SD5R := 8]
try[SD5R == 2, SD5R := 8]
try[SD5R == 3, SD5R := 9]
try[SD5R == 4, SD5R := 9]
try[SD5R == 5, SD5R := 10]
try[SD5R == 6, SD5R := 10]
try[SD5R == 8, SD5R := 1]
try[SD5R == 9, SD5R := 2]
try[SD5R == 10, SD5R := 3]
try[SD5R == 7, SD5R := NA]
table (try$SD5R, useNA = "always")
names(try)[names(try)=="SD5R"] <- "age"
table (try$age, useNA = "always")

# LR SCALE  ----
## D1 ## --> lrscale (LR-scale 1-10) 
table (try$D1)
try[D1 == 11, D1 := NA]
try[D1 == 12, D1 := NA]
table (try$D1, useNA = "always")
#rename column in base R -> permanent change
names(try)[names(try)=="D1"] <- "lrscale"
table (try$lrscale, useNA = "always")

## D1.3 ## --> lrscale2 in categories
# 1-2 / 3-4 / 5-6 / 7-8 / 9-10 
table (try$D1.3)
try[D1.3 == 6, D1.3 := NA]
try[D1.3 == 7, D1.3 := NA]
table (try$D1.3, useNA = "always")
names(try)[names(try)=="D1.3"] <- "lrscale2"
table (try$lrscale2, useNA = "always")


# EDUCATION   ----

# IN CATEGORIES   1649 NA less than in eduyr
## D8R ## --> educat (how old when stopped full time edu CATEGORICAL) 
# 1=15- / 2=16-19 / 3=20+ / 4=still studying / 5=no full time education
table (try$D8R)
hist(as.vector(try$D8R))
try[D8R == 6, D8R := NA] # refusal
try[D8R == 7, D8R := NA] # don't know: so many
table(try$D8R, useNA = "always")
names(try)[names(try)=="D8R"] <- "educat"
table(try$educat, useNA = "always")

# GENDER  ----
## D10 ## --> female  
# 0=male 1=female  2=non binary
table (try$D10)
try[D10 == 1, D10 := 0]
try[D10 == 2, D10 := 1]
try[D10 == 3, D10 := 2]
table (try$D10, useNA = "always")
#rename column in base R -> permanent change
names(try)[names(try)=="D10"] <- "female"
table (try$female, useNA = "always")

# CURRENT JOB (socio professional category)  ----
# 1= self empl / 2=manager / 3=other white / 4=manual / 5=house / 6=unemp / 7=retired / 8=stude
## C14 ## --> jobcurr (current employment) 
table (try$C14, useNA = "always")
# all good, as factor
names(try)[names(try)=="C14"] <- "jobcurr"

# AREA OF LIVING  ----
## D25 ## --> larea (living area)  
# 1=rural / 2=small or middle / 3=large town
table (try$D25)
try[D25 == 4, D25 := NA]
table (try$D25, useNA = "always")
#rename column in base R -> permanent change
names(try)[names(try)=="D25"] <- "larea"
table (try$larea, useNA = "always")

# DIFFICULTIES PAYING BILLS  ----
## D60 ## --> bills (difficulties in paying bills last year)
# 1=most of time / 2=time to time / 3=almost never
table (try$D60)
try[D60 == 4, D60 := NA]
table (try$D60, useNA = "always")
#rename column in base R -> permanent change
names(try)[names(try)=="D60"] <- "bills"

# INTERNET USE  ----
#D62R.1=everyday, D62R.2=often sometimes, D62R.3=never, D62R.4=no internet,
table (try$D62R.1)
table (try$D62R.2)
try[D62R.2 == 1, D62R.2 := 2]
table (try$D62R.3)
try[D62R.3 == 1, D62R.3 := 3]
table (try$D62R.4)
try[D62R.4 == 1, D62R.4 := 4]
try$intuse <- (try$D62R.1 + try$D62R.2 + try$D62R.3 + try$D62R.4)
table (try$intuse, useNA = "always")

# SOCIAL CLASS SELF ASSESSED  ----
## D63 ## --> soclev (self assessed level in society)
# 1= low / 2=middle / 3=high
table (try$D63)
try[D63 == 2, D63 := 1]
try[D63 == 3, D63 := 2]
try[D63 == 4, D63 := 3]
try[D63 == 5, D63 := 3]
try[D63 == 6, D63 := NA]
try[D63 == 7, D63 := NA]
try[D63 == 8, D63 := NA]
try[D63 == 9, D63 := NA]
table (try$D63, useNA = "always")
#rename column in base R -> permanent change
names(try)[names(try)=="D63"] <- "soclev"

# COUNTRY OF FIELDWORK  ----
## B ## --> cntry (country of fieldwork)
table (try$B)
try[B == 41, B := 40]
try[B == 42, B := 40]
#rename column in base R -> permanent change
names(try)[names(try)=="B"] <- "cntry"
try
# need a label variable for cntry
try[cntry == 1, cntrylbl := "AL"]
try[cntry == 2, cntrylbl := "AT"]
try[cntry == 3, cntrylbl := "BE"]
try[cntry == 4, cntrylbl := "BG"]
try[cntry == 5, cntrylbl := "BA"]
try[cntry == 6, cntrylbl := "CH"]
try[cntry == 7, cntrylbl := "CY"]

try[cntry == 9, cntrylbl := "CZ"]

try[cntry == 11, cntrylbl := "DK"]
try[cntry == 12, cntrylbl := "EE"]
try[cntry == 13, cntrylbl := "GR"]
try[cntry == 14, cntrylbl := "ES"]
try[cntry == 15, cntrylbl := "FI"]
try[cntry == 16, cntrylbl := "FR"]
try[cntry == 17, cntrylbl := "HR"]
try[cntry == 18, cntrylbl := "HU"]
try[cntry == 19, cntrylbl := "IE"]
try[cntry == 20, cntrylbl := "IS"]
try[cntry == 21, cntrylbl := "IT"]
try[cntry == 22, cntrylbl := "KV"]
try[cntry == 23, cntrylbl := "LT"]
try[cntry == 24, cntrylbl := "LU"]
try[cntry == 25, cntrylbl := "LV"]
try[cntry == 26, cntrylbl := "ME"]
try[cntry == 27, cntrylbl := "MK"]
try[cntry == 28, cntrylbl := "MT"]
try[cntry == 29, cntrylbl := "NL"]
try[cntry == 30, cntrylbl := "NO"]
try[cntry == 31, cntrylbl := "PL"]
try[cntry == 32, cntrylbl := "PT"]
try[cntry == 33, cntrylbl := "RO"]
try[cntry == 34, cntrylbl := "RS"]
try[cntry == 35, cntrylbl := "SE"]
try[cntry == 36, cntrylbl := "SI"]
try[cntry == 37, cntrylbl := "SK"]
try[cntry == 38, cntrylbl := "TR"]
try[cntry == 39, cntrylbl := "UK"]
try[cntry == 40, cntrylbl := "DE"]
unique(try[, .(cntry, cntrylbl)])


try


#### ATTITUDINAL#####################################################

# IMPEDIMENTS TO ENGAGEMENT  ----
##QA16.01:QA16.11## impediments to engagement

## QA16.01 ## --> lacktime (lack time)
table (try$QA16.01)
# 0 = not selected  1 = yes
#rename column in base R -> permanent change
names(try)[names(try)=="QA16.01"] <- "lacktime"
table (try$lacktime, useNA = "always")

## QA16.02 ## --> lackfin (lack of financial resources)
table (try$QA16.02)
# 0 = not selected  1 = yes
#rename column in base R -> permanent change
names(try)[names(try)=="QA16.02"] <- "lackfin"


## QA16.03 ## --> lackint (lack of interest)
table (try$QA16.03)
# 0 = not selected  1 = yes
#rename column in base R -> permanent change
names(try)[names(try)=="QA16.03"] <- "lackint"


## QA16.04 ## --> lackinfo (lack of information on activities or events related to science and technology)
table (try$QA16.04)
# 0 = not selected  1 = yes
#rename column in base R -> permanent change
names(try)[names(try)=="QA16.04"] <- "lackinfo"


## QA16.05 ## --> lackknowl (lack of knowledge in the field of science and technology)
table (try$QA16.05)
# 0 = not selected  1 = yes
#rename column in base R -> permanent change
names(try)[names(try)=="QA16.05"] <- "lackknowl"


## QA16.06 ## --> lackactiv (lack or poor quality activities or events related to science and technology in the area you live)
table (try$QA16.06)
# 0 = not selected  1 = yes
#rename column in base R -> permanent change
names(try)[names(try)=="QA16.06"] <- "lackactiv"


## QA16.07 ## --> lackwelc (feeling that you would not be welcomed or that it is somehting for you)
table (try$QA16.07)
# 0 = not selected  1 = yes
#rename column in base R -> permanent change
names(try)[names(try)=="QA16.07"] <- "lackwelc"


## QA16.08 ## --> lackpriv (Privacy concerns, e.g. data misuse)
table (try$QA16.08)
# 0 = not selected  1 = yes
#rename column in base R -> permanent change
names(try)[names(try)=="QA16.08"] <- "lackpriv"


# PUBLIC DECISION MAKING  ----
##QA7## --> author  (appropriate level of public engagement)
table (try$QA7)
#recoding dk and other into NA
#reversing the scale for more meaningful interpretation (from more to less public involvement = from less to more "authoritarianism")
try[QA7 == 4, author := 0]
try[QA7 == 3, author := 1]
try[QA7 == 2, author := 2]    
try[QA7 == 1, author := 3]  
try[QA7 == 6, author := NA]
try[QA7 == 5, author := NA]
table (try$author, useNA = "always")
# 0=pubopin main concern / 1= public consulted / 2= public informed / 3=public no involv


# INTEREST IN SCIENCE  ----
##QA2_2## --> intsci
table (try$QA2_2)
try[QA2_2 == 3, intsci := 0]
try[QA2_2 == 2, intsci := 1]  
try[QA2_2 == 1, intsci := 2]
try[QA2_2 == 4, intsci := NA]
table (try$intsci, useNA = "always")
# 0=not at all / 1= moderately / 2= very interested


# SCIENCE LITERACY  ----
## QA20T ## --> sciliter (sum of the correct answers) no missings
# 1=less 5 correct / 2= 5-8 / 3=more than 8
table (try$QA20T)
names(try)[names(try)=="QA20T"] <- "sciliter"
try


# CLOSENESS TO SCIENCE  ----
##D93.1:D93.4## family members close to science or themselves

## D93.1 ## --> scime
table (try$D93.1)
names(try)[names(try)=="D93.1"] <- "scime"

## D93.2 ## --> scifam
table (try$D93.2)
try[D93.2 == 1, D93.2 := 2]
names(try)[names(try)=="D93.2"] <- "scifam"

## D93.3 ## --> scino
table (try$D93.3)
try[D93.3 == 1, D93.3 := 3]
names(try)[names(try)=="D93.3"] <- "scino"


# SCIENCE GOVERNANCE  ----
## QA13A decision on science and tech based on people or scientists
table (try$QA13A)
# 1=expers 2=thepoeple
try[QA13A == 3, QA13A := NA]
names(try)[names(try)=="QA13A"] <- "governance"
table (try$governance, useNA = "always")


#############################################!
# 5. FIXING CLUSTERS ORDER ----
#############################################!

# reordering clusters_4 by size
setDT(try)
table(try$clusters_4)
try[clusters_4 == 1, clusters_4 := 5] 
try[clusters_4 == 2, clusters_4 := 7]
try[clusters_4 == 3, clusters_4 := 8]
try[clusters_4 == 4, clusters_4 := 6]
table(try$clusters_4)
try[clusters_4 == 5, clusters_4 := 1] 
try[clusters_4 == 6, clusters_4 := 2]
try[clusters_4 == 7, clusters_4 := 3]
try[clusters_4 == 8, clusters_4 := 4]
table(try$clusters_4)


## Unique_ID ## call back

try$Unique_ID <- row.names(try)
try

# final subset


final <- subset(try, select = c(Unique_ID, V1, V2, V3, V4, V5, V6, V7, V8, V9, V12, V10, V11,
                                clusters_4 , clusters_5, clusters_3, Dim.1, Dim.2, 
                                female, age, larea, jobcurr, educat, relicat, edupar, intuse,
                                lrscale, lrscale2, bills, soclev, cntry, cntrylbl, 
                                lacktime, lackfin, lackint, lackinfo, lackknowl, lackactiv, lackwelc, lackpriv,
                                intsci, author, governance, scime, scifam, scino, sciliter))
                                



final

# 6. SAVE SUBSET ---------------------------


saveRDS(final, "data/clean/complete_clust_demo&att.rds")












