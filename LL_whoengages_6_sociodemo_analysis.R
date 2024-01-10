#
#
# LL
# 30 November, 2021 - Checked September 2022
# Socio Demo on clusters
# 

#clear
rm(list = ls())


library("data.table")  # data management
library("tidyverse")
library("ggplot2")     # visualization
library("psych")       # descriptives

# 1. LOAD DATA -----------------------------------------------------------

dt <- readRDS("data/clean/complete_clust_demo&att.rds")
dt

# 2. PREPARE DATA ---------------------------

## 2.1 drop  NA in subset
#getting rid of the individuals with missing values on cluster variables
#obtaining the same individuals on which clusterization was performed. 

nomiss <- dt[rowSums(is.na(dt[ , 14])) == 0, ]
nomiss


# 2.2 put Unique_ID as row name
# convert to dataframe
setDF(nomiss)
nomiss
# now can set Unique_ID as row name
row.names(nomiss) <- nomiss$Unique_ID
nomiss$Unique_ID <- NULL
nomiss


## Unique_ID ## call back if needed
#nomiss$Unique_ID <- row.names(nomiss)
#nomiss



# 3. EXPLORE DATA -------------------------------------------------------------

######## 3.1 STATISTICAL RELIANCE #################

# MANOVA ANALYSIS - works
# https://www.reneshbedre.com/blog/manova.html
fact_vars <- cbind(nomiss$V1, nomiss$V2, nomiss$V3, nomiss$V4, nomiss$V5, nomiss$V6, nomiss$V7, nomiss$V8, nomiss$V9, nomiss$V10, nomiss$V11, nomiss$V12)
res.man <- manova(cbind(Dim.2, Dim.2) ~ clusters_4, data = nomiss)
res.man <- manova(fact_vars ~ clusters_4, data = nomiss)
summary(res.man)
library(effectsize)
effectsize::eta_squared(res.man)
# the clusters differ significantly
# either are they 5 or 4 or 3. With 3 pca the 3 cluster one was not significant.


# SCHEFFÉ ANALYSES of differences by component by cluster ----
library(DescTools)
model1 <- aov(Dim.1~as.factor(clusters_4), data=nomiss)
summary(model1)
model2 <- aov(Dim.2~as.factor(clusters_4), data=nomiss)
summary(model2)
ScheffeTest(model1)
ScheffeTest(model2) 
# with only 2components all the differences are significant

# COMPARISON OF TWO GROUP MEANS continuous variables ### ----
# statistical significance of difference of mean of a variale between clusters
# two sample t test
library(gridExtra)

## V1 clusters 1&2 ##
sub3 <-nomiss %>%
  filter(clusters_4=="1"|clusters_4=="2") %>%
  select(clusters_4, V1)
summary(sub3 %>% filter(clusters_4=="1") %>% .$V1)
summary(sub3 %>% filter(clusters_4=="2") %>% .$V1)
p1 <- ggplot(sub3, aes(V1)) +
  geom_histogram(fill = "white", color = "grey30") +
  facet_wrap(~ clusters_4)
p1

t.test(V1~clusters_4, data=sub3, var.equal=T )

## V1 clusters 3&2 ##
sub3 <-nomiss %>%
  filter(clusters_4=="3"|clusters_4=="2") %>%
  select(clusters_4, V1)
t.test(V1~clusters_4, data=sub3, var.equal=T )

## V1 clusters 4&2 ##
sub3 <-nomiss %>%
  filter(clusters_4=="4"|clusters_4=="2") %>%
  select(clusters_4, V1)
t.test(V1~clusters_4, data=sub3, var.equal=T )

## V1 clusters 1&3 ##
sub3 <-nomiss %>%
  filter(clusters_4=="1"|clusters_4=="3") %>%
  select(clusters_4, V1)
t.test(V1~clusters_4, data=sub3, var.equal=T )

## V1 clusters 1&4 ##
sub3 <-nomiss %>%
  filter(clusters_4=="1"|clusters_4=="4") %>%
  select(clusters_4, V1)
t.test(V1~clusters_4, data=sub3, var.equal=T )

## V1 clusters 3&4 ##
sub3 <-nomiss %>%
  filter(clusters_4=="3"|clusters_4=="4") %>%
  select(clusters_4, V1)
t.test(V1~clusters_4, data=sub3, var.equal=T )

# 4. DESCRIBE DATA -------------------------------------------------------------


 ##############"
 # DEMOGRAPHICS ----
 ##############"

# general descriptives
dem <- subset(nomiss, select = c(female, age, larea, jobcurr, educat, relicat, edupar, intuse, bills, soclev, lrscale))
descrdem <- psych::describe(dem) %>% dplyr::select(n, mean, sd, median, min, max, skew, kurtosis)
print(descrdem)


#´March2023 added the following to exclude retired people and students
filter(jobcurr != 7 & jobcurr != 8) %>% reframe


# MEANS FOR EACH ITEM IN EACH CLUSTER
byclst4 <- nomiss %>% group_by(clusters_4)

female <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(female = (prop.table(table(female, useNA = "no")))*100)
female
write.csv(female, "figures/demo/gender.csv", row.names=F)
chisq.test(nomiss$female, nomiss$clusters_4, correct=FALSE)
age <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(age = (prop.table(table(age, useNA = "no")))*100)
age
write.csv(age, "figures/demo/age.csv", row.names=F)
chisq.test(nomiss$age, nomiss$clusters_4, correct=FALSE)
larea <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(larea = (prop.table(table(larea, useNA = "no")))*100)
larea
write.csv(larea, "figures/demo/larea.csv", row.names=F)
chisq.test(nomiss$larea, nomiss$clusters_4, correct=FALSE)
jobcurr <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(jobcurr = (prop.table(table(jobcurr, useNA = "no")))*100)
jobcurr
write.csv(jobcurr, "figures/demo/jobcurr.csv", row.names=F)
chisq.test(nomiss$jobcurr, nomiss$clusters_4, correct=FALSE)
educat <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(educat = (prop.table(table(educat, useNA = "no")))*100)
educat
write.csv(educat, "figures/demo/educat.csv", row.names=F)
chisq.test(nomiss$educat, nomiss$clusters_4, correct=FALSE)
relicat <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(relicat = (prop.table(table(relicat, useNA = "no")))*100)
relicat
write.csv(relicat, "figures/demo/relicat.csv", row.names=F)
chisq.test(nomiss$relicat, nomiss$clusters_4, correct=FALSE)
edupar <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(edupar = (prop.table(table(edupar, useNA = "no")))*100)
edupar
write.csv(edupar, "figures/demo/edupar.csv", row.names=F)
chisq.test(nomiss$edupar, nomiss$clusters_4, correct=FALSE)
intuse <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(intuse = (prop.table(table(intuse, useNA = "no")))*100)
intuse
write.csv(intuse, "figures/demo/intuse.csv", row.names=F)
chisq.test(nomiss$intuse, nomiss$clusters_4, correct=FALSE)

bills <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(bills = (prop.table(table(bills, useNA = "no")))*100)
bills
write.csv(bills, "figures/demo/bills.csv", row.names=F)
chisq.test(nomiss$bills, nomiss$clusters_4, correct=FALSE)
soclev <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(soclev = (prop.table(table(soclev, useNA = "no")))*100)
soclev
write.csv(soclev, "figures/demo/soclev.csv", row.names=F)
chisq.test(nomiss$soclev, nomiss$clusters_4, correct=FALSE)
lrscale2 <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(lrscale2 = (prop.table(table(lrscale2, useNA = "no")))*100)
lrscale2
write.csv(lrscale2, "figures/demo/lrscale2.csv", row.names=F)
chisq.test(nomiss$lrscale2, nomiss$clusters_4, correct=FALSE)



 # SCHEFFÉ ANALYSES of differences by component by cluster
 library(DescTools)
 model1 <- aov(edupar~as.factor(clusters_4), data=nomiss)
 summary(model1)
 ScheffeTest(model1)


 
 
# EXPLORING EDUCATION 
# removing youngest age group to check if education changes
 try <- subset(dt, select = c(Unique_ID, clusters_4, jobcurr, age, educat))
 setDT(try)
 try
 table (try$educat)
 hist(try$educat)
 table (try$jobcurr, try$educat)
 table (try$age, try$educat)
 table (try$jobcurr, try$age)
 
 ## age intervals remove the youngest one
 table (try$age)
 try[age == 1, age := NA]
 table (try$age, useNA = "always")
 byclst4 <- try %>% group_by(clusters_4)
 byclst4 %>% summarise(
   educat = mean(educat, na.rm=T)
           )
# it does not!
 
 ## jobcurr remove the students
 table (try$jobcurr)
 try[jobcurr == 8, jobcurr := NA]
 table (try$jobcurr, useNA = "always")
 byclst4 <- try %>% group_by(clusters_4)
 byclst4 %>% summarise(
   eduyr = mean(educat, na.rm=T)
 )
 # nothing changes in the distribution of education either eliminating the youngest
 # nor eliminating the students, and not even eliminating both
 table (try$educat)
 try[educat == 1, educat := NA]
 table (try$educat, useNA = "always")
 
 byclst4 <- try %>% group_by(clusters_4)
 educat <- byclst4 %>% summarise(educat = (prop.table(table(educat, useNA = "always")))*100)
 print(educat)
 
 
 
 
 
 ##############"
 # ATTITUDINAL ----
 ##############"
 # general descriptives
 att <- subset(nomiss, select = c(lacktime, lackfin, lackint, lackinfo, lackknowl, lackactiv, lackwelc, lackpriv,
                                  intsci, author, governance, scime, scifam, scino, sciliter))
 descratt <- psych::describe(att) %>% dplyr::select(n, mean, sd, median, min, max, skew, kurtosis)
 print(descratt)

 
 #´March2023 added the following to exclude retired people and students
 filter(jobcurr != 7 & jobcurr != 8) %>% reframe
 #reframe instead of summarize
 
 byclst4 <- nomiss %>% group_by(clusters_4)
 
 # impediments to participation
 lacktime <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(lacktime = (prop.table(table(lacktime, useNA = "no")))*100) # 2 and 4 but not significant diff
 lacktime
 write.csv(lacktime, "figures/attitu/lacktime.csv", row.names=F)
 chisq.test(nomiss$lacktime, nomiss$clusters_4, correct=FALSE)
 lackfin <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(lackfin = (prop.table(table(lackfin, useNA = "no")))*100)   #
 lackfin
 write.csv(lackfin, "figures/attitu/lackfin.csv", row.names=F)
 chisq.test(nomiss$lackfin, nomiss$clusters_4, correct=FALSE)
 lackint <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(lackint = (prop.table(table(lackint, useNA = "no")))*100)   #disengaged lack interest ok
 lackint
 write.csv(lackint, "figures/attitu/lackint.csv", row.names=F)
 chisq.test(nomiss$lackint, nomiss$clusters_4, correct=FALSE)
 lackinfo <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(lackinfo = (prop.table(table(lackinfo, useNA = "no")))*100)   #
 lackinfo
 write.csv(lackinfo, "figures/attitu/lackinfo.csv", row.names=F)
 chisq.test(nomiss$lackinfo, nomiss$clusters_4, correct=FALSE)
 lackknowl <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(lackknowl = (prop.table(table(lackknowl, useNA = "no")))*100)   #
 lackknowl
 write.csv(lackknowl, "figures/attitu/lackknowl.csv", row.names=F)
 chisq.test(nomiss$lackknowl, nomiss$clusters_4, correct=FALSE)
 lackactiv <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(lackactiv = (prop.table(table(lackactiv, useNA = "no")))*100)   #
 lackactiv
 write.csv(lackactiv, "figures/attitu/lackactiv.csv", row.names=F)
 chisq.test(nomiss$lackactiv, nomiss$clusters_4, correct=FALSE)
 lackwelc <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(lackwelc = (prop.table(table(lackwelc, useNA = "no")))*100)   #
 lackwelc
 write.csv(lackwelc, "figures/attitu/lackwelc.csv", row.names=F)
 chisq.test(nomiss$lackwelc, nomiss$clusters_4, correct=FALSE)
 lackpriv <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(lackpriv = (prop.table(table(lackpriv, useNA = "no")))*100)   #
 lackpriv
 write.csv(lackpriv, "figures/attitu/lackpriv.csv", row.names=F)
 chisq.test(nomiss$lackpriv, nomiss$clusters_4, correct=FALSE)
 
 
 intsci <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(intsci = (prop.table(table(intsci, useNA = "no")))*100)   # disengaged less interested
 intsci
 write.csv(intsci, "figures/attitu/intsci.csv", row.names=F)
 chisq.test(nomiss$intsci, nomiss$clusters_4, correct=FALSE)
 
 author <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(author = (prop.table(table(author, useNA = "no")))*100)   #there are some differences
 author
 write.csv(author, "figures/attitu/author.csv", row.names=F)
 chisq.test(nomiss$author, nomiss$clusters_4, correct=FALSE)
 
 governance <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(governance = (prop.table(table(governance, useNA = "no")))*100)   #there are some differences
 governance
 write.csv(governance, "figures/attitu/governance.csv", row.names=F)
 chisq.test(nomiss$governance, nomiss$clusters_4, correct=FALSE)
 
 scime <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(scime = (prop.table(table(scime, useNA = "no")))*100)   # disengaged less close
 scime
 write.csv(scime, "figures/attitu/scime.csv", row.names=F)
 chisq.test(nomiss$scime, nomiss$clusters_4, correct=FALSE)
 scifam <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(scifam = (prop.table(table(scifam, useNA = "no")))*100)   # disengaged less close
 scifam
 write.csv(scifam, "figures/attitu/scifam.csv", row.names=F)
 chisq.test(nomiss$scifam, nomiss$clusters_4, correct=FALSE)
 scino <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(scino = (prop.table(table(scino, useNA = "no")))*100)   # disengaged less close
 scino
 write.csv(scino, "figures/attitu/scino.csv", row.names=F)
 chisq.test(nomiss$scino, nomiss$clusters_4, correct=FALSE)

 sciliter <- byclst4 %>% filter(jobcurr != 7 & jobcurr != 8) %>% reframe(sciliter = (prop.table(table(sciliter, useNA = "no")))*100)   # disengaged less knowledge huh
 sciliter
 write.csv(sciliter, "figures/attitu/sciliter.csv", row.names=F)
 chisq.test(nomiss$sciliter, nomiss$clusters_4, correct=FALSE)

 # these are the ones that make sense to look into in this paper
 
 

 
 
 

 
 
 
 
 
 
 















