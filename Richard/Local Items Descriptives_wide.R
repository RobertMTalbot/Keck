# Richard Noone
# Keck Project
# Local Items frequencies, reliability, item-total correlations
# packages used: dplyr, CTT, recoder, ggplot2
# 11-06-2017
# Purpose:
#------------------------------------------------------------------
# this script will:
#       1) replicate Jennifers' year 2 descriptive statistics in ordinal scale using the wide format data
#       2) replicate Jannifer's regressions using those results
#---------------------------------------------------------------
setwd("/Users/richardnoone/Dropbox/2017 CU Fall/Github/Keck/Keck Analysis")

library(dplyr)
library(CTT)
library(recoder)
library(ggplot2)
# getting data
g <- read.csv("GCAStudy_ALL DATA__09072017_working.csv", skip = 1, header=T)

head(g)
str(g)
colnames(g)

#Only retain students with valid student IDs
g<-g[!(is.na(g$StudentID)),]

#---------------------------------------------------------------------------------
# need to make some changes to demographic variables in order to run regressions

# sex changing male from 1 to 0 and female from 2 to 1
g$Sex <- recoder(g$Sex, '1:0; 2:1')

# Now moving on to ethnicity, which looks like it got complicated as a result of some choosing more than one option
g$more.than.one_eth <- apply(g[,71:76], MARGIN=1, function(x) {sum(x)}) 
g$more.than.one_eth <- ifelse(g$more.than.one_eth >1, 1, 0)
g$White <- ifelse(g$more.than.one_eth == 0 & g$Demo4White5 == 1, 1, 0)
g$Black <- ifelse(g$more.than.one_eth == 0 & g$Demo4Black3 == 1, 1, 0)
g$Asian <- ifelse(g$more.than.one_eth == 0 & g$Demo4Asian2 == 1, 1, 0)
g$AmInd <- ifelse(g$more.than.one_eth == 0 & g$Demo4AmIndAl1 == 1, 1, 0)
g$Haw <- ifelse(g$more.than.one_eth == 0 & g$Demo4Haw4 == 1, 1, 0)
g$Hisp <- ifelse(g$White == 0 & g$Black==0 & g$Asian==0 & g$AmInd==0 & g$Haw==0 & g$more.than.one_eth==0 & g$Ethnicity == 1, 1, 0) # Hispanic was obscure from the Stata code, but I think this captures it

# now moving to class (freshman, sophpomore, ...)
g$Freshman <- ifelse(g$Class == 1, 1, 0)
g$Sophomore <- ifelse(g$Class == 2, 1, 0)
g$Junior <- ifelse(g$Class == 3, 1, 0)
g$Senior <- ifelse(g$Class == 4, 1, 0)
g$fifth_year <- ifelse(g$Class == 5, 1, 0)
g$other_year <- ifelse(g$Class == 6, 1, 0)

# now doing Institution dummies
g$CU <- ifelse(g$cl == 1 | g$cl== 7 | g$cl==11 | g$cl==17, 1, 0)
g$JHU <- ifelse(g$cl==2 | g$cl==12, 1, 0)
g$Metro <- ifelse(g$cl==3 | g$cl==8 | g$cl==18 | g$cl== 13, 1, 0)
g$StMu <- ifelse(g$cl==4 | g$cl==9 | g$cl==14 | g$cl==19, 1, 0)
g$UCD <- ifelse(g$cl==5 | g$cl==6 | g$cl==15, 1, 0)
g$UGa <- ifelse(g$cl==10 | g$cl==20, 1, 0)

# finally coding for large class
g$Large_class <- ifelse(g$avgenrollpersec > 70, 1, 0) 

# ----------------------------------------------------------------------
# recoding items to ordinal scale
# subsetting pre items for recoding
pre <- select(g,ends_with("pre", ignore.case = F))
colnames(pre)
# removing extra variables
pre <- pre[,-c(1:2)]

# starting with cl1_ items
cl1_pre <- select(pre, starts_with("cl1_", ignore.case = F))
apply(cl1_pre, 2, function(x) table(x, useNA='always'))

cl1_pre$cl1_fc44_16pre <- recoder(cl1_pre$cl1_fc44_16pre, '6:3')
cl1_pre$cl1_fc42a_16pre <- recoder(cl1_pre$cl1_fc42a_16pre, '1.5:2; 2:3; 2.5:4; 3:5')
cl1_pre$cl1_fc42b_16pre <- recoder(cl1_pre$cl1_fc42b_16pre, '2:1')
cl1_pre$cl1_fc42c_16pre <- recoder(cl1_pre$cl1_fc42c_16pre, '3:1')
cl1_pre$cl1_fc42d_16pre <- recoder(cl1_pre$cl1_fc42d_16pre, '3:1')
apply(cl1_pre, 2, function(x) table(x, useNA='always'))

# change item names
colnames(cl1_pre) <- paste(colnames(cl1_pre), "ord", sep = "_")
colnames(cl1_pre)


# cl2_ irems
cl2_pre <- select(pre, starts_with("cl2_", ignore.case = F))
apply(cl2_pre, 2, function(x) table(x, useNA='always'))
cl2_pre$cl2_me3_3arev_16pre <- recoder(cl2_pre$cl2_me3_3arev_16pre, '4:1; 6:2')
cl2_pre$cl2_me3_3brev_16pre <- recoder(cl2_pre$cl2_me3_3brev_16pre, '2:1; 6:2')
cl2_pre$cl2_me3_7arev_16pre <- recoder(cl2_pre$cl2_me3_7arev_16pre, '3:1; 6:2')
cl2_pre$cl2_me3_7brev_16pre <- recoder(cl2_pre$cl2_me3_7brev_16pre, '6:4')
apply(cl2_pre, 2, function(x) table(x, useNA='always'))

# change item names
colnames(cl2_pre) <- paste(colnames(cl2_pre), "ord", sep = "_")
colnames(cl2_pre)


# cl3_ items
cl3_pre <- select(pre, starts_with("cl3_", ignore.case = F))
apply(cl3_pre, 2, function(x) table(x, useNA='always'))
# no recoding needed (all dichotomous)

# change item names
colnames(cl3_pre) <- paste(colnames(cl3_pre), "ord", sep = "_")
colnames(cl3_pre)


# cl4_ items
cl4_pre <- select(pre, starts_with("cl4_", ignore.case = F))
apply(cl4_pre, 2, function(x) table(x, useNA='always'))
cl4_pre$cl4_fcsa3a_16pre <- recoder(cl4_pre$cl4_fcsa3a_16pre, '2:1')
cl4_pre$cl4_fcp2_16pre <- recoder(cl4_pre$cl4_fcp2_16pre, '3:1')
apply(cl4_pre, 2, function(x) table(x, useNA='always'))

# change item names
colnames(cl4_pre) <- paste(colnames(cl4_pre), "ord", sep = "_")
colnames(cl4_pre)


# cl5_ items
cl5_pre <- select(pre, starts_with("cl5_", ignore.case = F))
apply(cl5_pre, 2, function(x) table(x, useNA='always'))
cl5_pre$cl5_me1_3a_16pre <- recoder(cl5_pre$cl5_me1_3a_16pre, '2:1; 4:2')
cl5_pre$cl5_me1_3b_16pre <- recoder(cl5_pre$cl5_me1_3b_16pre, '2:1; 4:2')
cl5_pre$cl5_me1_9a_16pre <- recoder(cl5_pre$cl5_me1_9a_16pre, '2:1; 3:2; 4:3; 5:4; 8:5')
cl5_pre$cl5_me1_9b_16pre <- recoder(cl5_pre$cl5_me1_9b_16pre, '2:1')
cl5_pre$cl5_me4_2a_16pre <- recoder(cl5_pre$cl5_me4_2a_16pre, '2:1; 4:2')
cl5_pre$cl5_me4_5a_16pre <- recoder(cl5_pre$cl5_me4_5a_16pre, '1.5:2; 2:3')
cl5_pre$cl5_me4_5b_16pre <- recoder(cl5_pre$cl5_me4_5b_16pre, '2.5:1')
apply(cl5_pre, 2, function(x) table(x, useNA='always'))

# change item names
colnames(cl5_pre) <- paste(colnames(cl5_pre), "ord", sep = "_")
colnames(cl5_pre)


# cl6_ items (none)


# cl7_ items
cl7_pre <- select(pre, starts_with("cl7_", ignore.case = F))
apply(cl7_pre, 2, function(x) table(x, useNA='always'))
cl7_pre$cl7_fcsa2a_16pre <- recoder(cl7_pre$cl7_fcsa2a_16pre, '5:4; 6:5')
cl7_pre$cl7_fcsa2b_16pre <- recoder(cl7_pre$cl7_fcsa2b_16pre, '3:2; 4:3; 6:4')
cl7_pre$cl7_fcsa2c_16pre <- recoder(cl7_pre$cl7_fcsa2c_16pre, '2:1')
cl7_pre$cl7_fcsa3a_16pre <- recoder(cl7_pre$cl7_fcsa3a_16pre, '3:1')
cl7_pre$cl7_fcsa3b_16pre <- recoder(cl7_pre$cl7_fcsa3b_16pre, '3:1')
cl7_pre$cl7_fcsa3c_16pre <- recoder(cl7_pre$cl7_fcsa3c_16pre, '2.5:1; 5:2')
cl7_pre$cl7_fcsa3d_16pre <- recoder(cl7_pre$cl7_fcsa3d_16pre, '3:1')
cl7_pre$cl7_fcsa1a_16pre <- recoder(cl7_pre$cl7_fcsa1a_16pre, '2:1')
cl7_pre$cl7_fcsa1b_16pre <- recoder(cl7_pre$cl7_fcsa1b_16pre, '3:1')
cl7_pre$cl7_fcsa1c_16pre <- recoder(cl7_pre$cl7_fcsa1c_16pre, '2:1')
cl7_pre$cl7_fcsa1d_16pre <- recoder(cl7_pre$cl7_fcsa1d_16pre, '3:1')
cl7_pre$cl7_fcsa5a_16pre <- recoder(cl7_pre$cl7_fcsa5a_16pre, '0.5:1')
cl7_pre$cl7_fcsa5b_16pre <- recoder(cl7_pre$cl7_fcsa5b_16pre, '2:1')
cl7_pre$cl7_fcsa5c_16pre <- recoder(cl7_pre$cl7_fcsa5c_16pre, '6:5')
apply(cl7_pre, 2, function(x) table(x, useNA='always'))

# change item names
colnames(cl7_pre) <- paste(colnames(cl7_pre), "ord", sep = "_")
colnames(cl7_pre)


# cl8_ items 
cl8_pre <- select(pre, starts_with("cl8_", ignore.case = F))
apply(cl8_pre, 2, function(x) table(x, useNA='always'))
# no recoding needed (all dichotomous)

# change item names
colnames(cl8_pre) <- paste(colnames(cl8_pre), "ord", sep = "_")
colnames(cl8_pre)


# cl9_ items
cl9_pre <- select(pre, starts_with("cl9_", ignore.case = F))
apply(cl9_pre, 2, function(x) table(x, useNA='always'))
# no recoding needed (all ordinal already)

# change item names
colnames(cl9_pre) <- paste(colnames(cl9_pre), "ord", sep = "_")
colnames(cl9_pre)


# cl10_ items
cl10_pre <- select(pre, starts_with("cl10_", ignore.case = F))
apply(cl10_pre, 2, function(x) table(x, useNA='always'))
cl10_pre$cl10_me1_44a_16pre <- recoder(cl10_pre$cl10_me1_44a_16pre, '1.2:1; 2.4:2; 3.6:3; 4.8:4; 6:5')
cl10_pre$cl10_me1_44b_16pre <- recoder(cl10_pre$cl10_me1_44b_16pre, '4:1')
cl10_pre$cl10_me1_44c_16pre <- recoder(cl10_pre$cl10_me1_44c_16pre, '4:1')
cl10_pre$cl10_me2_40_16pre <- recoder(cl10_pre$cl10_me2_40_16pre, '8:6')
cl10_pre$cl10_me3_sa7b_16pre <- recoder(cl10_pre$cl10_me3_sa7b_16pre, '2:1; 4:2; 6:3')
cl10_pre$cl10_me3_sa7c_16pre <- recoder(cl10_pre$cl10_me3_sa7c_16pre, '2:1')
cl10_pre$cl10_me4_sa6a_16pre <- recoder(cl10_pre$cl10_me4_sa6a_16pre, '2:1')
apply(cl10_pre, 2, function(x) table(x, useNA='always'))

# change item names
colnames(cl10_pre) <- paste(colnames(cl10_pre), "ord", sep = "_")
colnames(cl10_pre)


# subsetting post items for recoding
post <- select(g,ends_with("post", ignore.case = F))
colnames(post)
# removing extra variables
post <- post[,-c(1:2)]

# cl1_ items
cl1_post <- select(post, starts_with("cl1_", ignore.case = F))
apply(cl1_post, 2, function(x) table(x, useNA='always'))
cl1_post$cl1_fc42a_16post <- recoder(cl1_post$cl1_fc42a_16post, '1:0; 2:1; 3:2')
cl1_post$cl1_fc42b_16post <- recoder(cl1_post$cl1_fc42b_16post, '2:1')
cl1_post$cl1_fc42c_16post <- recoder(cl1_post$cl1_fc42c_16post, '3:1')
cl1_post$cl1_fc42d_16post <- recoder(cl1_post$cl1_fc42d_16post, '3:1')
cl1_post$cl1_fc44_16post <- recoder(cl1_post$cl1_fc44_16post, '1.5:2; 2:3; 3:4; 4:5; 4.5:6; 5:7; 6:8')
apply(cl1_post, 2, function(x) table(x, useNA='always'))

# change item names
colnames(cl1_post) <- paste(colnames(cl1_post), "ord", sep = "_")
colnames(cl1_post)


# cl2_ items
cl2_post <- select(post, starts_with("cl2_", ignore.case = F))
apply(cl2_post, 2, function(x) table(x, useNA='always'))
cl2_post$cl2_me3_3brev_16post <- recoder(cl2_post$cl2_me3_3brev_16post, '3:1; 6:2')
cl2_post$cl2_me3_7arev_16post <- recoder(cl2_post$cl2_me3_7arev_16post, '6:1')
cl2_post$cl2_me3_7brev_16post <- recoder(cl2_post$cl2_me3_7brev_16post, '3:2; 4:3; 6:4')
apply(cl2_post, 2, function(x) table(x, useNA='always'))

# change item names
colnames(cl2_post) <- paste(colnames(cl2_post), "ord", sep = "_")
colnames(cl2_post)


# cl3_ items
cl3_post <- select(post, starts_with("cl3_", ignore.case = F))
apply(cl3_post, 2, function(x) table(x, useNA='always'))
# (all dichotomous)

# change item names
colnames(cl3_post) <- paste(colnames(cl3_post), "ord", sep = "_")
colnames(cl3_post)


# cl4_ items
cl4_post <- select(post, starts_with("cl4_", ignore.case = F))
apply(cl4_post, 2, function(x) table(x, useNA='always'))
cl4_post$cl4_fcsa3a_16post <- recoder(cl4_post$cl4_fcsa3a_16post, '2:1')
cl4_post$cl4_fcsa9b_16post <- recoder(cl4_post$cl4_fcsa9b_16post, '1:0; 2:1')
cl4_post$cl4_fcp2_16post <- recoder(cl4_post$cl4_fcp2_16post, '2:1; 4:2')
apply(cl4_post, 2, function(x) table(x, useNA='always'))

# change item names
colnames(cl4_post) <- paste(colnames(cl4_post), "ord", sep = "_")
colnames(cl4_post)


# cl5_ items
cl5_post <- select(post, starts_with("cl5_", ignore.case = F))
apply(cl5_post, 2, function(x) table(x, useNA='always'))
cl5_post$cl5_me1_3a_16post <- recoder(cl5_post$cl5_me1_3a_16post, '4:1')
cl5_post$cl5_me1_3b_16post <- recoder(cl5_post$cl5_me1_3b_16post, '4:1')
cl5_post$cl5_me1_9a_16post <- recoder(cl5_post$cl5_me1_9a_16post, '3:1; 4.5:2; 5:3; 6:4; 6.5:5; 8:6')
cl5_post$cl5_me1_9b_16post <- recoder(cl5_post$cl5_me1_9b_16post, '2:1')
cl5_post$cl5_me3_4a_16post <- recoder(cl5_post$cl5_me3_4a_16post, '5:1')
cl5_post$cl5_me3_4b_16post <- recoder(cl5_post$cl5_me3_4b_16post, '4:2; 5:3')
cl5_post$cl5_me4_2a_16post <- recoder(cl5_post$cl5_me4_2a_16post, '2:1; 3:2; 6:3')
cl5_post$cl5_me4_2b_16post <- recoder(cl5_post$cl5_me4_2b_16post, '2:1; 3:2; 4:3')
cl5_post$cl5_me4_5a_16post <- recoder(cl5_post$cl5_me4_5a_16post, '1.5:2; 2:3; 2.5:4; 3:5; 3.5:6; 4:7; 5:8')
cl5_post$cl5_me4_5b_16post <- recoder(cl5_post$cl5_me4_5b_16post, '2.5:1; 5:2')
apply(cl5_post, 2, function(x) table(x, useNA='always'))

# change item names
colnames(cl5_post) <- paste(colnames(cl5_post), "ord", sep = "_")
colnames(cl5_post)


# cl7_ items
cl7_post <- select(post, starts_with("cl7_", ignore.case = F))
apply(cl7_post, 2, function(x) table(x, useNA='always'))
cl7_post$cl7_fcsa1a_16post <- recoder(cl7_post$cl7_fcsa1a_16post, '2:1')
cl7_post$cl7_fcsa1b_16post <- recoder(cl7_post$cl7_fcsa1b_16post, '1.5:1; 3:2')
cl7_post$cl7_fcsa1c_16post <- recoder(cl7_post$cl7_fcsa1c_16post, '2:1')
cl7_post$cl7_fcsa1d_16post <- recoder(cl7_post$cl7_fcsa1d_16post, '1.5:1; 3:2')
cl7_post$cl7_fcsa2a_16post <- recoder(cl7_post$cl7_fcsa2a_16post, '2:1; 3:2; 4:3; 6:4')
cl7_post$cl7_fcsa2b_16post <- recoder(cl7_post$cl7_fcsa2b_16post, '2:1; 3:2; 4:3; 6:4')
cl7_post$cl7_fcsa3a_16post <- recoder(cl7_post$cl7_fcsa3a_16post, '2:1; 3:2')
cl7_post$cl7_fcsa3b_16post <- recoder(cl7_post$cl7_fcsa3b_16post, '2:1; 3:2')
cl7_post$cl7_fcsa3c_16post <- recoder(cl7_post$cl7_fcsa3c_16post, '2.5:1; 4:2; 5:3')
cl7_post$cl7_fcsa3d_16post <- recoder(cl7_post$cl7_fcsa3d_16post, '3:1')
cl7_post$cl7_fcsa5c_16post <- recoder(cl7_post$cl7_fcsa5c_16post, '4.5:5; 5:6; 6:7')
apply(cl7_post, 2, function(x) table(x, useNA='always'))

# change item names
colnames(cl7_post) <- paste(colnames(cl7_post), "ord", sep = "_")
colnames(cl7_post)


# cl8_ items
cl8_post <- select(post, starts_with("cl8_", ignore.case = F))
apply(cl8_post, 2, function(x) table(x, useNA='always'))
# all dichotomous

# change item names
colnames(cl8_post) <- paste(colnames(cl8_post), "ord", sep = "_")
colnames(cl8_post)


# cl9_ items
cl9_post <- select(post, starts_with("cl9_", ignore.case = F))
apply(cl9_post, 2, function(x) table(x, useNA='always'))
cl9_post$cl9_fcsa1_16post <- recoder(cl9_post$cl9_fcsa1_16post, '2:1; 3:2')
cl9_post$cl9_fcsa2b_16post <- recoder(cl9_post$cl9_fcsa2b_16post, '2:1')
cl9_post$cl9_fcsa3a_16post <- recoder(cl9_post$cl9_fcsa3a_16post, '2:1')
cl9_post$cl9_fcsa3b_16post <- recoder(cl9_post$cl9_fcsa3b_16post, '2:1')
cl9_post$cl9_fcsa6a_16post <- recoder(cl9_post$cl9_fcsa6a_16post, '2:1')
cl9_post$cl9_fcp2_16post <- recoder(cl9_post$cl9_fcp2_16post, '1:0; 2:1; 3:2')
apply(cl9_post, 2, function(x) table(x, useNA='always'))

# change item names
colnames(cl9_post) <- paste(colnames(cl9_post), "ord", sep = "_")
colnames(cl9_post)


# cl10_ items
cl10_post <- select(post, starts_with("cl10_", ignore.case = F))
apply(cl10_post, 2, function(x) table(x, useNA='always'))
cl10_post$cl10_me1_44a_16post <- recoder(cl10_post$cl10_me1_44a_16post, '1.2:1; 2.4:2; 3.6:3; 4.8:4; 6:5')
cl10_post$cl10_me1_44b_16post <- recoder(cl10_post$cl10_me1_44b_16post, '4:1')
cl10_post$cl10_me1_44c_16post <- recoder(cl10_post$cl10_me1_44c_16post, '4:1')
cl10_post$cl10_me2_40_16post <- recoder(cl10_post$cl10_me2_40_16post, '8:4')
cl10_post$cl10_me3_sa7a_16post <- recoder(cl10_post$cl10_me3_sa7a_16post, '2:1; 4:2; 6:3')
cl10_post$cl10_me3_sa7b_16post <- recoder(cl10_post$cl10_me3_sa7b_16post, '2:1; 4:2')
cl10_post$cl10_me3_sa7c_16post <- recoder(cl10_post$cl10_me3_sa7c_16post, '4:1')
apply(cl10_post, 2, function(x) table(x, useNA='always'))

# change item names
colnames(cl10_post) <- paste(colnames(cl10_post), "ord", sep = "_")
colnames(cl10_post)

# add recoded iyems back into original df
g <- cbind(g, cl1_pre, cl2_pre, cl3_pre, cl4_pre, cl5_pre, cl7_pre, cl8_pre, cl9_pre, cl10_pre,
           cl1_post, cl2_post, cl3_post, cl4_post, cl5_post, cl7_post, cl8_post, cl9_post, cl10_post)

# creating item sets for scoring by matrix, class, or Institution as needed 
# Groups determined by grouping in "GCAStudy_yr2onlyprepostitems dofile.do"
# using colnames to copy paste pre items. Groups determined by grouping in "GCAStudy_yr2onlyprepostitems dofile.do"
colnames(pre)

cl12_pre <- c("cl2_me3_3arev_16pre", "cl2_me3_3brev_16pre", "cl2_me3_7arev_16pre", "cl2_me3_7brev_16pre")

cl13_pre <- c("cl3_fc13_16pre", "cl3_fc14_16pre", "cl3_fc15_16pre", "cl3_fc16_16pre", "cl3_fc19_16pre",
              "cl3_fc20_16pre", "cl3_fc23_16pre", "cl3_fc24_16pre")

cl14_pre <- c("cl4_fcsa1_16pre", "cl4_fcsa3a_16pre", "cl4_fcsa3b_16pre", "cl4_fcsa4_16pre", "cl4_fcsa5a_16pre",
              "cl4_fcsa5b_16pre", "cl4_fcsa5c_16pre", "cl4_fcsa5d_16pre", "cl4_fcsa9a_16pre", "cl4_fcsa9b_16pre",
              "cl4_fcsa9c_16pre", "cl4_fcp2_16pre")

cl11_pre1 <- c("cl1_fc26__16pre", "cl1_fc37_16pre", "cl1_fc39_16pre", "cl1_fc44_16pre")

cl11_pre2 <- c("cl1_fc28_16pre", "cl1_fc29_16pre", "cl1_fc38_16pre", "cl1_fc42a_16pre", "cl1_fc42b_16pre", 
               "cl1_fc42c_16pre", "cl1_fc42d_16pre", "cl1_fc44_16pre")

cl15_pre3 <- c("cl5_me1_3a_16pre", "cl5_me1_3b_16pre", "cl5_me1_3c_16pre", "cl5_me4_2a_16pre", "cl5_me4_2b_16pre")

cl15_pre4 <- c("cl5_me1_9a_16pre", "cl5_me1_9b_16pre", "cl5_me3_4a_16pre", "cl5_me3_4b_16pre", "cl5_me4_5a_16pre", 
               "cl5_me4_5b_16pre")

cl17_pre5 <- c("cl7_fcsa2a_16pre", "cl7_fcsa2b_16pre", "cl7_fcsa2c_16pre", "cl7_fcsa3a_16pre", "cl7_fcsa3b_16pre",
               "cl7_fcsa3c_16pre", "cl7_fcsa3d_16pre", "cl7_fc28_16pre",   "cl7_fc33_16pre",  "cl7_fc34_16pre", 
               "cl7_fc39_16pre")

cl17_pre6 <- c("cl7_fcsa1a_16pre", "cl7_fcsa1b_16pre", "cl7_fcsa1c_16pre", "cl7_fcsa1d_16pre", "cl7_fcsa5a_16pre",
               "cl7_fcsa5b_16pre", "cl7_fcsa5c_16pre", "cl7_fc30_16pre", "cl7_fc33_16pre",     "cl7_fc34_16pre",
               "cl7_fc36_16pre")

cl18_pre <- c("cl8_fc3_16pre", "cl8_fc6_16pre", "cl8_fc7_16pre", "cl8_fc8_16pre", "cl8_fc9_16pre", 
              "cl8_fc10_16pre", "cl8_fc11_16pre")

cl19_pre <- c("cl9_fcsa1_16pre", "cl9_fcsa2a_16pre", "cl9_fcsa2b_16pre", "cl9_fcsa3a_16pre", "cl9_fcsa3b_16pre", 
              "cl9_fcsa5_16pre", "cl9_fcsa6a_16pre", "cl9_fcsa6b_16pre", "cl9_fcsa6c_16pre",  "cl9_fcp2_16pre")

cl20_pre7 <- c("cl10_me1_2_16pre", "cl10_me4_mc17_16pre", "cl10_me6_41_16pre", "cl10_me1_44a_16pre", "cl10_me1_44b_16pre", 
               "cl10_me1_44c_16pre", "cl10_me2_40_16pre")

cl20_pre8 <- c("cl10_me5_tf20_16pre", "cl10_me5_mc29_16pre", "cl10_me6_32_16pre", "cl10_me3_sa7a_16pre", "cl10_me3_sa7b_16pre",
               "cl10_me3_sa7c_16pre", "cl10_me4_sa6a_16pre")

colnames(post)

cl12_post <- c("cl2_me3_3arev_16post", "cl2_me3_3brev_16post", "cl2_me3_7arev_16post", "cl2_me3_7brev_16post")

cl13_post <- c("cl3_fc13_16post", "cl3_fc14_16post", "cl3_fc15_16post", "cl3_fc16_16post", "cl3_fc19_16post", 
               "cl3_fc20_16post", "cl3_fc23_16post", "cl3_fc24_16post")

cl14_post <- c("cl4_fcsa1_16post", "cl4_fcsa3a_16post", "cl4_fcsa3b_16post", "cl4_fcsa4_16post", "cl4_fcsa5a_16post",
               "cl4_fcsa5b_16post", "cl4_fcsa5c_16post", "cl4_fcsa5d_16post", "cl4_fcsa9a_16post", "cl4_fcsa9b_16post",
               "cl4_fcsa9c_16post", "cl4_fcp2_16post")

cl11_post <- c("cl1_fc26_16post", "cl1_fc28_16post", "cl1_fc29_16post", "cl1_fc37_16post", "cl1_fc38_16post", 
               "cl1_fc39_16post", "cl1_fc42a_16post", "cl1_fc42b_16post", "cl1_fc42c_16post", "cl1_fc42d_16post", 
               "cl1_fc44_16post")

cl15_post <- c("cl5_me1_3a_16post", "cl5_me1_3b_16post", "cl5_me1_3c_16post", "cl5_me1_9a_16post", "cl5_me1_9b_16post", 
               "cl5_me3_4a_16post", "cl5_me3_4b_16post", "cl5_me4_2a_16post", "cl5_me4_2b_16post", "cl5_me4_5a_16post", 
               "cl5_me4_5b_16post")

cl17_post <- c("cl7_fc28_16post", "cl7_fc30_16post", "cl7_fc33_16post", "cl7_fc34_16post", "cl7_fc36_16post", 
               "cl7_fc39_16post", "cl7_fcsa1a_16post", "cl7_fcsa1b_16post", "cl7_fcsa1c_16post", "cl7_fcsa1d_16post", 
               "cl7_fcsa2a_16post", "cl7_fcsa2b_16post", "cl7_fcsa2c_16post", "cl7_fcsa3a_16post", "cl7_fcsa3b_16post", 
               "cl7_fcsa3c_16post", "cl7_fcsa3d_16post", "cl7_fcsa5a_16post", "cl7_fcsa5b_16post", "cl7_fcsa5c_16post")

cl18_post <- c("cl8_fc3_16post", "cl8_fc6_16post", "cl8_fc7_16post", "cl8_fc8_16post", "cl8_fc9_16post", 
               "cl8_fc10_16post", "cl8_fc11_16post")

cl19_post <- c("cl9_fcsa1_16post", "cl9_fcsa2a_16post", "cl9_fcsa2b_16post", "cl9_fcsa3a_16post", "cl9_fcsa3b_16post", 
               "cl9_fcsa5_16post", "cl9_fcsa6a_16post", "cl9_fcsa6b_16post", "cl9_fcsa6c_16post", "cl9_fcp2_16post")

cl20_post7 <- c("cl10_me1_2_16post", "cl10_me4_mc17_16post", "cl10_me6_41_16post", "cl10_me1_44a_16post", "cl10_me1_44b_16post", 
                "cl10_me1_44c_16post", "cl10_me2_40_16post")

cl20_post8 <- c("cl10_me5_tf20_16post", "cl10_me5_mc29_16post", "cl10_me6_32_16post", "cl10_me3_sa7a_16post", "cl10_me3_sa7b_16post", 
                "cl10_me3_sa7c_16post", "cl10_me4_sa6a_16post", "cl10_me4_sa6b_16post")


pre_items <- c(cl12_pre, cl13_pre, cl14_pre, cl11_pre1, cl11_pre2, cl15_pre3, cl15_pre4, cl17_pre5, 
               cl17_pre6, cl18_pre, cl19_pre, cl20_pre7, cl20_pre8)

post_items <- c(cl12_post, cl13_post, cl14_post, cl11_post, cl15_post, cl17_post, cl18_post,
                cl19_post, cl20_post7, cl20_post8)

# replacing NA with 0 for items if Local.pre.done == 1
# using colnames to identify indexing for ordinal pre/post items
colnames(g)
# pre items
g[g$cl==11 | g$cl==12 | g$cl==13 | g$cl==14 | g$cl==15 | g$cl==17 | g$cl==18 | g$cl==19 | g$cl==20
  & g$Local.pre.done==1, c(693:790)][is.na(g[g$cl==11 | g$cl==12 | g$cl==13 | g$cl==14 | g$cl==15 | 
                                               g$cl==17 | g$cl==18 | g$cl==19 | g$cl==20 & g$Local.pre.done==1, c(693:790)])]<-0
# post items
g[g$cl==11 | g$cl==12 | g$cl==13 | g$cl==14 | g$cl==15 | g$cl==17 | g$cl==18 | g$cl==19 | g$cl==20
  & g$Local.post.done==1, c(791:888)][is.na(g[g$cl==11 | g$cl==12 | g$cl==13 | g$cl==14 | g$cl==15 | 
                                               g$cl==17 | g$cl==18 | g$cl==19 | g$cl==20 & g$Local.post.done==1, c(791:888)])]<-0



# making variables in g to hold data
g$year2_pre_total <- rep(NA,times=1774)
g$year2_pre_prop <- rep(NA,times=1774)
g$year2_post_total <- rep(NA,times=1774)
g$year2_post_prop <- rep(NA,times=1774)
# making a grouping variable for proper sorting later
g$cl_ma <- rep(NA,times=1774)
g$cl_ma <- ifelse(g$cl==17 & g$matrix_yr2pre==5, 1, 0)
g$cl_ma <- ifelse(g$cl==17 & g$matrix_yr2pre==6, 2, g$cl_ma)
g$cl_ma <- ifelse(g$cl==11 & g$matrix_yr2pre==1, 3, g$cl_ma)
g$cl_ma <- ifelse(g$cl==11 & g$matrix_yr2pre==2, 4, g$cl_ma)
g$cl_ma <- ifelse(g$cl==12, 5, g$cl_ma)
g$cl_ma <- ifelse(g$cl==13, 6, g$cl_ma)
g$cl_ma <- ifelse(g$cl==18, 7, g$cl_ma)
g$cl_ma <- ifelse(g$cl==14, 8, g$cl_ma)
g$cl_ma <- ifelse(g$cl==19, 9, g$cl_ma)
g$cl_ma <- ifelse(g$cl==15 & g$matrix_yr2pre==3, 10, g$cl_ma)
g$cl_ma <- ifelse(g$cl==15 & g$matrix_yr2pre==4, 11, g$cl_ma)
g$cl_ma <- ifelse(g$cl==20 & g$matrix_yr2post==7, 12, g$cl_ma)
g$cl_ma <- ifelse(g$cl==20 & g$matrix_yr2post==8, 13, g$cl_ma)

# Preparing conditions to work with function and loop for pre
conda <- list((g$cl==12), (g$cl==13), (g$cl==14), (g$cl==11 & g$matrix_yr2pre==1), (g$cl==11 & g$matrix_yr2pre==2), (g$cl==15 & g$matrix_yr2pre==3),
              (g$cl==15 & g$matrix_yr2pre==4), (g$cl==17 & g$matrix_yr2pre==5), (g$cl==17 & g$matrix_yr2pre==6), (g$cl==18), (g$cl==19),
              (g$cl==20 & g$matrix_yr2pre==7), (g$cl==20 & g$matrix_yr2pre==8))
condb <- list(cl12_pre, cl13_pre, cl14_pre, cl11_pre1, cl11_pre2, cl15_pre3, cl15_pre4, cl17_pre5, 
              cl17_pre6, cl18_pre, cl19_pre, cl20_pre7, cl20_pre8)

# creating a function that gets the descriptive data; x= classes/times, y=item indexes for pre
class_descy2pre <- function (a,b) {
  y2pre_total <- apply(g[a,b], 1, sum, na.rm=T)
  y2pre_mean <- round(mean(y2pre_total, na.rm = T),2)
  y2pre_max <- apply(g[a,b], 2, max, na.rm=T)
  y2pre_total_poss <- sum(y2pre_max, na.rm =T)
  y2pre_prop <- y2pre_total/y2pre_total_poss
  list2 <- list(y2pre_mean=y2pre_mean, y2pre_total_poss=y2pre_total_poss, y2pre_total=y2pre_total, y2pre_prop=y2pre_prop)
  return(list2)
}

# creating a loop with conditions based on a and b values for pre
resultsy2pre <- list(13)
for (i in 1:13) {
  mya <- conda[[i]]
  myb <- condb[[i]]
  temp <- class_descy2pre(a=mya, b=myb)
  resultsy2pre[[i]] <- temp
  g[mya,"year2_pre_total"] <- temp[["y2pre_total"]]
  g[mya,"year2_pre_prop"] <- temp[["y2pre_prop"]]
}

# Preparing conditions to work with function and loop for post
cl11_postA <- cl11_post 
cl15_postA <- cl15_post
cl17_postA <- cl17_post


condc <-  list((g$cl==12), (g$cl==13), (g$cl==14), (g$cl==11 & g$matrix_yr2pre==1), (g$cl==11 & g$matrix_yr2pre==2), (g$cl==15 & g$matrix_yr2pre==3),
               (g$cl==15 & g$matrix_yr2pre==4), (g$cl==17 & g$matrix_yr2pre==5), (g$cl==17 & g$matrix_yr2pre==6), (g$cl==18), (g$cl==19),
               (g$cl==20 & g$matrix_yr2post==7), (g$cl==20 & g$matrix_yr2post==8))

condd <- list(cl12_post, cl13_post, cl14_post, cl11_post, cl11_postA, cl15_post, cl15_postA, cl17_post, cl17_postA, cl18_post,
           cl19_post, cl20_post7, cl20_post8)


# creating a function that gets the descriptive data; x= classes/times, y=item indexes for post
class_descy2post <- function (c,d) {
  y2post_total <- apply(g[c,d], 1, sum, na.rm=T)
  y2post_mean <- round(mean(y2post_total, na.rm = T),2)
  y2post_max <- apply(g[c,d], 2, max, na.rm=T)
  y2post_total_poss <- sum(y2post_max, na.rm =T)
  y2post_prop <- y2post_total/y2post_total_poss
  list3 <- list(y2post_mean=y2post_mean, y2post_total_poss=y2post_total_poss, y2post_total=y2post_total, y2post_prop=y2post_prop)
  return(list3)
}

# creating a loop with conditions based on a and b values for post
resultsy2post <- list(13)
for (i in 1:13) {
  myc <- condc[[i]]
  myd <- condd[[i]]
  temp <- class_descy2post(c=myc, d=myd)
  resultsy2post[[i]] <- temp
  g[myc,"year2_post_total"] <- temp[["y2post_total"]]
  g[myc,"year2_post_prop"] <- temp[["y2post_prop"]]
}


# getting item reliability for pre
cl12.pre.reliability <- reliability(g[g$cl==12, cl12_pre], NA.Delete = T)
cl12.pre.rel <- round(cl12.pre.reliability[[3]],2)

cl13.pre.reliability <- reliability(g[g$cl==13, cl13_pre], NA.Delete = T)
cl13.pre.rel <- round(cl13.pre.reliability[[3]],2)

cl14.pre.reliability <- reliability(g[g$cl==14, cl14_pre], NA.Delete = T)
cl14.pre.rel <- round(cl14.pre.reliability[[3]],2)

cl11_pre1.reliability <- reliability(g[g$cl==11  & g$matrix_yr2pre==1, cl11_pre1], NA.Delete = T)
cl11_pre1.rel <- round(cl11_pre1.reliability[[3]],2)

cl11_pre2.reliability <- reliability(g[g$cl==11 & g$matrix_yr2pre==2, cl11_pre2], NA.Delete = T)
cl11_pre2.rel <- round(cl11_pre2.reliability[[3]],2)

cl15_pre3.reliability <- reliability(g[g$cl==15 & g$matrix_yr2pre==3, cl15_pre3], NA.Delete = T)
cl15_pre3.rel <- round(cl15_pre3.reliability[[3]],2)

cl15_pre4.reliability <- reliability(g[g$cl==15 & g$matrix_yr2pre==4, cl15_pre4], NA.Delete = T)
cl15_pre4.rel <- round(cl15_pre4.reliability[[3]],2)

cl17_pre5.reliability <- reliability(g[g$cl==17 & g$matrix_yr2pre==5, cl17_pre5], NA.Delete = T)
cl17_pre5.rel <- round(cl17_pre5.reliability[[3]],2)

cl17_pre6.reliability <- reliability(g[g$cl==17 & g$matrix_yr2pre==6, cl17_pre6], NA.Delete = T)
cl17_pre6.rel <- round(cl17_pre6.reliability[[3]],2)

cl18_pre.reliability <- reliability(g[g$cl==18, cl18_pre], NA.Delete = T)
cl18_pre.rel <- round(cl18_pre.reliability[[3]],2)

cl19_pre.reliability <- reliability(g[g$cl==19, cl19_pre], NA.Delete = T)
cl19_pre.rel <- round(cl19_pre.reliability[[3]],2)

cl20_pre7.reliability <- reliability(g[g$cl==20 & g$matrix_yr2pre==7, cl20_pre7], NA.Delete = T)
cl20_pre7.rel <- round(cl20_pre7.reliability[[3]],2)

cl20_pre8.reliability <- reliability(g[g$cl==20 & g$matrix_yr2pre==8 & g$Local.pre.done==1, cl20_pre8], NA.Delete = T)
cl20_pre8.rel <- round(cl20_pre8.reliability[[3]],2)

pre_rel <- list(cl12.pre.rel, cl13.pre.rel, cl14.pre.rel, cl11_pre1.rel, cl11_pre2.rel, cl15_pre3.rel, cl15_pre4.rel,
                cl17_pre5.rel, cl17_pre6.rel, cl18_pre.rel, cl19_pre.rel, cl20_pre7.rel, cl20_pre8.rel)

# getting item reliability for post
cl12_post.reliability <- reliability(g[g$cl==12, cl12_post], NA.Delete = T)
cl12_post.rel <- round(cl12_post.reliability[[3]],2)

cl13_post.reliability <- reliability(g[g$cl==13, cl13_post], NA.Delete = T)
cl13_post.rel <- round(cl13_post.reliability[[3]],2)

cl14_post.reliability <- reliability(g[g$cl==14, cl14_post], NA.Delete = T)
cl14_post.rel <- round(cl14_post.reliability[[3]],2)

cl11_post.reliability <- reliability(g[g$cl==11  & g$matrix_yr2pre==1, cl11_post], NA.Delete = T)
cl11_post.rel <- round(cl11_post.reliability[[3]],2)

cl11_postA.reliability <- reliability(g[g$cl==11 & g$matrix_yr2pre==2, cl11_postA], NA.Delete = T)
cl11_postA.rel <- round(cl11_postA.reliability[[3]],2)

cl15_post.reliability <- reliability(g[g$cl==15 & g$matrix_yr2pre==3, cl15_post], NA.Delete = T)
cl15_post.rel <- round(cl15_post.reliability[[3]],2)

cl15_postA.reliability <- reliability(g[g$cl==15 & g$matrix_yr2pre==4, cl15_postA], NA.Delete = T)
cl15_postA.rel <- round(cl15_postA.reliability[[3]],2)

cl17_post.reliability <- reliability(g[g$cl==17 & g$matrix_yr2pre==5, cl17_post], NA.Delete = T)
cl17_post.rel <- round(cl17_post.reliability[[3]],2)

cl17_postA.reliability <- reliability(g[g$cl==17 & g$matrix_yr2pre==6, cl17_postA], NA.Delete = T)
cl17_postA.rel <- round(cl17_postA.reliability[[3]],2)

cl18_post.reliability <- reliability(g[g$cl==18, cl18_post], NA.Delete = T)
cl18_post.rel <- round(cl18_post.reliability[[3]],2)

cl19_post.reliability <- reliability(g[g$cl==19, cl19_post], NA.Delete = T)
cl19_post.rel <- round(cl19_post.reliability[[3]],2)

cl20_post7.reliability <- reliability(g[g$cl==20 & g$matrix_yr2post==7, cl20_post7], NA.Delete = T)
cl20_post7.rel <- round(cl20_post7.reliability[[3]],2)

cl20_post8.reliability <- reliability(g[g$cl==20 & g$matrix_yr2post==8, cl20_post8], NA.Delete = T)
cl20_post8.rel <- round(cl20_post8.reliability[[3]],2)

post_rel <- list(cl12_post.rel, cl13_post.rel, cl14_post.rel, cl11_post.rel, cl11_postA.rel, cl15_post.rel,
                 cl15_postA.rel, cl17_post.rel, cl17_postA.rel, cl18_post.rel, cl19_post.rel, cl20_post7.rel, cl20_post8.rel)

# preparing for creation of table

by_course <- group_by(g,Institution,Semester,Course.Code,cl,cl_ma)
year2_results_wide <- summarise(by_course,
                           Pre_mean_prop = round(mean(year2_pre_prop, na.rm = TRUE),2),
                           Pre_mprop_sd = round(sd(year2_pre_prop, na.rm = TRUE),2),
                           Post_mean_prop = round(mean(year2_post_prop, na.rm = TRUE),2),
                           Post_mprop_sd = round(sd(year2_post_prop, na.rm = TRUE),2))
year2_results_wide<-as.data.frame(year2_results_wide) 

year2_results_wide$rm <- ifelse(year2_results_wide$cl_ma==0, 1, 0)
year2_results_wide <- year2_results[year2_results_wide$rm==0,]

# getting needed data from lists created in loop to add max, mean, and reliability columns to results df
# adding pre n column
y2_pre_n <- c(52,69,25,30,65,29,21,2,3,14,29,53,34)
year2_results_wide$Pre_n <- rep(NA,times=13)
year2_results_wide$Pre_n <- y2_pre_n
# adding pre max column
y2_pre_max <- unlist(lapply(resultsy2pre, "[[", "y2pre_total_poss"))
year2_results_wide$Pre_max <- rep(NA,times=13)
year2_results_wide$Pre_max <- y2_pre_max
# adding pre mean column
y2_pre_mean <- unlist(lapply(resultsy2pre, "[[", "y2pre_mean"))
year2_results_wide$Pre_mean <- rep(NA,times=13)
year2_results_wide$Pre_mean <- y2_pre_mean
# adding pre reliability column
y2_pre_alpha <- unlist(lapply(pre_rel, "[[", 1))
year2_results_wide$Pre_alpha <- rep(NA, times=13)
year2_results_wide$Pre_alpha <- y2_pre_alpha
# adding post n column
y2_post_n <- c(122,147,21,25,83,28,21,10,5,44,52,68,63)
year2_results_wide$Post_n <- rep(NA,times=13)
year2_results_wide$Post_n <- y2_post_n
# adding post max colum
y2_post_max <- unlist(lapply(resultsy2post, "[[", "y2post_total_poss"))
year2_results_wide$Post_max <- rep(NA,times=13)
year2_results_wide$Post_max <- y2_post_max
# adding post mean column
y2_post_mean <- unlist(lapply(resultsy2post, "[[", "y2post_mean"))
year2_results_wide$Post_mean <- rep(NA,times=13)
year2_results_wide$Post_mean <- y2_post_mean
# adding post reliability column
y2_post_alpha <- unlist(lapply(post_rel, "[[", 1))
year2_results_wide$Post_alpha <- rep(NA, times=13)
year2_results_wide$Post_alpha <- y2_post_alpha
# adding pre-post diff column
year2_results_wide$Pre_Post_Diff <- year2_results$Post_mean_prop -  year2_results$Pre_mean_prop

# removing unwanted column
year2_results_wide$rm <- NULL

# ordering columns
year2_results_wide <- year2_results_wide[,c(4,1,2,3,5,10,11,12,6,7,13,14,15,16,8,9,17,18)]
names(year2_results_wide)[5]<-"matrix"
year2_results_wide$matrix <- c(5,6,1,2,0,0,0,0,0,3,4,7,8)

#exporting table to Excel
setwd("./Keck Tables and Figures")
write.csv(format(year2_results_wide, digits=2, nsmall=2),"year1_results_wide.csv") 
