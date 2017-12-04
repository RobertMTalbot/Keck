# Richard Noone
# Keck Project
# Local assessments pre-post descriptives
# packages used: dplyr, CTT, recoder
# 09-21-2017


setwd("/Users/richardnoone/Dropbox/2017 CU Fall/Github/Keck/Keck Analysis")

g <- read.csv("GCAStudy_ALL DATA__09072017_working.csv", skip = 1, header=T)

head(g)
str(g)
colnames(g)
summary(g)

g$StudentID<-as.character(g$StudentID)
#Only retain students with valid student IDs
g<-g[g$Student!="",]

nrow(g)

# begin with identifying scoring structures and making them all ordinal
# first I'm subsetting all of the pre-post local items so they're not mixed in with the other items
library(dplyr)
lpre <- select(g,ends_with("pre", ignore.case = F))
lpost <- select(g,ends_with("post", ignore.case = F))

str(lpre)
colnames(lpre)
str(lpost)
colnames(lpost)

lpre$matrix_yr2pre <- NULL
lpre$time_min_pre <-NULL
lpost$matrix_yr2post <- NULL
lpost$time_min_post <-NULL

# -------------------------------------------------------------------------
# recoding year two items
# cl_1
# selecting only cl1 items
lpre_cl1<-select(lpre,starts_with("cl1_", ignore.case = F))
lpost_cl1<-select(lpost,starts_with("cl1_", ignore.case = F))
# pre items
# extracting unique elements
lpre_cl1_scale <- apply(lpre_cl1, 2,function(x) table(x, useNA='always'))
lpre_cl1_scale
# recode according to lpre_cl1_scale data for each item in table 
## -----------------------------------------------------------Note that *** means that the scale was also collapsed
# item cl1_fc44_16pre: (6 -> 3)
lpre_cl1$cl1_fc44_16pre <- ifelse(lpre_cl1$cl1_fc44_16pre==6, 3, lpre_cl1$cl1_fc44_16pre)
# item cl1_fc42a_16pre: (1.0 -> 0),(1.5 -> 1), (2.0 -> 1), (2.5 -> 1), (3.0 -> 2) ***
lpre_cl1$cl1_fc42a_16pre <- ifelse(lpre_cl1$cl1_fc42a_16pre==1.0, 0, lpre_cl1$cl1_fc42a_16pre)
lpre_cl1$cl1_fc42a_16pre <- ifelse(lpre_cl1$cl1_fc42a_16pre==1.5, 1, lpre_cl1$cl1_fc42a_16pre)
lpre_cl1$cl1_fc42a_16pre <- ifelse(lpre_cl1$cl1_fc42a_16pre==2.0, 1, lpre_cl1$cl1_fc42a_16pre)
lpre_cl1$cl1_fc42a_16pre <- ifelse(lpre_cl1$cl1_fc42a_16pre==2.5, 1, lpre_cl1$cl1_fc42a_16pre)
lpre_cl1$cl1_fc42a_16pre <- ifelse(lpre_cl1$cl1_fc42a_16pre==3.0, 2, lpre_cl1$cl1_fc42a_16pre)
# item cl1_fc42b_16pre: (2 -> 1)
lpre_cl1$cl1_fc42b_16pre <- ifelse(lpre_cl1$cl1_fc42b_16pre==2, 1, lpre_cl1$cl1_fc42b_16pre)
# item cl1_fc42c_16pre: (3 -> 1)
lpre_cl1$cl1_fc42c_16pre <- ifelse(lpre_cl1$cl1_fc42c_16pre==3, 1, lpre_cl1$cl1_fc42c_16pre)
# item cl1_fc42d_16pre: (3 -> 1)
lpre_cl1$cl1_fc42d_16pre <- ifelse(lpre_cl1$cl1_fc42d_16pre==3, 1, lpre_cl1$cl1_fc42d_16pre)
#check all
lpre_cl1_scale <- apply(lpre_cl1, 2,function(x) table(x, useNA='always'))
lpre_cl1_scale

# now doing cl1_post
# extracting unique elements
lpost_cl1_scale <- apply(lpost_cl1, 2,function(x) table(x, useNA='always'))
lpost_cl1_scale
# recode according to table above
# item cl1_fc42a_16post: (1 <- 0), (2 <- 1), (3 <- 2)
lpost_cl1$cl1_fc42a_16post <- ifelse(lpost_cl1$cl1_fc42a_16post==1, 0, lpost_cl1$cl1_fc42a_16post)
lpost_cl1$cl1_fc42a_16post <- ifelse(lpost_cl1$cl1_fc42a_16post==2, 1, lpost_cl1$cl1_fc42a_16post)
lpost_cl1$cl1_fc42a_16post <- ifelse(lpost_cl1$cl1_fc42a_16post==3, 2, lpost_cl1$cl1_fc42a_16post)
# item cl1_fc42b_16post: (2 -> 1)
lpost_cl1$cl1_fc42b_16post <- ifelse(lpost_cl1$cl1_fc42b_16post==2, 1, lpost_cl1$cl1_fc42b_16post)
# item cl1_fc42c_16post: (3 -> 1)
lpost_cl1$cl1_fc42c_16post <- ifelse(lpost_cl1$cl1_fc42c_16post==3, 1, lpost_cl1$cl1_fc42c_16post)
# item cl1_fc42d_16post: (3 -> 1)
lpost_cl1$cl1_fc42d_16post <- ifelse(lpost_cl1$cl1_fc42d_16post==3, 1, lpost_cl1$cl1_fc42d_16post)
# item cl1_fc44_16post: (0.0 -> 0), (1.0 -> 1), (1.5 -> 2), (2.0 -> 3), (3.0 -> 4), (4.0 -> 5)
#                       (4.5 -> 6), (5.0 -> 7), (6.0 -> 8)
lpost_cl1$cl1_fc44_16post <- ifelse(lpost_cl1$cl1_fc44_16post==6.0, 8, lpost_cl1$cl1_fc44_16post)
lpost_cl1$cl1_fc44_16post <- ifelse(lpost_cl1$cl1_fc44_16post==5.0, 7, lpost_cl1$cl1_fc44_16post)
lpost_cl1$cl1_fc44_16post <- ifelse(lpost_cl1$cl1_fc44_16post==4.5, 6, lpost_cl1$cl1_fc44_16post)
lpost_cl1$cl1_fc44_16post <- ifelse(lpost_cl1$cl1_fc44_16post==4.0, 5, lpost_cl1$cl1_fc44_16post)
lpost_cl1$cl1_fc44_16post <- ifelse(lpost_cl1$cl1_fc44_16post==3.0, 4, lpost_cl1$cl1_fc44_16post)
lpost_cl1$cl1_fc44_16post <- ifelse(lpost_cl1$cl1_fc44_16post==2.0, 3, lpost_cl1$cl1_fc44_16post)
lpost_cl1$cl1_fc44_16post <- ifelse(lpost_cl1$cl1_fc44_16post==1.5, 2, lpost_cl1$cl1_fc44_16post)
lpost_cl1$cl1_fc44_16post <- ifelse(lpost_cl1$cl1_fc44_16post==1.0, 1, lpost_cl1$cl1_fc44_16post)
lpost_cl1$cl1_fc44_16post <- ifelse(lpost_cl1$cl1_fc44_16post==0.0, 0, lpost_cl1$cl1_fc44_16post)
#check all
lpost_cl1_scale <- apply(lpost_cl1, 2,function(x) table(x, useNA='always'))
lpost_cl1_scale
# renaming items
colnames(lpre_cl1) <- paste(colnames(lpre_cl1), "ordinal", sep = "_")
colnames(lpost_cl1) <- paste(colnames(lpost_cl1), "ordinal", sep = "_")
colnames(lpre_cl1)
colnames(lpost_cl1)

# ----------------------------------------------------------
# cl_2
# selecting only cl2 items
lpre_cl2<-select(lpre,starts_with("cl2_", ignore.case = F))
lpost_cl2<-select(lpost,starts_with("cl2_", ignore.case = F))
# pre items
# extracting unique elements
lpre_cl2_scale <- apply(lpre_cl2, 2,function(x) table(x, useNA='always'))
lpre_cl2_scale
# recode according to table above
# item cl2_me3_3arev_16pre: (4 -> 1), (6 -> 2)
lpre_cl2$cl2_me3_3arev_16pre <- ifelse(lpre_cl2$cl2_me3_3arev_16pre==4, 1, lpre_cl2$cl2_me3_3arev_16pre)
lpre_cl2$cl2_me3_3arev_16pre <- ifelse(lpre_cl2$cl2_me3_3arev_16pre==6, 2, lpre_cl2$cl2_me3_3arev_16pre)
# item cl2_me3_3brev_16pre: (2 -> 1), (6 -> 2)
lpre_cl2$cl2_me3_3brev_16pre <- ifelse(lpre_cl2$cl2_me3_3brev_16pre==2, 1, lpre_cl2$cl2_me3_3brev_16pre)
lpre_cl2$cl2_me3_3brev_16pre <- ifelse(lpre_cl2$cl2_me3_3brev_16pre==6, 2, lpre_cl2$cl2_me3_3brev_16pre)
# item cl2_me3_7arev_16pre: (6 -> 2), (3 <- 1)
lpre_cl2$cl2_me3_7arev_16pre <- ifelse(lpre_cl2$cl2_me3_7arev_16pre==6, 2, lpre_cl2$cl2_me3_7arev_16pre)
lpre_cl2$cl2_me3_7arev_16pre <- ifelse(lpre_cl2$cl2_me3_7arev_16pre==3, 1, lpre_cl2$cl2_me3_7arev_16pre)
# item cl2_me3_7brev_16pre: (6 -> 4)
lpre_cl2$cl2_me3_7brev_16pre <- ifelse(lpre_cl2$cl2_me3_7brev_16pre==6, 4, lpre_cl2$cl2_me3_7brev_16pre)
# check all
lpre_cl2_scale <- apply(lpre_cl2, 2,function(x) table(x, useNA='always'))
lpre_cl2_scale

# post items
# extracting unique elements
lpost_cl2_scale <- apply(lpost_cl2, 2,function(x) table(x, useNA='always'))
lpost_cl2_scale
# recode according to table above
# item cl2_me3_3arev_16post: (2 <- 1), (3 <- 2), (4 <- 2), (5 -> 3), (6 -> 3)***
lpost_cl2$cl2_me3_3arev_16post <- ifelse(lpost_cl2$cl2_me3_3arev_16post==2, 1, lpost_cl2$cl2_me3_3arev_16post)
lpost_cl2$cl2_me3_3arev_16post <- ifelse(lpost_cl2$cl2_me3_3arev_16post==3, 2, lpost_cl2$cl2_me3_3arev_16post)
lpost_cl2$cl2_me3_3arev_16post <- ifelse(lpost_cl2$cl2_me3_3arev_16post==4, 2, lpost_cl2$cl2_me3_3arev_16post)
lpost_cl2$cl2_me3_3arev_16post <- ifelse(lpost_cl2$cl2_me3_3arev_16post==5, 3, lpost_cl2$cl2_me3_3arev_16post)
lpost_cl2$cl2_me3_3arev_16post <- ifelse(lpost_cl2$cl2_me3_3arev_16post==6, 3, lpost_cl2$cl2_me3_3arev_16post)
# item cl2_me3_3brev_16post: (6 -> 2), (3 -> 1)
lpost_cl2$cl2_me3_3brev_16post <- ifelse(lpost_cl2$cl2_me3_3brev_16post==6, 2, lpost_cl2$cl2_me3_3brev_16post)
lpost_cl2$cl2_me3_3brev_16post <- ifelse(lpost_cl2$cl2_me3_3brev_16post==3, 1, lpost_cl2$cl2_me3_3brev_16post)
# item cl2_me3_7arev_16post: (6 -> 1)
lpost_cl2$cl2_me3_7arev_16post <- ifelse(lpost_cl2$cl2_me3_7arev_16post==6, 1, lpost_cl2$cl2_me3_7arev_16post)
# item cl2_me3_7brev_16post: (3 -> 1), (4 -> 2), (6 -> 2)***
lpost_cl2$cl2_me3_7brev_16post <- ifelse(lpost_cl2$cl2_me3_7brev_16post==3, 1, lpost_cl2$cl2_me3_7brev_16post)
lpost_cl2$cl2_me3_7brev_16post <- ifelse(lpost_cl2$cl2_me3_7brev_16post==4, 2, lpost_cl2$cl2_me3_7brev_16post)
lpost_cl2$cl2_me3_7brev_16post <- ifelse(lpost_cl2$cl2_me3_7brev_16post==6, 2, lpost_cl2$cl2_me3_7brev_16post)
# check all
lpost_cl2_scale <- apply(lpost_cl2, 2,function(x) table(x, useNA='always'))
lpost_cl2_scale
# renaming items
colnames(lpre_cl2) <- paste(colnames(lpre_cl2), "ordinal", sep = "_")
colnames(lpost_cl2) <- paste(colnames(lpost_cl2), "ordinal", sep = "_")
colnames(lpre_cl2)
colnames(lpost_cl2)

# ------------------------------------------------------------------------------
# cl3
# selecting only cl3 items
lpre_cl3<-select(lpre,starts_with("cl3_", ignore.case = F))
lpost_cl3<-select(lpost,starts_with("cl3_", ignore.case = F))
# pre items
# extracting unique elements
lpre_cl3_scale <- apply(lpre_cl3, 2,function(x) table(x, useNA='always'))
lpre_cl3_scale
# no recoding needed

# post items
# extracting unique elements
lpost_cl3_scale <- apply(lpost_cl3, 2,function(x) table(x, useNA='always'))
lpost_cl3_scale
# no recoding needed
# renaming items
colnames(lpre_cl3) <- paste(colnames(lpre_cl3), "ordinal", sep = "_")
colnames(lpost_cl3) <- paste(colnames(lpost_cl3), "ordinal", sep = "_")
colnames(lpre_cl3)
colnames(lpost_cl3)
# ------------------------------------------------------------------------------
#cl4
# selecting only cl4 items
lpre_cl4<-select(lpre,starts_with("cl4_", ignore.case = F))
lpost_cl4<-select(lpost,starts_with("cl4_", ignore.case = F))
# pre items
# extracting unique elements
lpre_cl4_scale <- apply(lpre_cl4, 2,function(x) table(x, useNA='always'))
lpre_cl4_scale
# recode according to table above
# item cl4_fcp2_16pre: (3 -> 1)
lpre_cl4$cl4_fcp2_16pre <- ifelse(lpre_cl4$cl4_fcp2_16pre==3, 1, lpre_cl4$cl4_fcp2_16pre)
# item cl4_fcsa3a_16pre: (2 -> 1)
lpre_cl4$cl4_fcsa3a_16pre <- ifelse(lpre_cl4$cl4_fcsa3a_16pre==2, 1, lpre_cl4$cl4_fcsa3a_16pre)
# check all
lpre_cl4_scale <- apply(lpre_cl4, 2,function(x) table(x, useNA='always'))
lpre_cl4_scale

# now doing post items
# extracting unique elements
lpost_cl4_scale <- apply(lpost_cl4, 2,function(x) table(x, useNA='always'))
lpost_cl4_scale
# recode according to table above
# item cl4_fcsa3a_16post: (2 -> 1)
lpost_cl4$cl4_fcsa3a_16post <- ifelse(lpost_cl4$cl4_fcsa3a_16post==2, 1, lpost_cl4$cl4_fcsa3a_16post)
# item cl4_fcsa9b_16post: (1 -> 0), (2 -> 1)
lpost_cl4$cl4_fcsa9b_16post <- ifelse(lpost_cl4$cl4_fcsa9b_16post==1, 0, lpost_cl4$cl4_fcsa9b_16post)
lpost_cl4$cl4_fcsa9b_16post <- ifelse(lpost_cl4$cl4_fcsa9b_16post==2, 1, lpost_cl4$cl4_fcsa9b_16post)
# item cl4_fcp2_16post: (2 -> 1), (4 -> 2)
lpost_cl4$cl4_fcp2_16post <- ifelse(lpost_cl4$cl4_fcp2_16post==2, 1, lpost_cl4$cl4_fcp2_16post)
lpost_cl4$cl4_fcp2_16post <- ifelse(lpost_cl4$cl4_fcp2_16post==4, 2, lpost_cl4$cl4_fcp2_16post)
# check all
lpost_cl4_scale <- apply(lpost_cl4, 2,function(x) table(x, useNA='always'))
lpost_cl4_scale
# renaming items
colnames(lpre_cl4) <- paste(colnames(lpre_cl4), "ordinal", sep = "_")
colnames(lpost_cl4) <- paste(colnames(lpost_cl4), "ordinal", sep = "_")
colnames(lpre_cl4)
colnames(lpost_cl4)
# ------------------------------------------------------------------------------
# cl5
# selecting only cl5 items
lpre_cl5<-select(lpre,starts_with("cl5_", ignore.case = F))
lpost_cl5<-select(lpost,starts_with("cl5_", ignore.case = F))
# pre items
# extracting unique elements
lpre_cl5_scale <- apply(lpre_cl5, 2,function(x) table(x, useNA='always'))
lpre_cl5_scale
# recode according to table above
# item cl5_me1_3a_16pre: (2 -> 1), (4 -> 2)
lpre_cl5$cl5_me1_3a_16pre <- ifelse(lpre_cl5$cl5_me1_3a_16pre==2, 1, lpre_cl5$cl5_me1_3a_16pre)
lpre_cl5$cl5_me1_3a_16pre <- ifelse(lpre_cl5$cl5_me1_3a_16pre==4, 2, lpre_cl5$cl5_me1_3a_16pre)
# item cl5_me1_3b_16pre: (2 -> 1), (4 -> 2)
lpre_cl5$cl5_me1_3b_16pre <- ifelse(lpre_cl5$cl5_me1_3b_16pre==2, 1, lpre_cl5$cl5_me1_3b_16pre)
lpre_cl5$cl5_me1_3b_16pre <- ifelse(lpre_cl5$cl5_me1_3b_16pre==4, 2, lpre_cl5$cl5_me1_3b_16pre)
# item cl5_me1_9a_16pre: (2 -> 1), (3 -> 2), (4 -> 3), (5 -> 4), (8 -> 5)
lpre_cl5$cl5_me1_9a_16pre <- ifelse(lpre_cl5$cl5_me1_9a_16pre==2, 1, lpre_cl5$cl5_me1_9a_16pre)
lpre_cl5$cl5_me1_9a_16pre <- ifelse(lpre_cl5$cl5_me1_9a_16pre==3, 2, lpre_cl5$cl5_me1_9a_16pre)
lpre_cl5$cl5_me1_9a_16pre <- ifelse(lpre_cl5$cl5_me1_9a_16pre==4, 3, lpre_cl5$cl5_me1_9a_16pre)
lpre_cl5$cl5_me1_9a_16pre <- ifelse(lpre_cl5$cl5_me1_9a_16pre==5, 4, lpre_cl5$cl5_me1_9a_16pre)
lpre_cl5$cl5_me1_9a_16pre <- ifelse(lpre_cl5$cl5_me1_9a_16pre==8, 5, lpre_cl5$cl5_me1_9a_16pre)
# item cl5_me1_9b_16pre: (2 -> 1)
lpre_cl5$cl5_me1_9b_16pre <- ifelse(lpre_cl5$cl5_me1_9b_16pre==2, 1, lpre_cl5$cl5_me1_9b_16pre)
# item cl5_me4_2a_16pre: (2 -> 1), (4 -> 2)
lpre_cl5$cl5_me4_2a_16pre <- ifelse(lpre_cl5$cl5_me4_2a_16pre==2, 1, lpre_cl5$cl5_me4_2a_16pre)
lpre_cl5$cl5_me4_2a_16pre <- ifelse(lpre_cl5$cl5_me4_2a_16pre==4, 2, lpre_cl5$cl5_me4_2a_16pre)
# item cl5_me4_5a_16pre: (0.0 -> 0), (1.0 -> 1), (1.5 -> 2), (2.0 -> 3)
lpre_cl5$cl5_me4_5a_16pre <- ifelse(lpre_cl5$cl5_me4_5a_16pre==2.0, 3, lpre_cl5$cl5_me4_5a_16pre)
lpre_cl5$cl5_me4_5a_16pre <- ifelse(lpre_cl5$cl5_me4_5a_16pre==1.5, 2, lpre_cl5$cl5_me4_5a_16pre)
lpre_cl5$cl5_me4_5a_16pre <- ifelse(lpre_cl5$cl5_me4_5a_16pre==1.0, 1, lpre_cl5$cl5_me4_5a_16pre)
lpre_cl5$cl5_me4_5a_16pre <- ifelse(lpre_cl5$cl5_me4_5a_16pre==0.0, 0, lpre_cl5$cl5_me4_5a_16pre)
# item cl5_me4_5b_16pre: (2.5 -> 1), (0.0 -> 0)
lpre_cl5$cl5_me4_5b_16pre <- ifelse(lpre_cl5$cl5_me4_5b_16pre==2.5, 1, lpre_cl5$cl5_me4_5b_16pre)
lpre_cl5$cl5_me4_5b_16pre <- ifelse(lpre_cl5$cl5_me4_5b_16pre==0.0, 0, lpre_cl5$cl5_me4_5b_16pre)
# check all
lpre_cl5_scale <- apply(lpre_cl5, 2,function(x) table(x, useNA='always'))
lpre_cl5_scale

# post items
# extracting unique elements
lpost_cl5_scale <- apply(lpost_cl5, 2,function(x) table(x, useNA='always'))
lpost_cl5_scale
# recode according to table above
# item cl5_me1_3a_16post: (4 -> 1)
lpost_cl5$cl5_me1_3a_16post <- ifelse(lpost_cl5$cl5_me1_3a_16post==4, 1, lpost_cl5$cl5_me1_3a_16post)
# item cl5_me1_3b_16post: (4 -> 1)
lpost_cl5$cl5_me1_3b_16post <- ifelse(lpost_cl5$cl5_me1_3b_16post==4, 1, lpost_cl5$cl5_me1_3b_16post)
# item cl5_me1_3c_16post: (2 -> 1)***
lpost_cl5$cl5_me1_3c_16post <- ifelse(lpost_cl5$cl5_me1_3c_16post==2, 1, lpost_cl5$cl5_me1_3c_16post)
# item cl5_me1_9a_16post: (0.0 -> 0), (3.0 -> 1), (4.5 -> 1), (5.0 -> 2), (6.0 -> 2), (6.5 -> 3), (8.0 -> 3)***
lpost_cl5$cl5_me1_9a_16post <- ifelse(lpost_cl5$cl5_me1_9a_16post==0.0, 0, lpost_cl5$cl5_me1_9a_16post)
lpost_cl5$cl5_me1_9a_16post <- ifelse(lpost_cl5$cl5_me1_9a_16post==3.0, 1, lpost_cl5$cl5_me1_9a_16post)
lpost_cl5$cl5_me1_9a_16post <- ifelse(lpost_cl5$cl5_me1_9a_16post==4.5, 1, lpost_cl5$cl5_me1_9a_16post)
lpost_cl5$cl5_me1_9a_16post <- ifelse(lpost_cl5$cl5_me1_9a_16post==5.0, 2, lpost_cl5$cl5_me1_9a_16post)
lpost_cl5$cl5_me1_9a_16post <- ifelse(lpost_cl5$cl5_me1_9a_16post==6.0, 2, lpost_cl5$cl5_me1_9a_16post)
lpost_cl5$cl5_me1_9a_16post <- ifelse(lpost_cl5$cl5_me1_9a_16post==6.5, 3, lpost_cl5$cl5_me1_9a_16post)
lpost_cl5$cl5_me1_9a_16post <- ifelse(lpost_cl5$cl5_me1_9a_16post==8.0, 3, lpost_cl5$cl5_me1_9a_16post)
# item cl5_me1_9b_16post: (2 -> 1)
lpost_cl5$cl5_me1_9b_16post <- ifelse(lpost_cl5$cl5_me1_9b_16post==2, 1, lpost_cl5$cl5_me1_9b_16post)
# item cl5_me3_4a_16post: (5 -> 1)
lpost_cl5$cl5_me3_4a_16post <- ifelse(lpost_cl5$cl5_me3_4a_16post==5, 1, lpost_cl5$cl5_me3_4a_16post)
# item cl5_me3_4b_16post: (4 -> 2), (5 -> 3)
lpost_cl5$cl5_me3_4b_16post <- ifelse(lpost_cl5$cl5_me3_4b_16post==4, 2, lpost_cl5$cl5_me3_4b_16post)
lpost_cl5$cl5_me3_4b_16post <- ifelse(lpost_cl5$cl5_me3_4b_16post==5, 3, lpost_cl5$cl5_me3_4b_16post)
# item cl5_me4_2a_16post: (2 -> 1), (3 <- 1), (6 -> 2)***
lpost_cl5$cl5_me4_2a_16post <- ifelse(lpost_cl5$cl5_me4_2a_16post==2, 1, lpost_cl5$cl5_me4_2a_16post)
lpost_cl5$cl5_me4_2a_16post <- ifelse(lpost_cl5$cl5_me4_2a_16post==3, 1, lpost_cl5$cl5_me4_2a_16post)
lpost_cl5$cl5_me4_2a_16post <- ifelse(lpost_cl5$cl5_me4_2a_16post==6, 2, lpost_cl5$cl5_me4_2a_16post)
# item cl5_me4_2b_16post: (2 -> 1), (3 -> 2), (4 -> 3)
lpost_cl5$cl5_me4_2b_16post <- ifelse(lpost_cl5$cl5_me4_2b_16post==2, 1, lpost_cl5$cl5_me4_2b_16post)
lpost_cl5$cl5_me4_2b_16post <- ifelse(lpost_cl5$cl5_me4_2b_16post==3, 2, lpost_cl5$cl5_me4_2b_16post)
lpost_cl5$cl5_me4_2b_16post <- ifelse(lpost_cl5$cl5_me4_2b_16post==4, 3, lpost_cl5$cl5_me4_2b_16post)
# item cl5_me4_5a_16post: (0.0 -> 0), (1.0 <- 1), (1.5 -> 1), (2.0 <- 2), (2.5 -> 2), (3.0 -> 3),
#                         (3.5 -> 3), (4.0 -> 4), (5.0 -> 5)***
lpost_cl5$cl5_me4_5a_16post <- ifelse(lpost_cl5$cl5_me4_5a_16post==0.0, 0, lpost_cl5$cl5_me4_5a_16post)
lpost_cl5$cl5_me4_5a_16post <- ifelse(lpost_cl5$cl5_me4_5a_16post==1.5, 1, lpost_cl5$cl5_me4_5a_16post)
lpost_cl5$cl5_me4_5a_16post <- ifelse(lpost_cl5$cl5_me4_5a_16post==2.5, 2, lpost_cl5$cl5_me4_5a_16post)
lpost_cl5$cl5_me4_5a_16post <- ifelse(lpost_cl5$cl5_me4_5a_16post==3.5, 3, lpost_cl5$cl5_me4_5a_16post)
# item cl5_me4_5b_16post: (0.0 -> 0), (2.5 -> 1), (5.0 -> 2)
lpost_cl5$cl5_me4_5b_16post <- ifelse(lpost_cl5$cl5_me4_5b_16post==0.0, 0, lpost_cl5$cl5_me4_5b_16post)
lpost_cl5$cl5_me4_5b_16post <- ifelse(lpost_cl5$cl5_me4_5b_16post==2.5, 1, lpost_cl5$cl5_me4_5b_16post)
lpost_cl5$cl5_me4_5b_16post <- ifelse(lpost_cl5$cl5_me4_5b_16post==5.0, 2, lpost_cl5$cl5_me4_5b_16post)
# check all
lpost_cl5_scale <- apply(lpost_cl5, 2,function(x) table(x, useNA='always'))
lpost_cl5_scale
# renaming items
colnames(lpre_cl5) <- paste(colnames(lpre_cl5), "ordinal", sep = "_")
colnames(lpost_cl5) <- paste(colnames(lpost_cl5), "ordinal", sep = "_")
colnames(lpre_cl5)
colnames(lpost_cl5)
# ------------------------------------------------------------------------------
# cl6 items do not exist pre-post
# ------------------------------------------------------------------------------
#cl7
# selecting only cl7 items
lpre_cl7<-select(lpre,starts_with("cl7_", ignore.case = F))
lpost_cl7<-select(lpost,starts_with("cl7_", ignore.case = F))
# pre items
# extracting unique elements
lpre_cl7_scale <- apply(lpre_cl7, 2,function(x) table(x, useNA='always'))
lpre_cl7_scale
# recode according to table above
# item cl7_fcsa2a_16pre: (2 -> 1), (3 -> 2), (5 -> 2), (6 -> 3)***
lpre_cl7$cl7_fcsa2a_16pre <- ifelse(lpre_cl7$cl7_fcsa2a_16pre==2, 1, lpre_cl7$cl7_fcsa2a_16pre)
lpre_cl7$cl7_fcsa2a_16pre <- ifelse(lpre_cl7$cl7_fcsa2a_16pre==3, 2, lpre_cl7$cl7_fcsa2a_16pre)
lpre_cl7$cl7_fcsa2a_16pre <- ifelse(lpre_cl7$cl7_fcsa2a_16pre==5, 2, lpre_cl7$cl7_fcsa2a_16pre)
lpre_cl7$cl7_fcsa2a_16pre <- ifelse(lpre_cl7$cl7_fcsa2a_16pre==6, 3, lpre_cl7$cl7_fcsa2a_16pre)
# item cl7_fcsa2b_16pre: (3 -> 1), (4 -> 2), (6 -> 2)***
lpre_cl7$cl7_fcsa2b_16pre <- ifelse(lpre_cl7$cl7_fcsa2b_16pre==3, 1, lpre_cl7$cl7_fcsa2b_16pre)
lpre_cl7$cl7_fcsa2b_16pre <- ifelse(lpre_cl7$cl7_fcsa2b_16pre==4, 2, lpre_cl7$cl7_fcsa2b_16pre)
lpre_cl7$cl7_fcsa2b_16pre <- ifelse(lpre_cl7$cl7_fcsa2b_16pre==6, 2, lpre_cl7$cl7_fcsa2b_16pre)
# item cl7_fcsa2c_16pre: (2 -> 1)
lpre_cl7$cl7_fcsa2c_16pre <- ifelse(lpre_cl7$cl7_fcsa2c_16pre==2, 1, lpre_cl7$cl7_fcsa2c_16pre)
# item cl7_fcsa3a_16pre: (3 -> 1)
lpre_cl7$cl7_fcsa3a_16pre <- ifelse(lpre_cl7$cl7_fcsa3a_16pre==3, 1, lpre_cl7$cl7_fcsa3a_16pre)
# item cl7_fcsa3b_16pre: (3 -> 1)
lpre_cl7$cl7_fcsa3b_16pre <- ifelse(lpre_cl7$cl7_fcsa3b_16pre==3, 1, lpre_cl7$cl7_fcsa3b_16pre)
# item cl7_fcsa3c_16pre: (0.0 -> 0), (2.5 -> 1), (5.0 -> 2)
lpre_cl7$cl7_fcsa3c_16pre <- ifelse(lpre_cl7$cl7_fcsa3c_16pre==0.0, 0, lpre_cl7$cl7_fcsa3c_16pre)
lpre_cl7$cl7_fcsa3c_16pre <- ifelse(lpre_cl7$cl7_fcsa3c_16pre==2.5, 1, lpre_cl7$cl7_fcsa3c_16pre)
lpre_cl7$cl7_fcsa3c_16pre <- ifelse(lpre_cl7$cl7_fcsa3c_16pre==5.0, 2, lpre_cl7$cl7_fcsa3c_16pre)
# item cl7_fcsa3d_16pre: (3 -> 1)
lpre_cl7$cl7_fcsa3d_16pre <- ifelse(lpre_cl7$cl7_fcsa3d_16pre==3, 1, lpre_cl7$cl7_fcsa3d_16pre)
# item cl7_fcsa1a_16pre: (2 -> 1)
lpre_cl7$cl7_fcsa1a_16pre <- ifelse(lpre_cl7$cl7_fcsa1a_16pre==2, 1, lpre_cl7$cl7_fcsa1a_16pre)
# item cl7_fcsa1b_16pre: (3 -> 1)
lpre_cl7$cl7_fcsa1b_16pre <- ifelse(lpre_cl7$cl7_fcsa1b_16pre==3, 1, lpre_cl7$cl7_fcsa1b_16pre)
# item cl7_fcsa1c_16pre: (2 -> 1)
lpre_cl7$cl7_fcsa1c_16pre <- ifelse(lpre_cl7$cl7_fcsa1c_16pre==2, 1, lpre_cl7$cl7_fcsa1c_16pre)
# item cl7_fcsa1d_16pre: (3 -> 1)
lpre_cl7$cl7_fcsa1d_16pre <- ifelse(lpre_cl7$cl7_fcsa1d_16pre==3, 1, lpre_cl7$cl7_fcsa1d_16pre)
# item cl7_fcsa5a_16pre: (0.0 -> 0), (0.5 -> 1), (2.0 -> 1)***
lpre_cl7$cl7_fcsa5a_16pre <- ifelse(lpre_cl7$cl7_fcsa5a_16pre==0.0, 0, lpre_cl7$cl7_fcsa5a_16pre)
lpre_cl7$cl7_fcsa5a_16pre <- ifelse(lpre_cl7$cl7_fcsa5a_16pre==0.5, 1, lpre_cl7$cl7_fcsa5a_16pre)
lpre_cl7$cl7_fcsa5a_16pre <- ifelse(lpre_cl7$cl7_fcsa5a_16pre==2.0, 1, lpre_cl7$cl7_fcsa5a_16pre)
# item cl7_fcsa5b_16pre: (2 -> 1)
lpre_cl7$cl7_fcsa5b_16pre <- ifelse(lpre_cl7$cl7_fcsa5b_16pre==2, 1, lpre_cl7$cl7_fcsa5b_16pre)
# item cl7_fcsa5c_16pre: (6 -> 5)
lpre_cl7$cl7_fcsa5c_16pre <- ifelse(lpre_cl7$cl7_fcsa5c_16pre==6, 5, lpre_cl7$cl7_fcsa5c_16pre)
# check all
lpre_cl7_scale <- apply(lpre_cl7, 2,function(x) table(x, useNA='always'))
lpre_cl7_scale

# post items 
# extracting unique elements
lpost_cl7_scale <- apply(lpost_cl7, 2,function(x) table(x, useNA='always'))
lpost_cl7_scale
# recode according to table above
# item cl7_fcsa1a_16post: (2 -> 1)
lpost_cl7$cl7_fcsa1a_16post <- ifelse(lpost_cl7$cl7_fcsa1a_16post==2, 1, lpost_cl7$cl7_fcsa1a_16post)
# item cl7_fcsa1b_16post: (0.0 -> 0), (1.5 -> 1), (3.0 -> 1)***
lpost_cl7$cl7_fcsa1b_16post <- ifelse(lpost_cl7$cl7_fcsa1b_16post==0.0, 0, lpost_cl7$cl7_fcsa1b_16post)
lpost_cl7$cl7_fcsa1b_16post <- ifelse(lpost_cl7$cl7_fcsa1b_16post==1.5, 1, lpost_cl7$cl7_fcsa1b_16post)
lpost_cl7$cl7_fcsa1b_16post <- ifelse(lpost_cl7$cl7_fcsa1b_16post==3.0, 1, lpost_cl7$cl7_fcsa1b_16post)
# item cl7_fcsa1c_16post: (2 -> 1)
lpost_cl7$cl7_fcsa1c_16post <- ifelse(lpost_cl7$cl7_fcsa1c_16post==2, 1, lpost_cl7$cl7_fcsa1c_16post)
# item cl7_fcsa1d_16post: (0.0 -> 0), (1.5 -> 1), (3.0 -> 2)***
lpost_cl7$cl7_fcsa1d_16post <- ifelse(lpost_cl7$cl7_fcsa1d_16post==0.0, 0, lpost_cl7$cl7_fcsa1d_16post)
lpost_cl7$cl7_fcsa1d_16post <- ifelse(lpost_cl7$cl7_fcsa1d_16post==1.5, 1, lpost_cl7$cl7_fcsa1d_16post)
lpost_cl7$cl7_fcsa1d_16post <- ifelse(lpost_cl7$cl7_fcsa1d_16post==3.0, 1, lpost_cl7$cl7_fcsa1d_16post)
# item cl7_fcsa2a_16post: (2 -> 1), (3 -> 2), (4 -> 3), (6 -> 4)
lpost_cl7$cl7_fcsa2a_16post <- ifelse(lpost_cl7$cl7_fcsa2a_16post==2, 1, lpost_cl7$cl7_fcsa2a_16post)
lpost_cl7$cl7_fcsa2a_16post <- ifelse(lpost_cl7$cl7_fcsa2a_16post==3, 2, lpost_cl7$cl7_fcsa2a_16post)
lpost_cl7$cl7_fcsa2a_16post <- ifelse(lpost_cl7$cl7_fcsa2a_16post==4, 3, lpost_cl7$cl7_fcsa2a_16post)
lpost_cl7$cl7_fcsa2a_16post <- ifelse(lpost_cl7$cl7_fcsa2a_16post==6, 4, lpost_cl7$cl7_fcsa2a_16post)
# item cl7_fcsa2b_16post: (2 -> 1), (3 -> 2), (4 -> 2), (6 -> 3)***
lpost_cl7$cl7_fcsa2b_16post <- ifelse(lpost_cl7$cl7_fcsa2b_16post==2, 1, lpost_cl7$cl7_fcsa2b_16post)
lpost_cl7$cl7_fcsa2b_16post <- ifelse(lpost_cl7$cl7_fcsa2b_16post==3, 2, lpost_cl7$cl7_fcsa2b_16post)
lpost_cl7$cl7_fcsa2b_16post <- ifelse(lpost_cl7$cl7_fcsa2b_16post==4, 2, lpost_cl7$cl7_fcsa2b_16post)
lpost_cl7$cl7_fcsa2b_16post <- ifelse(lpost_cl7$cl7_fcsa2b_16post==6, 3, lpost_cl7$cl7_fcsa2b_16post)
# item cl7_fcsa3a_16post: (2 -> 1), (3 -> 1)***
lpost_cl7$cl7_fcsa3a_16post <- ifelse(lpost_cl7$cl7_fcsa3a_16post==2, 1, lpost_cl7$cl7_fcsa3a_16post)
lpost_cl7$cl7_fcsa3a_16post <- ifelse(lpost_cl7$cl7_fcsa3a_16post==3, 1, lpost_cl7$cl7_fcsa3a_16post)
# item cl7_fcsa3b_16post: (2 -> 1), (3 -> 1)***
lpost_cl7$cl7_fcsa3b_16post <- ifelse(lpost_cl7$cl7_fcsa3b_16post==2, 1, lpost_cl7$cl7_fcsa3b_16post)
lpost_cl7$cl7_fcsa3b_16post <- ifelse(lpost_cl7$cl7_fcsa3b_16post==3, 1, lpost_cl7$cl7_fcsa3b_16post)
# item cl7_fcsa3c_16post: (0.0 -> 0), (2.5 -> 1), (4.0 -> 1), (5.0 -> 2)***
lpost_cl7$cl7_fcsa3c_16post <- ifelse(lpost_cl7$cl7_fcsa3c_16post==0.0, 0, lpost_cl7$cl7_fcsa3c_16post)
lpost_cl7$cl7_fcsa3c_16post <- ifelse(lpost_cl7$cl7_fcsa3c_16post==2.5, 1, lpost_cl7$cl7_fcsa3c_16post)
lpost_cl7$cl7_fcsa3c_16post <- ifelse(lpost_cl7$cl7_fcsa3c_16post==4.0, 1, lpost_cl7$cl7_fcsa3c_16post)
lpost_cl7$cl7_fcsa3c_16post <- ifelse(lpost_cl7$cl7_fcsa3c_16post==5.0, 2, lpost_cl7$cl7_fcsa3c_16post)
# item cl7_fcsa3d_16post: (3 -> 1)
lpost_cl7$cl7_fcsa3d_16post <- ifelse(lpost_cl7$cl7_fcsa3d_16post==3, 1, lpost_cl7$cl7_fcsa3d_16post)
# item cl7_fcsa5c_16post: (0.0 -> 0), (1.0 -> 1), (2.0 -> 1), (3.0 -> 2), (4.0 -> 2), (4.5 -> 3), (5.0 -> 3), (6.0 -> 4)***
lpost_cl7$cl7_fcsa5c_16post <- ifelse(lpost_cl7$cl7_fcsa5c_16post==2.0, 1, lpost_cl7$cl7_fcsa5c_16post)
lpost_cl7$cl7_fcsa5c_16post <- ifelse(lpost_cl7$cl7_fcsa5c_16post==3.0, 2, lpost_cl7$cl7_fcsa5c_16post)
lpost_cl7$cl7_fcsa5c_16post <- ifelse(lpost_cl7$cl7_fcsa5c_16post==4.0, 2, lpost_cl7$cl7_fcsa5c_16post)
lpost_cl7$cl7_fcsa5c_16post <- ifelse(lpost_cl7$cl7_fcsa5c_16post==4.5, 3, lpost_cl7$cl7_fcsa5c_16post)
lpost_cl7$cl7_fcsa5c_16post <- ifelse(lpost_cl7$cl7_fcsa5c_16post==5.0, 3, lpost_cl7$cl7_fcsa5c_16post)
lpost_cl7$cl7_fcsa5c_16post <- ifelse(lpost_cl7$cl7_fcsa5c_16post==6.0, 4, lpost_cl7$cl7_fcsa5c_16post)
# check all
lpost_cl7_scale <- apply(lpost_cl7, 2,function(x) table(x, useNA='always'))
lpost_cl7_scale
# renaming items
colnames(lpre_cl7) <- paste(colnames(lpre_cl7), "ordinal", sep = "_")
colnames(lpost_cl7) <- paste(colnames(lpost_cl7), "ordinal", sep = "_")
colnames(lpre_cl7)
colnames(lpost_cl7)
# ------------------------------------------------------------------------------
# cl8
# selecting only cl8 items
lpre_cl8<-select(lpre,starts_with("cl8_", ignore.case = F))
lpost_cl8<-select(lpost,starts_with("cl8_", ignore.case = F))
# pre items
# extracting unique elements
lpre_cl8_scale <- apply(lpre_cl8, 2,function(x) table(x, useNA='always'))
lpre_cl8_scale
# no recoding required
# post items
# extracting unique elements
lpost_cl8_scale <- apply(lpost_cl8, 2,function(x) table(x, useNA='always'))
lpost_cl8_scale
# no recoding required
# renaming items
colnames(lpre_cl8) <- paste(colnames(lpre_cl8), "ordinal", sep = "_")
colnames(lpost_cl8) <- paste(colnames(lpost_cl8), "ordinal", sep = "_")
colnames(lpre_cl8)
colnames(lpost_cl8)
# ------------------------------------------------------------------------------
# cl9
# selecting only cl9 items
lpre_cl9<-select(lpre,starts_with("cl9_", ignore.case = F))
lpost_cl9<-select(lpost,starts_with("cl9_", ignore.case = F))
# pre items
# extracting unique elements
lpre_cl9_scale <- apply(lpre_cl9, 2,function(x) table(x, useNA='always'))
lpre_cl9_scale
# recode according to table above
# item cl9_fcsa3a_16pre: (2 -> 1)
lpre_cl9$cl9_fcsa3a_16pre <- ifelse(lpre_cl9$cl9_fcsa3a_16pre==2, 1, lpre_cl9$cl9_fcsa3a_16pre)
# check all
lpre_cl9_scale <- apply(lpre_cl9, 2,function(x) table(x, useNA='always'))
lpre_cl9_scale

# post items
# extracting unique elements
lpost_cl9_scale <- apply(lpost_cl9, 2,function(x) table(x, useNA='always'))
lpost_cl9_scale
# recode according to table above
# item cl9_fcsa1_16post: (2 -> 1), (3 -> 2)
lpost_cl9$cl9_fcsa1_16post <- ifelse(lpost_cl9$cl9_fcsa1_16post==2, 1, lpost_cl9$cl9_fcsa1_16post)
lpost_cl9$cl9_fcsa1_16post <- ifelse(lpost_cl9$cl9_fcsa1_16post==3, 2, lpost_cl9$cl9_fcsa1_16post)
# item cl9_fcsa2b_16post: (2 -> 1)
lpost_cl9$cl9_fcsa2b_16post <- ifelse(lpost_cl9$cl9_fcsa2b_16post==2, 1, lpost_cl9$cl9_fcsa2b_16post)
# item cl9_fcsa3a_16post: (2 -> 1)
lpost_cl9$cl9_fcsa3a_16post <- ifelse(lpost_cl9$cl9_fcsa3a_16post==2, 1, lpost_cl9$cl9_fcsa3a_16post)
# item cl9_fcsa3b_16post: (2 -> 1)
lpost_cl9$cl9_fcsa3b_16post <- ifelse(lpost_cl9$cl9_fcsa3b_16post==2, 1, lpost_cl9$cl9_fcsa3b_16post)
# item cl9_fcsa6a_16post: (2 -> 1)
lpost_cl9$cl9_fcsa6a_16post <- ifelse(lpost_cl9$cl9_fcsa6a_16post==2, 1, lpost_cl9$cl9_fcsa6a_16post)
# item cl9_fcp2_16post: (1 -> 0), (2 -> 1), (3 -> 2)
lpost_cl9$cl9_fcp2_16post <- ifelse(lpost_cl9$cl9_fcp2_16post==1, 0, lpost_cl9$cl9_fcp2_16post)
lpost_cl9$cl9_fcp2_16post <- ifelse(lpost_cl9$cl9_fcp2_16post==2, 1, lpost_cl9$cl9_fcp2_16post)
lpost_cl9$cl9_fcp2_16post <- ifelse(lpost_cl9$cl9_fcp2_16post==3, 2, lpost_cl9$cl9_fcp2_16post)
# check all
lpost_cl9_scale <- apply(lpost_cl9, 2,function(x) table(x, useNA='always'))
lpost_cl9_scale
# renaming items
colnames(lpre_cl9) <- paste(colnames(lpre_cl9), "ordinal", sep = "_")
colnames(lpost_cl9) <- paste(colnames(lpost_cl9), "ordinal", sep = "_")
colnames(lpre_cl9)
colnames(lpost_cl9)
# ------------------------------------------------------------------------------
# cl10
# selecting only cl10 items
lpre_cl10<-select(lpre,starts_with("cl10_", ignore.case = F))
lpost_cl10<-select(lpost,starts_with("cl10_", ignore.case = F))
# pre items
# extracting unique elements
lpre_cl10_scale <- apply(lpre_cl10, 2,function(x) table(x, useNA='always'))
lpre_cl10_scale
# recode according to table above
# item cl10_me1_44a_16pre: (0.0 -> 0), (1.2 -> 1), (2.4 -> 2), (3.6 -> 3), (4.8 -> 4), (6.0 -> 5)
lpre_cl10$cl10_me1_44a_16pre <- ifelse(lpre_cl10$cl10_me1_44a_16pre==0.0, 0, lpre_cl10$cl10_me1_44a_16pre)
lpre_cl10$cl10_me1_44a_16pre <- ifelse(lpre_cl10$cl10_me1_44a_16pre==1.2, 1, lpre_cl10$cl10_me1_44a_16pre)
lpre_cl10$cl10_me1_44a_16pre <- ifelse(lpre_cl10$cl10_me1_44a_16pre==2.4, 2, lpre_cl10$cl10_me1_44a_16pre)
lpre_cl10$cl10_me1_44a_16pre <- ifelse(lpre_cl10$cl10_me1_44a_16pre==3.6, 3, lpre_cl10$cl10_me1_44a_16pre)
lpre_cl10$cl10_me1_44a_16pre <- ifelse(lpre_cl10$cl10_me1_44a_16pre==4.8, 4, lpre_cl10$cl10_me1_44a_16pre)
lpre_cl10$cl10_me1_44a_16pre <- ifelse(lpre_cl10$cl10_me1_44a_16pre==6.0, 5, lpre_cl10$cl10_me1_44a_16pre)
# item cl10_me1_44b_16pre: (4 -> 1)
lpre_cl10$cl10_me1_44b_16pre <- ifelse(lpre_cl10$cl10_me1_44b_16pre==4, 1, lpre_cl10$cl10_me1_44b_16pre)
# item cl10_me1_44c_16pre: (4 -> 1)
lpre_cl10$cl10_me1_44c_16pre <- ifelse(lpre_cl10$cl10_me1_44c_16pre==4, 1, lpre_cl10$cl10_me1_44c_16pre)
# item cl10_me2_40_16pre: (3 -> 2), (4 -> 2), (5 -> 3), (8 -> 3)***
lpre_cl10$cl10_me2_40_16pre <- ifelse(lpre_cl10$cl10_me2_40_16pre==3, 2, lpre_cl10$cl10_me2_40_16pre)
lpre_cl10$cl10_me2_40_16pre <- ifelse(lpre_cl10$cl10_me2_40_16pre==4, 2, lpre_cl10$cl10_me2_40_16pre)
lpre_cl10$cl10_me2_40_16pre <- ifelse(lpre_cl10$cl10_me2_40_16pre==5, 3, lpre_cl10$cl10_me2_40_16pre)
lpre_cl10$cl10_me2_40_16pre <- ifelse(lpre_cl10$cl10_me2_40_16pre==8, 3, lpre_cl10$cl10_me2_40_16pre)
# item cl10_me3_sa7b_16pre: (2 -> 1), (4 -> 2), (6 -> 3)
lpre_cl10$cl10_me3_sa7b_16pre <- ifelse(lpre_cl10$cl10_me3_sa7b_16pre==2, 1, lpre_cl10$cl10_me3_sa7b_16pre)
lpre_cl10$cl10_me3_sa7b_16pre <- ifelse(lpre_cl10$cl10_me3_sa7b_16pre==4, 2, lpre_cl10$cl10_me3_sa7b_16pre)
lpre_cl10$cl10_me3_sa7b_16pre <- ifelse(lpre_cl10$cl10_me3_sa7b_16pre==6, 3, lpre_cl10$cl10_me3_sa7b_16pre)
# item cl10_me3_sa7c_16pre: (2 -> 1)
lpre_cl10$cl10_me3_sa7c_16pre <- ifelse(lpre_cl10$cl10_me3_sa7c_16pre==2, 1, lpre_cl10$cl10_me3_sa7c_16pre)
# item cl10_me4_sa6a_16pre: (2 -> 1)
lpre_cl10$cl10_me4_sa6a_16pre <- ifelse(lpre_cl10$cl10_me4_sa6a_16pre==2, 1, lpre_cl10$cl10_me4_sa6a_16pre)
# check all
lpre_cl10_scale <- apply(lpre_cl10, 2,function(x) table(x, useNA='always'))
lpre_cl10_scale

# post items
# extracting unique elements
lpost_cl10_scale <- apply(lpost_cl10, 2,function(x) table(x, useNA='always'))
lpost_cl10_scale
# recode according to table above 
# item cl10_me1_44a_16post: (0.0 -> 0), (1.2 -> 1), (2.4 -> 2), (3.6 -> 3), (4.8 -> 4), (6.0 -> 5)
lpost_cl10$cl10_me1_44a_16post <- ifelse(lpost_cl10$cl10_me1_44a_16post==0.0, 0, lpost_cl10$cl10_me1_44a_16post)
lpost_cl10$cl10_me1_44a_16post <- ifelse(lpost_cl10$cl10_me1_44a_16post==1.2, 1, lpost_cl10$cl10_me1_44a_16post)
lpost_cl10$cl10_me1_44a_16post <- ifelse(lpost_cl10$cl10_me1_44a_16post==2.4, 2, lpost_cl10$cl10_me1_44a_16post)
lpost_cl10$cl10_me1_44a_16post <- ifelse(lpost_cl10$cl10_me1_44a_16post==3.6, 3, lpost_cl10$cl10_me1_44a_16post)
lpost_cl10$cl10_me1_44a_16post <- ifelse(lpost_cl10$cl10_me1_44a_16post==4.8, 4, lpost_cl10$cl10_me1_44a_16post)
lpost_cl10$cl10_me1_44a_16post <- ifelse(lpost_cl10$cl10_me1_44a_16post==6.0, 5, lpost_cl10$cl10_me1_44a_16post)
# item cl10_me1_44b_16post: (4 -> 1)
lpost_cl10$cl10_me1_44b_16post <- ifelse(lpost_cl10$cl10_me1_44b_16post==4, 1, lpost_cl10$cl10_me1_44b_16post)
# item cl10_me1_44c_16post: (4 -> 1)
lpost_cl10$cl10_me1_44c_16post <- ifelse(lpost_cl10$cl10_me1_44c_16post==4, 1, lpost_cl10$cl10_me1_44c_16post)
# item cl10_me2_40_16post: (3 -> 2), (8 -> 3)***
lpost_cl10$cl10_me2_40_16post <- ifelse(lpost_cl10$cl10_me2_40_16post==3, 2, lpost_cl10$cl10_me2_40_16post)
lpost_cl10$cl10_me2_40_16post <- ifelse(lpost_cl10$cl10_me2_40_16post==8, 3, lpost_cl10$cl10_me2_40_16post)
# item cl10_me3_sa7a_16post: (2 -> 1), (4 -> 2), (6 -> 3)
lpost_cl10$cl10_me3_sa7a_16post <- ifelse(lpost_cl10$cl10_me3_sa7a_16post==2, 1, lpost_cl10$cl10_me3_sa7a_16post)
lpost_cl10$cl10_me3_sa7a_16post <- ifelse(lpost_cl10$cl10_me3_sa7a_16post==4, 2, lpost_cl10$cl10_me3_sa7a_16post)
lpost_cl10$cl10_me3_sa7a_16post <- ifelse(lpost_cl10$cl10_me3_sa7a_16post==6, 3, lpost_cl10$cl10_me3_sa7a_16post)
# item cl10_me3_sa7b_16post: (2 -> 1), (4 -> 2)
lpost_cl10$cl10_me3_sa7b_16post <- ifelse(lpost_cl10$cl10_me3_sa7b_16post==2, 1, lpost_cl10$cl10_me3_sa7b_16post)
lpost_cl10$cl10_me3_sa7b_16post <- ifelse(lpost_cl10$cl10_me3_sa7b_16post==4, 2, lpost_cl10$cl10_me3_sa7b_16post)
# item cl10_me3_sa7c_16post: (4 -> 1)
lpost_cl10$cl10_me3_sa7c_16post <- ifelse(lpost_cl10$cl10_me3_sa7c_16post==4, 1, lpost_cl10$cl10_me3_sa7c_16post)
# check all
lpost_cl10_scale <- apply(lpost_cl10, 2,function(x) table(x, useNA='always'))
lpost_cl10_scale
# renaming items
colnames(lpre_cl10) <- paste(colnames(lpre_cl10), "ordinal", sep = "_")
colnames(lpost_cl10) <- paste(colnames(lpost_cl10), "ordinal", sep = "_")
colnames(lpre_cl10)
colnames(lpost_cl10)
# ------------------------------------------------------------------------------
# no other local items have pre-post designations
# Now I'm combining the dataframes together with the original

g_ordinal <- cbind(g,lpre_cl1,lpre_cl2,lpre_cl3,lpre_cl4,lpre_cl5,lpre_cl7,lpre_cl8,lpre_cl9,lpre_cl10,
                 lpost_cl1,lpost_cl2,lpost_cl3,lpost_cl4,lpost_cl5,lpost_cl7,lpost_cl8,lpost_cl9,lpost_cl10)
colnames(g_ordinal)
#test any one column out
table (g_ordinal$cl1_fc26_16post_ordinal, useNA="always")
#-------------------------------------------------------------------------------

# Now recoding year 1 local items
# begin with identifying scoring structures and making them all ordinal
# I'm subsetting all of the local items so they're not mixed in with the other items
colnames(g)
#starting with cl1 items
Y1_local_cl1 <- select(g,starts_with("cl1_", ignore.case = F))
colnames(Y1_local_cl1)
Y1_local_cl1[,21:42] <- NULL
colnames(Y1_local_cl1)

# extracting unique elements
Y1_local_cl1_scale <- apply(Y1_local_cl1, 2,function(x) table(x, useNA='always'))
Y1_local_cl1_scale
# recode according to table above
# item cl1_fc41: (2 -> 1), (3 -> 1), (4 -> 1), (5 -> 2), (6 -> 2), (7 -> 3), (8 -> 3), (9 -> 3) (10 -> 4)***
Y1_local_cl1$cl1_fc41 <- ifelse(Y1_local_cl1$cl1_fc41==2, 1, Y1_local_cl1$cl1_fc41)
Y1_local_cl1$cl1_fc41 <- ifelse(Y1_local_cl1$cl1_fc41==3, 1, Y1_local_cl1$cl1_fc41)
Y1_local_cl1$cl1_fc41 <- ifelse(Y1_local_cl1$cl1_fc41==4, 1, Y1_local_cl1$cl1_fc41)
Y1_local_cl1$cl1_fc41 <- ifelse(Y1_local_cl1$cl1_fc41==5, 2, Y1_local_cl1$cl1_fc41)
Y1_local_cl1$cl1_fc41 <- ifelse(Y1_local_cl1$cl1_fc41==6, 2, Y1_local_cl1$cl1_fc41)
Y1_local_cl1$cl1_fc41 <- ifelse(Y1_local_cl1$cl1_fc41==7, 3, Y1_local_cl1$cl1_fc41)
Y1_local_cl1$cl1_fc41 <- ifelse(Y1_local_cl1$cl1_fc41==8, 3, Y1_local_cl1$cl1_fc41)
Y1_local_cl1$cl1_fc41 <- ifelse(Y1_local_cl1$cl1_fc41==9, 3, Y1_local_cl1$cl1_fc41)
Y1_local_cl1$cl1_fc41 <- ifelse(Y1_local_cl1$cl1_fc41==10, 4, Y1_local_cl1$cl1_fc41)
# item cl1_fc42: (3 -> 0), (5 -> 0), (6 -> 0), (7 -> 0), (8 -> 1), (9 -> 1), (11 -> 2)***
Y1_local_cl1$cl1_fc42 <- ifelse(Y1_local_cl1$cl1_fc42==3, 0, Y1_local_cl1$cl1_fc42)
Y1_local_cl1$cl1_fc42 <- ifelse(Y1_local_cl1$cl1_fc42==5, 0, Y1_local_cl1$cl1_fc42)
Y1_local_cl1$cl1_fc42 <- ifelse(Y1_local_cl1$cl1_fc42==6, 0, Y1_local_cl1$cl1_fc42)
Y1_local_cl1$cl1_fc42 <- ifelse(Y1_local_cl1$cl1_fc42==7, 0, Y1_local_cl1$cl1_fc42)
Y1_local_cl1$cl1_fc42 <- ifelse(Y1_local_cl1$cl1_fc42==8, 1, Y1_local_cl1$cl1_fc42)
Y1_local_cl1$cl1_fc42 <- ifelse(Y1_local_cl1$cl1_fc42==9, 1, Y1_local_cl1$cl1_fc42)
Y1_local_cl1$cl1_fc42 <- ifelse(Y1_local_cl1$cl1_fc42==11, 2, Y1_local_cl1$cl1_fc42)
# item cl1_fc43: (2 -> 0), (3 -> 0), (4 -> 0), (4.5 -> 0), (5 -> 0), (6 -> 1), (6.5 -> 1), (7 -> 1), (7.5 -> 1), (8 -> 1),***
                #  (8.5 -> 1), (9 -> 1), (9.5 -> 1), (10 -> 2)***
Y1_local_cl1$cl1_fc43 <- ifelse(Y1_local_cl1$cl1_fc43==2, 0, Y1_local_cl1$cl1_fc43)
Y1_local_cl1$cl1_fc43 <- ifelse(Y1_local_cl1$cl1_fc43==3, 0, Y1_local_cl1$cl1_fc43)
Y1_local_cl1$cl1_fc43 <- ifelse(Y1_local_cl1$cl1_fc43==4, 0, Y1_local_cl1$cl1_fc43)
Y1_local_cl1$cl1_fc43 <- ifelse(Y1_local_cl1$cl1_fc43==4.5, 0, Y1_local_cl1$cl1_fc43)
Y1_local_cl1$cl1_fc43 <- ifelse(Y1_local_cl1$cl1_fc43==5, 0, Y1_local_cl1$cl1_fc43)
Y1_local_cl1$cl1_fc43 <- ifelse(Y1_local_cl1$cl1_fc43==6, 1, Y1_local_cl1$cl1_fc43)
Y1_local_cl1$cl1_fc43 <- ifelse(Y1_local_cl1$cl1_fc43==6.5, 1, Y1_local_cl1$cl1_fc43)
Y1_local_cl1$cl1_fc43 <- ifelse(Y1_local_cl1$cl1_fc43==7, 1, Y1_local_cl1$cl1_fc43)
Y1_local_cl1$cl1_fc43 <- ifelse(Y1_local_cl1$cl1_fc43==7.5, 1, Y1_local_cl1$cl1_fc43)
Y1_local_cl1$cl1_fc43 <- ifelse(Y1_local_cl1$cl1_fc43==8, 1, Y1_local_cl1$cl1_fc43)
Y1_local_cl1$cl1_fc43 <- ifelse(Y1_local_cl1$cl1_fc43==8.5, 1, Y1_local_cl1$cl1_fc43)
Y1_local_cl1$cl1_fc43 <- ifelse(Y1_local_cl1$cl1_fc43==9, 1, Y1_local_cl1$cl1_fc43)
Y1_local_cl1$cl1_fc43 <- ifelse(Y1_local_cl1$cl1_fc43==9.5, 1, Y1_local_cl1$cl1_fc43)
Y1_local_cl1$cl1_fc43 <- ifelse(Y1_local_cl1$cl1_fc43==10, 2, Y1_local_cl1$cl1_fc43)
# item cl1_fc44: (1 -> 0), (2 -> 0), (3 -> 0), (4 -> 0), (5 -> 1), (6 -> 2)***
Y1_local_cl1$cl1_fc44 <- ifelse(Y1_local_cl1$cl1_fc44==1, 0, Y1_local_cl1$cl1_fc44)
Y1_local_cl1$cl1_fc44 <- ifelse(Y1_local_cl1$cl1_fc44==2, 0, Y1_local_cl1$cl1_fc44)
Y1_local_cl1$cl1_fc44 <- ifelse(Y1_local_cl1$cl1_fc44==3, 0, Y1_local_cl1$cl1_fc44)
Y1_local_cl1$cl1_fc44 <- ifelse(Y1_local_cl1$cl1_fc44==4, 0, Y1_local_cl1$cl1_fc44)
Y1_local_cl1$cl1_fc44 <- ifelse(Y1_local_cl1$cl1_fc44==5, 1, Y1_local_cl1$cl1_fc44)
Y1_local_cl1$cl1_fc44 <- ifelse(Y1_local_cl1$cl1_fc44==6, 2, Y1_local_cl1$cl1_fc44)
# item cl1_fc45: (6 -> 0), (7 -> 0), (8 -> 1), (9 -> 2), (10 -> 2), (11 -> 3), (12 -> 3), (13 -> 4)***
Y1_local_cl1$cl1_fc45 <- ifelse(Y1_local_cl1$cl1_fc45==6, 0, Y1_local_cl1$cl1_fc45)
Y1_local_cl1$cl1_fc45 <- ifelse(Y1_local_cl1$cl1_fc45==7, 0, Y1_local_cl1$cl1_fc45)
Y1_local_cl1$cl1_fc45 <- ifelse(Y1_local_cl1$cl1_fc45==8, 1, Y1_local_cl1$cl1_fc45)
Y1_local_cl1$cl1_fc45 <- ifelse(Y1_local_cl1$cl1_fc45==9, 2, Y1_local_cl1$cl1_fc45)
Y1_local_cl1$cl1_fc45 <- ifelse(Y1_local_cl1$cl1_fc45==10, 2, Y1_local_cl1$cl1_fc45)
Y1_local_cl1$cl1_fc45 <- ifelse(Y1_local_cl1$cl1_fc45==11, 3, Y1_local_cl1$cl1_fc45)
Y1_local_cl1$cl1_fc45 <- ifelse(Y1_local_cl1$cl1_fc45==12, 3, Y1_local_cl1$cl1_fc45)
Y1_local_cl1$cl1_fc45 <- ifelse(Y1_local_cl1$cl1_fc45==13, 4, Y1_local_cl1$cl1_fc45)
# check all
Y1_local_cl1_scale <- apply(Y1_local_cl1, 2,function(x) table(x, useNA='always'))
Y1_local_cl1_scale
# renaming items
colnames(Y1_local_cl1) <- paste(colnames(Y1_local_cl1), "ordinal", sep = "_")
colnames(Y1_local_cl1)
# ----------------------------------------------------------------------------------------
# cl_2 items
Y1_local_cl2 <- select(g,starts_with("cl2_", ignore.case = F))
colnames(Y1_local_cl2)
Y1_local_cl2[,32:39] <- NULL
colnames(Y1_local_cl2)

# extracting unique elements
Y1_local_cl2_scale <- apply(Y1_local_cl2, 2,function(x) table(x, useNA='always'))
Y1_local_cl2_scale
# recode according to table above
# item cl2_me1_1: (0.5 -> 0), (3 -> 2), (3.5 -> 2), (4 -> 3)***
Y1_local_cl2$cl2_me1_1 <- ifelse(Y1_local_cl2$cl2_me1_1==0.5, 0, Y1_local_cl2$cl2_me1_1)
Y1_local_cl2$cl2_me1_1 <- ifelse(Y1_local_cl2$cl2_me1_1==3, 2, Y1_local_cl2$cl2_me1_1)
Y1_local_cl2$cl2_me1_1 <- ifelse(Y1_local_cl2$cl2_me1_1==3.5, 2, Y1_local_cl2$cl2_me1_1)
Y1_local_cl2$cl2_me1_1 <- ifelse(Y1_local_cl2$cl2_me1_1==4, 3, Y1_local_cl2$cl2_me1_1)
# item cl2_me1_2: (0.5 -> 1), (3 -> 1)*** 
Y1_local_cl2$cl2_me1_2 <- ifelse(Y1_local_cl2$cl2_me1_2==0.5, 1, Y1_local_cl2$cl2_me1_2)
Y1_local_cl2$cl2_me1_2 <- ifelse(Y1_local_cl2$cl2_me1_2==3, 1, Y1_local_cl2$cl2_me1_2)
# item cl2_me1_3: (2 -> 1), (3 -> 2), (4 -> 2), (5 -> 3), (6 -> 3), (7 -> 4), (8 -> 4), (9 -> 5) (10 -> 5)***
Y1_local_cl2$cl2_me1_3 <- ifelse(Y1_local_cl2$cl2_me1_3==2, 1, Y1_local_cl2$cl2_me1_3)
Y1_local_cl2$cl2_me1_3 <- ifelse(Y1_local_cl2$cl2_me1_3==3, 2, Y1_local_cl2$cl2_me1_3)
Y1_local_cl2$cl2_me1_3 <- ifelse(Y1_local_cl2$cl2_me1_3==4, 2, Y1_local_cl2$cl2_me1_3)
Y1_local_cl2$cl2_me1_3 <- ifelse(Y1_local_cl2$cl2_me1_3==5, 3, Y1_local_cl2$cl2_me1_3)
Y1_local_cl2$cl2_me1_3 <- ifelse(Y1_local_cl2$cl2_me1_3==6, 3, Y1_local_cl2$cl2_me1_3)
Y1_local_cl2$cl2_me1_3 <- ifelse(Y1_local_cl2$cl2_me1_3==7, 4, Y1_local_cl2$cl2_me1_3)
Y1_local_cl2$cl2_me1_3 <- ifelse(Y1_local_cl2$cl2_me1_3==8, 4, Y1_local_cl2$cl2_me1_3)
Y1_local_cl2$cl2_me1_3 <- ifelse(Y1_local_cl2$cl2_me1_3==9, 5, Y1_local_cl2$cl2_me1_3)
Y1_local_cl2$cl2_me1_3 <- ifelse(Y1_local_cl2$cl2_me1_3==10, 5, Y1_local_cl2$cl2_me1_3)
# item cl2_me1_4: (6 -> 0), (8 -> 0), (10 -> 0), (12 -> 1), (14 -> 2), (16 -> 3)***
Y1_local_cl2$cl2_me1_4 <- ifelse(Y1_local_cl2$cl2_me1_4==6, 0, Y1_local_cl2$cl2_me1_4)
Y1_local_cl2$cl2_me1_4 <- ifelse(Y1_local_cl2$cl2_me1_4==8, 0, Y1_local_cl2$cl2_me1_4)
Y1_local_cl2$cl2_me1_4 <- ifelse(Y1_local_cl2$cl2_me1_4==10, 0, Y1_local_cl2$cl2_me1_4)
Y1_local_cl2$cl2_me1_4 <- ifelse(Y1_local_cl2$cl2_me1_4==12, 1, Y1_local_cl2$cl2_me1_4)
Y1_local_cl2$cl2_me1_4 <- ifelse(Y1_local_cl2$cl2_me1_4==14, 2, Y1_local_cl2$cl2_me1_4)
Y1_local_cl2$cl2_me1_4 <- ifelse(Y1_local_cl2$cl2_me1_4==16, 3, Y1_local_cl2$cl2_me1_4)
# item cl2_me1_5: (1.5 -> 1), (2 -> 1), (2.5 -> 1), (3 -> 2), (3.5 -> 2), (4 -> 2), (4.5 -> 2), (5 -> 3) (6 -> 3)***
#                 (6.5 -> 3), (7 -> 4), (8 -> 4), (9 -> 5), (10 -> 5), (11 -> 6)
Y1_local_cl2$cl2_me1_5 <- ifelse(Y1_local_cl2$cl2_me1_5==1.5, 1, Y1_local_cl2$cl2_me1_5)
Y1_local_cl2$cl2_me1_5 <- ifelse(Y1_local_cl2$cl2_me1_5==2, 1, Y1_local_cl2$cl2_me1_5)
Y1_local_cl2$cl2_me1_5 <- ifelse(Y1_local_cl2$cl2_me1_5==2.5, 1, Y1_local_cl2$cl2_me1_5)
Y1_local_cl2$cl2_me1_5 <- ifelse(Y1_local_cl2$cl2_me1_5==3, 2, Y1_local_cl2$cl2_me1_5)
Y1_local_cl2$cl2_me1_5 <- ifelse(Y1_local_cl2$cl2_me1_5==3.5, 2, Y1_local_cl2$cl2_me1_5)
Y1_local_cl2$cl2_me1_5 <- ifelse(Y1_local_cl2$cl2_me1_5==4, 2, Y1_local_cl2$cl2_me1_5)
Y1_local_cl2$cl2_me1_5 <- ifelse(Y1_local_cl2$cl2_me1_5==4.5, 2, Y1_local_cl2$cl2_me1_5)
Y1_local_cl2$cl2_me1_5 <- ifelse(Y1_local_cl2$cl2_me1_5==5, 3, Y1_local_cl2$cl2_me1_5)
Y1_local_cl2$cl2_me1_5 <- ifelse(Y1_local_cl2$cl2_me1_5==6, 3, Y1_local_cl2$cl2_me1_5)
Y1_local_cl2$cl2_me1_5 <- ifelse(Y1_local_cl2$cl2_me1_5==6.5, 3, Y1_local_cl2$cl2_me1_5)
Y1_local_cl2$cl2_me1_5 <- ifelse(Y1_local_cl2$cl2_me1_5==7, 4, Y1_local_cl2$cl2_me1_5)
Y1_local_cl2$cl2_me1_5 <- ifelse(Y1_local_cl2$cl2_me1_5==8, 4, Y1_local_cl2$cl2_me1_5)
Y1_local_cl2$cl2_me1_5 <- ifelse(Y1_local_cl2$cl2_me1_5==9, 5, Y1_local_cl2$cl2_me1_5)
Y1_local_cl2$cl2_me1_5 <- ifelse(Y1_local_cl2$cl2_me1_5==10, 5, Y1_local_cl2$cl2_me1_5)
Y1_local_cl2$cl2_me1_5 <- ifelse(Y1_local_cl2$cl2_me1_5==11, 6, Y1_local_cl2$cl2_me1_5)
# item cl2_me1_6: (2 -> 1), (6 -> 2)***
Y1_local_cl2$cl2_me1_6 <- ifelse(Y1_local_cl2$cl2_me1_6==2, 1, Y1_local_cl2$cl2_me1_6)
Y1_local_cl2$cl2_me1_6 <- ifelse(Y1_local_cl2$cl2_me1_6==6, 2, Y1_local_cl2$cl2_me1_6)
# item cl2_me1_7: (5 -> 3), (6 -> 3), (7 -> 4)***
Y1_local_cl2$cl2_me1_7 <- ifelse(Y1_local_cl2$cl2_me1_7==5, 3, Y1_local_cl2$cl2_me1_7)
Y1_local_cl2$cl2_me1_7 <- ifelse(Y1_local_cl2$cl2_me1_7==6, 3, Y1_local_cl2$cl2_me1_7)
Y1_local_cl2$cl2_me1_7 <- ifelse(Y1_local_cl2$cl2_me1_7==7, 4, Y1_local_cl2$cl2_me1_7)
# item cl2_me1_9: (1 -> 0), (2 -> 0), (3 -> 0), (5 -> 1), (6 -> 2), (7 -> 3), (8 -> 3), (9 -> 3) (10 -> 4)***
Y1_local_cl2$cl2_me1_9 <- ifelse(Y1_local_cl2$cl2_me1_9==1, 0, Y1_local_cl2$cl2_me1_9)
Y1_local_cl2$cl2_me1_9 <- ifelse(Y1_local_cl2$cl2_me1_9==2, 0, Y1_local_cl2$cl2_me1_9)
Y1_local_cl2$cl2_me1_9 <- ifelse(Y1_local_cl2$cl2_me1_9==3, 0, Y1_local_cl2$cl2_me1_9)
Y1_local_cl2$cl2_me1_9 <- ifelse(Y1_local_cl2$cl2_me1_9==5, 1, Y1_local_cl2$cl2_me1_9)
Y1_local_cl2$cl2_me1_9 <- ifelse(Y1_local_cl2$cl2_me1_9==6, 2, Y1_local_cl2$cl2_me1_9)
Y1_local_cl2$cl2_me1_9 <- ifelse(Y1_local_cl2$cl2_me1_9==7, 3, Y1_local_cl2$cl2_me1_9)
Y1_local_cl2$cl2_me1_9 <- ifelse(Y1_local_cl2$cl2_me1_9==8, 3, Y1_local_cl2$cl2_me1_9)
Y1_local_cl2$cl2_me1_9 <- ifelse(Y1_local_cl2$cl2_me1_9==9, 3, Y1_local_cl2$cl2_me1_9)
Y1_local_cl2$cl2_me1_9 <- ifelse(Y1_local_cl2$cl2_me1_9==10, 4, Y1_local_cl2$cl2_me1_9)
# item cl2_me1_10: (0.5 -> 0), (1 -> 0), (1.5 -> 0), (2 -> 0), (2.5 -> 1), (3 -> 1), (3.5 -> 1), (4 -> 1) (4.5 -> 2)***
#                 (5 -> 2), (5.5 -> 2), (6 -> 3), (6.5 -> 3), (7 -> 3), (7.5 -> 3), 
#                 (8 -> 4), (8.5 -> 4), (9 -> 5), (10 -> 5), (10.5 -> 5), (11 -> 5)*** 
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==0.5, 0, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==1, 0, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==1.5, 0, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==2, 0, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==2.5, 1, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==3, 1, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==3.5, 1, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==4, 1, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==4.5, 2, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==5, 2, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==5.5, 2, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==6, 3, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==6.5, 3, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==7, 3, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==7.5, 3, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==8, 4, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==8.5, 4, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==9, 5, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==10, 5, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==10.5, 5, Y1_local_cl2$cl2_me1_10)
Y1_local_cl2$cl2_me1_10 <- ifelse(Y1_local_cl2$cl2_me1_10==11, 5, Y1_local_cl2$cl2_me1_10)
# item cl2_me1_11: (8 -> 7)
Y1_local_cl2$cl2_me1_11 <- ifelse(Y1_local_cl2$cl2_me1_11==8, 7, Y1_local_cl2$cl2_me1_11)
# item cl2_me1_12: (3 -> 2), (5 -> 2), (7 -> 2), (8 -> 2), (9 -> 3), (10 -> 4)***
Y1_local_cl2$cl2_me1_12 <- ifelse(Y1_local_cl2$cl2_me1_12==3, 2, Y1_local_cl2$cl2_me1_12)
Y1_local_cl2$cl2_me1_12 <- ifelse(Y1_local_cl2$cl2_me1_12==5, 2, Y1_local_cl2$cl2_me1_12)
Y1_local_cl2$cl2_me1_12 <- ifelse(Y1_local_cl2$cl2_me1_12==7, 2, Y1_local_cl2$cl2_me1_12)
Y1_local_cl2$cl2_me1_12 <- ifelse(Y1_local_cl2$cl2_me1_12==8, 2, Y1_local_cl2$cl2_me1_12)
Y1_local_cl2$cl2_me1_12 <- ifelse(Y1_local_cl2$cl2_me1_12==9, 3, Y1_local_cl2$cl2_me1_12)
Y1_local_cl2$cl2_me1_12 <- ifelse(Y1_local_cl2$cl2_me1_12==10, 4, Y1_local_cl2$cl2_me1_12)

# ------------- installing recoder package to save time ---------------
library(recoder)
# item cl2_me2_1: (1.5 -> 0), (3 -> 1), (4.5 -> 2), (6 -> 3), (7.5 -> 4)***
Y1_local_cl2$cl2_me2_1 <- recoder(Y1_local_cl2$cl2_me2_1, '1.5:0;3:1;4.5:2;6:3;7.5:4')
#  --------- from this point, refer to the code directly for recoded values -----------------
# --------   *** still means the scale was collapsed in some way --------------
# item cl2_me2_2 ***
Y1_local_cl2$cl2_me2_2 <- recoder(Y1_local_cl2$cl2_me2_2, '1:0;1.5:1;2.5:2')
# item cl2_me2_3 ***
Y1_local_cl2$cl2_me2_3 <- recoder(Y1_local_cl2$cl2_me2_3, '1:0; 2:0; 3:0; 4:1; 5:2; 6:2; 6.5:2; 7:3')
# item cl2_me2_4 ***
Y1_local_cl2$cl2_me2_4 <- recoder(Y1_local_cl2$cl2_me2_4, '1.5:0; 4.5:0; 6:0; 7.5:1; 9:1; 10.5:1; 12:2; 13.5:2; 15:3; 16:4; 16.5:4')
# item cl2_me2_5 ***
Y1_local_cl2$cl2_me2_5 <- recoder(Y1_local_cl2$cl2_me2_5, '3.5:0; 5:0; 6:1; 6.5:1; 7:1; 7.5:2; 8:2; 8.5:3; 9:3; 9.5:4; 10.5:4; 11:5; 12:5')
# item cl2_me2_6 ***
Y1_local_cl2$cl2_me2_6 <- recoder(Y1_local_cl2$cl2_me2_6, '1:0; 2:0; 3:1; 4:1; 5:1; 6:2; 7:2; 8:2; 9:3; 10:4')
# item cl2_me2_7 ***
Y1_local_cl2$cl2_me2_7 <- recoder(Y1_local_cl2$cl2_me2_7, '1:0; 1.5:0; 2:0; 3.5:0; 4:0; 4.5:0; 5:0; 5.5:0; 6:1; 6.5:1; 7:1; 8:1; 8.5:1; 9:1;
                                  9.5:2; 10:2; 10.5:2; 11:2; 11.5:2; 12:3; 12.5:3; 13:3; 13.5:4; 14:4; 14.5:4; 15:4; 15.5:5; 16.5:5')
#item cl2_me2_8 ***
Y1_local_cl2$cl2_me2_8 <- recoder(Y1_local_cl2$cl2_me2_8, '0.5:0; 1:0; 2:0; 2.5:1; 3:1; 4:1; 4.5:1; 5:1; 5.5:1; 6:2; 6.5:2; 7:2; 7.5:3;
                                  8:3; 8.5:3; 9:4; 9.5:4; 10:5; 10.5:5; 11:5')
# item cl2_me2_9 ***
Y1_local_cl2$cl2_me2_9 <- recoder(Y1_local_cl2$cl2_me2_9, '1:0; 2:1; 3:1; 4:2')
# item cl2_me2_11***
Y1_local_cl2$cl2_me2_11 <- recoder(Y1_local_cl2$cl2_me2_11, '1.5:1; 3:2; 4.5:3; 6:4; 7.5:4')
# item cl2_me3_1 ***
Y1_local_cl2$cl2_me3_1 <- recoder(Y1_local_cl2$cl2_me3_1, '3:0; 6:0; 8:0; 9:0; 12:1; 13:1; 15:2; 17:2; 18:3; 21:4; 22:4; 24:5')
# item cl2_me3_2 ***
Y1_local_cl2$cl2_me3_2 <- recoder(Y1_local_cl2$cl2_me3_2, '4:0; 5:0; 6:1; 7:2; 8:3')
# item cl2_me3_3 ***
Y1_local_cl2$cl2_me3_3 <- recoder(Y1_local_cl2$cl2_me3_3, '4:0; 5:0; 6:0; 7:1; 8:1; 9:1; 10:2; 11:2; 12:3')
# item cl2_me3_4 ***
Y1_local_cl2$cl2_me3_4 <- recoder(Y1_local_cl2$cl2_me3_4, '1:1; 2:1; 3:1; 4:1; 5:2; 6:2; 8:3')
# item cl2_me3_5 ***
Y1_local_cl2$cl2_me3_5 <- recoder(Y1_local_cl2$cl2_me3_5, '2:1; 3:1; 4:2; 6:3; 8:4')
# item cl2_me3_6 ***
Y1_local_cl2$cl2_me3_6 <- recoder(Y1_local_cl2$cl2_me3_6, '1:0; 2:0; 3:0; 4:1; 5:1; 6:2; 7:2; 8:2; 9:2; 10:2; 12:3; 14:4; 15:4; 16:4')
# item cl2_me3_7 ***
Y1_local_cl2$cl2_me3_7 <- recoder(Y1_local_cl2$cl2_me3_7, '2:0; 6:0; 8:1; 9:2; 10:2; 11:2; 12:3')
# item cl2_me3_8 ***
Y1_local_cl2$cl2_me3_8 <- recoder(Y1_local_cl2$cl2_me3_8, '6:0; 8:0; 10:1; 12:2')

# Check all
Y1_local_cl2_scale <- apply(Y1_local_cl2, 2,function(x) table(x, useNA='always'))
Y1_local_cl2_scale
# renaming items
colnames(Y1_local_cl2) <- paste(colnames(Y1_local_cl2), "ordinal", sep = "_")
colnames(Y1_local_cl2)

# ---------------------------------------------------------------------------------
# cl3_ items
Y1_local_cl3 <- select(g,starts_with("cl3_", ignore.case = F))
colnames(Y1_local_cl3)
Y1_local_cl3[,24:39] <- NULL
colnames(Y1_local_cl3)

# extracting unique elements
Y1_local_cl3_scale <- apply(Y1_local_cl3, 2,function(x) table(x, useNA='always'))
Y1_local_cl3_scale
# recode according to table above
# no recoding required!
# renaming items anyway to keep things neat
colnames(Y1_local_cl3) <- paste(colnames(Y1_local_cl3), "ordinal", sep = "_")
colnames(Y1_local_cl3)

# -------------------------------------------------------------------------------
# cl4_ items
Y1_local_cl4 <- select(g,starts_with("cl4_", ignore.case = F))
colnames(Y1_local_cl4)
Y1_local_cl4[,32:55] <- NULL
colnames(Y1_local_cl4)

# extracting unique elements
Y1_local_cl4_scale <- apply(Y1_local_cl4, 2,function(x) table(x, useNA='always'))
Y1_local_cl4_scale
# recode according to table above
# item cl4_fcsa1 ***
Y1_local_cl4$cl4_fcsa1 <- recoder(Y1_local_cl4$cl4_fcsa1, '1:0; 1.5:1; 2:1; 2.5:2; 3:2')
# item cl4_fcsa3a
Y1_local_cl4$cl4_fcsa3a <- recoder(Y1_local_cl4$cl4_fcsa3a, '2:1')
# item cl4_fcsa4 ***
Y1_local_cl4$cl4_fcsa4 <- recoder(Y1_local_cl4$cl4_fcsa4, '1.5:1; 3:2')
# item cl4_fcsa5 ***
Y1_local_cl4$cl4_fcsa5 <- recoder(Y1_local_cl4$cl4_fcsa5, '1:0; 1.5:1; 2:1; 3:2')
# item cl4_fcsa6a
Y1_local_cl4$cl4_fcsa6a <- recoder(Y1_local_cl4$cl4_fcsa6a, '2:1')
# item cl4_fcsa6b
Y1_local_cl4$cl4_fcsa6b <- recoder(Y1_local_cl4$cl4_fcsa6b, '1:0; 2:1')
# item cl4_fcsa7 ***
Y1_local_cl4$cl4_fcsa7 <- recoder(Y1_local_cl4$cl4_fcsa7, '1.5:0; 3:1')
# item cl4_fcsa8b
Y1_local_cl4$cl4_fcsa8b <- recoder(Y1_local_cl4$cl4_fcsa8b, '2:1')
# item cl4_fcsa9a
Y1_local_cl4$cl4_fcsa9a <- recoder(Y1_local_cl4$cl4_fcsa9a, '2:1')
# item cl4_fcsa9b
Y1_local_cl4$cl4_fcsa9b <- recoder(Y1_local_cl4$cl4_fcsa9b, '2:1')
# item cl4_fcsa10a ***
Y1_local_cl4$cl4_fcsa10a <- recoder(Y1_local_cl4$cl4_fcsa10a, '1:0; 2:1')
# item cl4_fcsa10b
Y1_local_cl4$cl4_fcsa10b <- recoder(Y1_local_cl4$cl4_fcsa10b, '2:1; 3:2; 4:3')
# item cl4_fcp1a
Y1_local_cl4$cl4_fcp1a <- recoder(Y1_local_cl4$cl4_fcp1a, '3:2')
# item cl4_fcp1b
Y1_local_cl4$cl4_fcp1b <- recoder(Y1_local_cl4$cl4_fcp1b, '2:1')
# item cl4_fcp4b
Y1_local_cl4$cl4_fcp4b <- recoder(Y1_local_cl4$cl4_fcp4b, '2:1')
# item cl4_fcp5a ***
Y1_local_cl4$cl4_fcp5a <- recoder(Y1_local_cl4$cl4_fcp5a, '2:1; 2.5:1; 3:2')
# item cl4_fcp5b ***
Y1_local_cl4$cl4_fcp5b <- recoder(Y1_local_cl4$cl4_fcp5b, '1.5:1')
# item cl4_fcp5d ***
Y1_local_cl4$cl4_fcp5d <- recoder(Y1_local_cl4$cl4_fcp5d, '2:1; 3:1; 4:2')
# item cl4_fcp5e
Y1_local_cl4$cl4_fcp5e <- recoder(Y1_local_cl4$cl4_fcp5e, '2:1')

# check all
Y1_local_cl4_scale <- apply(Y1_local_cl4, 2,function(x) table(x, useNA='always'))
Y1_local_cl4_scale

# renaming items 
colnames(Y1_local_cl4) <- paste(colnames(Y1_local_cl4), "ordinal", sep = "_")
colnames(Y1_local_cl4)

# -------------------------------------------------------------------------------
# cl5_ items
Y1_local_cl5 <- select(g,starts_with("cl5_", ignore.case = F))
colnames(Y1_local_cl5)
Y1_local_cl5[,6:27] <- NULL
colnames(Y1_local_cl5)

# extracting unique elements
Y1_local_cl5_scale <- apply(Y1_local_cl5, 2,function(x) table(x, useNA='always'))
Y1_local_cl5_scale
# recode according to table above
# item cl5_me4_1 ***
Y1_local_cl5$cl5_me4_1 <- recoder(Y1_local_cl5$cl5_me4_1, '5:1; 6:1; 7:1; 8:2; 9:3; 10:4')
# item cl5_me4_2 ***
Y1_local_cl5$cl5_me4_2 <- recoder(Y1_local_cl5$cl5_me4_2, '1:1; 3:1; 4:1; 6:2; 7:2; 8:3; 9:3; 10:4')
# item cl5_me4_3 ***
Y1_local_cl5$cl5_me4_3 <- recoder(Y1_local_cl5$cl5_me4_3, '2:1; 3:1; 4:1; 5:2; 6:2; 7:2; 8:3; 9:3; 10:4')
# item cl5_me4_4 ***
Y1_local_cl5$cl5_me4_4 <- recoder(Y1_local_cl5$cl5_me4_4, '2.5:1; 5:1; 6.5:2; 7:2; 7.5:2; 9:3; 10:3')
# item cl5_me4_5 ***
Y1_local_cl5$cl5_me4_5 <- recoder(Y1_local_cl5$cl5_me4_5, '2.5:0; 4:1; 4.5:1; 5:1; 6.5:2; 7.5:2; 9:2; 10:3')

# check all
Y1_local_cl5_scale <- apply(Y1_local_cl5, 2,function(x) table(x, useNA='always'))
Y1_local_cl5_scale

# renaming items 
colnames(Y1_local_cl5) <- paste(colnames(Y1_local_cl5), "ordinal", sep = "_")
colnames(Y1_local_cl5)

# --------------------------------------------------------------------------------
# cl6_ items
Y1_local_cl6 <- select(g,starts_with("cl6_", ignore.case = F))
colnames(Y1_local_cl6)

# extracting unique elements
Y1_local_cl6_scale <- apply(Y1_local_cl6, 2,function(x) table(x, useNA='always'))
Y1_local_cl6_scale
# recode according to table above
# item cl6_me4_1 ***
Y1_local_cl6$cl6_me4_1 <- recoder(Y1_local_cl6$cl6_me4_1, '2:0; 3:0; 4:1; 5:1; 6:1; 6.5:1; 7:2; 8:2; 9:3; 10:3')
# item cl6_me4_2 ***
Y1_local_cl6$cl6_me4_2 <- recoder(Y1_local_cl6$cl6_me4_2, '3:0; 4:0; 6:1; 7:1; 7.5:1; 8:2; 8.5:3; 9:3; 9.5:3; 10:4')
# item cl6_me4_3 ***
Y1_local_cl6$cl6_me4_3 <- recoder(Y1_local_cl6$cl6_me4_3, '2:0; 4:0; 5:1; 6:1; 7:2; 8:2; 9:2; 10:3')
# cl6_me4_4 ***
Y1_local_cl6$cl6_me4_4 <- recoder(Y1_local_cl6$cl6_me4_4, '1:0; 2:1; 3:1; 4:1; 5:2; 6:2; 7:2; 8:3; 9:3; 10:3')
# item cl6_me4_5 ***
Y1_local_cl6$cl6_me4_5 <- recoder(Y1_local_cl6$cl6_me4_5, '2:0; 3:1; 4:1; 5:1; 6:1; 7:2; 8:3; 9:3; 10:3')
# check all
Y1_local_cl6_scale <- apply(Y1_local_cl6, 2,function(x) table(x, useNA='always'))
Y1_local_cl6_scale

# renaming items 
colnames(Y1_local_cl6) <- paste(colnames(Y1_local_cl6), "ordinal", sep = "_")
colnames(Y1_local_cl6)

# --------------------------------------------------------------------------------
# cl7_ items
Y1_local_cl7 <- select(g,starts_with("cl7_", ignore.case = F))
colnames(Y1_local_cl7)
Y1_local_cl7[,37:76] <- NULL
colnames(Y1_local_cl7)

# extracting unique elements
Y1_local_cl7_scale <- apply(Y1_local_cl7, 2,function(x) table(x, useNA='always'))
Y1_local_cl7_scale
# recode according to table above
# item cl7_fcsa1a 
Y1_local_cl7$cl7_fcsa1a <- recoder(Y1_local_cl7$cl7_fcsa1a, '2:1')
# item cl7_fcsa1c
Y1_local_cl7$cl7_fcsa1c <- recoder(Y1_local_cl7$cl7_fcsa1c, '2:1')
# item cl7_fcsa2a ***
Y1_local_cl7$cl7_fcsa2a <- recoder(Y1_local_cl7$cl7_fcsa2a, '2:1; 3:1; 4:2; 6:3')
# item cl7_fcsa2b ***
Y1_local_cl7$cl7_fcsa2b <- recoder(Y1_local_cl7$cl7_fcsa2b, '2:1; 3:1; 4:2; 5:2; 6:2')
# item cl7_fcsa2c ***
Y1_local_cl7$cl7_fcsa2c <- recoder(Y1_local_cl7$cl7_fcsa2c, '2:1')
# item cl7_fcsa3a ***
Y1_local_cl7$cl7_fcsa3a <- recoder(Y1_local_cl7$cl7_fcsa3a, '1.5:1; 2:1; 2.5:2')
# item cl7_fcsa3b ***
Y1_local_cl7$cl7_fcsa3b <- recoder(Y1_local_cl7$cl7_fcsa3b, '2:1; 3:2')
# cl7_fcsa3c ***
Y1_local_cl7$cl7_fcsa3c <- recoder(Y1_local_cl7$cl7_fcsa3c, '2:1; 3:2; 4:2; 5:3')
# item cl7_fcsa4a ***
Y1_local_cl7$cl7_fcsa4a <- recoder(Y1_local_cl7$cl7_fcsa4a, '0.5:1; 2:1')
# item cl7_fcsa4b ***
Y1_local_cl7$cl7_fcsa4b <- recoder(Y1_local_cl7$cl7_fcsa4b, '1:1; 2:1; 3:2; 3.5:2; 4:2; 5:2; 5.5:2; 6:3')
# item cl7_fcsa5a ***
Y1_local_cl7$cl7_fcsa5a <- recoder(Y1_local_cl7$cl7_fcsa5a, '2:1')
# item cl7_fcsa5b ***
Y1_local_cl7$cl7_fcsa5b <- recoder(Y1_local_cl7$cl7_fcsa5b, '2:1')
# item cl7_fcsa5c ***
Y1_local_cl7$cl7_fcsa5c <- recoder(Y1_local_cl7$cl7_fcsa5c, '2:1; 3:1; 4:1; 6:2')
# item cl7_fcsa6a
Y1_local_cl7$cl7_fcsa6a <- recoder(Y1_local_cl7$cl7_fcsa6a, '2:1')
# item cl7_fcsa6b ***
Y1_local_cl7$cl7_fcsa6b <- recoder(Y1_local_cl7$cl7_fcsa6b, '2:1; 3:1; 4:2')
# item cl7_fcsa6c
Y1_local_cl7$cl7_fcsa6c <- recoder(Y1_local_cl7$cl7_fcsa6c, '2:1')
# item cl7_fcsa6d ***
Y1_local_cl7$cl7_fcsa6d <- recoder(Y1_local_cl7$cl7_fcsa6d, '2:1')
# item cl7_fcsa6e ***
Y1_local_cl7$cl7_fcsa6e <- recoder(Y1_local_cl7$cl7_fcsa6e, '3:2; 4:3')
# check all
Y1_local_cl7_scale <- apply(Y1_local_cl7, 2,function(x) table(x, useNA='always'))
Y1_local_cl7_scale

# renaming items 
colnames(Y1_local_cl7) <- paste(colnames(Y1_local_cl7), "ordinal", sep = "_")
colnames(Y1_local_cl7)

# --------------------------------------------------------------------------------
# cl8_ items
Y1_local_cl8 <- select(g,starts_with("cl8_", ignore.case = F))
colnames(Y1_local_cl8)
Y1_local_cl8[,21:34] <- NULL
colnames(Y1_local_cl8)

# extracting unique elements
Y1_local_cl8_scale <- apply(Y1_local_cl8, 2,function(x) table(x, useNA='always'))
Y1_local_cl8_scale
# no recoding needed!
# renaming items 
colnames(Y1_local_cl8) <- paste(colnames(Y1_local_cl8), "ordinal", sep = "_")
colnames(Y1_local_cl8)

# --------------------------------------------------------------------------------
# cl9_ items
Y1_local_cl9 <- select(g,starts_with("cl9_", ignore.case = F))
colnames(Y1_local_cl9)
Y1_local_cl9[,16:35] <- NULL
colnames(Y1_local_cl9)

# extracting unique elements
Y1_local_cl9_scale <- apply(Y1_local_cl9, 2,function(x) table(x, useNA='always'))
Y1_local_cl9_scale
# recoding according to table above
# item cl9_fcsa1 ***
Y1_local_cl9$cl9_fcsa1 <- recoder(Y1_local_cl9$cl9_fcsa1, '1.5:1; 2:1; 2.5:1; 3:2')
# item cl9_fcsa3
Y1_local_cl9$cl9_fcsa3 <- recoder(Y1_local_cl9$cl9_fcsa3, '2:1; 4:2')
# item cl9_fcsa4 ***
Y1_local_cl9$cl9_fcsa4 <- recoder(Y1_local_cl9$cl9_fcsa4, '1:0; 3:1; 4:2')
# item cl9_fcsa5 ***
Y1_local_cl9$cl9_fcsa5 <- recoder(Y1_local_cl9$cl9_fcsa5, '1.5:1; 2:1; 3:2')
# item cl9_fcsa6 ***
Y1_local_cl9$cl9_fcsa6 <- recoder(Y1_local_cl9$cl9_fcsa6, '1:0; 2:1; 3:1; 4:1; 5:2; 5.5:2; 6:2')
# item cl9_fcsa7 ***
Y1_local_cl9$cl9_fcsa7 <- recoder(Y1_local_cl9$cl9_fcsa7, '2.5:1; 5:1')
# item cl9_fcsa8 
Y1_local_cl9$cl9_fcsa8 <- recoder(Y1_local_cl9$cl9_fcsa8, '3:2')
# item cl9_fcp1 ***
Y1_local_cl9$cl9_fcp1 <- recoder(Y1_local_cl9$cl9_fcp1, '0.5:1; 1.5:1; 4:1; 6:2; 8:2; 8.5:2; 9:3')
# item cl9_fcp2 ***
Y1_local_cl9$cl9_fcp2 <- recoder(Y1_local_cl9$cl9_fcp2, '1:1; 2:1; 3:2; 4:2')
# item cl9_fcp3 ***
Y1_local_cl9$cl9_fcp3 <- recoder(Y1_local_cl9$cl9_fcp3, '4:1; 6:1; 8:2')
# item cl9_fcp4 ***
Y1_local_cl9$cl9_fcp4 <- recoder(Y1_local_cl9$cl9_fcp4, '3:1; 3.5:1; 4:2; 5:3; 6:3')
# item cl9_fcp5
Y1_local_cl9$cl9_fcp5 <- recoder(Y1_local_cl9$cl9_fcp5, '2:1; 4:2')
# item cl9_fcp6
Y1_local_cl9$cl9_fcp6 <- recoder(Y1_local_cl9$cl9_fcp6, '2:0; 3:1; 5:1; 6:2; 7:2; 8:3; 9:4')
# item cl9_fcp7
Y1_local_cl9$cl9_fcp7 <- recoder(Y1_local_cl9$cl9_fcp7, '1.5:1; 3.5:2; 4:2')
# check all
Y1_local_cl9_scale <- apply(Y1_local_cl9, 2,function(x) table(x, useNA='always'))
Y1_local_cl9_scale
# renaming items 
colnames(Y1_local_cl9) <- paste(colnames(Y1_local_cl9), "ordinal", sep = "_")
colnames(Y1_local_cl9)

# --------------------------------------------------------------------------------
# cl10_ items
Y1_local_cl10 <- select(g,starts_with("cl10_", ignore.case = F))
colnames(Y1_local_cl10)
Y1_local_cl10[,32:61] <- NULL
colnames(Y1_local_cl10)

# extracting unique elements
Y1_local_cl10_scale <- apply(Y1_local_cl10, 2,function(x) table(x, useNA='always'))
Y1_local_cl10_scale
# recoding according to table above
# item cl10_me1_44 ***
Y1_local_cl10$cl10_me1_44 <- recoder(Y1_local_cl10$cl10_me1_44, '2.5:1; 3:1; 4:1; 5:1; 5.5:1; 6:1; 7:1; 7.5:2; 8:2; 8.5:2; 9:2; 9.5:3; 10:3; 10.5:3;
                                     11:4; 11.5:4; 12:5; 12.5:5; 13:6; 14:7')
# item cl10_me2_39 ***
Y1_local_cl10$cl10_me2_39 <- recoder(Y1_local_cl10$cl10_me2_39, '1:0; 2:0; 3:0; 5:1; 6:1; 7:1; 9:1; 10:1; 11:1; 12:1; 13:2; 14:2; 15:2; 16:2;
                                     17:2; 17.5:2; 18:2; 19:3; 20:3; 20.5:3; 21:4; 21.5:4; 22:5')
# item cl10_me2_40 ***
Y1_local_cl10$cl10_me2_40 <- recoder(Y1_local_cl10$cl10_me2_40, '2:1; 3:2; 4:2; 6:2; 7:2; 7.5:2; 8:3')
# item cl10_me3_sa7 ***
Y1_local_cl10$cl10_me3_sa7 <- recoder(Y1_local_cl10$cl10_me3_sa7, '0.5:1')
# item cl10_me4_sa1 ***
Y1_local_cl10$cl10_me4_sa1 <- recoder(Y1_local_cl10$cl10_me4_sa1, '1:0; 1.5:0; 2:1; 3:1; 3.5:1; 4:1; 4.5:1; 5:2; 5.5:2; 6:2; 6.5:2; 7:3; 7.5:3; 8:3;
                                     8.5:3; 9:3; 9.5:4; 10:4; 10.5:5; 11:5; 11.5:6; 12:6; 12.5:7; 13:7; 13.5:8; 14:8; 14.5:9; 15:9')
# item cl10_me4_sa4 ***
Y1_local_cl10$cl10_me4_sa4 <- recoder(Y1_local_cl10$cl10_me4_sa4, '2:1; 3:2; 4:2; 5:3; 6:4; 7:5; 8:6; 9:7; 10:8; 11:9; 12:10; 13:11')
# item cl10_me5_sa3 ***
Y1_local_cl10$cl10_me5_sa3 <- recoder(Y1_local_cl10$cl10_me5_sa3, '2:1; 4:2; 5:3; 6:3; 7:3; 8:3; 9:4; 10:5')
# item cl10_me6_49 ***
Y1_local_cl10$cl10_me6_49 <- recoder(Y1_local_cl10$cl10_me6_49, '2:1; 3:1; 4:2; 5:2; 6:3; 7:3; 8:4; 9:5; 10:6; 11:7; 12:8; 13:9; 14:9; 15:9; 16:10')
# check all
Y1_local_cl10_scale <- apply(Y1_local_cl10, 2,function(x) table(x, useNA='always'))
Y1_local_cl10_scale

# renaming items 
colnames(Y1_local_cl10) <- paste(colnames(Y1_local_cl10), "ordinal", sep = "_")
colnames(Y1_local_cl10)

# --------------------------------------------------------------------------------
# cl11_ items
Y1_local_cl11 <- select(g,starts_with("cl11_", ignore.case = F))
colnames(Y1_local_cl11)

# extracting unique elements
Y1_local_cl11_scale <- apply(Y1_local_cl11, 2,function(x) table(x, useNA='always'))
Y1_local_cl11_scale
# recoding according to table above
# item cl11_fcsa3a ***
Y1_local_cl11$cl11_fcsa3a <- recoder(Y1_local_cl11$cl11_fcsa3a, '2:1')
# item cl11_fcsa3b
Y1_local_cl11$cl11_fcsa3b <- recoder(Y1_local_cl11$cl11_fcsa3b, '1.5:2; 2:3')
# item cl11_fcsa3c ***
Y1_local_cl11$cl11_fcsa3c <- recoder(Y1_local_cl11$cl11_fcsa3c, '0.5:1; 1.5:1; 2:1; 2.5:1; 3:2')
# item cl11_fcsa4_3
Y1_local_cl11$cl11_fcsa4_3 <- recoder(Y1_local_cl11$cl11_fcsa4_3, '3:1')
# item cl11_fcsa4_4 ***
Y1_local_cl11$cl11_fcsa4_4 <- recoder(Y1_local_cl11$cl11_fcsa4_4, '1.5:1; 2:1; 3:2')
# item cl11_fcsa5a ***
Y1_local_cl11$cl11_fcsa5a <- recoder(Y1_local_cl11$cl11_fcsa5a, '0.5:1; 2.5:2; 4:3; 4.5:3; 5:4')
# item cl11_fcsa5b
Y1_local_cl11$cl11_fcsa5b <- recoder(Y1_local_cl11$cl11_fcsa5b, '2:1')
# item cl11_fcsa6a ***
Y1_local_cl11$cl11_fcsa6a <- recoder(Y1_local_cl11$cl11_fcsa6a, '2:1; 3:2')
# item cl11_fcsa6b ***
Y1_local_cl11$cl11_fcsa6b <- recoder(Y1_local_cl11$cl11_fcsa6b, '2:1; 3:1')
# item cl11_fcsa7b ***
Y1_local_cl11$cl11_fcsa7b <- recoder(Y1_local_cl11$cl11_fcsa7b, '1.5:1')
# item cl11_fcsa7d ***
Y1_local_cl11$cl11_fcsa7d <- recoder(Y1_local_cl11$cl11_fcsa7d, '0.5:1; 1:2; 1.5:2; 2:3')
# item cl11_fcsa7e ***
Y1_local_cl11$cl11_fcsa7e <- recoder(Y1_local_cl11$cl11_fcsa7e, '0.5:1; 1:1; 1.5:1')
# item cl11_fcsa7f ***
Y1_local_cl11$cl11_fcsa7f <- recoder(Y1_local_cl11$cl11_fcsa7f, '2:1')
# item cl11_fcsa8b ***
Y1_local_cl11$cl11_fcsa8b <- recoder(Y1_local_cl11$cl11_fcsa8b, '1.5:1; 2.5:2; 3:2')
# item cl11_fcsa8c ***
Y1_local_cl11$cl11_fcsa8c <- recoder(Y1_local_cl11$cl11_fcsa8c, '2:1')
# item cl11_fcsa8d ***
Y1_local_cl11$cl11_fcsa8d <- recoder(Y1_local_cl11$cl11_fcsa8d, '1.5:1; 2:1; 3:2')
# item cl11_fcsa8e 
Y1_local_cl11$cl11_fcsa8e <- recoder(Y1_local_cl11$cl11_fcsa8e, '2:1')
# item cl11_fcsa8f 
Y1_local_cl11$cl11_fcsa8f <- recoder(Y1_local_cl11$cl11_fcsa8f, '2:1')
# check all
Y1_local_cl11_scale <- apply(Y1_local_cl11, 2,function(x) table(x, useNA='always'))
Y1_local_cl11_scale
# renaming items 
colnames(Y1_local_cl11) <- paste(colnames(Y1_local_cl11), "ordinal", sep = "_")
colnames(Y1_local_cl11)

# --------------------------------------------------------------------------------
# cl12_ items
Y1_local_cl12 <- select(g,starts_with("cl12_", ignore.case = F))
colnames(Y1_local_cl12)

# extracting unique elements
Y1_local_cl12_scale <- apply(Y1_local_cl12, 2,function(x) table(x, useNA='always'))
Y1_local_cl12_scale
# recoding according to table above
# item cl12_me1_1 ***
Y1_local_cl12$cl12_me1_1 <- recoder(Y1_local_cl12$cl12_me1_1, '2:1; 4:1; 5:1; 7:1; 6:2')
# item cl12_me1_2 
Y1_local_cl12$cl12_me1_2 <- recoder(Y1_local_cl12$cl12_me1_2, '6:1')
# item cl12_me1_3
Y1_local_cl12$cl12_me1_3 <- recoder(Y1_local_cl12$cl12_me1_3, '4:1; 5:2; 6:3; 7:4; 8:5')
# item cl12_me1_5 ***
Y1_local_cl12$cl12_me1_5 <- recoder(Y1_local_cl12$cl12_me1_5, '0.5:0; 2:1; 3:1; 3.5:2; 5:2; 7:3')
# item cl12_me1_7 
Y1_local_cl12$cl12_me1_7 <- recoder(Y1_local_cl12$cl12_me1_7, '2:1; 4:2; 6:3')
# item cl12_me1_8 ***
Y1_local_cl12$cl12_me1_8 <- recoder(Y1_local_cl12$cl12_me1_8, '2:1; 3:1; 4:1; 5:2; 6:3; 8:3')
# item cl12_me1_9 **
Y1_local_cl12$cl12_me1_9 <- recoder(Y1_local_cl12$cl12_me1_9, '3:1; 6:1; 8:2')
# item cl12_me1_10 ***
Y1_local_cl12$cl12_me1_10 <- recoder(Y1_local_cl12$cl12_me1_10, '3:1; 4:1; 5:1; 6:1; 7:1; 8:2; 10:3; 11:3; 12:3; 13:3')
# item cl12_me1_11 ***
Y1_local_cl12$cl12_me1_11 <- recoder(Y1_local_cl12$cl12_me1_11, '3:1; 4:1; 5:1; 5.5:1; 6:1; 8:2; 9:2; 10:2; 10.5:2; 11:2; 12:3; 13:3; 14:4;
                                     15:4; 15.5:4; 16:5')
# item cl12_me1_12 ***
Y1_local_cl12$cl12_me1_12 <- recoder(Y1_local_cl12$cl12_me1_12, '2:1; 3:1; 4:1; 5:2; 6:3; 7:3; 8:3; 9:3; 10:4; 11:4; 13:5')
# item cl12_me2_3 ***
Y1_local_cl12$cl12_me2_3 <- recoder(Y1_local_cl12$cl12_me2_3, '3:1; 5:1')
# item cl12_me2_4 ***
Y1_local_cl12$cl12_me2_4 <- recoder(Y1_local_cl12$cl12_me2_4, '4.5:0; 5:1; 5.5:1; 6.5:1; 7.5:1; 8:1; 8.5:2; 9.5:2; 10.5:3')
# item cl12_me2_5 ***
Y1_local_cl12$cl12_me2_5 <- recoder(Y1_local_cl12$cl12_me2_5, '1:0; 2:0; 4:0; 7:1')
# item cl12_me2_6 ***
Y1_local_cl12$cl12_me2_6 <- recoder(Y1_local_cl12$cl12_me2_6, '2:1; 3:2; 4:2; 5:3; 6:3; 7:4; 8:4')
# item cl12_me2_7 ***
Y1_local_cl12$cl12_me2_7 <- recoder(Y1_local_cl12$cl12_me2_7, '2:1; 5:1; 6:1; 7:2; 8:2; 9:3')
# item cl12_me2_8 
Y1_local_cl12$cl12_me2_8 <- recoder(Y1_local_cl12$cl12_me2_8, '1.5:1; 3:2; 4.5:3; 6:4; 7.5:5')
# item cl12_me2_9 ***
Y1_local_cl12$cl12_me2_9 <- recoder(Y1_local_cl12$cl12_me2_9, '2:1; 3:2; 4:2; 5:2; 6:3; 7:4; 8:5; 9:6; 10:7')
# item cl12_me2_10 ***
Y1_local_cl12$cl12_me2_10 <- recoder(Y1_local_cl12$cl12_me2_10, '3:0; 9:0; 10.5:1; 12:1; 13.5:1; 15:1; 16.5:2; 18:3; 19:3; 19.5:4; 21:5')
# item cl12_me2_11 ***
Y1_local_cl12$cl12_me2_11 <- recoder(Y1_local_cl12$cl12_me2_11, '2:1; 3:2; 4:2; 5:3; 6:3; 7:3; 8:4; 9:4; 10:4; 11:5; 12:6')
# item cl12_me3_1 ***
Y1_local_cl12$cl12_me3_1 <- recoder(Y1_local_cl12$cl12_me3_1, '3:0; 4:1; 5:2; 6:3; 7:4; 8:4; 9:5; 10:5')
# item cl12_me3_2 ***
Y1_local_cl12$cl12_me3_2 <- recoder(Y1_local_cl12$cl12_me3_2, '4:0; 5:0; 7:0; 8:1; 10:1; 12:1; 13:2; 14:2; 15:3; 16:4; 17:4; 18:4; 19:5; 20:5')
# item cl12_me3_3 ***
Y1_local_cl12$cl12_me3_3 <- recoder(Y1_local_cl12$cl12_me3_3, '2:1; 3:1; 4:1; 5:1; 7:2; 8:2; 9:2; 10:3; 11:3; 12:4; 13:4; 15:5')
# item cl12_me3_4 ***
Y1_local_cl12$cl12_me3_4 <- recoder(Y1_local_cl12$cl12_me3_4, '5:0; 6:1; 7:1; 8:1; 9:1; 10:2; 11:3; 12:3; 13:4; 14:4; 15:5')
# item cl12_me3_5 ***
Y1_local_cl12$cl12_me3_5 <- recoder(Y1_local_cl12$cl12_me3_5, '2:1; 2.5:2; 3:2; 4:2; 5:3')
# item cl12_me3_6 ***
Y1_local_cl12$cl12_me3_6 <- recoder(Y1_local_cl12$cl12_me3_6, '1:0; 2:1; 3:1; 4:1; 5:2')
# item cl12_me3_9 ***
Y1_local_cl12$cl12_me3_9 <- recoder(Y1_local_cl12$cl12_me3_9, '2:1; 3:1; 4:1; 5:2; 6:3; 7:3; 8:4; 9:4; 10:5')
# check all
Y1_local_cl12_scale <- apply(Y1_local_cl12, 2,function(x) table(x, useNA='always'))
Y1_local_cl12_scale
# renaming items 
colnames(Y1_local_cl12) <- paste(colnames(Y1_local_cl12), "ordinal", sep = "_")
colnames(Y1_local_cl12)

# --------------------------------------------------------------------------------
# cl13_ items
Y1_local_cl13 <- select(g,starts_with("cl13_", ignore.case = F))
colnames(Y1_local_cl13)

# extracting unique elements
Y1_local_cl13_scale <- apply(Y1_local_cl13, 2,function(x) table(x, useNA='always'))
Y1_local_cl13_scale
# recoding accoeding to table above
# item cl13_sa1 ***
Y1_local_cl13$cl13_sa1 <- recoder(Y1_local_cl13$cl13_sa1, '5:0; 9:1; 12:1; 13:2; 15:2')
# item cl13_sa2 ***
Y1_local_cl13$cl13_sa2 <- recoder(Y1_local_cl13$cl13_sa2, '3:0; 6:1; 6.5:1; 9:1; 12:2; 13:2; 13.5:2; 14:3; 15:3')
# item cl13_sa3 
Y1_local_cl13$cl13_sa3 <- recoder(Y1_local_cl13$cl13_sa3, '12:0; 15:1')
# check all
Y1_local_cl13_scale <- apply(Y1_local_cl13, 2,function(x) table(x, useNA='always'))
Y1_local_cl13_scale
# renaming items 
colnames(Y1_local_cl13) <- paste(colnames(Y1_local_cl13), "ordinal", sep = "_")
colnames(Y1_local_cl13)

# ---------------------------------------------------------------------------------
# cl14_ items
Y1_local_cl14 <- select(g,starts_with("cl14_", ignore.case = F))
colnames(Y1_local_cl14)

# extracting unique elements
Y1_local_cl14_scale <- apply(Y1_local_cl14, 2,function(x) table(x, useNA='always'))
Y1_local_cl14_scale
# recoding according to table above
# item cl14_fcp1 ***
Y1_local_cl14$cl14_fcp1 <- recoder(Y1_local_cl14$cl14_fcp1, '2.5:2')
# item cl14_fcp2b
Y1_local_cl14$cl14_fcp2b <- recoder(Y1_local_cl14$cl14_fcp2b, '0.5:1; 1:2')
# item cl14_fcp2c
Y1_local_cl14$cl14_fcp2c <- recoder(Y1_local_cl14$cl14_fcp2c, '0.5:1; 1:2')
# item cl14_fcp2e ***
Y1_local_cl14$cl14_fcp2e <- recoder(Y1_local_cl14$cl14_fcp2e, '3:1; 4:2')
# item cl14_fcp2f ***
Y1_local_cl14$cl14_fcp2f <- recoder(Y1_local_cl14$cl14_fcp2f, '2:1; 4:2')
# item cl14_fcp3 
Y1_local_cl14$cl14_fcp3 <- recoder(Y1_local_cl14$cl14_fcp3, '3:2')
# item cl14_fcp4a
Y1_local_cl14$cl14_fcp4a <- recoder(Y1_local_cl14$cl14_fcp4a, '3:2')
# item cl14_fcp4b ***
Y1_local_cl14$cl14_fcp4b <- recoder(Y1_local_cl14$cl14_fcp4b, '2.5:1; 4:2; 4.5:2; 5:3')
# item cl14_fcp5 ***
Y1_local_cl14$cl14_fcp5 <- recoder(Y1_local_cl14$cl14_fcp5, '2.5:0; 3:1; 3.5:2; 4:3')
# item cl14_fcp6a
Y1_local_cl14$cl14_fcp6a <- recoder(Y1_local_cl14$cl14_fcp6a, '1:0; 2:1')
# item cl14_fcp6b 
Y1_local_cl14$cl14_fcp6b <- recoder(Y1_local_cl14$cl14_fcp6b, '2:1; 4:2')
# item cl14_fcp6c 
Y1_local_cl14$cl14_fcp6c <- recoder(Y1_local_cl14$cl14_fcp6c, '1:0; 2:1')
# item cl14_fcp7a 
Y1_local_cl14$cl14_fcp7a <- recoder(Y1_local_cl14$cl14_fcp7a, '2:1')
# item cl14_fcp7b 
Y1_local_cl14$cl14_fcp7b <- recoder(Y1_local_cl14$cl14_fcp7b, '5:3')
# check all
Y1_local_cl14_scale <- apply(Y1_local_cl14, 2,function(x) table(x, useNA='always'))
Y1_local_cl14_scale
# renaming items 
colnames(Y1_local_cl14) <- paste(colnames(Y1_local_cl14), "ordinal", sep = "_")
colnames(Y1_local_cl14)

# ---------------------------------------------------------------------------------
# cl15_ items
Y1_local_cl15 <- select(g,starts_with("cl15_", ignore.case = F))
colnames(Y1_local_cl15)
# No cl15_ items

# ---------------------------------------------------------------------------------
# cl16_ items
Y1_local_cl16 <- select(g,starts_with("cl16_", ignore.case = F))
colnames(Y1_local_cl16)
# No cl16_ items

# ---------------------------------------------------------------------------------
# cl17_ items
Y1_local_cl17 <- select(g,starts_with("cl17_", ignore.case = F))
colnames(Y1_local_cl17)

# extracting unique elements
Y1_local_cl17_scale <- apply(Y1_local_cl17, 2,function(x) table(x, useNA='always'))
Y1_local_cl17_scale
# recoding according to table above
# item cl17_fcsa1a ***
Y1_local_cl17$cl17_fcsa1a <- recoder(Y1_local_cl17$cl17_fcsa1a, '0.5:1; 1.5:1')
# item cl17_fcsa1b ***
Y1_local_cl17$cl17_fcsa1b <- recoder(Y1_local_cl17$cl17_fcsa1b, '2:1; 3:1')
# item cl17_fcsa1c
Y1_local_cl17$cl17_fcsa1c <- recoder(Y1_local_cl17$cl17_fcsa1c, '2:1')
# item cl17_fcsa1d
Y1_local_cl17$cl17_fcsa1d <- recoder(Y1_local_cl17$cl17_fcsa1d, '2:1')
# item cl17_fcsa1e
Y1_local_cl17$cl17_fcsa1e <- recoder(Y1_local_cl17$cl17_fcsa1e, '2:1')
# item cl17_fcsa3a ***
Y1_local_cl17$cl17_fcsa3a <- recoder(Y1_local_cl17$cl17_fcsa3a, '2:1')
# item cl17_fcsa3b ***
Y1_local_cl17$cl17_fcsa3b <- recoder(Y1_local_cl17$cl17_fcsa3b, '1.5:1; 4:3; 4.5:3; 5:3; 6:4')
# item cl17_fcsa5 ***
Y1_local_cl17$cl17_fcsa5 <- recoder(Y1_local_cl17$cl17_fcsa5, '2:1; 3:2; 4:2; 6:3')
# check all
Y1_local_cl17_scale <- apply(Y1_local_cl17, 2,function(x) table(x, useNA='always'))
Y1_local_cl17_scale
# renaming items 
colnames(Y1_local_cl17) <- paste(colnames(Y1_local_cl17), "ordinal", sep = "_")
colnames(Y1_local_cl17)

# ---------------------------------------------------------------------------------
# cl18_ items
Y1_local_cl18 <- select(g,starts_with("cl18_", ignore.case = F))
colnames(Y1_local_cl18)

# extracting unique elements
Y1_local_cl18_scale <- apply(Y1_local_cl18, 2,function(x) table(x, useNA='always'))
Y1_local_cl18_scale
# recoding according to table above
# item cl18_fcsa1 ***
Y1_local_cl18$cl18_fcsa1 <- recoder(Y1_local_cl18$cl18_fcsa1, '3:0; 6:0; 11:1; 12:1; 14:1; 15:2')
# item cl18_fcsa2 ***
Y1_local_cl18$cl18_fcsa2 <- recoder(Y1_local_cl18$cl18_fcsa2, '5:0; 6:0; 9:1; 10:1; 12:1; 13:1; 15:2')
# item cl18_fcsa3 ***
Y1_local_cl18$cl18_fcsa3 <- recoder(Y1_local_cl18$cl18_fcsa3, '9:0; 10:1; 12:1; 15:2')
# check all
Y1_local_cl18_scale <- apply(Y1_local_cl18, 2,function(x) table(x, useNA='always'))
Y1_local_cl18_scale
# renaming items 
colnames(Y1_local_cl18) <- paste(colnames(Y1_local_cl18), "ordinal", sep = "_")
colnames(Y1_local_cl18)


# ---------------------------------------------------------------------------------
# cl19_ items
Y1_local_cl19 <- select(g,starts_with("cl19_", ignore.case = F))
colnames(Y1_local_cl19)

# extracting unique elements
Y1_local_cl19_scale <- apply(Y1_local_cl19, 2,function(x) table(x, useNA='always'))
Y1_local_cl19_scale
# recoding according to table above
# item cl19_fcsa2b
Y1_local_cl19$cl19_fcsa2b <- recoder(Y1_local_cl19$cl19_fcsa2b, '0.5:1; 1:2')
# item cl19_fcsa2c
Y1_local_cl19$cl19_fcsa2c <- recoder(Y1_local_cl19$cl19_fcsa2c, '0.5:1; 1:2')
# item cl19_fcsa2d
Y1_local_cl19$cl19_fcsa2d <- recoder(Y1_local_cl19$cl19_fcsa2d, '0.5:1; 1:2')
# item cl19_fcsa2e
Y1_local_cl19$cl19_fcsa2e <- recoder(Y1_local_cl19$cl19_fcsa2e, '3:1; 4:2')
# item cl19_fcsa2f
Y1_local_cl19$cl19_fcsa2f <- recoder(Y1_local_cl19$cl19_fcsa2f, '4:1')
# item cl19_fcsa3
Y1_local_cl19$cl19_fcsa3 <- recoder(Y1_local_cl19$cl19_fcsa3, '2:1; 3:2')
# item cl19_fcsa4a
Y1_local_cl19$cl19_fcsa4a <- recoder(Y1_local_cl19$cl19_fcsa4a, '2:1; 3:2')
# item cl19_fcsa4b
Y1_local_cl19$cl19_fcsa4b <- recoder(Y1_local_cl19$cl19_fcsa4b, '4.5:4')
# item cl19_fcsa5
Y1_local_cl19$cl19_fcsa5 <- recoder(Y1_local_cl19$cl19_fcsa5, '1.5:0; 2.5:1; 3:2; 3.5:3')
# item cl19_fcsa6b 
Y1_local_cl19$cl19_fcsa6b <- recoder(Y1_local_cl19$cl19_fcsa6b, '2:1; 4:2')
# item cl19_fcsa7b
Y1_local_cl19$cl19_fcsa7b <- recoder(Y1_local_cl19$cl19_fcsa7b, '5:3')
# check all
Y1_local_cl19_scale <- apply(Y1_local_cl19, 2,function(x) table(x, useNA='always'))
Y1_local_cl19_scale
# renaming items 
colnames(Y1_local_cl19) <- paste(colnames(Y1_local_cl19), "ordinal", sep = "_")
colnames(Y1_local_cl19)
#--------------------------------------------------------------------------------------
# Adding all of the recoded items to the main data set

g_ordinal <- cbind(g_ordinal,Y1_local_cl1, Y1_local_cl2, Y1_local_cl3, Y1_local_cl4, Y1_local_cl5, Y1_local_cl6, Y1_local_cl7, Y1_local_cl8,
                   Y1_local_cl9, Y1_local_cl10, Y1_local_cl11, Y1_local_cl12, Y1_local_cl13, Y1_local_cl14, Y1_local_cl17, Y1_local_cl18, Y1_local_cl19)

# -------------------------------------------------------
# making variables in g_ordinal to hold data
g_ordinal$year1_total <- rep(NA,times=1734)
g_ordinal$year1_prop <- rep(NA,times=1734)
#----------------------------------------------------
# class 1
# identifying items
colnames(g_ordinal) # cl1 items range from 869:888
# getting basic descriptives
Y1cl1_total <- apply(g_ordinal[g_ordinal$cl==1,869:888], 1, sum, na.rm=T)
Y1cl1_mean <- round(mean(Y1cl1_total),2)
Y1cl1_max <- apply(g_ordinal[g_ordinal$cl==1,869:888], 2, max, na.rm=T)
Y1cl1_total_poss <- sum(Y1cl1_max)
Y1cl1_prop <- Y1cl1_total/Y1cl1_total_poss
# adding the class 1 data to the variable
g_ordinal[g_ordinal$cl==1,"year1_total"] <- Y1cl1_total
g_ordinal[g_ordinal$cl==1,"year1_prop"] <- Y1cl1_prop
# way to confirm
View(g_ordinal[g_ordinal$cl==1,1269:1270])
#--------------------------------------------------------------
# class 2
# identifying items
colnames(g_ordinal[889:1000]) # cl2 items range from (888 + 31) 889:919
888 + 31
# getting basic descriptives
Y1cl2_total <- apply(g_ordinal[g_ordinal$cl==2,889:919], 1, sum, na.rm=T)
Y1cl2_mean <- round(mean(Y1cl2_total),2)
Y1cl2_max <- apply(g_ordinal[g_ordinal$cl==2,889:919], 2, max, na.rm=T)
Y1cl2_total_poss <- sum(Y1cl2_max)
Y1cl2_prop <- Y1cl2_total/Y1cl2_total_poss
# adding the class 1 data to the variable
g_ordinal[g_ordinal$cl==2,"year1_total"] <- Y1cl2_total
g_ordinal[g_ordinal$cl==2,"year1_prop"] <- Y1cl2_prop
# way to confirm
View(g_ordinal[g_ordinal$cl==2,1269:1270])

#--------------------------------------------------------------
# class 3
# identifying items 
colnames(g_ordinal[920:1000]) # cl3 items range from (919 + 23) 920:942
919+23
# getting basic descriptives
Y1cl3_total <- apply(g_ordinal[g_ordinal$cl==3,920:942], 1, sum, na.rm=T)
Y1cl3_mean <- round(mean(Y1cl3_total),2)
Y1cl3_max <- apply(g_ordinal[g_ordinal$cl==3,920:942], 2, max, na.rm=T)
Y1cl3_total_poss <- sum(Y1cl3_max)
Y1cl3_prop <- Y1cl3_total/Y1cl3_total_poss
# adding the class 1 data to the variable
g_ordinal[g_ordinal$cl==3,"year1_total"] <- Y1cl3_total
g_ordinal[g_ordinal$cl==3,"year1_prop"] <- Y1cl3_prop
# way to confirm
View(g_ordinal[g_ordinal$cl==3,1269:1270])

#--------------------------------------------------------------
# class 4
# identifying items 
colnames(g_ordinal[943:1050]) # cl4 items range from (943 + 31) 943:973
942+31
# getting basic descriptives
Y1cl4_total <- apply(g_ordinal[g_ordinal$cl==4,943:973], 1, sum, na.rm=T)
Y1cl4_mean <- round(mean(Y1cl4_total),2)
Y1cl4_max <- apply(g_ordinal[g_ordinal$cl==4,943:973], 2, max, na.rm=T)
Y1cl4_total_poss <- sum(Y1cl4_max)
Y1cl4_prop <- Y1cl4_total/Y1cl4_total_poss
# adding the class 1 data to the variable
g_ordinal[g_ordinal$cl==4,"year1_total"] <- Y1cl4_total
g_ordinal[g_ordinal$cl==4,"year1_prop"] <- Y1cl4_prop
# way to confirm
View(g_ordinal[g_ordinal$cl==4,1269:1270])
#---------------------------------------------------------------
# class 5
# identifying items 
colnames(g_ordinal[974:1050]) # cl5 items range from (973 + 5) 974:978
973+5
# getting basic descriptives
Y1cl5_total <- apply(g_ordinal[g_ordinal$cl==5,974:978], 1, sum, na.rm=T)
Y1cl5_mean <- round(mean(Y1cl5_total),2)
Y1cl5_max <- apply(g_ordinal[g_ordinal$cl==5,974:978], 2, max, na.rm=T)
Y1cl5_total_poss <- sum(Y1cl5_max)
Y1cl5_prop <- Y1cl5_total/Y1cl5_total_poss
# adding the class 1 data to the variable
g_ordinal[g_ordinal$cl==5,"year1_total"] <- Y1cl5_total
g_ordinal[g_ordinal$cl==5,"year1_prop"] <- Y1cl5_prop
# way to confirm
View(g_ordinal[g_ordinal$cl==5,1269:1270])

#---------------------------------------------------------------
# class 6
# identifying items 
colnames(g_ordinal[979:1050]) # cl6 items range from (978 + 5) 979:983
978+5
# getting basic descriptives
Y1cl6_total <- apply(g_ordinal[g_ordinal$cl==6,979:983], 1, sum, na.rm=T)
Y1cl6_mean <- round(mean(Y1cl6_total),2)
Y1cl6_max <- apply(g_ordinal[g_ordinal$cl==6,979:983], 2, max, na.rm=T)
Y1cl6_total_poss <- sum(Y1cl6_max)
Y1cl6_prop <- Y1cl6_total/Y1cl6_total_poss
# adding the class 1 data to the variable
g_ordinal[g_ordinal$cl==6,"year1_total"] <- Y1cl6_total
g_ordinal[g_ordinal$cl==6,"year1_prop"] <- Y1cl6_prop
# way to confirm
View(g_ordinal[g_ordinal$cl==6,1269:1270])

#---------------------------------------------------------------
# class 7
# identifying items 
colnames(g_ordinal[984:1100]) # cl7 items range from (983 + 36) 984:1019
983+36
# getting basic descriptives
Y1cl7_total <- apply(g_ordinal[g_ordinal$cl==7,984:1019], 1, sum, na.rm=T)
Y1cl7_mean <- round(mean(Y1cl7_total),2)
Y1cl7_max <- apply(g_ordinal[g_ordinal$cl==7,984:1019], 2, max, na.rm=T)
Y1cl7_total_poss <- sum(Y1cl7_max)
Y1cl7_prop <- Y1cl7_total/Y1cl7_total_poss
# adding the class 1 data to the variable
g_ordinal[g_ordinal$cl==7,"year1_total"] <- Y1cl7_total
g_ordinal[g_ordinal$cl==7,"year1_prop"] <- Y1cl7_prop
# way to confirm
View(g_ordinal[g_ordinal$cl==7,1269:1270])

#---------------------------------------------------------------
# class 8
# identifying items 
colnames(g_ordinal[1020:1270]) # cl8 items range from (1019 + 20) 1020:1039
1019+20
# getting basic descriptives
Y1cl8_total <- apply(g_ordinal[g_ordinal$cl==8,1020:1039], 1, sum, na.rm=T)
Y1cl8_mean <- round(mean(Y1cl8_total),2)
Y1cl8_max <- apply(g_ordinal[g_ordinal$cl==8,1020:1039], 2, max, na.rm=T)
Y1cl8_total_poss <- sum(Y1cl8_max)
Y1cl8_prop <- Y1cl8_total/Y1cl8_total_poss
# adding the class 1 data to the variable
g_ordinal[g_ordinal$cl==8,"year1_total"] <- Y1cl8_total
g_ordinal[g_ordinal$cl==8,"year1_prop"] <- Y1cl8_prop
# way to confirm
View(g_ordinal[g_ordinal$cl==8,1269:1270])

#---------------------------------------------------------------
# class 9
# identifying items 
colnames(g_ordinal[1040:1270]) # cl9 items range from (1039 + 15) 1040:1054
1039+15
# getting basic descriptives
Y1cl9_total <- apply(g_ordinal[g_ordinal$cl==9,1040:1054], 1, sum, na.rm=T)
Y1cl9_mean <- round(mean(Y1cl9_total),2)
Y1cl9_max <- apply(g_ordinal[g_ordinal$cl==9,1040:1054], 2, max, na.rm=T)
Y1cl9_total_poss <- sum(Y1cl9_max)
Y1cl9_prop <- Y1cl9_total/Y1cl9_total_poss
# adding the class 1 data to the variable
g_ordinal[g_ordinal$cl==9,"year1_total"] <- Y1cl9_total
g_ordinal[g_ordinal$cl==9,"year1_prop"] <- Y1cl9_prop
# way to confirm
View(g_ordinal[g_ordinal$cl==9,1269:1270])

#---------------------------------------------------------------
# class 10
# identifying items 
colnames(g_ordinal[1055:1270]) # cl10 items range from (1054 + 31) 1055:1085
1054+31
# getting basic descriptives
Y1cl10_total <- apply(g_ordinal[g_ordinal$cl==10,1055:1085], 1, sum, na.rm=T)
Y1cl10_mean <- round(mean(Y1cl10_total),2)
Y1cl10_max <- apply(g_ordinal[g_ordinal$cl==10,1055:1085], 2, max, na.rm=T)
Y1cl10_total_poss <- sum(Y1cl10_max)
Y1cl10_prop <- Y1cl10_total/Y1cl10_total_poss
# adding the class 1 data to the variable
g_ordinal[g_ordinal$cl==10,"year1_total"] <- Y1cl10_total
g_ordinal[g_ordinal$cl==10,"year1_prop"] <- Y1cl10_prop
# way to confirm
View(g_ordinal[g_ordinal$cl==10,1269:1270])

# ---------------------------------------------------------
# subsetting classes 1:10
filter <- (g_ordinal$cl==1 | g_ordinal$cl==2 | g_ordinal$cl==3 | g_ordinal$cl==4 | g_ordinal$cl==5 |
             g_ordinal$cl==6 | g_ordinal$cl==7 | g_ordinal$cl==8 | g_ordinal$cl==9 | g_ordinal$cl==10)
Year1_ordinal <- g_ordinal[filter,]
# subsetting used variables only
Year1_ordinal <- Year1_ordinal[,c(2:5,1269:1270)]
# preparing for creation of table
by_course <- group_by(Year1_ordinal,Semester,Institution,Course.Code,cl)
year1_results <- summarise(by_course,
                     count = n(),
                     Year1_mean_prop = round(mean(year1_prop, na.rm = TRUE),2),
                     Year1_mprop_sd = round(sd(year1_prop, na.rm = TRUE),2))
year1_results<-as.data.frame(year1_results) 
# adding max colum
year1_max <- list(Y1cl1_total_poss, Y1cl2_total_poss, Y1cl3_total_poss, Y1cl4_total_poss, 
                  Y1cl5_total_poss, Y1cl6_total_poss, Y1cl7_total_poss, Y1cl8_total_poss, Y1cl9_total_poss, Y1cl10_total_poss)
year1_results$Year1_max <- year1_results$Year1_max <- rep(NA,times=10)
year1_results$Year1_max <- year1_max
# adding mean column
year1_mean <- list(Y1cl1_mean, Y1cl2_mean, Y1cl3_mean, Y1cl4_mean, 
                  Y1cl5_mean, Y1cl6_mean, Y1cl7_mean, Y1cl8_mean, Y1cl9_mean, Y1cl10_mean)
year1_results$Year1_mean <- year1_results$Year1_mean <- rep(NA,times=10)
year1_results$Year1_mean <- year1_mean
# ordering columns
year1_results <- year1_results[,c(4,1,2,3,5,9,8,6,7)]
#exporting to Excel
setwd("./Keck Tables and Figures")
write.csv(format(year1_results, digits=2, nsmall=2),"year1_results.csv") 


