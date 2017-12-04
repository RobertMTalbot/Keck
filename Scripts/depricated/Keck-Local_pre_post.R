# Richard Noone
# Keck Project
# Local assessments pre-post descriptives
# packages used: dplyr, CTT
# 09-21-2017


setwd("/Users/richardnoone/Dropbox/2017 CU Fall/Github/Keck/Keck Analysis")

g <- read.csv("GCAStudy_ALL DATA__09072017_working.csv", skip = 1, header=T)

head(g)
str(g)
colnames(g)
summary(g)

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
# cl_1
# selecting only class 1 items
lpre_cl1<-select(lpre,starts_with("cl1_", ignore.case = F))
lpost_cl1<-select(lpost,starts_with("cl1_", ignore.case = F))
# pre items
# extracting unique elements
lpre_cl1_scale <- apply(lpre_cl1, 2,function(x) unique(x, incomparables = F,1))
lpre_cl1_scale
# recode according to lpre_cl1_scale data for each item on list 
# item cl1_fc44_16pre: (6 -> 3)
lpre_cl1$cl1_fc44_16pre <- ifelse(lpre_cl1$cl1_fc44_16pre==6, 3, lpre_cl1$cl1_fc44_16pre)
# item cl1_fc42a_16pre: (1.0 -> 1),(1.5 -> 2), (2.0 -> 3), (2.5 -> 4), (3.0 -> 5)
lpre_cl1$cl1_fc42a_16pre <- ifelse(lpre_cl1$cl1_fc42a_16pre==3.0, 5, lpre_cl1$cl1_fc42a_16pre)
lpre_cl1$cl1_fc42a_16pre <- ifelse(lpre_cl1$cl1_fc42a_16pre==2.5, 4, lpre_cl1$cl1_fc42a_16pre)
lpre_cl1$cl1_fc42a_16pre <- ifelse(lpre_cl1$cl1_fc42a_16pre==2.0, 3, lpre_cl1$cl1_fc42a_16pre)
lpre_cl1$cl1_fc42a_16pre <- ifelse(lpre_cl1$cl1_fc42a_16pre==1.5, 2, lpre_cl1$cl1_fc42a_16pre)
lpre_cl1$cl1_fc42a_16pre <- ifelse(lpre_cl1$cl1_fc42a_16pre==1.0, 1, lpre_cl1$cl1_fc42a_16pre)
# item cl1_fc42b_16pre: (2 -> 1)
lpre_cl1$cl1_fc42b_16pre <- ifelse(lpre_cl1$cl1_fc42b_16pre==2, 1, lpre_cl1$cl1_fc42b_16pre)
# item cl1_fc42c_16pre: (3 -> 1)
lpre_cl1$cl1_fc42c_16pre <- ifelse(lpre_cl1$cl1_fc42c_16pre==3, 1, lpre_cl1$cl1_fc42c_16pre)
# item cl1_fc42d_16pre: (3 -> 1)
lpre_cl1$cl1_fc42d_16pre <- ifelse(lpre_cl1$cl1_fc42d_16pre==3, 1, lpre_cl1$cl1_fc42d_16pre)
#check all
lpre_cl1_scale <- apply(lpre_cl1, 2,function(x) unique(x, incomparables = F,1))
lpre_cl1_scale

# now doing cl1_post
# extracting unique elements
lpost_cl1_scale <- apply(lpost_cl1, 2,function(x) unique(x, incomparables = F,1))
lpost_cl1_scale
# recode according to list above
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
lpost_cl1_scale <- apply(lpost_cl1, 2,function(x) unique(x, incomparables = F,1))
lpost_cl1_scale
# renaming items
colnames(lpre_cl1) <- paste(colnames(lpre_cl1), "ordinal", sep = "_")
colnames(lpost_cl1) <- paste(colnames(lpost_cl1), "ordinal", sep = "_")
colnames(lpre_cl1)
colnames(lpost_cl1)


# ----------------------------------------------------------
# cl_2
# selecting only class 2 items
lpre_cl2<-select(lpre,starts_with("cl2_", ignore.case = F))
lpost_cl2<-select(lpost,starts_with("cl2_", ignore.case = F))
# pre items
# extracting unique elements
lpre_cl2_scale <- apply(lpre_cl2, 2,function(x) unique(x, incomparables = F,1))
lpre_cl2_scale
# recode according to list above
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
lpre_cl2_scale <- apply(lpre_cl2, 2,function(x) unique(x, incomparables = F,1))
lpre_cl2_scale

# post items
# extracting unique elements
lpost_cl2_scale <- apply(lpost_cl2, 2,function(x) unique(x, incomparables = F,1))
lpost_cl2_scale
# recode according to list above
# item cl2_me3_3brev_16post: (6 -> 2), (3 -> 1)
lpost_cl2$cl2_me3_3brev_16post <- ifelse(lpost_cl2$cl2_me3_3brev_16post==6, 2, lpost_cl2$cl2_me3_3brev_16post)
lpost_cl2$cl2_me3_3brev_16post <- ifelse(lpost_cl2$cl2_me3_3brev_16post==3, 1, lpost_cl2$cl2_me3_3brev_16post)
# item cl2_me3_7arev_16post: (6 -> 1)
lpost_cl2$cl2_me3_7arev_16post <- ifelse(lpost_cl2$cl2_me3_7arev_16post==6, 1, lpost_cl2$cl2_me3_7arev_16post)
# item cl2_me3_7brev_16post: (3 -> 2), (4 -> 3), (6 -> 4)
lpost_cl2$cl2_me3_7brev_16post <- ifelse(lpost_cl2$cl2_me3_7brev_16post==3, 2, lpost_cl2$cl2_me3_7brev_16post)
lpost_cl2$cl2_me3_7brev_16post <- ifelse(lpost_cl2$cl2_me3_7brev_16post==4, 3, lpost_cl2$cl2_me3_7brev_16post)
lpost_cl2$cl2_me3_7brev_16post <- ifelse(lpost_cl2$cl2_me3_7brev_16post==6, 4, lpost_cl2$cl2_me3_7brev_16post)
# check all
lpost_cl2_scale <- apply(lpost_cl2, 2,function(x) unique(x, incomparables = F,1))
lpost_cl2_scale
# renaming items
colnames(lpre_cl2) <- paste(colnames(lpre_cl2), "ordinal", sep = "_")
colnames(lpost_cl2) <- paste(colnames(lpost_cl2), "ordinal", sep = "_")
colnames(lpre_cl2)
colnames(lpost_cl2)

# ------------------------------------------------------------------------------
# cl3
# selecting only class 3 items
lpre_cl3<-select(lpre,starts_with("cl3_", ignore.case = F))
lpost_cl3<-select(lpost,starts_with("cl3_", ignore.case = F))
# pre items
# extracting unique elements
lpre_cl3_scale <- apply(lpre_cl3, 2,function(x) unique(x, incomparables = F,1))
lpre_cl3_scale
# no recoding needed

# post items
# extracting unique elements
lpost_cl3_scale <- apply(lpost_cl3, 2,function(x) unique(x, incomparables = F,1))
lpost_cl3_scale
# no recoding needed
# renaming items
colnames(lpre_cl3) <- paste(colnames(lpre_cl3), "ordinal", sep = "_")
colnames(lpost_cl3) <- paste(colnames(lpost_cl3), "ordinal", sep = "_")
colnames(lpre_cl3)
colnames(lpost_cl3)
# ------------------------------------------------------------------------------
#cl4
# selecting only class 4 items
lpre_cl4<-select(lpre,starts_with("cl4_", ignore.case = F))
lpost_cl4<-select(lpost,starts_with("cl4_", ignore.case = F))
# pre items
# extracting unique elements
lpre_cl4_scale <- apply(lpre_cl4, 2,function(x) unique(x, incomparables = F,1))
lpre_cl4_scale
# recode according to list above
# item cl4_fcp2_16pre: (3 -> 1)
lpre_cl4$cl4_fcp2_16pre <- ifelse(lpre_cl4$cl4_fcp2_16pre==3, 1, lpre_cl4$cl4_fcp2_16pre)
# item cl4_fcsa3a_16pre: (2 -> 1)
lpre_cl4$cl4_fcsa3a_16pre <- ifelse(lpre_cl4$cl4_fcsa3a_16pre==2, 1, lpre_cl4$cl4_fcsa3a_16pre)
# check all
lpre_cl4_scale <- apply(lpre_cl4, 2,function(x) unique(x, incomparables = F,1))
lpre_cl4_scale

# now doing post items
# extracting unique elements
lpost_cl4_scale <- apply(lpost_cl4, 2,function(x) unique(x, incomparables = F,1))
lpost_cl4_scale
# recode according to list above
# item cl4_fcsa3a_16post: (2 -> 1)
lpost_cl4$cl4_fcsa3a_16post <- ifelse(lpost_cl4$cl4_fcsa3a_16post==2, 1, lpost_cl4$cl4_fcsa3a_16post)
# item cl4_fcp2_16post: (2 -> 1), (4 -> 2)
lpost_cl4$cl4_fcp2_16post <- ifelse(lpost_cl4$cl4_fcp2_16post==2, 1, lpost_cl4$cl4_fcp2_16post)
lpost_cl4$cl4_fcp2_16post <- ifelse(lpost_cl4$cl4_fcp2_16post==4, 2, lpost_cl4$cl4_fcp2_16post)
# check all
lpost_cl4_scale <- apply(lpost_cl4, 2,function(x) unique(x, incomparables = F,1))
lpost_cl4_scale
# renaming items
colnames(lpre_cl4) <- paste(colnames(lpre_cl4), "ordinal", sep = "_")
colnames(lpost_cl4) <- paste(colnames(lpost_cl4), "ordinal", sep = "_")
colnames(lpre_cl4)
colnames(lpost_cl4)
# ------------------------------------------------------------------------------
# cl5
# selecting only class 5 items
lpre_cl5<-select(lpre,starts_with("cl5_", ignore.case = F))
lpost_cl5<-select(lpost,starts_with("cl5_", ignore.case = F))
# pre items
# extracting unique elements
lpre_cl5_scale <- apply(lpre_cl5, 2,function(x) unique(x, incomparables = F,1))
lpre_cl5_scale
# recode according to list above
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
lpre_cl5_scale <- apply(lpre_cl5, 2,function(x) unique(x, incomparables = F,1))
lpre_cl5_scale

# post items
# extracting unique elements
lpost_cl5_scale <- apply(lpost_cl5, 2,function(x) unique(x, incomparables = F,1))
lpost_cl5_scale
# recode according to list above
# item cl5_me1_3a_16post: (4 -> 1)
lpost_cl5$cl5_me1_3a_16post <- ifelse(lpost_cl5$cl5_me1_3a_16post==4, 1, lpost_cl5$cl5_me1_3a_16post)
# item cl5_me1_3b_16post: (4 -> 1)
lpost_cl5$cl5_me1_3b_16post <- ifelse(lpost_cl5$cl5_me1_3b_16post==4, 1, lpost_cl5$cl5_me1_3b_16post)
# item cl5_me1_9a_16post: (0.0 -> 0), (3.0 -> 1), (4.5 -> 2), (5.0 -> 3), (6.0 -> 4), (6.5 -> 5), (8.0 -> 6)
lpost_cl5$cl5_me1_9a_16post <- ifelse(lpost_cl5$cl5_me1_9a_16post==0.0, 0, lpost_cl5$cl5_me1_9a_16post)
lpost_cl5$cl5_me1_9a_16post <- ifelse(lpost_cl5$cl5_me1_9a_16post==3.0, 1, lpost_cl5$cl5_me1_9a_16post)
lpost_cl5$cl5_me1_9a_16post <- ifelse(lpost_cl5$cl5_me1_9a_16post==4.5, 2, lpost_cl5$cl5_me1_9a_16post)
lpost_cl5$cl5_me1_9a_16post <- ifelse(lpost_cl5$cl5_me1_9a_16post==5.0, 3, lpost_cl5$cl5_me1_9a_16post)
lpost_cl5$cl5_me1_9a_16post <- ifelse(lpost_cl5$cl5_me1_9a_16post==6.0, 4, lpost_cl5$cl5_me1_9a_16post)
lpost_cl5$cl5_me1_9a_16post <- ifelse(lpost_cl5$cl5_me1_9a_16post==6.5, 5, lpost_cl5$cl5_me1_9a_16post)
lpost_cl5$cl5_me1_9a_16post <- ifelse(lpost_cl5$cl5_me1_9a_16post==8.0, 6, lpost_cl5$cl5_me1_9a_16post)
# item cl5_me1_9b_16post: (2 -> 1)
lpost_cl5$cl5_me1_9b_16post <- ifelse(lpost_cl5$cl5_me1_9b_16post==2, 1, lpost_cl5$cl5_me1_9b_16post)
# item cl5_me3_4a_16post: (5 -> 1)
lpost_cl5$cl5_me3_4a_16post <- ifelse(lpost_cl5$cl5_me3_4a_16post==5, 1, lpost_cl5$cl5_me3_4a_16post)
# item cl5_me3_4b_16post: (4 -> 2), (5 -> 3)
lpost_cl5$cl5_me3_4b_16post <- ifelse(lpost_cl5$cl5_me3_4b_16post==4, 2, lpost_cl5$cl5_me3_4b_16post)
lpost_cl5$cl5_me3_4b_16post <- ifelse(lpost_cl5$cl5_me3_4b_16post==5, 3, lpost_cl5$cl5_me3_4b_16post)
# item cl5_me4_2a_16post: (2 -> 1), (3 <- 2), (6 -> 3)
lpost_cl5$cl5_me4_2a_16post <- ifelse(lpost_cl5$cl5_me4_2a_16post==2, 1, lpost_cl5$cl5_me4_2a_16post)
lpost_cl5$cl5_me4_2a_16post <- ifelse(lpost_cl5$cl5_me4_2a_16post==3, 2, lpost_cl5$cl5_me4_2a_16post)
lpost_cl5$cl5_me4_2a_16post <- ifelse(lpost_cl5$cl5_me4_2a_16post==6, 3, lpost_cl5$cl5_me4_2a_16post)
# item cl5_me4_2b_16post: (2 -> 1), (3 -> 2), (4 -> 3)
lpost_cl5$cl5_me4_2b_16post <- ifelse(lpost_cl5$cl5_me4_2b_16post==2, 1, lpost_cl5$cl5_me4_2b_16post)
lpost_cl5$cl5_me4_2b_16post <- ifelse(lpost_cl5$cl5_me4_2b_16post==3, 2, lpost_cl5$cl5_me4_2b_16post)
lpost_cl5$cl5_me4_2b_16post <- ifelse(lpost_cl5$cl5_me4_2b_16post==4, 3, lpost_cl5$cl5_me4_2b_16post)
# item cl5_me4_5a_16post: (0.0 -> 0), (1.0 <- 1), (1.5 -> 2), (2.0 <- 3), (2.5 -> 4), (3.0 -> 5),
#                         (3.5 -> 6), (4.0 -> 7), (5.0 -> 8)
lpost_cl5$cl5_me4_5a_16post <- ifelse(lpost_cl5$cl5_me4_5a_16post==5.0, 8, lpost_cl5$cl5_me4_5a_16post)
lpost_cl5$cl5_me4_5a_16post <- ifelse(lpost_cl5$cl5_me4_5a_16post==4.0, 7, lpost_cl5$cl5_me4_5a_16post)
lpost_cl5$cl5_me4_5a_16post <- ifelse(lpost_cl5$cl5_me4_5a_16post==3.5, 6, lpost_cl5$cl5_me4_5a_16post)
lpost_cl5$cl5_me4_5a_16post <- ifelse(lpost_cl5$cl5_me4_5a_16post==3.0, 5, lpost_cl5$cl5_me4_5a_16post)
lpost_cl5$cl5_me4_5a_16post <- ifelse(lpost_cl5$cl5_me4_5a_16post==2.5, 4, lpost_cl5$cl5_me4_5a_16post)
lpost_cl5$cl5_me4_5a_16post <- ifelse(lpost_cl5$cl5_me4_5a_16post==2.0, 3, lpost_cl5$cl5_me4_5a_16post)
lpost_cl5$cl5_me4_5a_16post <- ifelse(lpost_cl5$cl5_me4_5a_16post==1.5, 2, lpost_cl5$cl5_me4_5a_16post)
lpost_cl5$cl5_me4_5a_16post <- ifelse(lpost_cl5$cl5_me4_5a_16post==1.0, 1, lpost_cl5$cl5_me4_5a_16post)
lpost_cl5$cl5_me4_5a_16post <- ifelse(lpost_cl5$cl5_me4_5a_16post==0.0, 0, lpost_cl5$cl5_me4_5a_16post)
# item cl5_me4_5b_16post: (0.0 -> 0), (2.5 -> 1), (5.0 -> 2)
lpost_cl5$cl5_me4_5b_16post <- ifelse(lpost_cl5$cl5_me4_5b_16post==0.0, 0, lpost_cl5$cl5_me4_5b_16post)
lpost_cl5$cl5_me4_5b_16post <- ifelse(lpost_cl5$cl5_me4_5b_16post==2.5, 1, lpost_cl5$cl5_me4_5b_16post)
lpost_cl5$cl5_me4_5b_16post <- ifelse(lpost_cl5$cl5_me4_5b_16post==5.0, 2, lpost_cl5$cl5_me4_5b_16post)
# check all
lpost_cl5_scale <- apply(lpost_cl5, 2,function(x) unique(x, incomparables = F,1))
lpost_cl5_scale
# renaming items
colnames(lpre_cl5) <- paste(colnames(lpre_cl5), "ordinal", sep = "_")
colnames(lpost_cl5) <- paste(colnames(lpost_cl5), "ordinal", sep = "_")
colnames(lpre_cl5)
colnames(lpost_cl5)
# ------------------------------------------------------------------------------
# cl6 # does not exist pre-post
# ------------------------------------------------------------------------------
#cl7
# selecting only class 7 items
lpre_cl7<-select(lpre,starts_with("cl7_", ignore.case = F))
lpost_cl7<-select(lpost,starts_with("cl7_", ignore.case = F))
# pre items
# extracting unique elements
lpre_cl7_scale <- apply(lpre_cl7, 2,function(x) unique(x, incomparables = F,1))
lpre_cl7_scale
# recode according to list above
# item cl7_fcsa2a_16pre: (5 -> 4), (6 -> 5)
lpre_cl7$cl7_fcsa2a_16pre <- ifelse(lpre_cl7$cl7_fcsa2a_16pre==5, 4, lpre_cl7$cl7_fcsa2a_16pre)
lpre_cl7$cl7_fcsa2a_16pre <- ifelse(lpre_cl7$cl7_fcsa2a_16pre==6, 5, lpre_cl7$cl7_fcsa2a_16pre)
# item cl7_fcsa2b_16pre: (3 -> 2), (4 -> 3), (6 -> 4)
lpre_cl7$cl7_fcsa2b_16pre <- ifelse(lpre_cl7$cl7_fcsa2b_16pre==3, 2, lpre_cl7$cl7_fcsa2b_16pre)
lpre_cl7$cl7_fcsa2b_16pre <- ifelse(lpre_cl7$cl7_fcsa2b_16pre==4, 3, lpre_cl7$cl7_fcsa2b_16pre)
lpre_cl7$cl7_fcsa2b_16pre <- ifelse(lpre_cl7$cl7_fcsa2b_16pre==6, 4, lpre_cl7$cl7_fcsa2b_16pre)
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
# item cl7_fcsa5a_16pre: (0.0 -> 0), (0.5 -> 1), (2.0 -> 2)
lpre_cl7$cl7_fcsa5a_16pre <- ifelse(lpre_cl7$cl7_fcsa5a_16pre==0.0, 0, lpre_cl7$cl7_fcsa5a_16pre)
lpre_cl7$cl7_fcsa5a_16pre <- ifelse(lpre_cl7$cl7_fcsa5a_16pre==0.5, 1, lpre_cl7$cl7_fcsa5a_16pre)
lpre_cl7$cl7_fcsa5a_16pre <- ifelse(lpre_cl7$cl7_fcsa5a_16pre==2.0, 2, lpre_cl7$cl7_fcsa5a_16pre)
# item cl7_fcsa5b_16pre: (2 -> 1)
lpre_cl7$cl7_fcsa5b_16pre <- ifelse(lpre_cl7$cl7_fcsa5b_16pre==2, 1, lpre_cl7$cl7_fcsa5b_16pre)
# item cl7_fcsa5c_16pre: (6 -> 5)
lpre_cl7$cl7_fcsa5c_16pre <- ifelse(lpre_cl7$cl7_fcsa5c_16pre==6, 5, lpre_cl7$cl7_fcsa5c_16pre)
# check all
lpre_cl7_scale <- apply(lpre_cl7, 2,function(x) unique(x, incomparables = F,1))
lpre_cl7_scale

# post items 
# extracting unique elements
lpost_cl7_scale <- apply(lpost_cl7, 2,function(x) unique(x, incomparables = F,1))
lpost_cl7_scale
# recode according to list above
# item cl7_fcsa1a_16post: (2 -> 1)
lpost_cl7$cl7_fcsa1a_16post <- ifelse(lpost_cl7$cl7_fcsa1a_16post==2, 1, lpost_cl7$cl7_fcsa1a_16post)
# item cl7_fcsa1b_16post: (0.0 -> 0), (1.5 -> 1), (3.0 -> 2)
lpost_cl7$cl7_fcsa1b_16post <- ifelse(lpost_cl7$cl7_fcsa1b_16post==0.0, 0, lpost_cl7$cl7_fcsa1b_16post)
lpost_cl7$cl7_fcsa1b_16post <- ifelse(lpost_cl7$cl7_fcsa1b_16post==1.5, 1, lpost_cl7$cl7_fcsa1b_16post)
lpost_cl7$cl7_fcsa1b_16post <- ifelse(lpost_cl7$cl7_fcsa1b_16post==3.0, 2, lpost_cl7$cl7_fcsa1b_16post)
# item cl7_fcsa1c_16post: (2 -> 1)
lpost_cl7$cl7_fcsa1c_16post <- ifelse(lpost_cl7$cl7_fcsa1c_16post==2, 1, lpost_cl7$cl7_fcsa1c_16post)
# item cl7_fcsa1d_16post: (0.0 -> 0), (1.5 -> 1), (3.0 -> 2)
lpost_cl7$cl7_fcsa1d_16post <- ifelse(lpost_cl7$cl7_fcsa1d_16post==0.0, 0, lpost_cl7$cl7_fcsa1d_16post)
lpost_cl7$cl7_fcsa1d_16post <- ifelse(lpost_cl7$cl7_fcsa1d_16post==1.5, 1, lpost_cl7$cl7_fcsa1d_16post)
lpost_cl7$cl7_fcsa1d_16post <- ifelse(lpost_cl7$cl7_fcsa1d_16post==3.0, 2, lpost_cl7$cl7_fcsa1d_16post)
# item cl7_fcsa2a_16post: (2 -> 1), (3 -> 2), (4 -> 3), (6 -> 4)
lpost_cl7$cl7_fcsa2a_16post <- ifelse(lpost_cl7$cl7_fcsa2a_16post==2, 1, lpost_cl7$cl7_fcsa2a_16post)
lpost_cl7$cl7_fcsa2a_16post <- ifelse(lpost_cl7$cl7_fcsa2a_16post==3, 2, lpost_cl7$cl7_fcsa2a_16post)
lpost_cl7$cl7_fcsa2a_16post <- ifelse(lpost_cl7$cl7_fcsa2a_16post==4, 3, lpost_cl7$cl7_fcsa2a_16post)
lpost_cl7$cl7_fcsa2a_16post <- ifelse(lpost_cl7$cl7_fcsa2a_16post==6, 4, lpost_cl7$cl7_fcsa2a_16post)
# item cl7_fcsa2b_16post: (2 -> 1), (3 -> 2), (4 -> 3), (6 -> 4)
lpost_cl7$cl7_fcsa2b_16post <- ifelse(lpost_cl7$cl7_fcsa2b_16post==2, 1, lpost_cl7$cl7_fcsa2b_16post)
lpost_cl7$cl7_fcsa2b_16post <- ifelse(lpost_cl7$cl7_fcsa2b_16post==3, 2, lpost_cl7$cl7_fcsa2b_16post)
lpost_cl7$cl7_fcsa2b_16post <- ifelse(lpost_cl7$cl7_fcsa2b_16post==4, 3, lpost_cl7$cl7_fcsa2b_16post)
lpost_cl7$cl7_fcsa2b_16post <- ifelse(lpost_cl7$cl7_fcsa2b_16post==6, 4, lpost_cl7$cl7_fcsa2b_16post)
# item cl7_fcsa3a_16post: (2 -> 1), (3 -> 2)
lpost_cl7$cl7_fcsa3a_16post <- ifelse(lpost_cl7$cl7_fcsa3a_16post==2, 1, lpost_cl7$cl7_fcsa3a_16post)
lpost_cl7$cl7_fcsa3a_16post <- ifelse(lpost_cl7$cl7_fcsa3a_16post==3, 2, lpost_cl7$cl7_fcsa3a_16post)
# item cl7_fcsa3b_16post: (2 -> 1), (3 -> 2)
lpost_cl7$cl7_fcsa3b_16post <- ifelse(lpost_cl7$cl7_fcsa3b_16post==2, 1, lpost_cl7$cl7_fcsa3b_16post)
lpost_cl7$cl7_fcsa3b_16post <- ifelse(lpost_cl7$cl7_fcsa3b_16post==3, 2, lpost_cl7$cl7_fcsa3b_16post)
# item cl7_fcsa3c_16post: (0.0 -> 0), (2.5 -> 1), (4.0 -> 2), (5.0 -> 3)
lpost_cl7$cl7_fcsa3c_16post <- ifelse(lpost_cl7$cl7_fcsa3c_16post==0.0, 0, lpost_cl7$cl7_fcsa3c_16post)
lpost_cl7$cl7_fcsa3c_16post <- ifelse(lpost_cl7$cl7_fcsa3c_16post==2.5, 1, lpost_cl7$cl7_fcsa3c_16post)
lpost_cl7$cl7_fcsa3c_16post <- ifelse(lpost_cl7$cl7_fcsa3c_16post==4.0, 2, lpost_cl7$cl7_fcsa3c_16post)
lpost_cl7$cl7_fcsa3c_16post <- ifelse(lpost_cl7$cl7_fcsa3c_16post==5.0, 3, lpost_cl7$cl7_fcsa3c_16post)
# item cl7_fcsa3d_16post: (3 -> 1)
lpost_cl7$cl7_fcsa3d_16post <- ifelse(lpost_cl7$cl7_fcsa3d_16post==3, 1, lpost_cl7$cl7_fcsa3d_16post)
# item cl7_fcsa5c_16post: (0.0 -> 0), (1.0 -> 1), (2.0 -> 2), (3.0 -> 3), (4.0 -> 4), (4.5 -> 5), (5.0 -> 6), (6.0 -> 7)
lpost_cl7$cl7_fcsa5c_16post <- ifelse(lpost_cl7$cl7_fcsa5c_16post==6.0, 7, lpost_cl7$cl7_fcsa5c_16post)
lpost_cl7$cl7_fcsa5c_16post <- ifelse(lpost_cl7$cl7_fcsa5c_16post==5.0, 6, lpost_cl7$cl7_fcsa5c_16post)
lpost_cl7$cl7_fcsa5c_16post <- ifelse(lpost_cl7$cl7_fcsa5c_16post==4.5, 5, lpost_cl7$cl7_fcsa5c_16post)
lpost_cl7$cl7_fcsa5c_16post <- ifelse(lpost_cl7$cl7_fcsa5c_16post==4.0, 4, lpost_cl7$cl7_fcsa5c_16post)
lpost_cl7$cl7_fcsa5c_16post <- ifelse(lpost_cl7$cl7_fcsa5c_16post==3.0, 3, lpost_cl7$cl7_fcsa5c_16post)
lpost_cl7$cl7_fcsa5c_16post <- ifelse(lpost_cl7$cl7_fcsa5c_16post==2.0, 2, lpost_cl7$cl7_fcsa5c_16post)
lpost_cl7$cl7_fcsa5c_16post <- ifelse(lpost_cl7$cl7_fcsa5c_16post==1.0, 1, lpost_cl7$cl7_fcsa5c_16post)
lpost_cl7$cl7_fcsa5c_16post <- ifelse(lpost_cl7$cl7_fcsa5c_16post==0.0, 0, lpost_cl7$cl7_fcsa5c_16post)
# check all
lpost_cl7_scale <- apply(lpost_cl7, 2,function(x) unique(x, incomparables = F,1))
lpost_cl7_scale
# renaming items
colnames(lpre_cl7) <- paste(colnames(lpre_cl7), "ordinal", sep = "_")
colnames(lpost_cl7) <- paste(colnames(lpost_cl7), "ordinal", sep = "_")
colnames(lpre_cl7)
colnames(lpost_cl7)
# ------------------------------------------------------------------------------
# cl8
# selecting only class 8 items
lpre_cl8<-select(lpre,starts_with("cl8_", ignore.case = F))
lpost_cl8<-select(lpost,starts_with("cl8_", ignore.case = F))
# pre items
# extracting unique elements
lpre_cl8_scale <- apply(lpre_cl8, 2,function(x) unique(x, incomparables = F,1))
lpre_cl8_scale
# no recoding required
# post items
# extracting unique elements
lpost_cl8_scale <- apply(lpost_cl8, 2,function(x) unique(x, incomparables = F,1))
lpost_cl8_scale
# no recoding required
# renaming items
colnames(lpre_cl8) <- paste(colnames(lpre_cl8), "ordinal", sep = "_")
colnames(lpost_cl8) <- paste(colnames(lpost_cl8), "ordinal", sep = "_")
colnames(lpre_cl8)
colnames(lpost_cl8)
# ------------------------------------------------------------------------------
# cl9
# selecting only class 9 items
lpre_cl9<-select(lpre,starts_with("cl9_", ignore.case = F))
lpost_cl9<-select(lpost,starts_with("cl9_", ignore.case = F))
# pre items
# extracting unique elements
lpre_cl9_scale <- apply(lpre_cl9, 2,function(x) unique(x, incomparables = F,1))
lpre_cl9_scale
# recode according to list above
# item cl9_fcsa3a_16pre: (2 -> 1)
lpre_cl9$cl9_fcsa3a_16pre <- ifelse(lpre_cl9$cl9_fcsa3a_16pre==2, 1, lpre_cl9$cl9_fcsa3a_16pre)
# check all
lpre_cl9_scale <- apply(lpre_cl9, 2,function(x) unique(x, incomparables = F,1))
lpre_cl9_scale

# post items
# extracting unique elements
lpost_cl9_scale <- apply(lpost_cl9, 2,function(x) unique(x, incomparables = F,1))
lpost_cl9_scale
# recode according to list above
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
# check all
lpost_cl9_scale <- apply(lpost_cl9, 2,function(x) unique(x, incomparables = F,1))
lpost_cl9_scale
# renaming items
colnames(lpre_cl9) <- paste(colnames(lpre_cl9), "ordinal", sep = "_")
colnames(lpost_cl9) <- paste(colnames(lpost_cl9), "ordinal", sep = "_")
colnames(lpre_cl9)
colnames(lpost_cl9)
# ------------------------------------------------------------------------------
# cl10
# selecting only class 10 items
lpre_cl10<-select(lpre,starts_with("cl10_", ignore.case = F))
lpost_cl10<-select(lpost,starts_with("cl10_", ignore.case = F))
# pre items
# extracting unique elements
lpre_cl10_scale <- apply(lpre_cl10, 2,function(x) unique(x, incomparables = F,1))
lpre_cl10_scale
# recode according to list above
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
# item cl10_me2_40_16pre: (8 -> 6)
lpre_cl10$cl10_me2_40_16pre <- ifelse(lpre_cl10$cl10_me2_40_16pre==8, 6, lpre_cl10$cl10_me2_40_16pre)
# item cl10_me3_sa7b_16pre: (2 -> 1), (4 -> 2), (6 -> 3)
lpre_cl10$cl10_me3_sa7b_16pre <- ifelse(lpre_cl10$cl10_me3_sa7b_16pre==2, 1, lpre_cl10$cl10_me3_sa7b_16pre)
lpre_cl10$cl10_me3_sa7b_16pre <- ifelse(lpre_cl10$cl10_me3_sa7b_16pre==4, 2, lpre_cl10$cl10_me3_sa7b_16pre)
lpre_cl10$cl10_me3_sa7b_16pre <- ifelse(lpre_cl10$cl10_me3_sa7b_16pre==6, 3, lpre_cl10$cl10_me3_sa7b_16pre)
# item cl10_me3_sa7c_16pre: (2 -> 1)
lpre_cl10$cl10_me3_sa7c_16pre <- ifelse(lpre_cl10$cl10_me3_sa7c_16pre==2, 1, lpre_cl10$cl10_me3_sa7c_16pre)
# item cl10_me4_sa6a_16pre: (2 -> 1)
lpre_cl10$cl10_me4_sa6a_16pre <- ifelse(lpre_cl10$cl10_me4_sa6a_16pre==2, 1, lpre_cl10$cl10_me4_sa6a_16pre)
# check all
lpre_cl10_scale <- apply(lpre_cl10, 2,function(x) unique(x, incomparables = F,1))
lpre_cl10_scale

# post items
# extracting unique elements
lpost_cl10_scale <- apply(lpost_cl10, 2,function(x) unique(x, incomparables = F,1))
lpost_cl10_scale
# recode according to list above
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
# item cl10_me2_40_16post: (8 -> 4)
lpost_cl10$cl10_me2_40_16post <- ifelse(lpost_cl10$cl10_me2_40_16post==8, 4, lpost_cl10$cl10_me2_40_16post)
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
lpost_cl10_scale <- apply(lpost_cl10, 2,function(x) unique(x, incomparables = F,1))
lpost_cl10_scale
# renaming items
colnames(lpre_cl10) <- paste(colnames(lpre_cl10), "ordinal", sep = "_")
colnames(lpost_cl10) <- paste(colnames(lpost_cl10), "ordinal", sep = "_")
colnames(lpre_cl10)
colnames(lpost_cl10)
# ------------------------------------------------------------------------------
# no other local classes have pre-post
# Now I'm combining the dataframes together with the original
g_local <- cbind(g,lpre_cl1,lpre_cl2,lpre_cl3,lpre_cl4,lpre_cl5,lpre_cl7,lpre_cl8,lpre_cl9,lpre_cl10,
                 lpost_cl1,lpost_cl2,lpost_cl3,lpost_cl4,lpost_cl5,lpost_cl7,lpost_cl8,lpost_cl9,lpost_cl10)
colnames(g_local)
#-------------------------------------------------------------------------------

# Now I'm prepping the local pre-post items for analysis
# applying missing data rules to local-post items NOT matrix sampled (matrix sampled items are year 2 "pre" items)
library(CTT)

#These two variables indicate the number of missing response on pre-test and post-test for each student
totMiss_post_cl1 <- g_local[g_local$Local.pre.done==1 & g_local$cl==1,]
totMiss_post_cl1<-apply(totMiss_post_lpd_cl1[,771:781], MARGIN=1, function(x) {sum(is.na(x))}) 
totMiss_post_cl1 # the number of missing here is way beyond what we should expect, we need to come up with a better way of identifying/isolating the items
totMiss_post_cl1[,868]

totMiss_post_lpd # the number of missing here is way beyond what we should expect, we need to come up with a better way of identifying/isolating the items
