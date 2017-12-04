# Richard Noone
# Keck Project
# Local Items by Institution
# packages used: dplyr, CTT, recoder, ggplot2
# 11-09-2017
# Purpose:
#------------------------------------------------------------------
# this script will investigate differences in sample size of year 2 groups using different grouping conventions in the wide GCA data set
#---------------------------------------------------------------
setwd("/Users/richardnoone/Dropbox/2017 CU Fall/Github/Keck/Keck Analysis")

g <- read.csv("GCAStudy_ALL DATA__09072017_working.csv", skip = 1, header=T)

head(g)
str(g)
colnames(g)

#Only retain students with valid student IDs
g<-g[!(is.na(g$StudentID)),] # No invalid ids

# item grouping2 to see what happens to n when classes are paired with items after removing NAa and 0s
# pre items
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

# post items

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




# cl11 class grouping options
cl11_all <- g[g$cl==11,]
cl11_alln <- length(rownames(cl11_all))

cl11_m1 <- g[g$cl==11 & g$matrix_yr2pre==1,]
cl11_m1n <- length(rownames(cl11_m1))

cl11_m2 <- g[g$cl==11 & g$matrix_yr2pre==2,]
cl11_m2n <- length(rownames(cl11_m2))

cl11_all_lposdone1 <- g[g$cl==11 & g$Local.post.done==1,]
cl11_all_lposdone1n <- length(rownames(cl11_all_lposdone1))

cl11_all_lposdone0 <- g[g$cl==11 & g$Local.post.done==0,]
cl11_all_lposdone0n <- length(rownames(cl11_all_lposdone0))

cl11_m1_lposdone1 <- g[g$cl==11 & g$matrix_yr2pre==1 & g$Local.post.done==1,]
cl11_m1_lposdone1n <- length(rownames(cl11_m1_lposdone1))

cl11_m1_lposdone0 <- g[g$cl==11 & g$matrix_yr2pre==1 & g$Local.post.done==0,]
cl11_m1_lposdone0n <- length(rownames(cl11_m1_lposdone0))

cl11_m2_lposdone1 <- g[g$cl==11 & g$matrix_yr2pre==1 & g$Local.post.done==1,]
cl11_m2_lposdone1n <- length(rownames(cl11_m2_lposdone1))

cl11_m2_lposdone0 <- g[g$cl==11 & g$matrix_yr2pre==1 & g$Local.post.done==0,]
cl11_m2_lposdone0n <- length(rownames(cl11_m2_lposdone0))

# replacing NAs with 0s for when localpostdone==1
g[g$Local.post.done==1 & g$cl==11, cl11_post][is.na(g[g$Local.post.done==1 & g$cl==11, cl11_post])]<-0




# cl12 class grouping options
cl12_all <- g[g$cl==12,]
cl12_alln <- length(rownames(cl12_all))

cl12_ <- g[g$cl==12,]
cl12_alln <- length(rownames(cl12_all))

