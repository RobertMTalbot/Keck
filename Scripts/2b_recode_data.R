## The purpose of this little script is to (1) create character vectors of local items for each of 7
## institution/course participating in the Keck study. (2) recode/collapse items where
## necessary, (3) exclude items that can't be readily collapsed or show no variability
## create new dataframe as output that only includes item and institutions to be
## be used in IRT fixed item calibrations so that all final exam items
## can be placed on common scale.

setwd("~/Dropbox/Github/Keck/Analysis Data")

load("fulldata.RData")

library(recoder)
library(CTT)
library(dplyr)

##----------Scoring GCA items with function "score" from CTT package
# Basis for this answer key comes from the tab "GCA Details"
# in spreadsheet "GCAStudy_ALL DATA__06112017_working.xlsx"
# Import dataframe "full.data.RData"

GCA<-paste0("GCA",seq(1,25,1))
answer.key<-c(5,3,3,1,3,3,4,2,2,3,4,3,4,3,3,2,5,3,2,3,3,4,1,4,2)
out<-score(full.data[,GCA],output.scored = TRUE,answer.key,rel=TRUE)
full.data[,GCA]<-out$scored

#apply(full.data[,GCA], 2,function(x) table(x, useNA='always'))

## Import information about items for recoding by institution/course
## taken from item profile established by Jennifer Avena & Richard Noone

setwd("~/Dropbox/Github/Keck/Original Data/Item Documentation")
IP <- read.csv("Items Profile_12_20_17.csv", header=T)

IP$Item<-as.character(IP$Item)

CU.KK<-IP$Item[IP$Excluded==0 & IP$CUKK==1] ##  UNIVERSITY OF COLORADO 
CU.JK<-IP$Item[IP$Excluded==0 & IP$CUJK==1] ##  UNIVERSITY OF COLORADO
Metro<-IP$Item[IP$Excluded==0 & IP$Metro==1]  ## Metro
STMU<-IP$Item[IP$Excluded==0 & IP$STMU==1]   ## St Marys
UCD<-IP$Item[IP$Excluded==0 & IP$UCD==1]    ## UC Denver
UGA<-IP$Item[IP$Excluded==0 & IP$UGA==1]  ## University of Georgia

# MCDB2150_KK

## Items to exclude because they have no clear basis for recoding, collapsing: need to check with JA & JK
## cl1_fc41,  cl1_fc43, cl1_fc44, cl1_fc45, cl11_fcsa3c, cl11_fcsa4_1

# For now I am also going to exclude cl11_fcsa5a, cl11_fcsa5b, cl11_fcsa5c
# even though in previous version defined cl11_fcsa5 as sum of these 3 and then recoded
     
## Also excluding cl1_fc31 because 113 out of 114 students answered it correctly

d1<-full.data[full.data$Institution=="CUB_KK" & full.data$post==1 & full.data$`Local post done`==1 ,
              c("StudentID","Semester","Institution","Course Code","cl","year",
                GCA,CU.KK)]

d1$cl1_fc42<- recoder(d1$cl1_fc42,'1:0; 2:0; 3:0; 4:0; 5:0; 6:0; 7:0; 8:1; 9:1; 10:1; 11:2')
d1$cl1_fc43<- recoder(d1$cl1_fc43,'2:0; 3:0; 3:0; 4:0; 4.5:0; 5:0; 6:1; 6.5:1; 7:1; 7.5:1; 8:1; 
                      8.5:1; 9:1; 9.5:1; 10:2')
d1$cl11_fcsa3a<- recoder(d1$cl11_fcsa3a,'1:0; 2:1')
d1$cl11_fcsa3b<- recoder(d1$cl11_fcsa3b,'1:0; 1.5:0; 2:1')
d1$cl11_fcsa4_1<- recoder(d1$cl11_fcsa4_1,'1:0; 2:0; 3:1')
d1$cl11_fcsa4_2<- recoder(d1$cl11_fcsa4_2,'1:0; 2:0; 3:1')
d1$cl11_fcsa4_3<- recoder(d1$cl11_fcsa4_3,'3:1')
d1$cl11_fcsa4_4<- recoder(d1$cl11_fcsa4_4,'1.5:0; 2:0; 3:1')
d1$cl11_fcsa6a<-recoder(d1$cl11_fcsa6a,'1:0; 2:0; 3:1')
d1$cl11_fcsa6b<-recoder(d1$cl11_fcsa6a,'2:0; 3:1')
d1$cl11_fcsa7b<-recoder(d1$cl11_fcsa7b,'1:0; 1.5:0; 2:1')
d1$cl11_fcsa7d<-recoder(d1$cl11_fcsa7d,'0.5:1; 1:1; 1.5:1; 2:2')
d1$cl11_fcsa7e<-recoder(d1$cl11_fcsa7e,'0.5:1; 1:0; 1.5:0; 2:1')
d1$cl11_fcsa7f<-recoder(d1$cl11_fcsa7f,'1:0; 2:1')
d1$cl11_fcsa8a<-recoder(d1$cl11_fcsa8a,'1:0; 2:1')
d1$cl11_fcsa8b<-recoder(d1$cl11_fcsa8b,'1:1; 1.5:1; 2:1; 2.5:1; 3:2')
d1$cl11_fcsa8c<-recoder(d1$cl11_fcsa8c,'1:0; 2:1')
d1$cl11_fcsa8d<-recoder(d1$cl11_fcsa8d,'1:1; 1.5:1; 2:1; 2.5:1; 3:2')
d1$cl11_fcsa8e<-recoder(d1$cl11_fcsa8e,'2:1')
d1$cl11_fcsa8f<-recoder(d1$cl11_fcsa8f,'2:1')

d1<-d1[,-c(75:77)]

apply(d1[,-(1:6)], 2,function(x) table(x, useNA='always'))

# MCDB2150_JK

## Items I excluded from list above because they have no clear basis for recoding, 
## collapsing: cl17_fcsa5

d2<-full.data[full.data$Institution=="CUB_JK" & full.data$post==1 & full.data$`Local post done`==1,
              c("StudentID","Semester","Institution","Course Code","cl","year",
              GCA,CU.JK)]
#cl7 items
d2$cl7_fcsa1a<- recoder(d2$cl7_fcsa1a,'2:1')
d2$cl7_fcsa1c<- recoder(d2$cl7_fcsa1c,'2:1')
d2$cl7_fcsa1b<- recoder(d2$cl7_fcsa1b,'2:1; 3:2')
d2$cl7_fcsa1d<- recoder(d2$cl7_fcsa1d,'2:1; 3:2')
d2$cl7_fcsa2a<- recoder(d2$cl7_fcsa2a,'2:1; 3:1; 4:1; 5:1; 6:2')
d2$cl7_fcsa2b<- recoder(d2$cl7_fcsa2b,'2:1; 3:1; 4:1; 5:1; 6:2')
d2$cl7_fcsa3a<- recoder(d2$cl7_fcsa3a,'1.5:1; 2:1; 2.5:1; 3:2')
d2$cl7_fcsa3b<- recoder(d2$cl7_fcsa3b,'2:1; 3:1')
d2$cl7_fcsa3c<- recoder(d2$cl7_fcsa3c,'1:0; 2:1; 2.5:1; 3:1; 4:1; 5:2')
d2$cl7_fcsa3d<- recoder(d2$cl7_fcsa3d,'2:1; 3:2')
d2$cl7_fcsa4a<- recoder(d2$cl7_fcsa4a,'0.5:0; 2:1')
d2$cl7_fcsa4b<- recoder(d2$cl7_fcsa4b,'1:0; 2:1; 3:1; 3.5:1; 4:1; 5:1; 5.5:1; 6:2')
d2$cl7_fcsa5a<- recoder(d2$cl7_fcsa5a,'2:1')
d2$cl7_fcsa5c<- recoder(d2$cl7_fcsa5c,'1:0; 2:0; 3:0; 4:0; 4.5:0; 5:0; 6:1')
d2$cl7_fcsa6a<- recoder(d2$cl7_fcsa6a,'2:1')
d2$cl7_fcsa6b<- recoder(d2$cl7_fcsa6b,'1:0; 2:0; 3:0; 4:1')
d2$cl7_fcsa6c<- recoder(d2$cl7_fcsa6c,'2:1')
d2$cl7_fcsa6d<- recoder(d2$cl7_fcsa6d,'2:1')
d2$cl7_fcsa6e<- recoder(d2$cl7_fcsa6e,'2:1; 3:1; 4:1')
#cl17 items
d2$cl17_fcsa1a<- recoder(d2$cl17_fcsa1a,'0.5:0; 1:0; 1.5:1; 2:1')
d2$cl17_fcsa1b<- recoder(d2$cl17_fcsa1b,'2:0; 3:1')
d2$cl17_fcsa1c<- recoder(d2$cl17_fcsa1c,'2:1')
d2$cl17_fcsa1d<- recoder(d2$cl17_fcsa1d,'2:1')
d2$cl17_fcsa1e<- recoder(d2$cl17_fcsa1e,'2:1')
d2$cl17_fcsa1f<- recoder(d2$cl17_fcsa1f,'2:1')
d2$cl17_fcsa2a<- recoder(d2$cl17_fcsa2a,'2:1')
d2$cl17_fcsa2b<- recoder(d2$cl17_fcsa2b,'1.5:1; 3:1')
d2$cl17_fcsa2c<- recoder(d2$cl17_fcsa2c,'2:1')
d2$cl17_fcsa2d<- recoder(d2$cl17_fcsa2d,'1.5:1; 3:1')
d2$cl17_fcsa3a<- recoder(d2$cl17_fcsa3a,'2:1')
d2$cl17_fcsa3b<- recoder(d2$cl17_fcsa3b,'1:0; 1.5:1; 2:1; 3:1; 4:1; 4.5:1; 5:1; 6:2')
d2$cl17_fcsa3c<- recoder(d2$cl17_fcsa3c,'1:1; 2:1; 3:2')

apply(d2[-(1:6)], 2,function(x) table(x, useNA='always'))

## JHU: Genetics020.330

d3<-full.data[full.data$Institution=="JHU_EF" & full.data$post==1 & full.data$`Local post done`==1,
              c("StudentID","Semester","Institution","Course Code","cl","year",
              GCA,JHU)]

apply(d3[-(1:6)], 2,function(x) table(x, useNA='always'))

## I don't think it makes sense to try to model these JHU items
## all come from midterms, very unusual scoring, 
## will be excluded from what follows

## METRO: BIO3600

# Items I'm excluding: cl13_fc10, cl8_fc14, cl13_fc6 (all students answered correct), 
# cl13_sa1, cl13_sa2, cl13_sa3 (odd scoring, no obvious recode strategy)
# remaining items are all MC, no need to recode

d4<-full.data[full.data$post==1 & full.data$Institution=="Metro_VM" & full.data$`Local post done`==1,
              c("StudentID","Semester","Institution","Course Code","cl","year",
              GCA,Metro)]

d4$cl13_sa1<- recoder(d4$cl13_sa1,'3:0; 5:0; 6:0; 9:0; 11:0; 12:0; 13:0; 14:0; 15:1')
d4$cl13_sa2<- recoder(d4$cl13_sa2,'3:0; 5:0; 6:0; 6.5:0; 9:0; 10:0; 12:0; 13:0; 13.5:0; 14:0; 15:1')
d4$cl13_sa3<- recoder(d4$cl13_sa3,'9:0; 10:0; 12:0; 15:1')

apply(d4[-(1:6)], 2,function(x) table(x, useNA='always'))

## ST MARYS: BL2330

## Excluded Items:  cl4_fcsa8b (no variation ); cl9_fcp4 ; cl4_fcsa5; cl4_fcsa10a

d5<-full.data[full.data$Institution=="StMU_CG" & full.data$post==1 & full.data$`Local post done`==1,
              c("StudentID","Semester","Institution","Course Code","cl","year",
              GCA,STMU)]

d5$cl4_fcsa3a<- recoder(d5$cl4_fcsa3a,'2:1')
d5$cl4_fcsa6a<- recoder(d5$cl4_fcsa6a, '2:1')
d5$cl4_fcsa6b<- recoder(d5$cl4_fcsa6b, '2:1; 1:0')
d5$cl4_fcsa8a<- recoder(d5$cl4_fcsa8a, '1:0; 2:1')
d5$cl4_fcp1b<- recoder(d5$cl4_fcp1b, '2:1')
d5$cl4_fcp4b <- recoder(d5$cl4_fcp4b, '2:1')
d5$cl4_fcp5b <- recoder(d5$cl4_fcp5b, '1.5:1; 2:1')
d5$cl4_fcp5c <- recoder(d5$cl4_fcp5c, '2:1')
d5$cl4_fcp5e <- recoder(d5$cl4_fcp5e, '2:1')
d5$cl9_fcsa7 <- recoder(d5$cl9_fcsa7, '2.5:1; 5:1')
d5$cl9_fcp5 <- recoder(d5$cl9_fcp5, '2:1; 4:1')
d5$cl14_fcp5 <- recoder(d5$cl14_fcp5, '1.5:0; 2.5:0; 3:0; 3.5:0; 4:1')
d5$cl14_fcp6a <- recoder(d5$cl14_fcp6a, '1:0; 2:1')
d5$cl14_fcp6b <- recoder(d5$cl14_fcp6b, '1:0; 2:0; 4:1')
d5$cl14_fcp6c <- recoder(d5$cl14_fcp6c, '1:0; 2:1')
d5$cl14_fcp7a <- recoder(d5$cl14_fcp7a, '2:1')
d5$cl14_fcp7b <- recoder(d5$cl14_fcp7b, '1:0; 2:1; 5:1')
d5$cl19_fcsa6b <- recoder(d5$cl19_fcsa6b, '2:1; 4:1')
d5$cl19_fcsa6c <- recoder(d5$cl19_fcsa6c, '2:1; 4:1')
d5$cl4_fcsa1<- recoder(d5$cl4_fcsa1,'1.5:1; 2:1; 2.5:1; 3:2')
d5$cl4_fcsa4<- recoder(d5$cl4_fcsa4, '1.5:1; 2:1; 3:2')
d5$cl4_fcsa7<- recoder(d5$cl4_fcsa7, '1.5:1; 3:2')
d5$cl4_fcsa10b<- recoder(d5$cl4_fcsa10b, '2:1; 3:1; 4:2')
d5$cl4_fcp1a<- recoder(d5$cl4_fcp1a, '3:2')
d5$cl4_fcp2 <- recoder(d5$cl4_fcp2, '2:1; 3:1; 4:2')
d5$cl4_fcp3 <- recoder(d5$cl4_fcp3, '2:1; 3:1; 4:2')
d5$cl4_fcp5a <- recoder(d5$cl4_fcp5a, '2:1; 2.5:1; 3:2')
d5$cl4_fcp5d <- recoder(d5$cl4_fcp5d, '2:1; 3:1; 4:2')
d5$cl4_fcsa9 <- recoder(d5$cl4_fcsa9, '1:1; 2:1; 3:1; 4:1; 5:1; 5.5:1; 6:2')
d5$cl9_fcsa2 <- recoder(d5$cl9_fcsa2, '2:1; 3:2')
d5$cl9_fcsa3 <- recoder(d5$cl9_fcsa3, '2:1; 4:2')
d5$cl9_fcsa4 <- recoder(d5$cl9_fcsa4, '1:0; 3:1; 4:2')
d5$cl9_fcsa5 <- recoder(d5$cl9_fcsa5, '1.5:1; 2:1; 3:2')
d5$cl9_fcp1 <- recoder(d5$cl9_fcp1, '0.5:0; 1.5:0; 4:1; 6:1; 8:1;8.5:2; 9:2')
d5$cl9_fcp3 <- recoder(d5$cl9_fcp3, '4:1; 6:1; 8:2')
d5$cl9_fcp6 <- recoder(d5$cl9_fcp6, '2:0; 3:0; 5:0; 6:0; 7:0; 8:0; 9:1')
d5$cl9_fcp7 <- recoder(d5$cl9_fcp7, '1.5:1; 3.5:1; 4:2')
d5$cl14_fcp1 <- recoder(d5$cl14_fcp1, '2:1; 2.5:1; 3:2')
d5$cl14_fcp2b <- recoder(d5$cl14_fcp2b, '0.5:1; 1:2')
d5$cl14_fcp2c <- recoder(d5$cl14_fcp2c, '0.5:1; 1:2')
d5$cl14_fcp2d <- recoder(d5$cl14_fcp2d, '0.5:1; 1:2')
d5$cl14_fcp2e <- recoder(d5$cl14_fcp2e, '3:1; 4:2')
d5$cl14_fcp2f <- recoder(d5$cl14_fcp2f, '2:1; 4:2')
d5$cl14_fcp3 <- recoder(d5$cl14_fcp3, '2:1; 3:2')
d5$cl14_fcp4a <- recoder(d5$cl14_fcp4a, '2:1; 3:2')
d5$cl14_fcp4b <- recoder(d5$cl14_fcp4b, '2:1; 2.5:1; 3:1; 4:1; 4.5:2; 5:2')
d5$cl14_fcp5 <- recoder(d5$cl14_fcp5, '1.5:0; 2.5:0; 3:0; 3.5:0; 4:1')

apply(d5[-(1:6)], 2,function(x) table(x, useNA='always'))

## UCD: BIOL3832

# Exclude  "cl5_me4_1", "cl6_me4_1","cl6_me4_2", "cl6_me4_2","cl6_me4_4", "cl6_me4_5"
# This has the effect of excluding all students that where in UCD_AJ2 in Fall 20151607-

d6<-full.data[full.data$post==1 & full.data$`Local post done`==1 & (full.data$Institution=="UCD_AJ" | full.data$Institution=="UCD_AJ1"),
   c("StudentID","Semester","Institution","Course Code","cl","year",GCA, UCD)]

d6$cl5_me1_3a <- recoder(d6$cl5_me1_3a, '4:1')
d6$cl5_me1_3b <- recoder(d6$cl5_me1_3b, '4:1')
d6$cl5_me1_3c <- recoder(d6$cl5_me1_3c, '1:0;2:1')
d6$cl5_me1_9a <- recoder(d6$cl5_me1_9a, '2:0; 3:0; 4.5:0; 5:0; 6:1; 6.5:1; 8:2')
d6$cl5_me1_9b <- recoder(d6$cl5_me1_9b, '2:1')
d6$cl5_me3_4a <- recoder(d6$cl5_me3_4a, '5:1')
d6$cl5_me3_4b <- recoder(d6$cl5_me3_4b, '1:0; 4:0; 5:1')
d6$cl5_me4_2<-recoder(d6$cl5_me4_2, '1:0; 2:0; 3:0; 4:0; 5:0; 6:1; 7:1; 8:1; 9:1; 10:2')
d6$cl5_me4_3<-recoder(d6$cl5_me4_3, '1:0; 2:0; 3:0; 4:0; 5:1; 6:1; 7:1; 8:1; 9:1; 10:2')
d6$cl5_me4_4<-recoder(d6$cl5_me4_4, '1:0; 2.5:1; 5:1; 6.5:1; 7:1; 7.5:1; 9:1; 10:2')
d6$cl5_me4_5<-recoder(d6$cl5_me4_5, '1:0; 1.5:0; 2:0; 2.5:0; 3:0; 3.5:0; 4:0; 4.5:0; 
                      5:1; 5.5:1; 6:1; 6.5:1; 7:1; 7.5:1; 8:1; 8.5:1; 9:1; 10:2')

apply(d6[-(1:6)], 2,function(x) table(x, useNA='always'))

## UGA: GENE3200

# Exclude  cl10_me2_39, cl10_me4_sa1, cl10_me4_sa4,  (odd scoring, no obvious recode strategy)

d7<-full.data[full.data$post==1 & full.data$`Local post done`==1 & full.data$Institution=="Uga_NA",
              c("StudentID","Semester","Institution","Course Code","cl","year",
              GCA,UGA)]

d7$cl10_me1_44 <- recoder(d7$cl10_me1_44, '2.4:0; 2.5:1; 3:0; 3.6:0; 4:1; 4.8:1; 5:1; 5.2:1; 5.5:1; 6:1; 6.4:1; 7:1;
                          7.5:1; 7.6:1; 8:2; 8.5:2; 8.8:2; 9:2; 9.2:2; 9.5:2; 10:2; 10.4:2; 10.5:2;
                          11:2; 11.5:2; 11.6:2; 12:2; 12.5:2; 12.8:2; 13:2; 14:2')

d7$cl10_me2_40 <- recoder(d7$cl10_me2_40, '2:1; 3:1; 4:1; 6:1; 7:1; 7.5:1; 8:2')

d7$cl10_me3_sa7 <- recoder(d7$cl10_me3_sa7, '0.5:1; 2:1; 3:1; 4:1; 5:1; 6:1; 7:1; 8:1; 9:1; 10:1; 11:1; 12:1; 13:1; 14:2')

d7$cl10_me4_sa6 <- recoder(d7$cl10_me4_sa6, '2:1; 3:1; 4:2')

d7$cl10_me5_sa3 <- recoder(d7$cl10_me5_sa3, '2:1; 4:1; 5:1; 6:1; 7:1; 8:1; 9:1; 10:2')

d7$cl10_me6_46 <- recoder(d7$cl10_me6_46, '1:0; 2:0; 3:1; 4:1; 5:1; 6:1; 7:2')

d7$cl10_me6_49 <- recoder(d7$cl10_me6_49, '2:0; 3:0; 4:1; 5:1; 6:1; 7:1; 8:1; 9:1; 
                          10:1; 11:1; 12:1; 13:1; 14:1; 15:1; 16:2')

apply(d7[-(1:6)], 2,function(x) table(x, useNA='always'))

#CHECK NUMBERS
tot.items<-length(GCA)+length(CU.KK)+length(CU.JK)+length(Metro)+length(STMU)+length(UCD)+length(UGA)
tot.items
tot.local.items<-tot.items-25
tot.local.items
tot.students<-nrow(d1)+nrow(d2)+nrow(d4)+nrow(d5)+nrow(d6)+nrow(d7)
tot.students

## Merge all these item sets (d1, d2, d4, d5, d7)
## by course into a new dataframe

library(dplyr)

d12<-full_join(d1,d2)
d124<-full_join(d12,d4)
d1245<-full_join(d124,d5)
d12456<-full_join(d1245,d6)
it.data<-full_join(d12456,d7)

rm(d12,d124,d1245,d12456)

#For use in Rasch calibrations in 2c

setwd("~/Dropbox/Github/Keck/Analysis Data")

write.csv(it.data,"Keck_local_item_data.csv") 

It_Names<-list(CU.JK, CU.KK, Metro, STMU, UCD, UGA)

save(it.data, file="itemdata.RData")
save(It_Names, file="item_names.RData")

### TOTAL SCORES in PERCENT OF TOTAL METRIC

local_tot_score<-apply(it.data[,32:ncol(it.data)],1,function(x) sum(x, na.rm=T))

table(local_tot_score)

#it.data<-it.data[local_tot_score>0,]

max.score<-rep(0,nrow(it.data))

#Invoke separate script to find local exam max possible after recoding

setwd("~/Dropbox/Github/Keck/Scripts")

source("max_poss_on_tests.R")

# The dataframe Maxdf is created in script above

Maxdf

max.score[it.data$Institution=="CUB_KK" & it.data$Semester=="Fa2015"]<-Maxdf[1,2]
max.score[it.data$Institution=="CUB_KK" & it.data$Semester=="Fa2016"]<-Maxdf[1,4]
max.score[it.data$Institution=="CUB_JK" & it.data$Semester=="Sp2016"]<-Maxdf[2,3]
max.score[it.data$Institution=="CUB_JK" & it.data$Semester=="Sp2017"]<-Maxdf[2,5]
max.score[it.data$Institution=="Metro_VM" & it.data$Semester=="Fa2015"]<-Maxdf[3,2]
max.score[it.data$Institution=="Metro_VM" & it.data$Semester=="Sp2016"]<-Maxdf[3,3]
max.score[it.data$Institution=="Metro_VM" & it.data$Semester=="Fa2016"]<-Maxdf[3,4]
max.score[it.data$Institution=="Metro_VM" & it.data$Semester=="Sp2017"]<-Maxdf[3,5]
max.score[it.data$Institution=="StMU_CG" & it.data$Semester=="Fa2015"]<-Maxdf[4,2]
max.score[it.data$Institution=="StMU_CG" & it.data$Semester=="Sp2016"]<-Maxdf[4,3]
max.score[it.data$Institution=="StMU_CG" & it.data$Semester=="Fa2016"]<-Maxdf[4,4]
max.score[it.data$Institution=="StMU_CG" & it.data$Semester=="Sp2017"]<-Maxdf[4,5]
max.score[it.data$Institution=="UCD_AJ" & it.data$Semester=="Fa2016"]<-Maxdf[5,4]
max.score[it.data$Institution=="UCD_AJ1" & it.data$Semester=="Fa2015"]<-Maxdf[5,2]
max.score[it.data$Institution=="Uga_NA" & it.data$Semester=="Sp2016"]<-Maxdf[6,3]
max.score[it.data$Institution=="Uga_NA" & it.data$Semester=="Sp2017"]<-Maxdf[7,5]

table(max.score)

per.total<-(local_tot_score/max.score)*100

#Check to make sure no values are over 100! 
summary(per.total)

raw.scores<-data.frame(it.data,local_tot_score,max.score,per.total)

raw.scores<-arrange(raw.scores,per.total)

setwd("~/Dropbox/Github/Keck/Analysis Data")

#For use in regression analyses in 2d
write.csv(raw.scores,"Keck_post_test_per_correct.csv") 


