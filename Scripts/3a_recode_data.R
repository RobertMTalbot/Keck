## Focus is on pre and post local exam items in year 2

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

CU.KKs<-IP$Item[IP$Excluded==0 & IP$CUKK==1 & IP$y2_pre==1] ##  UNIVERSITY OF COLORADO 
CU.JKs<-IP$Item[IP$Excluded==0 & IP$CUJK==1 & IP$y2_pre==1] ##  UNIVERSITY OF COLORADO
Metros<-IP$Item[IP$Excluded==0 & IP$Metro==1 & IP$y2_pre==1]  ## Metro
STMUs<-IP$Item[IP$Excluded==0 & IP$STMU==1 & IP$y2_pre==1]   ## St Marys
UCDs<-IP$Item[IP$Excluded==0 & IP$UCD==1 & IP$y2_pre==1]    ## UC Denver
UGAs<-IP$Item[IP$Excluded==0 & IP$UGA==1 & IP$y2_pre==1]  ## University of Georgia

# MCDB2150_KK

## Items to exclude because they have no clear basis for recoding, collapsing: need to check with JA & JK
## cl1_fc41,  cl1_fc43, cl1_fc44, cl1_fc45, cl11_fcsa3c, cl11_fcsa4_1

# For now I am also going to exclude cl11_fcsa5a, cl11_fcsa5b, cl11_fcsa5c
# even though in previous version defined cl11_fcsa5 as sum of these 3 and then recoded
     
## Also excluding cl1_fc31 because 113 out of 114 students answered it correctly

d1<-full.data[full.data$Institution=="CUB_KK" & full.data$year==2 & (full.data$`Local post done`==1 | full.data$`Local pre done`==1),
              c("StudentID","Semester","Institution","Course Code","cl","year","post",
                GCA,CU.KKs)]

d1$cl1_fc42<- recoder(d1$cl1_fc42,'1:0; 2:0; 3:0; 3.5:0; 4:0; 5:0; 5.5:0; 6:0; 7:0; 8:1; 9:1; 10:1; 11:2')

apply(d1[,-(1:6)], 2,function(x) table(x, useNA='always'))

# MCDB2150_JK

## Items I excluded from list above because they have no clear basis for recoding, 
## collapsing: cl17_fcsa5

d2<-full.data[full.data$Institution=="CUB_JK" & full.data$year==2 & (full.data$`Local post done`==1 | full.data$`Local pre done`==1),
              c("StudentID","Semester","Institution","Course Code","cl","year","post",
              GCA,CU.JKs)]
#cl7 items
d2$cl7_fcsa2a<- recoder(d2$cl7_fcsa2a,'2:1; 3:1; 4:1; 5:1; 6:2')
d2$cl7_fcsa2b<- recoder(d2$cl7_fcsa2b,'2:1; 3:1; 4:1; 5:1; 6:2')
d2$cl7_fcsa3a<- recoder(d2$cl7_fcsa3a,'1.5:0; 2:0; 2.5:0; 3:1')
d2$cl7_fcsa3b<- recoder(d2$cl7_fcsa3b,'2:1; 3:1')
d2$cl7_fcsa3c<- recoder(d2$cl7_fcsa3c,'1:0; 2:1; 2.5:1; 3:1; 4:1; 5:2')
d2$cl7_fcsa3d<- recoder(d2$cl7_fcsa3d,'2:1; 3:1')
d2$cl7_fcsa5a<- recoder(d2$cl7_fcsa5a,'0.5:0; 2:1')
d2$cl7_fcsa5c<- recoder(d2$cl7_fcsa5c,'1:0; 2:0; 3:0; 4:0; 4.5:0; 5:0; 6:1')
#cl17 items
d2$cl17_fcsa2a<- recoder(d2$cl17_fcsa2a,'2:1')
d2$cl17_fcsa2b<- recoder(d2$cl17_fcsa2b,'1.5:1; 3:1')
d2$cl17_fcsa2c<- recoder(d2$cl17_fcsa2c,'2:1')
d2$cl17_fcsa2d<- recoder(d2$cl17_fcsa2d,'1.5:1; 3:1')
d2$cl17_fcsa3a<- recoder(d2$cl17_fcsa3a,'2:1')
d2$cl17_fcsa3b<- recoder(d2$cl17_fcsa3b,'1:0; 1.5:1; 2:1; 3:1; 4:1; 4.5:1; 5:1; 6:2')
d2$cl17_fcsa3c<- recoder(d2$cl17_fcsa3c,'1:1; 2:1; 3:2')

apply(d2[-(1:6)], 2,function(x) table(x, useNA='always'))

## JHU: Genetics020.330

## I don't think it makes sense to try to model these JHU items
## all come from midterms, very unusual scoring, 
## will be excluded from what follows

## METRO: BIO3600

# Items I'm excluding: cl13_fc10, cl8_fc14, cl13_fc6 (all students answered correct), 
# cl13_sa1, cl13_sa2, cl13_sa3 (odd scoring, no obvious recode strategy)
# remaining items are all MC, no need to recode

d4<-full.data[full.data$year==2 & full.data$Institution=="Metro_VM" & (full.data$`Local post done`==1 | full.data$`Local pre done`==1),
              c("StudentID","Semester","Institution","Course Code","cl","year","post",
              GCA,Metros)]

apply(d4[-(1:6)], 2,function(x) table(x, useNA='always'))

## ST MARYS: BL2330

##Can't include St Marys in this--sample size on matrix items is just too small to support use of an IRT model

## UCD: BIOL3832

# Exclude  "cl5_me4_1", "cl6_me4_1","cl6_me4_2", "cl6_me4_2","cl6_me4_4", "cl6_me4_5"
# This has the effect of excluding all students that where in UCD_AJ2 in Fall 20151607-

d6<-full.data[full.data$year==2  & (full.data$`Local post done`==1 | full.data$`Local pre done`==1) & (full.data$Institution=="UCD_AJ" | full.data$Institution=="UCD_AJ1"),
   c("StudentID","Semester","Institution","Course Code","cl","year","post",GCA, UCDs)]

d6$cl5_me1_3a <- recoder(d6$cl5_me1_3a, '4:1')
d6$cl5_me1_3b <- recoder(d6$cl5_me1_3b, '2:1; 3:1; 4:1')
d6$cl5_me1_3c <- recoder(d6$cl5_me1_3c, '1:0;2:1')
d6$cl5_me1_9a <- recoder(d6$cl5_me1_9a, '2:0; 3:0; 4:0; 4.5:0; 5:0; 6:1; 6.5:1; 8:2')
d6$cl5_me1_9b <- recoder(d6$cl5_me1_9b, '2:1')
d6$cl5_me3_4a <- recoder(d6$cl5_me3_4a, '5:1')
d6$cl5_me3_4b <- recoder(d6$cl5_me3_4b, '1:0; 4:0; 5:1')
d6$cl5_me4_2<-recoder(d6$cl5_me4_2, '1:0; 2:0; 3:0; 4:0; 5:0; 6:1; 7:1; 8:1; 9:1; 10:2')
d6$cl5_me4_5<-recoder(d6$cl5_me4_5, '1:0; 1.5:0; 2:0; 2.5:0; 3:0; 3.5:0; 4:0; 4.5:0; 
                      5:1; 5.5:1; 6:1; 6.5:1; 7:1; 7.5:1; 8:1; 8.5:1; 9:1; 10:2')

apply(d6[-(1:6)], 2,function(x) table(x, useNA='always'))

## UGA: GENE3200

# Exclude  cl10_me2_39, cl10_me4_sa1, cl10_me4_sa4,  (odd scoring, no obvious recode strategy)

d7<-full.data[full.data$year==2  & (full.data$`Local post done`==1 | full.data$`Local pre done`==1) & full.data$Institution=="Uga_NA",
              c("StudentID","Semester","Institution","Course Code","cl","year","post",
              GCA,UGAs)]

d7$cl10_me1_44 <- recoder(d7$cl10_me1_44, '1.2:0; 2.4:0; 2.5:1; 3:0; 3.6:0; 4:1; 4.8:1; 5:1; 5.2:1; 5.5:1; 6:1; 6.4:1; 7:1;
                          7.5:1; 7.6:1; 8:2; 8.5:2; 8.8:2; 9:2; 9.2:2; 9.5:2; 10:2; 10.4:2; 10.5:2;
                          11:2; 11.5:2; 11.6:2; 12:2; 12.5:2; 12.8:2; 13:2; 14:2')

d7$cl10_me2_40 <- recoder(d7$cl10_me2_40, '2:1; 3:1; 4:1;5:1; 6:1; 7:1; 7.5:1; 8:2')

#I had to recode the item below to be dichotomous b/c in pre no student scores in top category
d7$cl10_me3_sa7 <- recoder(d7$cl10_me3_sa7, '0.5:1; 2:1; 3:1; 4:1; 5:1; 6:1; 7:1; 8:1; 9:1; 10:1; 11:1; 12:1; 13:1; 14:1')

d7$cl10_me4_sa6 <- recoder(d7$cl10_me4_sa6, '2:1; 3:1; 4:2')

apply(d7[-(1:6)], 2,function(x) table(x, useNA='always'))

#CHECK NUMBERS
tot.items<-length(GCA)+length(CU.KKs)+length(CU.JKs)+length(Metros)+length(UCDs)+length(UGAs)
tot.items
tot.local.items<-tot.items-25
tot.local.items


## Merge all these item sets (d1, d2, d4, d5, d7)
## by course into a new dataframe

library(dplyr)

d12<-full_join(d1,d2)
d124<-full_join(d12,d4)
d1246<-full_join(d124,d6)
gain.data<-full_join(d1246,d7)

#Number of Students

#Pre
nrow(gain.data[gain.data$post==0,])
nrow(gain.data[gain.data$post==1,])

rm(d12,d124,d1246)

#For use in Rasch calibrations in 3b

setwd("~/Dropbox/Github/Keck/Analysis Data")

write.csv(gain.data,"Keck_gain_data.csv") 

It_Names<-list(CU.JKs, CU.KKs, Metros, UCDs, UGAs)

save(gain.data, file="gaindata.RData")
save(It_Names, file="gaindata_itemnames.RData")

setwd("~/Dropbox/Github/Keck/Analysis Data")


