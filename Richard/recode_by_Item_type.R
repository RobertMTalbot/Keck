## The purpose of this little script is to (1) create character vectors of local items for each of 7
## institution/course participating in the Keck study. (2) recode/collapse items where
## necessary, (3) exclude items that can't be readily collapsed or show no variability
## create new dataframe as output that only includes item and institutions to be
## be used in IRT fixed item calibrations so that all final exam items
## can be placed on common scale.

setwd("/Users/richardnoone/Dropbox/2017 CU Fall/Github/Keck/Keck Analysis")
load("fulldata.RData")

library(recoder)

##----------Scoring GCA items with function "score" from CTT package
# Basis for this answer key comes from the tab "GCA Details"
# in spreadsheet "GCAStudy_ALL DATA__06112017_working.xlsx"
# Import dataframe "full.data.RData"

GCA<-paste0("GCA",seq(1,25,1))
answer.key<-c(5,3,3,1,3,3,4,2,2,3,4,3,4,3,3,2,5,3,2,3,3,4,1,4,2)
out<-score(full.data[,GCA],output.scored = TRUE,answer.key,rel=TRUE)
full.data[,GCA]<-out$scored

apply(full.data[,GCA], 2,function(x) table(x, useNA='always'))

##  UNIVERSITY OF COLORADO 

# MCDB2150_KK

CU.KK <- c("cl1_fc26" , "cl1_fc27","cl1_fc28",              
"cl1_fc29" ,"cl1_fc30", "cl1_fc32" ,             
"cl1_fc33", "cl1_fc34" ,  "cl1_fc35"  , "cl1_fc36",         
"cl1_fc37" ,"cl1_fc38" , "cl1_fc39" ,  "cl1_fc40" , 
"cl7_fc38","cl11_fcmc26" , "cl11_fcmc27", "cl11_fcmc28" ,
"cl11_fcmc29", "cl11_fcmc31" , "cl11_fcmc38")


## Items to exclude because they have no clear basis for recoding, collapsing: need to check with JA & JK
## cl1_fc41, cl1_fc42, cl1_fc43, cl1_fc44, cl1_fc45, cl11_fcsa3a, cl11_fcsa3b, cl11_fcsa3c, cl11_fcsa4_1
## cl11_fcsa3a" , "cl11_fcsa3b" ,"cl11_fcsa3c", "cl11_fcsa4_1"  , "cl11_fcsa4_2"  ,"cl11_fcsa4_3" ,         
## "cl11_fcsa4_4", "cl11_fcsa5a"   , "cl11_fcsa5b" , "cl11_fcsa5c",           
## "cl11_fcsa6a" , "cl11_fcsa6b"   ,  "cl11_fcsa7a"  , "cl11_fcsa7b",           
## "cl11_fcsa7c"  ,"cl11_fcsa7d"  ,"cl11_fcsa7e"  , "cl11_fcsa7f",           
## "cl11_fcsa8a"  ,"cl11_fcsa8b","cl11_fcsa8c" ,"cl11_fcsa8d",           
## "cl11_fcsa8e","cl11_fcsa8f"
## Also excluding cl1_fc31 because 113 out of 114 students answered it correctly

d1<-full.data[full.data$Institution=="CUB_KK" & full.data$post==1,
              c("StudentID","Semester","Institution","Course Code","cl","year",
                GCA,CU.KK)]

apply(d1[,-(1:6)], 2,function(x) table(x, useNA='always'))

# MCDB2150_JK

CU.JK <- c("cl7_fc26", "cl7_fc27", "cl7_fc28", "cl7_fc29", "cl7_fc30", "cl7_fc31", "cl7_fc32",
           "cl7_fc33","cl7_fc34", "cl7_fc35", "cl7_fc36", "cl7_fc37", "cl7_fc38", "cl7_fc39",              
           "cl7_fc40","cl7_fcsa1a",  "cl7_fcsa1c","cl7_fcsa4a", "cl7_fcsa5a",  "cl7_fcsa6a",
           "cl7_fcsa6c", "cl7_fcsa6d", "cl7_fcsa6e", "cl11_fcmc26", "cl11_fcmc27",
           "cl17_fcsa2a", "cl17_fcsa2b", "cl17_fcsa2c", "cl17_fcsa2d", # missing from original
           "cl17_fcmc3", "cl17_fcmc4" , "cl17_fcsa1a" , "cl17_fcsa1b" , "cl17_fcsa1c","cl17_fcsa1d" , 
           "cl17_fcsa1e", "cl17_fcsa1f" , "cl17_fcsa3a",
           "cl7_fcsa1b", "cl7_fcsa1d", "cl7_fcsa2a",
           "cl7_fcsa2b","cl7_fcsa2c", "cl7_fcsa3a", "cl7_fcsa3d", "cl7_fcsa5b", "cl17_fcsa3c",
           "cl7_fcsa3b")    # missing from original  

## Items I excluded from list above because they have no clear basis for recoding, 
## collapsing: need to check with JA & JK
## cl17_fcsa3b, cl7_fcsa3c, cl7_fcsa4b, cl17_fcsa5, cl7_fcsa5c, cl7_fcsa6b, cl17_fcsa3b

## Items I recode/collapse: cl7_fcsa1a, cl7_fcsa1b, cl7_fcsa1c, cl7_fcsa1d, cl7_fcsa2a, cl7_fcsa2b, cl7_fcsa2c, cl7_fcsa3a
## cl7_fcsa3c, cl7_fcsa4a, cl7_fcsa4b, cl7_fcsa5a, cl7_fcsa6a, cl7_fcsa6b, cl7_fcsa6c, cl7_fcsa6d,
## cl7_fcsa6e, cl17_fcsa1b, cl17_fcsa1e, cl17_fcsa3a, cl17_fcsa3c 

d2<-full.data[full.data$Institution=="CUB_JK" & full.data$post==1,
              c("StudentID","Semester","Institution","Course Code","cl","year",
              GCA,CU.JK)]

d2$cl7_fcsa1a<- recoder(d2$cl7_fcsa1a,'2:1')
d2$cl7_fcsa1c<- recoder(d2$cl7_fcsa1c,'2:1')
d2$cl7_fcsa4a<- recoder(d2$cl7_fcsa4a,'0.5:0; 2:1')
d2$cl7_fcsa5a<- recoder(d2$cl7_fcsa5a,'2:1')
d2$cl7_fcsa6a<- recoder(d2$cl7_fcsa6a,'2:1')
d2$cl7_fcsa6c<- recoder(d2$cl7_fcsa6c,'2:1')
d2$cl7_fcsa6d<- recoder(d2$cl7_fcsa6d,'2:1')
d2$cl7_fcsa6e<- recoder(d2$cl7_fcsa6e,'2:1; 3:1; 4:1')
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
d2$cl7_fcsa1b<- recoder(d2$cl7_fcsa1b,'2:1; 3:2')
d2$cl7_fcsa1d<- recoder(d2$cl7_fcsa1d,'2:1; 3:2')
d2$cl7_fcsa2a<- recoder(d2$cl7_fcsa2a,'2:1; 3:1; 4:1; 5:1; 6:2')
d2$cl7_fcsa2b<- recoder(d2$cl7_fcsa2b,'2:1; 3:1; 4:1; 5:1; 6:2')
d2$cl7_fcsa3a<- recoder(d2$cl7_fcsa3a,'1.5:1; 2:1; 2.5:1; 3:2')
d2$cl7_fcsa3b<- recoder(d2$cl7_fcsa3b,'2:1; 3:2')
d2$cl7_fcsa3d<- recoder(d2$cl7_fcsa3d,'2:1; 3:2')
d2$cl17_fcsa3c<- recoder(d2$cl17_fcsa3c,'2:1; 3:2')

apply(d2[-(1:6)], 2,function(x) table(x, useNA='always'))

## JHU: Genetics020.330

JHU<-c("cl2_me1_1", "cl2_me1_2"  ,"cl2_me1_3"  ,            
 "cl2_me1_4" , "cl2_me1_5" ,  "cl2_me1_6"  ,  "cl2_me1_7"  ,            
 "cl2_me1_8" , "cl2_me1_9"  , "cl2_me1_10" ,  "cl2_me1_11" ,           
 "cl2_me1_12" , "cl2_me2_1"  ,  "cl2_me2_2"  , "cl2_me2_3"  ,            
 "cl2_me2_4"  , "cl2_me2_5" ,   "cl2_me2_6" ,   "cl2_me2_7"  ,           
 "cl2_me2_8"  , "cl2_me2_9" ,  "cl2_me2_10" ,   "cl2_me2_11" ,          
 "cl2_me3_1"  , "cl2_me3_2"  ,  "cl2_me3_3"  ,  "cl2_me3_4"  ,          
 "cl2_me3_5"  , "cl2_me3_6"  , "cl2_me3_7"  ,   "cl2_me3_8"  ,  
 "cl12_me1_1" , "cl12_me1_2" ,  "cl12_me1_3" ,  "cl12_me1_4" ,          
 "cl12_me1_5" , "cl12_me1_6" ,   "cl12_me1_7"   , "cl12_me1_8" ,       
 "cl12_me1_9", "cl12_me1_10" , "cl12_me1_11"  , "cl12_me1_12",        
  "cl12_me2_1"  , "cl12_me2_2",  "cl12_me2_5"  , "cl12_me2_6"  ,        
 "cl12_me2_7"   , "cl12_me2_8" ,  "cl12_me2_9"  , "cl12_me2_10" ,         
 "cl12_me2_11"  , "cl12_me3_1" ,   "cl12_me3_2"  , "cl12_me3_3"  ,         
 "cl12_me3_4"   , "cl12_me3_5" ,   "cl12_me3_6"  ,  "cl12_me3_9",
 "cl12_me2_3",   "cl12_me2_4",    "cl2_me3_3arev",  "cl2_me3_3brev", # missing from original
 "cl2_me3_7arev", "cl2_me3_7brev")      # missing from original

d3<-full.data[full.data$Institution=="JHU_EF" & full.data$post==1,
              c("StudentID","Semester","Institution","Course Code","cl","year",
              GCA,JHU)]

apply(d3[-(1:6)], 2,function(x) table(x, useNA='always'))

## I don't think it makes sense to try to model these JHU items
## all come from midterms, very unusual scoring

## METRO: BIO3600

Metro<-c( "cl3_fc11", "cl3_fc12" , "cl3_fc13" ,             
 "cl3_fc14" ,   "cl3_fc15"   ,  "cl3_fc16",  "cl3_fc17" ,             
 "cl3_fc18" ,   "cl3_fc19"   ,  "cl3_fc20"  ,   "cl3_fc21" ,             
 "cl3_fc22" ,   "cl3_fc23"   ,  "cl3_fc24"   ,  "cl3_fc25" ,             
 "cl3_fc26" ,   "cl3_fc27"   ,  "cl3_fc28"  , "cl3_fc29" ,             
 "cl3_fc30" ,   "cl3_fc31"   ,  "cl3_fc32"   ,  "cl8_fc3"  ,   
  "cl8_fc4"    ,  "cl8_fc5"  ,   "cl8_fc10"   ,   "cl8_fc11"   ,          
   "cl8_fc17" ,   "cl8_fc20"  ,   "cl8_fc22"  , "cl13_fc2" ,   "cl13_fc7"   ,  "cl13_fc8" ,             
 "cl13_fc11",   "cl13_fc12"  ,  "cl13_fc13"  ,   "cl13_fc14",             
 "cl18_fc2" ,   "cl18_fc3"   , "cl18_fc9" ,  "cl18_fc16" ,"cl18_fc17",
 "cl18_fc1") # missing from original 

# Items I'm excluding: cl13_fc10, cl8_fc14, cl13_fc6 (all students answered correct), 
# cl13_sa1, cl13_sa2, cl13_sa3 (odd scoring, no obvious recode strategy)
# remaining items are all MC, no need to recode

d4<-full.data[full.data$post==1 & full.data$Institution=="Metro_VM",
              c("StudentID","Semester","Institution","Course Code","cl","year",
              GCA,Metro)]

apply(d4[-(1:6)], 2,function(x) table(x, useNA='always'))

## ST MARYS: BL2330

STMU<-c( "cl4_fcsa2a",    "cl4_fcsa3a"   ,  "cl4_fcsa6a"  ,"cl4_fcsa6b",            
         "cl4_fcsa8a"   ,  "cl4_fcsa10c"  , "cl4_fcp1b"   ,    "cl4_fcp1d"  ,        
           "cl4_fcp4b" ,   "cl4_fcp5b"   , "cl4_fcp5e" ,     "cl9_fcsa7" , 
         "cl4_fcp5c",    # missing from original 
         "cl9_fcp5"  ,       "cl14_fcmc1"  , "cl14_fcmc2"  ,   "cl14_fcmc3"  ,      
         "cl14_fcmc4"  ,  "cl14_fcmc5" ,   "cl14_fcmc6"  ,   "cl14_fcmc7"  ,           
          "cl14_fcmc8"  , "cl14_fcmc9" , "cl14_fcmc10" ,   "cl14_fcmc11" ,           
          "cl14_fcmc12"   , "cl14_fcmc13" ,  "cl14_fcmc14" ,   "cl14_fcmc15" ,         
          "cl14_fcmc16"   ,  "cl14_fcmc17" , "cl14_fcmc18" ,   "cl14_fcmc19" ,          
          "cl14_fcmc20"   ,  "cl14_fcmc21" , "cl14_fcmc22" ,   "cl14_fcmc23" ,          
          "cl14_fcmc24"   , "cl14_fcmc25" ,   "cl14_fcp6a"  ,  "cl14_fcp6b"  , 
         "cl14_fcp6c"  ,   "cl14_fcp7a"    ,  "cl14_fcp7b" ,   "cl19_fcsa6a" , 
         "cl19_fcsa6b" ,   "cl19_fcsa6c",
        "cl4_fcsa1"   , "cl4_fcsa2b" , "cl4_fcsa3b"  , "cl4_fcsa4"   ,
        "cl4_fcsa7"   , "cl4_fcsa10b" , "cl4_fcp1a"  , "cl4_fcp1c"   ,
        "cl4_fcp2"    ,  "cl4_fcp3"    ,   "cl4_fcp4a"   , "cl4_fcp5a"   ,
        "cl4_fcp5d"   ,  "cl9_fcsa2"  ,  "cl9_fcsa3"   ,   "cl9_fcsa4"   ,   
        "cl9_fcsa5" ,    "cl9_fcp1"    ,   "cl9_fcp3"    , "cl9_fcp7"    ,
        "cl14_fcp1"   ,   "cl14_fcp2a"  ,   "cl14_fcp2b"    , "cl14_fcp2c"  ,  
        "cl14_fcp2d"  ,   "cl14_fcp2e"  ,    "cl14_fcp2f"    , "cl14_fcp3"   ,  
        "cl14_fcp4a"  ,   "cl14_fcp4b") 
        

## Excluded Items:  cl4_fcsa8b, cl4_fcsa9, cl4_fcsa10a (no variation or unusual scoring);
## Exlcuded Items: cl9_fcp4, cl9_fcp6, cl14_fcp5, cl4_fcsa5
## Items to Recode/Collapse:  cl4_fcsa1, cl4_fcsa3a, cl4_fcsa4, cl4_fcsa5, cl4_fcsa6a
## cl4_fcsa7, cl4_fcsa8a, cl4_fcsa9, cl4_fcsa10b, cl4_fcp1a, cl4_fcp1b, cl4_fcp1c, 
## cl4_fcp2, 

d5<-full.data[full.data$post==1 & full.data$Institution=="StMU_CG",
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
d5$cl9_fcsa2 <- recoder(d5$cl9_fcsa2, '2:1; 3:2')
d5$cl9_fcsa3 <- recoder(d5$cl9_fcsa3, '2:1; 4:2')
d5$cl9_fcsa4 <- recoder(d5$cl9_fcsa4, '1:0; 3:1; 4:2')
d5$cl9_fcsa5 <- recoder(d5$cl9_fcsa5, '1.5:1; 2:1; 3:2')
d5$cl9_fcp1 <- recoder(d5$cl9_fcp1, '0.5:0; 1.5:0; 4:1; 6:1; 8:1;8.5:2; 9:2')
d5$cl9_fcp3 <- recoder(d5$cl9_fcp3, '4:1; 6:1; 8:2')
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


apply(d5[-(1:6)], 2,function(x) table(x, useNA='always'))

## UCD: BIOL3832

UCD<-c("cl5_me1_3a","cl5_me1_3b" ,"cl5_me1_3c" ,"cl5_me1_9b", 
       "cl5_me3_4a", "cl5_me3_4b")
      

#Exclude cl5_me1_9a
# Exclude  "cl5_me4_1", "cl5_me4_2", "cl5_me4_3", "cl5_me4_4", "cl5_me4_5", # missing from original
# "cl6_me4_1", "cl6_me4_2", "cl6_me4_3", "cl6_me4_4", "cl6_me4_5") # missing from original

d6<-full.data[full.data$post==1 & full.data$Institution=="UCD_AJ" | full.data$Institution=="UCD_AJ1" 
   | full.data$Institution=="UCD_AJ2",
   c("StudentID","Semester","Institution","Course Code","cl","year",GCA, UCD)]

d6$cl5_me1_3a <- recoder(d6$cl5_me1_3a, '4:1')
d6$cl5_me1_3b <- recoder(d6$cl5_me1_3b, '4:1')
d6$cl5_me1_3c <- recoder(d6$cl5_me1_3c, '1:0')
d6$cl5_me1_9b <- recoder(d6$cl5_me1_9b, '2:1')
d6$cl5_me3_4a <- recoder(d6$cl5_me3_4a, '5:1')
d6$cl5_me3_4b <- recoder(d6$cl5_me3_4b, '1:0; 4:0; 5:1')

apply(d6[-(1:6)], 2,function(x) table(x, useNA='always'))

### These UCD items are very strange--only see a subset of students who took them.
### need to check with Jennifer about these...exclude for now

## UGA: GENE3200

UGA<-c("cl10_me1_2" , "cl10_me1_28","cl10_me1_30", "cl10_me2_2"  ,           
   "cl10_me2_11"  , "cl10_me2_17" , "cl10_me2_20"  ,  "cl10_me2_25"  ,        
   "cl10_me2_28"  ,  "cl10_me2_32"  , "cl10_me2_33"  ,  "cl10_me3_tf2"   ,       
  "cl10_me3_mc12" , "cl10_me4_tf1" , "cl10_me4_mc17"  ,  "cl10_me5_tf20"  ,       
  "cl10_me5_mc29" ,  "cl10_me6_20"  , "cl10_me6_25"    ,   "cl10_me6_32"   ,          
  "cl10_me6_41" )                    
               
# Exclude cl10_me1_44, cl10_me2_39, cl10_me2_40, cl10_me3_sa7, cl10_me4_sa1,
# cl10_me4_sa4, cl10_me4_sa6, cl10_me5_sa3, cl10_me6_46, cl10_me6_49 (odd scoring, no obvious recode strategy)

d7<-full.data[full.data$post==1 & full.data$Institution=="Uga_NA",
              c("StudentID","Semester","Institution","Course Code","cl","year",
              GCA,UGA)]

apply(d7[-(1:6)], 2,function(x) table(x, useNA='always'))

#CHECK NUMBERS
tot.items<-length(GCA)+length(CU.KK)+length(CU.JK)+length(Metro)+length(STMU)+length(UGA)
tot.items
tot.students<-nrow(d1)+nrow(d2)+nrow(d4)+nrow(d5)+nrow(d7)
tot.students

## Merge all these item sets (d1, d2, d4, d5, d7)
## by course into a new dataframe

library(dplyr)

d12<-full_join(d1,d2)
d124<-full_join(d12,d4)
d1245<-full_join(d124,d5)
it.data<-full_join(d1245,d7)

rm(d12,d124, d1245)

##Drop Students with no Responses to Local Items (even if they took GCA)

local_tot_score<-apply(it.data[,26:ncol(it.data)],1,function(x) sum(x, na.rm=T))

#xx<-it.data[local_tot_score==0,c(STMU,Metro)]

it.data<-it.data[local_tot_score>0,]

setwd("~/Dropbox/Github/Keck/Analysis Data")

write.csv(it.data,"Keck_local_item_data.csv")

