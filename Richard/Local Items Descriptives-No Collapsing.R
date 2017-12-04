# Richard Noone
# Keck Project
# Local Items by Institution
# packages used: dplyr, CTT, recoder, ggplot2
# 10-11-2017
# Purpose:
#------------------------------------------------------------------
# this script will:
#                  1) place local items organized by Institution on an ordinal scale; there is no collapsing of items performed.
#                  2) get descriptive statistics from item sets for year 1 post and year 2 pre/post
#                  3) provide visual representations of the results
#---------------------------------------------------------------
setwd("/Users/richardnoone/Dropbox/2017 CU Fall/Github/Keck/Keck Analysis")

library(dplyr)
library(CTT)
library(recoder)
library(ggplot2)
# getting data
g <- read.csv("GCA_data_09072017_long-2.csv", header=T)
# checking data structure
head(g)
str(g)
colnames(g)
g$X=NULL
#g$StudentID<-as.character(g$StudentID)
#Only retain students with valid student IDs
g<-g[!(is.na(g$StudentID)),]
nrow(g)
colnames(g)

#-------------------------------------------------------------------------------------------
#                           Putting items on ordinal scale
#-------------------------------------------------------------------------------------------
# Making item sets for re-scaling using recoder
# Item by Institution
#  UNIVERSITY OF COLORADO 

# MCDB2150_JK

CU.JK <- c("cl7_fc26", "cl7_fc27", "cl7_fc28", "cl7_fc29", "cl7_fc30", "cl7_fc31", "cl7_fc32",
           "cl7_fc33", "cl7_fc34", "cl7_fc35", "cl7_fc36", "cl7_fc37", "cl7_fc38", "cl7_fc39",              
           "cl7_fc40", "cl7_fcsa1a", "cl7_fcsa1b", "cl7_fcsa1c", "cl7_fcsa1d", "cl7_fcsa2a",
           "cl7_fcsa2b", "cl7_fcsa2c", "cl7_fcsa3a", "cl7_fcsa3b", "cl7_fcsa3c", "cl7_fcsa3d",            
           "cl7_fcsa4a", "cl7_fcsa4b", "cl7_fcsa5a", "cl7_fcsa5b", "cl7_fcsa5c", "cl7_fcsa6a",
           "cl7_fcsa6b", "cl7_fcsa6c", "cl7_fcsa6d", "cl7_fcsa6e", "cl11_fcmc26", "cl11_fcmc27",
           "cl17_fcmc3", "cl17_fcmc4" , "cl17_fcsa1a" , "cl17_fcsa1b" , "cl17_fcsa1c", "cl17_fcsa1d" , 
           "cl17_fcsa1e", "cl17_fcsa1f" , "cl17_fcsa3a","cl17_fcsa3b", "cl17_fcsa3c" , "cl17_fcsa5",
           "cl17_fcsa2a", 	"cl17_fcsa2b",	"cl17_fcsa2c",	"cl17_fcsa2d")	

# MCDB2150_KK

CU.KK <-  c("cl1_fc26" , "cl1_fc27","cl1_fc28",              
            "cl1_fc29" ,"cl1_fc30", "cl1_fc31", "cl1_fc32" ,             
            "cl1_fc33", "cl1_fc34" ,  "cl1_fc35"  , "cl1_fc36",         
            "cl1_fc37" ,"cl1_fc38" , "cl1_fc39" ,  "cl1_fc40" ,             
            "cl1_fc41" , "cl1_fc42"  , "cl1_fc43"  , "cl1_fc44"  ,            
            "cl1_fc45" ,  "cl7_fc38","cl11_fcmc26" , "cl11_fcmc27", "cl11_fcmc28" ,
            "cl11_fcmc29", "cl11_fcmc31" , "cl11_fcmc38", "cl11_fcsa3a" , "cl11_fcsa3b" ,          
            "cl11_fcsa3c", "cl11_fcsa4_1"  , "cl11_fcsa4_2"  ,"cl11_fcsa4_3" ,         
            "cl11_fcsa4_4", "cl11_fcsa5a"   , "cl11_fcsa5b" , "cl11_fcsa5c",           
            "cl11_fcsa6a" , "cl11_fcsa6b"   ,  "cl11_fcsa7a"  , "cl11_fcsa7b",           
            "cl11_fcsa7c"  ,"cl11_fcsa7d"  ,"cl11_fcsa7e"  , "cl11_fcsa7f",           
            "cl11_fcsa8a"  ,"cl11_fcsa8b","cl11_fcsa8c" ,"cl11_fcsa8d",           
            "cl11_fcsa8e","cl11_fcsa8f")    

           
## JHU: Genetics020.330

JHU<-c("cl2_me1_1",           "cl2_me1_2"  ,         "cl2_me1_3"  ,            
       "cl2_me1_4"    ,             "cl2_me1_5"  ,         "cl2_me1_6"  ,           "cl2_me1_7"  ,            
       "cl2_me1_8"    ,             "cl2_me1_9"  ,         "cl2_me1_10" ,           "cl2_me1_11" ,           
       "cl2_me1_12"   ,             "cl2_me2_1"  ,         "cl2_me2_2"  ,           "cl2_me2_3"  ,            
       "cl2_me2_4"    ,             "cl2_me2_5"  ,         "cl2_me2_6"  ,           "cl2_me2_7"  ,           
       "cl2_me2_8"    ,             "cl2_me2_9"  ,         "cl2_me2_10" ,           "cl2_me2_11" ,          
       "cl2_me3_1"    ,             "cl2_me3_2"  ,         "cl2_me3_3"  ,           "cl2_me3_4"  ,          
       "cl2_me3_5"    ,             "cl2_me3_6"  ,         "cl2_me3_7"  ,           "cl2_me3_8"  ,  
       "cl12_me1_1"   ,             "cl12_me1_2" ,         "cl12_me1_3" ,           "cl12_me1_4" ,          
       "cl12_me1_5"   ,             "cl12_me1_6" ,          
       "cl12_me1_7"   ,             "cl12_me1_8" ,         "cl12_me1_9"  ,          "cl12_me1_10" ,          
       "cl12_me1_11"  ,             "cl12_me1_12",         "cl12_me2_1"  ,          "cl12_me2_2"  ,         
       "cl12_me2_3"   ,             "cl12_me2_4" ,         "cl12_me2_5"  ,          "cl12_me2_6"  ,        
       "cl12_me2_7"   ,             "cl12_me2_8" ,         "cl12_me2_9"  ,          "cl12_me2_10" ,         
       "cl12_me2_11"  ,             "cl12_me3_1" ,         "cl12_me3_2"  ,          "cl12_me3_3"  ,         
       "cl12_me3_4"   ,             "cl12_me3_5" ,         "cl12_me3_6"  ,          "cl12_me3_9" ,
       "cl2_me3_3arev",	            "cl2_me3_3brev",	     "cl2_me3_7arev",	        "cl2_me3_7brev")      


## METRO: BIO3600

Metro<-c("cl3_fc10"  ,     "cl3_fc11"   ,            "cl3_fc12"    ,           "cl3_fc13" ,             
         "cl3_fc14"          ,     "cl3_fc15"   ,            "cl3_fc16"    ,           "cl3_fc17" ,             
         "cl3_fc18"         ,      "cl3_fc19"   ,            "cl3_fc20"    ,           "cl3_fc21" ,             
         "cl3_fc22"         ,      "cl3_fc23"   ,            "cl3_fc24"   ,            "cl3_fc25" ,             
         "cl3_fc26"         ,      "cl3_fc27"   ,            "cl3_fc28"    ,           "cl3_fc29" ,             
         "cl3_fc30"         ,      "cl3_fc31"   ,            "cl3_fc32"   ,     
         "cl8_fc3"          ,      "cl8_fc4"    ,           
         "cl8_fc5"          ,      "cl8_fc10"   ,            "cl8_fc11"   ,            "cl8_fc14" ,             
         "cl8_fc17"         ,      "cl8_fc20"   ,            "cl8_fc22"   ,
         "cl13_fc2"         ,      "cl13_fc6"   ,            "cl13_fc7"   ,            "cl13_fc8" ,             
         "cl13_fc11"        ,      "cl13_fc12"  ,            "cl13_fc13"  ,            "cl13_fc14",             
         "cl13_sa1"         ,      "cl13_sa2"   ,            "cl13_sa3"   ,            "cl18_fc1" ,
         "cl18_fc2"         ,      "cl18_fc3"   ,            "cl18_fc9"   ,            "cl18_fc16" ,            
         "cl18_fc17") 


## ST MARYS: BL2330

STMU<-c("cl4_fcsa1"   ,          "cl4_fcsa2a"    ,         "cl4_fcsa2b"     ,        "cl4_fcsa3a"   ,          
        "cl4_fcsa3b"  ,           "cl4_fcsa4"   ,           "cl4_fcsa5"     ,         "cl4_fcsa6a"  ,"cl4_fcsa6b",            
        "cl4_fcsa7"   ,           "cl4_fcsa8a"   ,          "cl4_fcsa8b"    ,         "cl4_fcsa9"   ,          
        "cl4_fcsa10a" ,           "cl4_fcsa10b" ,         "cl4_fcsa10c"     ,       "cl4_fcp1a"     ,        
        "cl4_fcp1b"   ,           "cl4_fcp1c"   ,           "cl4_fcp1d"     ,         "cl4_fcp2"    ,          
        "cl4_fcp3"    ,           "cl4_fcp4a"   ,           "cl4_fcp4b"     ,         "cl4_fcp5a"   ,          
        "cl4_fcp5b"   ,           "cl4_fcp5c"   ,           "cl4_fcp5d"     ,         "cl4_fcp5e" ,  "cl9_fcsa2",             
        "cl9_fcsa3"   ,           "cl9_fcsa4"   ,           "cl9_fcsa5"     ,         "cl9_fcsa7" ,            
        "cl9_fcp1"    ,           "cl9_fcp3"    ,           "cl9_fcp4"      ,         "cl9_fcp5"  ,            
        "cl9_fcp6"    ,           "cl9_fcp7"    ,          "cl14_fcmc1"     ,       
        "cl14_fcmc2"  ,           "cl14_fcmc3"  ,           "cl14_fcmc4"    ,         "cl14_fcmc5" ,           
        "cl14_fcmc6"  ,           "cl14_fcmc7"  ,           "cl14_fcmc8"    ,         "cl14_fcmc9" ,            
        "cl14_fcmc10" ,           "cl14_fcmc11" ,           "cl14_fcmc12"   ,         "cl14_fcmc13" ,          
        "cl14_fcmc14" ,           "cl14_fcmc15" ,           "cl14_fcmc16"   ,         "cl14_fcmc17" ,          
        "cl14_fcmc18" ,           "cl14_fcmc19" ,           "cl14_fcmc20"   ,         "cl14_fcmc21" ,          
        "cl14_fcmc22" ,           "cl14_fcmc23" ,           "cl14_fcmc24"   ,         "cl14_fcmc25" ,          
        "cl14_fcp1"   ,           "cl14_fcp2a"  ,           "cl14_fcp2b"    ,         "cl14_fcp2c"  ,          
        "cl14_fcp2d"  ,           "cl14_fcp2e"  ,           "cl14_fcp2f"    ,         "cl14_fcp3"   ,          
        "cl14_fcp4a"  ,           "cl14_fcp4b"  ,           "cl14_fcp5"     ,         "cl14_fcp6a"  ,          
        "cl14_fcp6b"  ,           "cl14_fcp6c"  ,           "cl14_fcp7a"    ,         "cl14_fcp7b" ,
        "cl19_fcsa6a" ,           "cl19_fcsa6b" ,           "cl19_fcsa6c" ) 


## UCD: BIOL3832

UCD<-c("cl5_me1_3a",             "cl5_me1_3b" ,            "cl5_me1_3c" ,           
       "cl5_me1_9a" ,            "cl5_me1_9b"  ,           "cl5_me3_4a" ,            "cl5_me3_4b", 
       "cl5_me4_1",             "cl5_me4_2",        	     "cl5_me4_3",   	         "cl5_me4_4",
       "cl5_me4_5",             "cl6_me4_1",	             "cl6_me4_2",      	       "cl6_me4_3",
       "cl6_me4_4",               "cl6_me4_5")


## UGA: GENE3200

UGA<-c("cl10_me1_2"  ,           "cl10_me1_28",           
       "cl10_me1_30"    ,        "cl10_me1_44"  ,          "cl10_me2_2"   ,          "cl10_me2_11"  ,         
       "cl10_me2_17"    ,        "cl10_me2_20"  ,          "cl10_me2_25"  ,          "cl10_me2_28"  ,         
       "cl10_me2_32"    ,        "cl10_me2_33"  ,          "cl10_me2_39"  ,          "cl10_me2_40"  ,         
       "cl10_me3_tf2"   ,        "cl10_me3_mc12" ,         "cl10_me3_sa7" ,          "cl10_me4_tf1" ,         
       "cl10_me4_mc17"  ,        "cl10_me4_sa1" ,          "cl10_me4_sa4" ,          "cl10_me4_sa6" ,         
       "cl10_me5_tf20"  ,        "cl10_me5_mc29" ,         "cl10_me5_sa3" ,          "cl10_me6_20"  ,         
       "cl10_me6_25"    ,        "cl10_me6_32"   ,         "cl10_me6_41"  ,          "cl10_me6_46"  ,         
       "cl10_me6_49"  ) 


Local.Item<-list(CU.JK, CU.KK, JHU, Metro, STMU, UCD, UGA)

# making new df for recoded items
# these will be combined to original df after recoding
CU.JK.ord <- g[,CU.JK]
CU.KK.ord <- g[,CU.KK]
JHU.ord <- g[,JHU]
Metro.ord <- g[,Metro]
STMU.ord <- g[,STMU]
UCD.ord <- g[,UCD]
UGA.ord <- g[,UGA]

# getting current scale
CU.JK.ord_scale <- apply(CU.JK.ord, 2,function(x) table(x, useNA='always'))
CU.JK.ord_scale
# recoding according to table above
CU.JK.ord$cl7_fcsa1a <- recoder(CU.JK.ord$cl7_fcsa1a, '2:1') 
CU.JK.ord$cl7_fcsa1c <- recoder(CU.JK.ord$cl7_fcsa1c, '2:1')
CU.JK.ord$cl7_fcsa3a <- recoder(CU.JK.ord$cl7_fcsa3a, '1.5:2; 2:3; 2.5:4; 3:5') 

CU.JK.ord$cl7_fcsa3c <- recoder(CU.JK.ord$cl7_fcsa3c, '2.5:3; 3:4; 4:5; 5:6') 
CU.JK.ord$cl7_fcsa4a <- recoder(CU.JK.ord$cl7_fcsa4a, '0.5:1') 
CU.JK.ord$cl7_fcsa4b <- recoder(CU.JK.ord$cl7_fcsa4b, '3.5:4; 4:5; 5:6; 5.5:7; 6:8') 

CU.JK.ord$cl7_fcsa5a <- recoder(CU.JK.ord$cl7_fcsa5a, '0.5:1; 1:2; 2:3') 
CU.JK.ord$cl7_fcsa5c <- recoder(CU.JK.ord$cl7_fcsa5c, '4.5:5; 5:6; 6:7') 
CU.JK.ord$cl7_fcsa6a <- recoder(CU.JK.ord$cl7_fcsa6a, '2:1')
CU.JK.ord$cl7_fcsa6b <- recoder(CU.JK.ord$cl7_fcsa6b, '2:1; 3:2; 4:3')
CU.JK.ord$cl7_fcsa6c <- recoder(CU.JK.ord$cl7_fcsa6c, '2:1')
CU.JK.ord$cl17_fcsa1a <- recoder(CU.JK.ord$cl17_fcsa1a, '0.5:1; 1:2; 1.5:3; 2:4') 
CU.JK.ord$cl17_fcsa1b <- recoder(CU.JK.ord$cl17_fcsa1b, '2:1; 3:2') 
CU.JK.ord$cl17_fcsa1c <- recoder(CU.JK.ord$cl17_fcsa1c, '2:1')
CU.JK.ord$cl17_fcsa1d <- recoder(CU.JK.ord$cl17_fcsa1d, '2:1')
CU.JK.ord$cl17_fcsa1e <- recoder(CU.JK.ord$cl17_fcsa1e, '2:1')
CU.JK.ord$cl17_fcsa3b <- recoder(CU.JK.ord$cl17_fcsa3b, '1.5:2; 2:3; 3:4; 4:5; 4.5:6; 5:7; 6:8') 
CU.JK.ord$cl17_fcsa5 <- recoder(CU.JK.ord$cl17_fcsa5, '2:1; 3:2; 4:3; 6:4') 
CU.JK.ord$cl17_fcsa2a <- recoder(CU.JK.ord$cl17_fcsa2a, '2:1')
CU.JK.ord$cl17_fcsa2b <- recoder(CU.JK.ord$cl17_fcsa2b, '1.5:1; 3:2')
CU.JK.ord$cl17_fcsa2c <- recoder(CU.JK.ord$cl17_fcsa2c, '2:1')
CU.JK.ord$cl17_fcsa2d <- recoder(CU.JK.ord$cl17_fcsa2d, '1.5:1; 3:2')
# check
CU.JK.ord_scale <- apply(CU.JK.ord, 2,function(x) table(x, useNA='always'))
CU.JK.ord_scale
# change item names
colnames(CU.JK.ord) <- paste(colnames(CU.JK.ord), "CUJK_ord", sep = "_")
colnames(CU.JK.ord)

# getting current scale
CU.KK.ord_scale <- apply(CU.KK.ord, 2,function(x) table(x, useNA='always'))
CU.KK.ord_scale
# recoding according to table above
CU.KK.ord$cl1_fc41 <- recoder(CU.KK.ord$cl1_fc41, '2:1; 3:2; 4:3; 5:4; 6:5; 7:6; 8:7; 9:8; 10:9') 
CU.KK.ord$cl1_fc42 <- recoder(CU.KK.ord$cl1_fc42, '3.5:4; 4:5; 5:6; 5.5:7; 6:8; 7:9; 8:10; 9:11; 10:12; 11:13') 
CU.KK.ord$cl1_fc43 <- recoder(CU.KK.ord$cl1_fc43, '2:1; 3:2; 4:3; 4.5:4; 6.5:7; 7:8; 7.5:9; 8:10; 8.5:11; 9:12; 9.5:13; 10:14') 
CU.KK.ord$cl1_fc44 <- recoder(CU.KK.ord$cl1_fc44, '1.5:2; 2:3; 3:4; 4:5; 4.5:6; 5:7; 6:8') 
CU.KK.ord$cl1_fc45 <- recoder(CU.KK.ord$cl1_fc45, '6:1; 7:2; 8:3; 9:4; 10:5; 11:6; 12:7; 13:8') 
CU.KK.ord$cl11_fcsa3b <- recoder(CU.KK.ord$cl11_fcsa3b, '1.5:2; 2:3') 
CU.KK.ord$cl11_fcsa3c <- recoder(CU.KK.ord$cl11_fcsa3c, '0.5:1; 1.5:2; 2:3; 2.5:4; 3:5') 
CU.KK.ord$cl11_fcsa4_3 <- recoder(CU.KK.ord$cl11_fcsa4_3, '3:1')
CU.KK.ord$cl11_fcsa4_4 <- recoder(CU.KK.ord$cl11_fcsa4_4, '1.5:1') 
CU.KK.ord$cl11_fcsa5a <- recoder(CU.KK.ord$cl11_fcsa5a, '0.5:1; 2.5:2; 4.5:5; 5:6') 
CU.KK.ord$cl11_fcsa5b <- recoder(CU.KK.ord$cl11_fcsa5b, '2:1')
CU.KK.ord$cl11_fcsa6b <- recoder(CU.KK.ord$cl11_fcsa6b, '2:1; 3:2') 
CU.KK.ord$cl11_fcsa7b <- recoder(CU.KK.ord$cl11_fcsa7b, '1.5:2; 2:3')
CU.KK.ord$cl11_fcsa7d <- recoder(CU.KK.ord$cl11_fcsa7d, '0.5:1; 1:2; 1.5:3; 2:4') 
CU.KK.ord$cl11_fcsa7e <- recoder(CU.KK.ord$cl11_fcsa7e, '0.5:1; 1:2; 1.5:3; 2:4') 
CU.KK.ord$cl11_fcsa8b <- recoder(CU.KK.ord$cl11_fcsa8b, '1.5:2; 2:3; 2.5:4; 3:5') 
CU.KK.ord$cl11_fcsa8d <- recoder(CU.KK.ord$cl11_fcsa8d, '1.5:2; 2:3; 3:4') 
CU.KK.ord$cl11_fcsa8e <- recoder(CU.KK.ord$cl11_fcsa8e, '2:1') 
CU.KK.ord$cl11_fcsa8f <- recoder(CU.KK.ord$cl11_fcsa8f, '2:1') 
# check
CU.KK.ord_scale <- apply(CU.KK.ord, 2,function(x) table(x, useNA='always'))
CU.KK.ord_scale
# change item names
colnames(CU.KK.ord) <- paste(colnames(CU.KK.ord), "CUKK_ord", sep = "_")
colnames(CU.KK.ord)


# getting current scale
JHU.ord_scale <- apply(JHU.ord, 2,function(x) table(x, useNA='always'))
JHU.ord_scale
# recoding according to table above
JHU.ord$cl2_me1_1 <- recoder(JHU.ord$cl2_me1_1, '0.5:1; 1:2; 3.5:4; 4:5') 
JHU.ord$cl2_me1_2 <- recoder(JHU.ord$cl2_me1_2, '0.5:1; 3:2') 
JHU.ord$cl2_me1_4 <- recoder(JHU.ord$cl2_me1_4, '6:1; 8:2; 10:3; 12:4; 14:5; 16:6') 
JHU.ord$cl2_me1_5 <- recoder(JHU.ord$cl2_me1_5, '1.5:2; 2:3; 2.5:4; 3:5; 3.5:6; 4:7; 4.5:8; 5:9; 6:10; 6.5:11; 7:12; 8:13; 9:14; 10:15; 11:16') 
JHU.ord$cl2_me1_6 <- recoder(JHU.ord$cl2_me1_6, '6:3') 
JHU.ord$cl2_me1_7 <- recoder(JHU.ord$cl2_me1_7, '5:3; 6:4; 7:5') 
JHU.ord$cl2_me1_9 <- recoder(JHU.ord$cl2_me1_9, '5:4; 6:5; 7:6; 8:7; 9:8; 10:9') 
JHU.ord$cl2_me1_10 <- recoder(JHU.ord$cl2_me1_10, '0.5:1; 1.5:2; 2:3; 2.5:4; 3:5; 3.5:6; 4:7; 4.5:8; 5:9; 5.5:10; 6:11; 6.5:12; 7:13; 7.5:14;
                              8:15; 8.5:16; 9:17; 10:18; 10.5:19; 11:20') 
JHU.ord$cl2_me1_11 <- recoder(JHU.ord$cl2_me1_11, '8:7') 
JHU.ord$cl2_me1_12 <- recoder(JHU.ord$cl2_me1_12, '3:2; 5:3; 7:4; 8:5; 9:6; 10:7') 
JHU.ord$cl2_me2_1 <- recoder(JHU.ord$cl2_me2_1, '1.5:1; 3:2; 4.5:3; 6:4; 7.5:5') 
JHU.ord$cl2_me2_2 <- recoder(JHU.ord$cl2_me2_2, '1.5:2; 2:3; 2.5:4; 3:5; 4:6')
JHU.ord$cl2_me2_3 <- recoder(JHU.ord$cl2_me2_3, '6.5:7; 7:8') 
JHU.ord$cl2_me2_4 <- recoder(JHU.ord$cl2_me2_4, '1.5:1; 4.5:2; 6:3; 7.5:4; 9:5; 10.5:6; 12:7; 13.5:8; 15:9; 16:10; 16.5:11') 
JHU.ord$cl2_me2_5 <- recoder(JHU.ord$cl2_me2_5, '3.5:1; 5:2; 6:3; 6.5:4; 7:5; 7.5:6; 8:7; 8.5:8; 9.5:10; 10.5:11; 11:12; 12:13') 
JHU.ord$cl2_me2_7 <- recoder(JHU.ord$cl2_me2_7, '1.5:2; 2:3; 3.5:4; 4:5; 4.5:6; 5:7; 5.5:8; 6:9; 6.5:10; 7:11; 8:12; 8.5:13; 9:14; 9.5:15; 10:16; 10.5:17; 11:18;
                             11.5:19; 12:20; 12.5:21; 13:22; 13.5:23; 14:24; 14.5:25; 15:26; 15.5:27; 16.5:28') 
JHU.ord$cl2_me2_8 <- recoder(JHU.ord$cl2_me2_8, '0.5:1; 1:2; 2:3; 2.5:4; 3:5; 4:6; 4.5:7; 5:8; 5.5:9; 6:10; 6.5:11; 7:12; 7.5:13; 8:14; 8.5:15; 9:16; 9.5:17;
                             10:18; 10.5:19; 11:20') 
JHU.ord$cl2_me2_11 <- recoder(JHU.ord$cl2_me2_11, '1.5:1; 3:2; 4.5:3; 6:4; 7.5:5')
JHU.ord$cl2_me3_1 <- recoder(JHU.ord$cl2_me3_1, '3:1; 6:2; 8:3; 9:4; 12:5; 13:6; 15:7; 17:8; 18:9; 21:10; 22:11; 24:12') 
JHU.ord$cl2_me3_2 <- recoder(JHU.ord$cl2_me3_2, '4:1; 5:2; 6:3; 7:4; 8:5') 
JHU.ord$cl2_me3_3 <- recoder(JHU.ord$cl2_me3_3, '4:1; 5:2; 6:3; 7:4; 8:5; 9:6; 10:7; 11:8; 12:9') 
JHU.ord$cl2_me3_4 <- recoder(JHU.ord$cl2_me3_4, '8:7') 
JHU.ord$cl2_me3_5 <- recoder(JHU.ord$cl2_me3_5, '2:1; 3:2; 4:3; 6:4; 8:5') 
JHU.ord$cl2_me3_6 <- recoder(JHU.ord$cl2_me3_6, '12:11; 14:12; 15:13; 16:14') 
JHU.ord$cl2_me3_7 <- recoder(JHU.ord$cl2_me3_7, '2:1; 6:2; 8:3; 9:4; 10:5; 11:6; 12:7') 
JHU.ord$cl2_me3_8 <- recoder(JHU.ord$cl2_me3_8, '6:1; 8:2; 10:3; 12:4') 
JHU.ord$cl12_me1_1 <- recoder(JHU.ord$cl12_me1_1, '2:1; 4:2; 5:3; 6:4') 
JHU.ord$cl12_me1_2 <- recoder(JHU.ord$cl12_me1_2, '6:1') 
JHU.ord$cl12_me1_3 <- recoder(JHU.ord$cl12_me1_3, '4:1; 5:2; 6:3; 7:4; 8:5')
JHU.ord$cl12_me1_5 <- recoder(JHU.ord$cl12_me1_5, '0.5:0; 2:1; 3:2; 3.5:3; 5:4; 7:5')
JHU.ord$cl12_me1_7 <- recoder(JHU.ord$cl12_me1_7, '2:1; 4:2; 6:3')
JHU.ord$cl12_me1_8 <- recoder(JHU.ord$cl12_me1_8, '2:1; 3:2; 4:3; 5:4; 6:5; 8:6') 
JHU.ord$cl12_me1_9 <- recoder(JHU.ord$cl12_me1_9, '3:1; 6:2; 8:3') 
JHU.ord$cl12_me1_10 <- recoder(JHU.ord$cl12_me1_10, '3:1; 4:2; 5:3; 6:4; 7:5; 8:6; 10:7; 11:8; 12:9; 13:10')
JHU.ord$cl12_me1_11 <- recoder(JHU.ord$cl12_me1_11, '3:2; 4:3; 5:4; 5.5:5; 6:6; 8:7; 9:8; 10:9; 10.5:10; 11:11; 12:12; 13:13; 14:14; 15:15; 15.5:16; 16:17') 
JHU.ord$cl12_me1_12 <- recoder(JHU.ord$cl12_me1_12, '13:12') 
JHU.ord$cl12_me2_3 <- recoder(JHU.ord$cl12_me2_3, '3:1; 5:2') 
JHU.ord$cl12_me2_4 <- recoder(JHU.ord$cl12_me2_4, '4.5:0; 5:1; 5.5:2; 6.5:3; 7.5:4; 8:5; 8.5:6; 9.5:7; 10.5:8') 
JHU.ord$cl12_me2_5 <- recoder(JHU.ord$cl12_me2_5, '1:0; 2:1; 4:2; 7:3') 
JHU.ord$cl12_me2_7 <- recoder(JHU.ord$cl12_me2_7, '2:1; 5:2; 6:3; 7:4; 8:5; 9:6') 
JHU.ord$cl12_me2_8 <- recoder(JHU.ord$cl12_me2_8, '1.5:1; 3:2; 4.5:3; 6:4; 7.5:5') 
JHU.ord$cl12_me2_10 <- recoder(JHU.ord$cl12_me2_10, '3:0; 9:1; 10.5:2; 12:3; 13.5:4; 15:5; 16.5:6; 18:7; 19:8; 19.5:9; 21:10') 
JHU.ord$cl12_me3_1 <- recoder(JHU.ord$cl12_me3_1, '3:0; 4:1; 5:2; 6:3; 7:4; 8:5; 9:6; 10:7') 
JHU.ord$cl12_me3_2 <- recoder(JHU.ord$cl12_me3_2, '4:0; 5:1; 7:2; 8:3; 10:4; 12:5; 13:6; 14:7; 15:8; 16:9; 17:10; 18:11;
                              19:12; 20:13')
JHU.ord$cl12_me3_3 <- recoder(JHU.ord$cl12_me3_3, '2:1; 3:2; 4:3; 5:4; 7:5; 8:6; 9:7; 10:8; 11:9; 12:10; 13:11; 15:12') 
JHU.ord$cl12_me3_4 <- recoder(JHU.ord$cl12_me3_4, '5:0; 6:1; 7:2; 8:3; 9:4; 10:5; 11:6; 12:7; 13:8; 14:9; 15:10') 
JHU.ord$cl12_me3_5 <- recoder(JHU.ord$cl12_me3_5, '2.5:3; 3:4; 4:5; 5:6') 
JHU.ord$cl12_me3_6 <- recoder(JHU.ord$cl12_me3_6, '1:0; 2:1; 3:2; 4:3; 5:4') 
JHU.ord$cl12_me3_9 <- recoder(JHU.ord$cl12_me3_9, '2:1; 3:2; 4:3; 5:4; 6:5; 7:6; 8:7; 9:8; 10:9') 
JHU.ord$cl2_me3_3brev <- recoder(JHU.ord$cl2_me3_3brev, '2:1; 3:2; 6:3') 
JHU.ord$cl2_me3_7arev <- recoder(JHU.ord$cl2_me3_7arev, '3:1; 6:2') 
JHU.ord$cl2_me3_7brev <- recoder(JHU.ord$cl2_me3_7brev, '6:5') 
# check
JHU.ord_scale <- apply(JHU.ord, 2,function(x) table(x, useNA='always'))
JHU.ord_scale
# change item names
colnames(JHU.ord) <- paste(colnames(JHU.ord), "JHU_ord", sep = "_")
colnames(JHU.ord)

# getting current scale
Metro.ord_scale <- apply(Metro.ord, 2,function(x) table(x, useNA='always'))
Metro.ord_scale
# recoding according to table above
Metro.ord$cl13_sa1 <- recoder(Metro.ord$cl13_sa1, '3:0; 5:1; 6:2; 9:3; 11:4; 12:5; 13:6; 14:7; 15:8') #***
Metro.ord$cl13_sa2 <- recoder(Metro.ord$cl13_sa2, '3:0; 5:1; 6:2; 6.5:3; 9:4; 10:5; 12:6; 13:7; 13.5:8; 14:9; 15:10') #***
Metro.ord$cl13_sa3 <- recoder(Metro.ord$cl13_sa3, '9:0; 10:1; 12:2; 15:3') #***
# check
Metro.ord_scale <- apply(Metro.ord, 2,function(x) table(x, useNA='always'))
Metro.ord_scale
# change item names
colnames(Metro.ord) <- paste(colnames(Metro.ord), "Metro_ord", sep = "_")
colnames(Metro.ord)

# getting current scale
STMU.ord_scale <- apply(STMU.ord, 2,function(x) table(x, useNA='always'))
STMU.ord_scale
# recoding according to table above
STMU.ord$cl4_fcsa1 <- recoder(STMU.ord$cl4_fcsa1, '1.5:2; 2:3; 2.5:4; 3:5') 
STMU.ord$cl4_fcsa3a <- recoder(STMU.ord$cl4_fcsa3a, '2:1')
STMU.ord$cl4_fcsa4 <- recoder(STMU.ord$cl4_fcsa4, '1.5:2; 2:3; 3:4') 
STMU.ord$cl4_fcsa5 <- recoder(STMU.ord$cl4_fcsa5, '1.5:2; 2:3; 3:4; 4:5') 
STMU.ord$cl4_fcsa6a <- recoder(STMU.ord$cl4_fcsa6a, '2:1') 
STMU.ord$cl4_fcsa7 <- recoder(STMU.ord$cl4_fcsa7, '1.5:2') 
STMU.ord$cl4_fcsa8b <- recoder(STMU.ord$cl4_fcsa8b, '2:1')
STMU.ord$cl4_fcsa9 <- recoder(STMU.ord$cl4_fcsa9, '5.5:6; 6:7') 
STMU.ord$cl4_fcsa10b <- recoder(STMU.ord$cl4_fcsa10b, '2:1; 3:2; 4:3')
STMU.ord$cl4_fcp1a <- recoder(STMU.ord$cl4_fcp1a, '3:2')
STMU.ord$cl4_fcp1b <- recoder(STMU.ord$cl4_fcp1b, '2:1')
STMU.ord$cl4_fcp4b <- recoder(STMU.ord$cl4_fcp4b, '2:1')
STMU.ord$cl4_fcp5a <- recoder(STMU.ord$cl4_fcp5a, '2.5:3; 3:4') 
STMU.ord$cl4_fcp5b <- recoder(STMU.ord$cl4_fcp5b, '1.5:2; 2:3') 
STMU.ord$cl4_fcp5d <- recoder(STMU.ord$cl4_fcp5d, '2:1; 3:2; 4:3')
STMU.ord$cl4_fcp5e <- recoder(STMU.ord$cl4_fcp5e, '2:1')
STMU.ord$cl9_fcsa3 <- recoder(STMU.ord$cl9_fcsa3, '2:1; 4:2')
STMU.ord$cl9_fcsa4 <- recoder(STMU.ord$cl9_fcsa4, '3:2; 4:3')
STMU.ord$cl9_fcsa5 <- recoder(STMU.ord$cl9_fcsa5, '1.5:2; 2:3; 3:4')
STMU.ord$cl9_fcsa7 <- recoder(STMU.ord$cl9_fcsa7, '2.5:2; 5:3')
STMU.ord$cl9_fcp1 <- recoder(STMU.ord$cl9_fcp1, '0.5:1; 1.5:2; 4:3; 6:4; 8:5; 8.5:6; 9:7')
STMU.ord$cl9_fcp3 <- recoder(STMU.ord$cl9_fcp3, '4:1; 6:2; 8:3')
STMU.ord$cl9_fcp4 <- recoder(STMU.ord$cl9_fcp4, '3:2; 3.5:3')
STMU.ord$cl9_fcp5 <- recoder(STMU.ord$cl9_fcp5, '2:1; 4:2')
STMU.ord$cl9_fcp6 <- recoder(STMU.ord$cl9_fcp6, '2:1; 3:2; 5:3; 6:4; 7:5; 8:6; 9:7')
STMU.ord$cl9_fcp7 <- recoder(STMU.ord$cl9_fcp7, '1.5:1; 3.5:2; 4:3')
STMU.ord$cl14_fcp1 <- recoder(STMU.ord$cl14_fcp1, '2.5:3; 3:4')
STMU.ord$cl14_fcp2b <- recoder(STMU.ord$cl14_fcp2b, '0.5:1; 1:2')
STMU.ord$cl14_fcp2c <- recoder(STMU.ord$cl14_fcp2c, '0.5:1; 1:2')
STMU.ord$cl14_fcp2d <- recoder(STMU.ord$cl14_fcp2d, '0.5:1; 1:2')
STMU.ord$cl14_fcp2e <- recoder(STMU.ord$cl14_fcp2e, '3:2; 4:3')
STMU.ord$cl14_fcp2f <- recoder(STMU.ord$cl14_fcp2f, '4:3')
STMU.ord$cl14_fcp4b <- recoder(STMU.ord$cl14_fcp4b, '2.5:3; 3:4; 4:5; 4.5:6; 5:7')
STMU.ord$cl14_fcp5 <- recoder(STMU.ord$cl14_fcp5, '1.5:0; 2.5:1; 3:2; 3.5:3')
STMU.ord$cl14_fcp6a <- recoder(STMU.ord$cl14_fcp6a, '1:0; 2:1')
STMU.ord$cl14_fcp6b <- recoder(STMU.ord$cl14_fcp6b, '2:1; 4:2')
STMU.ord$cl14_fcp6c <- recoder(STMU.ord$cl14_fcp6c, '1:0; 2:1')
STMU.ord$cl14_fcp7b <- recoder(STMU.ord$cl14_fcp7b, '5:3')
STMU.ord$cl19_fcsa6b <- recoder(STMU.ord$cl19_fcsa6b, '2:1; 4:2')
# check
STMU.ord_scale <- apply(STMU.ord, 2,function(x) table(x, useNA='always'))
STMU.ord_scale
# change item names
colnames(STMU.ord) <- paste(colnames(STMU.ord), "STMU_ord", sep = "_")
colnames(STMU.ord)

# getting current scale
UCD.ord_scale <- apply(UCD.ord, 2,function(x) table(x, useNA='always'))
UCD.ord_scale
# recoding according to table above
UCD.ord$cl5_me1_3a <- recoder(UCD.ord$cl5_me1_3a, '2:1; 4:2')
UCD.ord$cl5_me1_3b <- recoder(UCD.ord$cl5_me1_3b, '2:1; 4:2')
UCD.ord$cl5_me1_9a <- recoder(UCD.ord$cl5_me1_9a, '2:1; 3:2; 4:3; 4.5:4; 6.5:7') 
UCD.ord$cl5_me1_9b <- recoder(UCD.ord$cl5_me1_9b, '2:1')
UCD.ord$cl5_me3_4a <- recoder(UCD.ord$cl5_me3_4a, '5:1')
UCD.ord$cl5_me3_4b <- recoder(UCD.ord$cl5_me3_4b, '4:2; 5:3')
UCD.ord$cl5_me4_1 <- recoder(UCD.ord$cl5_me4_1, '5:1; 6:2; 7:3; 8:4; 9:5; 10:6') 
UCD.ord$cl5_me4_3 <- recoder(UCD.ord$cl5_me4_3, '2:1; 3:2; 4:3; 5:4; 6:5; 7:6; 8:7; 9:8; 10:9')
UCD.ord$cl5_me4_4 <- recoder(UCD.ord$cl5_me4_4, '2.5:1; 5:2; 6.5:3; 7:4; 7.5:5; 9:6; 10:7')
UCD.ord$cl5_me4_5 <- recoder(UCD.ord$cl5_me4_5, '1.5:2; 2:3; 2.5:4; 3:5; 3.5:6; 4:7; 4.5:8; 5:9; 5.5:10; 6:11;
                             6.5:12; 7:13; 7.5:14; 8:15; 8.5:16; 9:17; 10:18')
UCD.ord$cl6_me4_1 <- recoder(UCD.ord$cl6_me4_1, '2:1; 3:2; 4:3; 5:4; 6:5; 6.5:6')
UCD.ord$cl6_me4_2 <- recoder(UCD.ord$cl6_me4_2, '3:1; 4:2; 6:3; 7:4; 7.5:5; 8:6; 8.5:7; 9:8; 9.5:9')
UCD.ord$cl6_me4_3 <- recoder(UCD.ord$cl6_me4_3, '2:1; 4:2; 5:3; 6:4; 7:5; 8:6; 9:7; 10:8')
UCD.ord$cl6_me4_5 <- recoder(UCD.ord$cl6_me4_5, '2:1; 3:2; 4:3; 5:4; 6:5; 7:6; 8:7; 9:8; 10:9')
# check
UCD.ord_scale <- apply(UCD.ord, 2,function(x) table(x, useNA='always'))
UCD.ord_scale
# change item names
colnames(UCD.ord) <- paste(colnames(UCD.ord), "UCD_ord", sep = "_")
colnames(UCD.ord)

# getting current scale
UGA.ord_scale <- apply(UGA.ord, 2,function(x) table(x, useNA='always'))
UGA.ord_scale
# recoding according to table above
UGA.ord$cl10_me1_44 <- recoder(UGA.ord$cl10_me1_44, '1.2:1; 2.4:2; 2.5:3; 3:4; 3.6:5; 4:6; 4.8:7; 5:8; 5.2:9; 5.5:10; 6:11; 6.4:12; 7:13; 7.5:14; 7.6:15;
                               8:16; 8.5:17; 8.8:18; 9:19; 9.2:20; 9.5:21; 10:22; 10.4:23; 10.5:24; 11:25; 11.5:26; 11.6:27; 12:28; 12.5:29;
                               12.8:30; 13:31; 14:32') 
UGA.ord$cl10_me2_39 <- recoder(UGA.ord$cl10_me2_39, '5:4; 6:5; 7:6; 9:7; 10:8; 11:9; 12:10; 13:11; 14:12; 15:13; 16:14; 17:15; 17.5:16; 18:17;
                               19:18; 20:19; 20.5:20; 21.5:22; 22:23') 
UGA.ord$cl10_me2_40 <- recoder(UGA.ord$cl10_me2_40, '7.5:8; 8:9') 
UGA.ord$cl10_me3_sa7 <- recoder(UGA.ord$cl10_me3_sa7, '0.5:1; 1:2; 2:3; 3:4; 4:5; 5:6; 6:7; 7:8; 8:9; 9:10; 10:11; 11:12; 12:13; 13:14') 
UGA.ord$cl10_me4_sa1 <- recoder(UGA.ord$cl10_me4_sa1, '1.5:2; 2:3; 3:4; 3.5:5; 4:6; 4.5:7; 5:8; 5.5:9; 6:10; 6.5:11; 7:12; 7.5:13; 8:14; 8.5:15; 9:16; 9.5:17; 10:18;
                                10.5:19; 11:20; 11.5:21; 12:22; 12.5:23; 13:24; 13.5:25; 14:26; 14.5:27; 15:28') 
UGA.ord$cl10_me5_sa3 <- recoder(UGA.ord$cl10_me5_sa3, '2:1; 4:2; 5:3; 6:4; 7:5; 8:6; 9:7; 10:8') 
UGA.ord$cl10_me6_49 <- recoder(UGA.ord$cl10_me6_49, '2:1; 3:2; 4:3; 5:4; 6:5; 7:6; 8:7; 9:8; 10:9; 11:10; 12:11; 13:12; 14:13; 15:14; 16:15') 
# check
UGA.ord_scale <- apply(UGA.ord, 2,function(x) table(x, useNA='always'))
UGA.ord_scale
# change item names
colnames(UGA.ord) <- paste(colnames(UGA.ord), "UGA_ord", sep = "_")
colnames(UGA.ord)

# adding recoded df to end of original df
g_ord <- cbind(g, CU.JK.ord, CU.KK.ord, JHU.ord, Metro.ord, STMU.ord, UCD.ord, UGA.ord)
colnames(g_ord)
#------------------------------------------------------------------------------------------------------------
#                          Getting Descriptives of items
# -----------------------------------------------------------------------------------------------------------
# creating item sets for scoring by matrix, class, or Institution as needed for year 1, and year 2 pre/post

#  UNIVERSITY OF COLORADO 


# MCDB2150_JK

CU.JK.y1 <- c("cl7_fc26", "cl7_fc27", "cl7_fc28", "cl7_fc29", "cl7_fc30", "cl7_fc31", "cl7_fc32",
              "cl7_fc33", "cl7_fc34", "cl7_fc35", "cl7_fc36", "cl7_fc37", "cl7_fc38", "cl7_fc39",              
              "cl7_fc40", "cl7_fcsa1a", "cl7_fcsa1b", "cl7_fcsa1c", "cl7_fcsa1d", "cl7_fcsa2a",
              "cl7_fcsa2b", "cl7_fcsa2c", "cl7_fcsa3a", "cl7_fcsa3b", "cl7_fcsa3c", "cl7_fcsa3d",            
              "cl7_fcsa4a", "cl7_fcsa4b", "cl7_fcsa5a", "cl7_fcsa5b", "cl7_fcsa5c", "cl7_fcsa6a",
              "cl7_fcsa6b", "cl7_fcsa6c", "cl7_fcsa6d", "cl7_fcsa6e")

CU.JK.pre.m5 <- c("cl7_fc28", "cl7_fc33", "cl7_fc34", "cl7_fc39", "cl7_fcsa2b", "cl7_fcsa2c", "cl7_fcsa3a",
                  "cl7_fcsa3b", "cl7_fcsa3c", "cl7_fcsa3d")

CU.JK.pre.m6 <- c("cl7_fc30", "cl7_fc33", "cl7_fc34", "cl7_fc36", "cl7_fcsa5a", "cl7_fcsa5b", "cl7_fcsa5c", 
                  "cl17_fcsa2a", 	"cl17_fcsa2b",	"cl17_fcsa2c",	"cl17_fcsa2d")

CU.JK.post <- c("cl7_fc26", "cl7_fc28", "cl7_fc30", "cl7_fc33", "cl7_fc34", "cl7_fc38", "cl7_fc39", 
                "cl7_fcsa2a", "cl7_fcsa2b","cl7_fcsa2c", "cl7_fcsa3a", "cl7_fcsa3b", "cl7_fcsa3c","cl7_fcsa3d",
                "cl7_fcsa5a", "cl7_fcsa5b", "cl7_fcsa5c", "cl17_fcsa2a", 	"cl17_fcsa2b",	"cl17_fcsa2c",
                "cl17_fcsa2d",	"cl11_fcmc26", "cl11_fcmc27", "cl17_fcmc3",	"cl17_fcmc4",	"cl17_fcsa1a",	
                "cl17_fcsa1b",	"cl17_fcsa1c",	"cl17_fcsa1d", "cl17_fcsa1e",	"cl17_fcsa1f",	"cl17_fcsa3a",	
                "cl17_fcsa3b",	"cl17_fcsa3c",	"cl17_fcsa5")


# MCDB2150_KK

CU.KK.y1 <- c("cl1_fc26" , "cl1_fc27","cl1_fc28",              
              "cl1_fc29" ,"cl1_fc30", "cl1_fc31", "cl1_fc32" ,             
              "cl1_fc33", "cl1_fc34" ,  "cl1_fc35"  , "cl1_fc36",         
              "cl1_fc37" ,"cl1_fc38" , "cl1_fc39" ,  "cl1_fc40" ,             
              "cl1_fc41" , "cl1_fc42"  , "cl1_fc43"  , "cl1_fc44"  ,            
              "cl1_fc45")

CU.KK.pre.m1 <- c("cl1_fc26",  "cl1_fc37", "cl1_fc39", "cl1_fc44")    

CU.KK.pre.m2 <- c("cl1_fc28",  "cl1_fc29", "cl1_fc38", "cl1_fc42", "cl1_fc44")

CU.KK.post <- c("cl1_fc26" , "cl1_fc28",  "cl1_fc29", "cl1_fc37" ,"cl1_fc38" , "cl1_fc39",  "cl1_fc42", 
                "cl1_fc44", "cl7_fc38", "cl11_fcmc26" , "cl11_fcmc27", "cl11_fcmc28" ,
                "cl11_fcmc29", "cl11_fcmc31" , "cl11_fcmc38", "cl11_fcsa3a" , "cl11_fcsa3b" ,          
                "cl11_fcsa3c", "cl11_fcsa4_1"  , "cl11_fcsa4_2"  ,"cl11_fcsa4_3" ,         
                "cl11_fcsa4_4", "cl11_fcsa5a"   , "cl11_fcsa5b" , "cl11_fcsa5c",           
                "cl11_fcsa6a" , "cl11_fcsa6b"   ,  "cl11_fcsa7a"  , "cl11_fcsa7b",           
                "cl11_fcsa7c"  ,"cl11_fcsa7d"  ,"cl11_fcsa7e"  , "cl11_fcsa7f",           
                "cl11_fcsa8a"  ,"cl11_fcsa8b","cl11_fcsa8c" ,"cl11_fcsa8d",           
                "cl11_fcsa8e","cl11_fcsa8f")


## JHU: Genetics020.330

JHU.y1 <-c("cl2_me1_1",           "cl2_me1_2"  ,         "cl2_me1_3"  ,            
           "cl2_me1_4"    ,             "cl2_me1_5"  ,         "cl2_me1_6"  ,           "cl2_me1_7"  ,            
           "cl2_me1_8"    ,             "cl2_me1_9"  ,         "cl2_me1_10" ,           "cl2_me1_11" ,           
           "cl2_me1_12"   ,             "cl2_me2_1"  ,         "cl2_me2_2"  ,           "cl2_me2_3"  ,            
           "cl2_me2_4"    ,             "cl2_me2_5"  ,         "cl2_me2_6"  ,           "cl2_me2_7"  ,           
           "cl2_me2_8"    ,             "cl2_me2_9"  ,         "cl2_me2_10" ,           "cl2_me2_11" ,          
           "cl2_me3_1"    ,             "cl2_me3_2"  ,         "cl2_me3_3"  ,           "cl2_me3_4"  ,          
           "cl2_me3_5"    ,             "cl2_me3_6"  ,         "cl2_me3_7"  ,           "cl2_me3_8")

JHU.pre <-c("cl2_me3_3arev",	"cl2_me3_3brev",	"cl2_me3_7arev",	"cl2_me3_7brev")

JHU.post <-c("cl2_me3_3arev",	"cl2_me3_3brev",	"cl2_me3_7arev",	"cl2_me3_7brev", 
             "cl12_me1_1"   ,             "cl12_me1_2" ,         "cl12_me1_3" ,           "cl12_me1_4" ,          
             "cl12_me1_5"   ,             "cl12_me1_6" ,          
             "cl12_me1_7"   ,             "cl12_me1_8" ,         "cl12_me1_9"  ,          "cl12_me1_10" ,          
             "cl12_me1_11"  ,             "cl12_me1_12",         "cl12_me2_1"  ,          "cl12_me2_2"  ,         
             "cl12_me2_3"   ,             "cl12_me2_4" ,         "cl12_me2_5"  ,          "cl12_me2_6"  ,        
             "cl12_me2_7"   ,             "cl12_me2_8" ,         "cl12_me2_9"  ,          "cl12_me2_10" ,         
             "cl12_me2_11"  ,             "cl12_me3_1" ,         "cl12_me3_2"  ,          "cl12_me3_3"  ,         
             "cl12_me3_4"   ,             "cl12_me3_5" ,         "cl12_me3_6"  ,          "cl12_me3_9")


## METRO: BIO3600

Metro.y1.cl3 <- c("cl3_fc10"  ,     "cl3_fc11"   ,            "cl3_fc12"    ,           "cl3_fc13" ,             
                  "cl3_fc14"          ,     "cl3_fc15"   ,            "cl3_fc16"    ,           "cl3_fc17" ,             
                  "cl3_fc18"         ,      "cl3_fc19"   ,            "cl3_fc20"    ,           "cl3_fc21" ,             
                  "cl3_fc22"         ,      "cl3_fc23"   ,            "cl3_fc24"   ,            "cl3_fc25" ,             
                  "cl3_fc26"         ,      "cl3_fc27"   ,            "cl3_fc28"    ,           "cl3_fc29" ,             
                  "cl3_fc30"         ,      "cl3_fc31"   ,            "cl3_fc32")

Metro.y1.cl8 <- c("cl3_fc10"  ,             "cl3_fc11"   ,           "cl3_fc14"          ,     "cl3_fc15"   ,            
                  "cl3_fc19"   ,            "cl3_fc20"    ,          "cl3_fc23"   ,            "cl3_fc24"   ,       
                  "cl3_fc29" ,             "cl3_fc30"     ,            "cl3_fc32",                 "cl8_fc3", 
                  "cl8_fc4"    ,           
                  "cl8_fc5"          ,      "cl8_fc10"   ,            "cl8_fc11"   ,            "cl8_fc14" ,             
                  "cl8_fc17"         ,      "cl8_fc20"   ,            "cl8_fc22")

Metro.pre.cl13 <- c("cl3_fc13" ,         "cl3_fc14"          ,     "cl3_fc15"   ,            "cl3_fc16"    ,
                    "cl3_fc19"   ,            "cl3_fc20"    ,         "cl3_fc23"   ,         "cl3_fc24")

Metro.pre.cl18 <- c("cl3_fc14"          ,     "cl3_fc15"   ,   "cl3_fc19"   ,            "cl3_fc20"    ,
                    "cl8_fc3",              "cl8_fc10"   ,     "cl8_fc11")

Metro.post.cl13 <- c("cl3_fc13" ,         "cl3_fc14"          ,     "cl3_fc15"   ,            "cl3_fc16"    ,
                     "cl3_fc19"   ,            "cl3_fc20"    ,         "cl3_fc23"   ,         "cl3_fc24"    ,
                     "cl13_fc2"         ,      "cl13_fc6"   ,            "cl13_fc7"   ,       "cl13_fc8" ,             
                     "cl13_fc11"        ,      "cl13_fc12"  ,            "cl13_fc13"  ,       "cl13_fc14",             
                     "cl13_sa1"         ,      "cl13_sa2"   ,            "cl13_sa3")

Metro.post.cl18 <- c("cl3_fc14"          ,     "cl3_fc15"   ,           "cl3_fc19"   ,            "cl3_fc20"    ,
                     "cl3_fc23"   ,         "cl3_fc24"    ,             "cl8_fc3",                 "cl8_fc5"    ,
                     "cl8_fc10"   ,            "cl8_fc11"   ,           "cl13_fc2",                "cl13_sa1"   ,      
                     "cl13_sa2"   ,            "cl13_sa3"   ,           "cl18_fc1" ,
                     "cl18_fc2"         ,      "cl18_fc3"   ,            "cl18_fc9"   ,            "cl18_fc16" ,            
                     "cl18_fc17")


## ST MARYS: BL2330

STMU.y1.cl4 <- c("cl4_fcsa1"   ,          "cl4_fcsa2a"    ,         "cl4_fcsa2b"     ,        "cl4_fcsa3a"   ,          
                 "cl4_fcsa3b"  ,           "cl4_fcsa4"   ,           "cl4_fcsa5"     ,         "cl4_fcsa6a"  ,"cl4_fcsa6b",            
                 "cl4_fcsa7"   ,           "cl4_fcsa8a"   ,          "cl4_fcsa8b"    ,         "cl4_fcsa9"   ,          
                 "cl4_fcsa10a" ,           "cl4_fcsa10b" ,         "cl4_fcsa10c"     ,       "cl4_fcp1a"     ,        
                 "cl4_fcp1b"   ,           "cl4_fcp1c"   ,           "cl4_fcp1d"     ,         "cl4_fcp2"    ,          
                 "cl4_fcp3"    ,           "cl4_fcp4a"   ,           "cl4_fcp4b"     ,         "cl4_fcp5a"   ,          
                 "cl4_fcp5b"   ,           "cl4_fcp5c"   ,           "cl4_fcp5d"     ,         "cl4_fcp5e")

STMU.y1.cl9 <- c("cl4_fcsa1"   ,           "cl4_fcsa9"   ,           "cl9_fcsa2",             
                 "cl9_fcsa3"   ,           "cl9_fcsa4"   ,           "cl9_fcsa5"     ,         "cl9_fcsa7" ,            
                 "cl9_fcp1"    ,           "cl9_fcp3"    ,           "cl9_fcp4"      ,         "cl9_fcp5"  ,            
                 "cl9_fcp6"    ,           "cl9_fcp7")

STMU.pre.cl14 <- c("cl4_fcsa1"   ,          "cl4_fcsa3a"   ,          
                  "cl4_fcsa3b"  ,           "cl4_fcsa4"   ,           "cl4_fcsa5",                  "cl4_fcsa9" , 
                  "cl4_fcp2")               

STMU.pre.cl19 <- c("cl4_fcsa1",              "cl4_fcsa9",             "cl4_fcp2",                  "cl9_fcsa2",             
                   "cl9_fcsa3"   ,           "cl9_fcsa5")

STMU.post.cl14 <- c("cl4_fcsa1"   ,       "cl4_fcsa3a"   ,          
                    "cl4_fcsa3b"  ,           "cl4_fcsa4"   ,           "cl4_fcsa5"     ,       "cl4_fcsa9" ,
                    "cl4_fcp2"    ,         "cl14_fcmc1"     ,       
                    "cl14_fcmc2"  ,           "cl14_fcmc3"  ,           "cl14_fcmc4"    ,         "cl14_fcmc5" ,           
                    "cl14_fcmc6"  ,           "cl14_fcmc7"  ,           "cl14_fcmc8"    ,         "cl14_fcmc9" ,            
                    "cl14_fcmc10" ,           "cl14_fcmc11" ,           "cl14_fcmc12"   ,         "cl14_fcmc13" ,          
                    "cl14_fcmc14" ,           "cl14_fcmc15" ,           "cl14_fcmc16"   ,         "cl14_fcmc17" ,          
                    "cl14_fcmc18" ,           "cl14_fcmc19" ,           "cl14_fcmc20"   ,         "cl14_fcmc21" ,          
                    "cl14_fcmc22" ,           "cl14_fcmc23" ,           "cl14_fcmc24"   ,         "cl14_fcmc25" ,          
                    "cl14_fcp1"   ,           "cl14_fcp2a"  ,           "cl14_fcp2b"    ,         "cl14_fcp2c"  ,          
                    "cl14_fcp2d"  ,           "cl14_fcp2e"  ,           "cl14_fcp2f"    ,         "cl14_fcp3"   ,          
                    "cl14_fcp4a"  ,           "cl14_fcp4b"  ,           "cl14_fcp5"     ,         "cl14_fcp6a"  ,          
                    "cl14_fcp6b"  ,           "cl14_fcp6c"  ,           "cl14_fcp7a"    ,         "cl14_fcp7b")

STMU.post.cl19 <- c("cl4_fcsa1"   ,          "cl4_fcsa9" ,              "cl4_fcp2"      ,         "cl9_fcsa2",             
                    "cl9_fcsa3"   ,          "cl9_fcsa5"     ,           "cl14_fcmc1"     ,       
                    "cl14_fcmc2"  ,           "cl14_fcmc3"  ,           "cl14_fcmc4"    ,         "cl14_fcmc5" ,           
                    "cl14_fcmc6"  ,           "cl14_fcmc7"  ,           "cl14_fcmc8"    ,         "cl14_fcmc9" ,            
                    "cl14_fcmc10" ,           "cl14_fcmc11" ,           "cl14_fcmc12"   ,         "cl14_fcmc13" ,          
                    "cl14_fcmc14" ,           "cl14_fcmc15" ,           "cl14_fcmc16"   ,         "cl14_fcmc17" ,          
                    "cl14_fcmc18" ,           "cl14_fcmc19" ,           "cl14_fcmc20"   ,         "cl14_fcmc21" ,          
                    "cl14_fcmc22" ,           "cl14_fcmc23" ,           "cl14_fcmc24"   ,         "cl14_fcmc25" ,          
                    "cl14_fcp1"   ,           "cl14_fcp2a"  ,           "cl14_fcp2b"    ,         "cl14_fcp2c"  ,          
                    "cl14_fcp2d"  ,           "cl14_fcp2e"  ,           "cl14_fcp2f"    ,         "cl14_fcp3"   ,          
                    "cl14_fcp4a"  ,           "cl14_fcp4b"  ,           "cl14_fcp5"     ,         
                    "cl19_fcsa6a" ,           "cl19_fcsa6b" ,           "cl19_fcsa6c")


## UCD: BIOL3832

UCD.y1.cl5 <- c("cl5_me4_1",             "cl5_me4_2",        	     "cl5_me4_3",   	         "cl5_me4_4",
                "cl5_me4_5")

UCD.y1.cl6 <- c("cl6_me4_1",	             "cl6_me4_2",      	       "cl6_me4_3",
                "cl6_me4_4",               "cl6_me4_5")

UCD.pre <- c("cl5_me4_2",                  "cl5_me1_3a",             "cl5_me1_3b" ,            "cl5_me1_3c")

UCD.post <- c("cl5_me4_2",              "cl5_me4_5",                 "cl5_me1_3a",             "cl5_me1_3b" ,            
              "cl5_me1_3c" ,            "cl5_me1_9a" ,            "cl5_me1_9b"  ,           "cl5_me3_4a" ,            
              "cl5_me3_4b")


## UGA: GENE3200

UGA.y1 <- c("cl10_me1_44"  ,     "cl10_me2_39"  ,          "cl10_me2_40"  ,         
            "cl10_me3_tf2"   ,        "cl10_me3_mc12" ,         "cl10_me3_sa7" ,          "cl10_me4_tf1" ,         
            "cl10_me4_mc17"  ,        "cl10_me4_sa1" ,          "cl10_me4_sa4" ,          "cl10_me4_sa6" ,         
            "cl10_me5_tf20"  ,        "cl10_me5_mc29" ,         "cl10_me5_sa3" ,          "cl10_me6_20"  ,         
            "cl10_me6_25"    ,        "cl10_me6_32"   ,         "cl10_me6_41"  ,          "cl10_me6_46"  ,         
            "cl10_me6_49") 

UGA.pre.post <- c("cl10_me1_2"  ,      "cl10_me1_44"  ,           "cl10_me2_40"  ,             "cl10_me4_mc17"  ,
                  "cl10_me6_41") 



Local_item_groups <- list(CU.JK.y1, CU.JK.pre.m5, CU.JK.pre.m6, CU.JK.post, CU.KK.y1, CU.KK.pre.m1, CU.KK.pre.m2, CU.KK.post, 
                          JHU.y1, JHU.pre, JHU.post, Metro.y1.cl3, Metro.y1.cl8, Metro.pre.cl13, Metro.pre.cl18, Metro.post.cl13,
                          Metro.post.cl18, STMU.y1.cl4, STMU.y1.cl9, STMU.pre.cl14, STMU.pre.cl19, STMU.post.cl14, STMU.post.cl19,
                          UCD.y1.cl5, UCD.y1.cl6, UCD.pre, UCD.post, UGA.y1, UGA.pre.post)

# recoded items have "x_ord", so pasting that to items listed above so recoded items get scored
CU.JK.y1 <- paste(CU.JK.y1, "CUJK_ord", sep = "_")
CU.JK.pre.m5 <- paste(CU.JK.pre.m5, "CUJK_ord", sep = "_")
CU.JK.pre.m6 <- paste(CU.JK.pre.m6, "CUJK_ord", sep = "_")
CU.JK.post <- paste(CU.JK.post, "CUJK_ord", sep = "_")
CU.KK.y1 <- paste(CU.KK.y1, "CUKK_ord", sep = "_")
CU.KK.pre.m1 <- paste(CU.KK.pre.m1, "CUKK_ord", sep = "_")
CU.KK.pre.m2 <- paste(CU.KK.pre.m2, "CUKK_ord", sep = "_")
CU.KK.post <- paste(CU.KK.post, "CUKK_ord", sep = "_")
JHU.y1 <- paste(JHU.y1, "JHU_ord", sep = "_")
JHU.pre <- paste(JHU.pre, "JHU_ord", sep = "_")
JHU.post <- paste(JHU.post, "JHU_ord", sep = "_")
Metro.y1.cl3 <- paste(Metro.y1.cl3, "Metro_ord", sep = "_")
Metro.y1.cl8 <- paste(Metro.y1.cl8, "Metro_ord", sep = "_")
Metro.pre.cl13 <- paste(Metro.pre.cl13, "Metro_ord", sep = "_")
Metro.pre.cl18 <- paste(Metro.pre.cl18, "Metro_ord", sep = "_")
Metro.post.cl13 <- paste(Metro.post.cl13, "Metro_ord", sep = "_")
Metro.post.cl18 <- paste(Metro.post.cl18, "Metro_ord", sep = "_")
STMU.y1.cl4 <- paste(STMU.y1.cl4, "STMU_ord", sep = "_")
STMU.y1.cl9 <- paste(STMU.y1.cl9, "STMU_ord", sep = "_")
STMU.pre.cl14 <- paste(STMU.pre.cl14, "STMU_ord", sep = "_")
STMU.pre.cl19 <- paste(STMU.pre.cl19, "STMU_ord", sep = "_")
STMU.post.cl14 <- paste(STMU.post.cl14, "STMU_ord", sep = "_")
STMU.post.cl19 <- paste(STMU.post.cl19, "STMU_ord", sep = "_")
UCD.y1.cl5 <- paste(UCD.y1.cl5, "UCD_ord", sep = "_")
UCD.y1.cl6 <- paste(UCD.y1.cl6, "UCD_ord", sep = "_")
UCD.pre <- paste(UCD.pre, "UCD_ord", sep = "_")
UCD.post <- paste(UCD.post, "UCD_ord", sep = "_")
UGA.y1 <- paste(UGA.y1, "UGA_ord", sep = "_")
UGA.pre.post <- paste(UGA.pre.post, "UGA_ord", sep = "_")

# ------------------------------------------starting with year 1------------------------------------------------------------------------ 
# replacing NAs with 0s
g_ord[g_ord$cl==7 & g_ord$post==1,CU.JK.y1][is.na(g_ord[g_ord$cl==7 & g_ord$post==1,CU.JK.y1])]<-0 
g_ord[g_ord$cl==1 & g_ord$post==1,CU.KK.y1][is.na(g_ord[g_ord$cl==1  & g_ord$post==1,CU.KK.y1])]<-0 
g_ord[g_ord$cl==2 & g_ord$post==1,JHU.y1][is.na(g_ord[g_ord$cl==2 & g_ord$post==1,JHU.y1])]<-0 
g_ord[g_ord$cl==3 & g_ord$post==1,Metro.y1.cl3][is.na(g_ord[g_ord$cl==3 & g_ord$post==1,Metro.y1.cl3])]<-0 
g_ord[g_ord$cl==8 & g_ord$post==1,Metro.y1.cl8][is.na(g_ord[g_ord$cl==8 & g_ord$post==1,Metro.y1.cl8])]<-0 
g_ord[g_ord$cl==4 & g_ord$post==1,STMU.y1.cl4][is.na(g_ord[g_ord$cl==4 & g_ord$post==1,STMU.y1.cl4])]<-0 
g_ord[g_ord$cl==9 & g_ord$post==1,STMU.y1.cl9][is.na(g_ord[g_ord$cl==9 & g_ord$post==1,STMU.y1.cl9])]<-0 
g_ord[g_ord$cl==5 & g_ord$post==1,UCD.y1.cl5][is.na(g_ord[g_ord$cl==5 & g_ord$post==1,UCD.y1.cl5])]<-0 
g_ord[g_ord$cl==6 & g_ord$post==1,UCD.y1.cl6][is.na(g_ord[g_ord$cl==6 & g_ord$post==1,UCD.y1.cl6])]<-0 
g_ord[g_ord$cl==10 & g_ord$post==1,UGA.y1][is.na(g_ord[g_ord$cl==10 & g_ord$post==1,UGA.y1])]<-0 

# making dummies to mark when a row has all 0s 
g_ord$CU.JK.y1.allzero <- rep(NA,3468)
g_ord$CU.KK.y1.allzero <- rep(NA,3468)
g_ord$JHU.y1.allzero <- rep(NA,3468)
g_ord$Metro.y1.cl3.allzero <- rep(NA,3468)
g_ord$Metro.y1.cl8.allzero <- rep(NA,3468)
g_ord$STMU.y1.cl4.allzero <- rep(NA,3468)
g_ord$STMU.y1.cl9.allzero <- rep(NA,3468)
g_ord$UCD.y1.cl5.allzero <- rep(NA,3468)
g_ord$UCD.y1.cl6.allzero <- rep(NA,3468)
g_ord$UGA.y1.allzero <- rep(NA,3468)
# adding conditions to dummies to indicate 1 for all zeroes and 0 otherwise
g_ord$CU.JK.y1.allzero <- ifelse(apply(g_ord[,CU.JK.y1], 1, sum, na.rm=T)==0,1,0)
g_ord$CU.KK.y1.allzero <- ifelse(apply(g_ord[,CU.KK.y1], 1, sum, na.rm=T)==0,1,0)
g_ord$JHU.y1.allzero <- ifelse(apply(g_ord[,JHU.y1], 1, sum, na.rm=T)==0,1,0)
g_ord$Metro.y1.cl3.allzero <- ifelse(apply(g_ord[,Metro.y1.cl3], 1, sum, na.rm=T)==0,1,0)
g_ord$Metro.y1.cl8.allzero <- ifelse(apply(g_ord[,Metro.y1.cl8], 1, sum, na.rm=T)==0,1,0)
g_ord$STMU.y1.cl4.allzero <- ifelse(apply(g_ord[,STMU.y1.cl4], 1, sum, na.rm=T)==0,1,0)
g_ord$STMU.y1.cl9.allzero <- ifelse(apply(g_ord[,STMU.y1.cl9], 1, sum, na.rm=T)==0,1,0)
g_ord$UCD.y1.cl5.allzero <- ifelse(apply(g_ord[,UCD.y1.cl5], 1, sum, na.rm=T)==0,1,0)
g_ord$UCD.y1.cl6.allzero <- ifelse(apply(g_ord[,UCD.y1.cl6], 1, sum, na.rm=T)==0,1,0)
g_ord$UGA.y1.allzero <- ifelse(apply(g_ord[,UGA.y1], 1, sum, na.rm=T)==0,1,0)

# subsetting to include only rows that don't have all 0s for each column for each item/class group
g_ord_CUJK_y1 <- g_ord[g_ord$cl==7 & g_ord$post==1 & g_ord$CU.JK.y1.allzero==0,] 
g_ord_CUKK_y1 <- g_ord[g_ord$cl==1 & g_ord$post==1 & g_ord$CU.KK.y1.allzero==0,] 
g_ord_JHU_y1 <- g_ord[g_ord$cl==2 & g_ord$post==1 & g_ord$JHU.y1.allzero==0,] 
g_ord_Metro3_y1 <- g_ord[g_ord$cl==3 & g_ord$post==1 & g_ord$Metro.y1.cl3.allzero==0,] 
g_ord_Metro8_y1 <- g_ord[g_ord$cl==8 & g_ord$post==1 & g_ord$Metro.y1.cl8.allzero==0,] 
g_ord_STMU4_y1 <- g_ord[g_ord$cl==4 & g_ord$post==1 & g_ord$STMU.y1.cl4.allzero==0,] 
g_ord_STMU9_y1 <- g_ord[g_ord$cl==9 & g_ord$post==1 & g_ord$STMU.y1.cl9.allzero==0,] 
g_ord_UCD5_y1 <- g_ord[g_ord$cl==5 & g_ord$post==1 & g_ord$UCD.y1.cl5.allzero==0,] 
g_ord_UCD6_y1 <- g_ord[g_ord$cl==6 & g_ord$post==1 & g_ord$UCD.y1.cl6.allzero==0,] 
g_ord_UGA_y1 <- g_ord[g_ord$cl==10 & g_ord$post==1 & g_ord$UGA.y1.allzero==0,] 
# putting g-ord back together as g-ord_y1
g_ord_y1 <- rbind(g_ord_CUJK_y1, g_ord_CUKK_y1, g_ord_JHU_y1, g_ord_Metro3_y1, g_ord_Metro8_y1, 
      g_ord_STMU4_y1, g_ord_STMU9_y1, g_ord_UCD5_y1, g_ord_UCD6_y1, g_ord_UGA_y1)

# making variables in g_ord_y1 to hold data
g_ord_y1$year1_total <- rep(NA,times=941)
g_ord_y1$year1_prop <- rep(NA,times=941)

# Preparing conditions to work with functions and loops
condx <-  list((g_ord_y1$cl==7 & g_ord_y1$post==1), (g_ord_y1$cl==1  & g_ord_y1$post==1), (g_ord_y1$cl==2 & g_ord_y1$post==1), (g_ord_y1$cl==3 & g_ord_y1$post==1), 
               (g_ord_y1$cl==8 & g_ord_y1$post==1), (g_ord_y1$cl==4 & g_ord_y1$post==1), (g_ord_y1$cl==9 & g_ord_y1$post==1), (g_ord_y1$cl==5 & g_ord_y1$post==1),
               (g_ord_y1$cl==6 & g_ord_y1$post==1), (g_ord_y1$cl==10 & g_ord_y1$post==1))
condy <- list(CU.JK.y1, CU.KK.y1, JHU.y1 ,Metro.y1.cl3, Metro.y1.cl8, STMU.y1.cl4, STMU.y1.cl9, UCD.y1.cl5, UCD.y1.cl6, UGA.y1)


# creating a function that gets the descriptive data
class_desc2 <- function (x,y) {
  y1total <- apply(g_ord_y1[x,y], 1, sum, na.rm=T)
  y1mean <- round(mean(y1total),2)
  y1max <- apply(g_ord_y1[x,y], 2, max, na.rm=T)
  y1total_poss <- sum(y1max)
  y1prop <- y1total/y1total_poss
  list1 <- list(y1mean=y1mean, y1total_poss=y1total_poss, y1total=y1total, y1prop=y1prop)
  return(list1)
}

# creating a loop with conditions based on x and y values above
results <- list(10)
for (i in 1:10) {
  myx <- condx[[i]]
  myy <- condy[[i]]
  temp <- class_desc2(x=myx, y=myy)
  results[[i]] <- temp
  g_ord_y1[myx,"year1_total"] <- temp[["y1total"]]
  g_ord_y1[myx,"year1_prop"] <- temp[["y1prop"]]
}

# getting reliability estimates for each
CU.JK.y1.reliability <- reliability(g_ord_y1[g_ord_y1$cl==7 & g_ord_y1$post==1, CU.JK.y1], NA.Delete = T)
CU.JK.y1.rel <- round(CU.JK.y1.reliability[[3]],2)

CU.KK.y1.reliability <- reliability(g_ord_y1[g_ord_y1$cl==1  & g_ord_y1$post==1, CU.KK.y1], NA.Delete = T)
CU.KK.y1.rel <- round(CU.KK.y1.reliability[[3]],2)

JHU.y1.reliability <- reliability(g_ord_y1[g_ord_y1$cl==2 & g_ord_y1$post==1, JHU.y1], NA.Delete = T)
JHU.y1.rel <- round(JHU.y1.reliability[[3]],2)

Metro.y1.cl3.reliability <- reliability(g_ord_y1[g_ord_y1$cl==3 & g_ord_y1$post==1, Metro.y1.cl3], NA.Delete = T)
Metro.y1.cl3.rel <- round(Metro.y1.cl3.reliability[[3]],2)

Metro.y1.cl8.reliability <- reliability(g_ord_y1[g_ord_y1$cl==8 & g_ord_y1$post==1, Metro.y1.cl8], NA.Delete = T)
Metro.y1.cl8.rel <- round(Metro.y1.cl8.reliability[[3]],2)

STMU.y1.cl4.reliability <- reliability(g_ord_y1[g_ord_y1$cl==4 & g_ord_y1$post==1, STMU.y1.cl4], NA.Delete = T)
STMU.y1.cl4.rel <- round(STMU.y1.cl4.reliability[[3]],2)

STMU.y1.cl9.reliability <- reliability(g_ord_y1[g_ord_y1$cl==9 & g_ord_y1$post==1, STMU.y1.cl9], NA.Delete = T)
STMU.y1.cl9.rel <- round(STMU.y1.cl9.reliability[[3]],2)

UCD.y1.cl5.reliability <- reliability(g_ord_y1[g_ord_y1$cl==5 & g_ord_y1$post==1, UCD.y1.cl5], NA.Delete = T)
UCD.y1.cl5.rel <- round(UCD.y1.cl5.reliability[[3]],2)

UCD.y1.cl6.reliability <- reliability(g_ord_y1[g_ord_y1$cl==6 & g_ord_y1$post==1, UCD.y1.cl6], NA.Delete = T)
UCD.y1.cl6.rel <- round(UCD.y1.cl6.reliability[[3]],2)

UGA.y1.reliability <- reliability(g_ord_y1[g_ord_y1$cl==10 & g_ord_y1$post==1, UGA.y1], NA.Delete = T)
UGA.y1.rel <- round(UGA.y1.reliability[[3]],2)

year1_rel_list <- list(CU.JK.y1.rel, CU.KK.y1.rel, JHU.y1.rel, Metro.y1.cl3.rel, Metro.y1.cl8.rel, 
                  STMU.y1.cl4.rel, STMU.y1.cl9.rel, UCD.y1.cl5.rel, UCD.y1.cl6.rel, UGA.y1.rel)

# preparing for creation of table
by_course <- group_by(g_ord_y1,Institution,Semester,Course.Code,cl)
year1_results <- summarise(by_course,
                           count = n(),
                           Year1_mean_prop = round(mean(year1_prop, na.rm = TRUE),2),
                           Year1_mprop_sd = round(sd(year1_prop, na.rm = TRUE),2))
year1_results<-as.data.frame(year1_results) 
# getting needed data from lists created in loop to add max, mean, and reliability columns to results df
# adding max colum
year1_max <- unlist(lapply(results, "[[", "y1total_poss"))
year1_results$Year1_max <- rep(NA,times=10)
year1_results$Year1_max <- year1_max
# adding mean column
year1_mean <- unlist(lapply(results, "[[", "y1mean"))
year1_results$Year1_mean <- rep(NA,times=10)
year1_results$Year1_mean <- year1_mean
# adding reliability column
year1_rel <- unlist(lapply(year1_rel_list, "[[", 1))
year1_results$Year1_alpha <- rep(NA, times=10)
year1_results$Year1_alpha <- year1_rel
# ordering columns
year1_results <- year1_results[,c(4,1,2,3,5,9,8,6,7,10)]

# ------------------------------------------ year 2 pre/post ------------------------------------------------------------------------ 
# replacing NAs with 0s
# pre
g_ord[g_ord$cl==17 & g_ord$matrix_yr2pre==5 & g_ord$post==0,CU.JK.pre.m5][is.na(g_ord[g_ord$cl==17 & g_ord$matrix_yr2pre==5 & g_ord$post==0,CU.JK.pre.m5])]<-0 
g_ord[g_ord$cl==17 & g_ord$matrix_yr2pre==6 & g_ord$post==0,CU.JK.pre.m6][is.na(g_ord[g_ord$cl==17 & g_ord$matrix_yr2pre==6 & g_ord$post==0,CU.JK.pre.m6])]<-0 
g_ord[g_ord$cl==11 & g_ord$matrix_yr2pre==1 & g_ord$post==0,CU.KK.pre.m1][is.na(g_ord[g_ord$cl==11 & g_ord$matrix_yr2pre==1 & g_ord$post==0,CU.KK.pre.m1])]<-0 
g_ord[g_ord$cl==11 & g_ord$matrix_yr2pre==2 & g_ord$post==0,CU.KK.pre.m2][is.na(g_ord[g_ord$cl==11 & g_ord$matrix_yr2pre==2 & g_ord$post==0,CU.KK.pre.m2])]<-0 
g_ord[g_ord$cl==12 & g_ord$post==0,JHU.pre][is.na(g_ord[g_ord$cl==12 & g_ord$post==0,JHU.pre])]<-0 
g_ord[g_ord$cl==13 & g_ord$post==0,Metro.pre.cl13][is.na(g_ord[g_ord$cl==13 & g_ord$post==0,Metro.pre.cl13])]<-0 
g_ord[g_ord$cl==18 & g_ord$post==0,Metro.pre.cl18][is.na(g_ord[g_ord$cl==18 & g_ord$post==0,Metro.pre.cl18])]<-0 
g_ord[g_ord$cl==14 & g_ord$post==0,STMU.pre.cl14][is.na(g_ord[g_ord$cl==14 & g_ord$post==0,STMU.pre.cl14])]<-0 
g_ord[g_ord$cl==19 & g_ord$post==0,STMU.pre.cl19][is.na(g_ord[g_ord$cl==19 & g_ord$post==0,STMU.pre.cl19])]<-0 
g_ord[g_ord$cl==15 & g_ord$post==0,UCD.pre][is.na(g_ord[g_ord$cl==15 & g_ord$post==0,UCD.pre])]<-0 
g_ord[g_ord$cl==20 & g_ord$post==0,UGA.pre.post][is.na(g_ord[g_ord$cl==20 & g_ord$post==0,UGA.pre.post])]<-0 
# post
g_ord[g_ord$cl==17 & g_ord$post==1,CU.JK.post][is.na(g_ord[g_ord$cl==17 & g_ord$post==1,CU.JK.post])]<-0 
g_ord[g_ord$cl==11 & g_ord$post==1,CU.KK.post][is.na(g_ord[g_ord$cl==11 & g_ord$post==1,CU.KK.post])]<-0 
g_ord[g_ord$cl==12 & g_ord$post==1,JHU.post][is.na(g_ord[g_ord$cl==12 & g_ord$post==1,JHU.post])]<-0 
g_ord[g_ord$cl==13 & g_ord$post==1,Metro.post.cl13][is.na(g_ord[g_ord$cl==13 & g_ord$post==1,Metro.post.cl13])]<-0 
g_ord[g_ord$cl==18 & g_ord$post==1,Metro.post.cl18][is.na(g_ord[g_ord$cl==18 & g_ord$post==1,Metro.post.cl18])]<-0 
g_ord[g_ord$cl==14 & g_ord$post==1,STMU.post.cl14][is.na(g_ord[g_ord$cl==14 & g_ord$post==1,STMU.post.cl14])]<-0 
g_ord[g_ord$cl==19 & g_ord$post==1,STMU.post.cl19][is.na(g_ord[g_ord$cl==19 & g_ord$post==1,STMU.post.cl19])]<-0 
g_ord[g_ord$cl==15 & g_ord$post==1,UCD.post][is.na(g_ord[g_ord$cl==15 & g_ord$post==1,UCD.post])]<-0 
g_ord[g_ord$cl==20 & g_ord$post==1,UGA.pre.post][is.na(g_ord[g_ord$cl==20 & g_ord$post==1,UGA.pre.post])]<-0 
# making dummies to mark when a row has all 0s 
# pre
g_ord$CU.JK.pre.m5.allzero <- rep(NA,3468)
g_ord$CU.JK.pre.m6.allzero <- rep(NA,3468)
g_ord$CU.KK.pre.m1.allzero <- rep(NA,3468)
g_ord$CU.KK.pre.m2.allzero <- rep(NA,3468)
g_ord$JHU.pre.allzero <- rep(NA,3468)
g_ord$Metro.pre.cl13.allzero <- rep(NA,3468)
g_ord$Metro.pre.cl18.allzero <- rep(NA,3468)
g_ord$STMU.pre.cl14.allzero <- rep(NA,3468)
g_ord$STMU.pre.cl19.allzero <- rep(NA,3468)
g_ord$UCD.pre.allzero <- rep(NA,3468)
g_ord$UGA.pre.allzero <- rep(NA,3468)
# post
g_ord$CU.JK.post.allzero <- rep(NA,3468)
g_ord$CU.KK.post.allzero <- rep(NA,3468)
g_ord$JHU.post.allzero <- rep(NA,3468)
g_ord$Metro.post.cl13.allzero <- rep(NA,3468)
g_ord$Metro.post.cl18.allzero <- rep(NA,3468)
g_ord$STMU.post.cl14.allzero <- rep(NA,3468)
g_ord$STMU.post.cl19.allzero <- rep(NA,3468)
g_ord$UCD.post.allzero <- rep(NA,3468)
g_ord$UGA.post.allzero <- rep(NA,3468)

# adding conditions to dummies
# pre
g_ord$CU.JK.pre.m5.allzero <- ifelse(apply(g_ord[,CU.JK.pre.m5], 1, sum, na.rm=T)==0,1,0)
g_ord$CU.JK.pre.m6.allzero <- ifelse(apply(g_ord[,CU.JK.pre.m6], 1, sum, na.rm=T)==0,1,0)
g_ord$CU.KK.pre.m1.allzero <- ifelse(apply(g_ord[,CU.KK.pre.m1], 1, sum, na.rm=T)==0,1,0)
g_ord$CU.KK.pre.m2.allzero <- ifelse(apply(g_ord[,CU.KK.pre.m2], 1, sum, na.rm=T)==0,1,0)
g_ord$JHU.pre.allzero <- ifelse(apply(g_ord[,JHU.pre], 1, sum, na.rm=T)==0,1,0)
g_ord$Metro.pre.cl13.allzero <- ifelse(apply(g_ord[,Metro.pre.cl13], 1, sum, na.rm=T)==0,1,0)
g_ord$Metro.pre.cl18.allzero <- ifelse(apply(g_ord[,Metro.pre.cl18], 1, sum, na.rm=T)==0,1,0)
g_ord$STMU.pre.cl14.allzero <- ifelse(apply(g_ord[,STMU.pre.cl14], 1, sum, na.rm=T)==0,1,0)
g_ord$STMU.pre.cl19.allzero <- ifelse(apply(g_ord[,STMU.pre.cl19], 1, sum, na.rm=T)==0,1,0)
g_ord$UCD.pre.allzero <- ifelse(apply(g_ord[,UCD.pre], 1, sum, na.rm=T)==0,1,0)
g_ord$UGA.pre.allzero <- ifelse(apply(g_ord[,UGA.pre.post], 1, sum, na.rm=T)==0,1,0)
# post
g_ord$CU.JK.post.allzero <- ifelse(apply(g_ord[,CU.JK.post], 1, sum, na.rm=T)==0,1,0)
g_ord$CU.KK.post.allzero <- ifelse(apply(g_ord[,CU.KK.post], 1, sum, na.rm=T)==0,1,0)
g_ord$JHU.post.allzero <- ifelse(apply(g_ord[,JHU.post], 1, sum, na.rm=T)==0,1,0)
g_ord$Metro.post.cl13.allzero <- ifelse(apply(g_ord[,Metro.post.cl13], 1, sum, na.rm=T)==0,1,0)
g_ord$Metro.post.cl18.allzero <- ifelse(apply(g_ord[,Metro.post.cl18], 1, sum, na.rm=T)==0,1,0)
g_ord$STMU.post.cl14.allzero <- ifelse(apply(g_ord[,STMU.post.cl14], 1, sum, na.rm=T)==0,1,0)
g_ord$STMU.post.cl19.allzero <- ifelse(apply(g_ord[,STMU.post.cl19], 1, sum, na.rm=T)==0,1,0)
g_ord$UCD.post.allzero <- ifelse(apply(g_ord[,UCD.post], 1, sum, na.rm=T)==0,1,0)
g_ord$UGA.post.allzero <- ifelse(apply(g_ord[,UGA.pre.post], 1, sum, na.rm=T)==0,1,0)

# subsetting to include only rows that don't have all 0s for each column for each item/class group 
# pre
g_ord_CUJKm5_pre <- g_ord[g_ord$cl==17 & g_ord$matrix_yr2pre==5 & g_ord$post==0 & g_ord$CU.JK.pre.m5.allzero==0,] 
g_ord_CUJKm6_pre <- g_ord[g_ord$cl==17 & g_ord$matrix_yr2pre==6 & g_ord$post==0 & g_ord$CU.JK.pre.m6.allzero==0,] 
g_ord_CUKKm1_pre <- g_ord[g_ord$cl==11 & g_ord$matrix_yr2pre==1 & g_ord$post==0 & g_ord$CU.KK.pre.m1.allzero==0,] 
g_ord_CUKKm2_pre <- g_ord[g_ord$cl==11 & g_ord$matrix_yr2pre==2 & g_ord$post==0 & g_ord$CU.KK.pre.m2.allzero==0,] 
g_ord_JHU_pre <- g_ord[g_ord$cl==12 & g_ord$post==0 & g_ord$JHU.pre.allzero==0,] 
g_ord_Metro13_pre <- g_ord[g_ord$cl==13 & g_ord$post==0 & g_ord$Metro.pre.cl13.allzero==0,] 
g_ord_Metro18_pre <- g_ord[g_ord$cl==18 & g_ord$post==0 & g_ord$Metro.pre.cl18.allzero==0,] 
g_ord_STMU14_pre <- g_ord[g_ord$cl==14 & g_ord$post==0 & g_ord$STMU.pre.cl14.allzero==0,] 
g_ord_STMU19_pre <- g_ord[g_ord$cl==19 & g_ord$post==0 & g_ord$STMU.pre.cl19.allzero==0,] 
g_ord_UCD_pre <- g_ord[g_ord$cl==15 & g_ord$post==0 & g_ord$UCD.pre.allzero==0,] 
g_ord_UGA_pre <- g_ord[g_ord$cl==20 & g_ord$post==0 & g_ord$UGA.pre.allzero==0,] 
#  post
g_ord_CUJK_post <- g_ord[g_ord$cl==17 & g_ord$post==1 & g_ord$CU.JK.post.allzero==0,] 
g_ord_CUKK_post <- g_ord[g_ord$cl==11 & g_ord$post==1 & g_ord$CU.KK.post.allzero==0,] 
g_ord_JHU_post <- g_ord[g_ord$cl==12 & g_ord$post==1 & g_ord$JHU.post.allzero==0,] 
g_ord_Metro13_post <- g_ord[g_ord$cl==13 & g_ord$post==1 & g_ord$Metro.post.cl13.allzero==0,] 
g_ord_Metro18_post <- g_ord[g_ord$cl==18 & g_ord$post==1 & g_ord$Metro.post.cl18.allzero==0,] 
g_ord_STMU14_post <- g_ord[g_ord$cl==14 & g_ord$post==1 & g_ord$STMU.post.cl14.allzero==0,] 
g_ord_STMU19_post <- g_ord[g_ord$cl==19 & g_ord$post==1 & g_ord$STMU.post.cl19.allzero==0,] 
g_ord_UCD_post <- g_ord[g_ord$cl==15 & g_ord$post==1 & g_ord$UCD.post.allzero==0,] 
g_ord_UGA_post <- g_ord[g_ord$cl==20 & g_ord$post==1 & g_ord$UGA.post.allzero==0,] 

# putting g-ord back together as g-ord_y2
g_ord_y2 <- rbind(g_ord_CUJKm5_pre, g_ord_CUJKm6_pre, g_ord_CUKKm1_pre, g_ord_CUKKm2_pre, g_ord_JHU_pre, 
                  g_ord_Metro13_pre, g_ord_Metro18_pre, g_ord_STMU14_pre, g_ord_STMU19_pre, g_ord_UCD_pre,
                  g_ord_UGA_pre, g_ord_CUJK_post, g_ord_CUKK_post, g_ord_JHU_post, g_ord_Metro13_post,
                  g_ord_Metro18_post, g_ord_STMU14_post, g_ord_STMU19_post, g_ord_UCD_post, g_ord_UGA_post)


# making variables in g_ord_y2 to hold data
g_ord_y2$year2_pre_total <- rep(NA,times=1296)
g_ord_y2$year2_pre_prop <- rep(NA,times=1296)
g_ord_y2$year2_post_total <- rep(NA,times=1296)
g_ord_y2$year2_post_prop <- rep(NA,times=1296)
# making a grouping variable for proper sorting later
g_ord_y2$cl_ma <- rep(NA,times=1296)
g_ord_y2$cl_ma <- ifelse(g_ord_y2$cl==17 & g_ord_y2$matrix_yr2pre==5, 1, 0)
g_ord_y2$cl_ma <- ifelse(g_ord_y2$cl==17 & g_ord_y2$matrix_yr2pre==6, 2, g_ord_y2$cl_ma)
g_ord_y2$cl_ma <- ifelse(g_ord_y2$cl==11 & g_ord_y2$matrix_yr2pre==1, 3, g_ord_y2$cl_ma)
g_ord_y2$cl_ma <- ifelse(g_ord_y2$cl==11 & g_ord_y2$matrix_yr2pre==2, 4, g_ord_y2$cl_ma)
g_ord_y2$cl_ma <- ifelse(g_ord_y2$cl==12, 5, g_ord_y2$cl_ma)
g_ord_y2$cl_ma <- ifelse(g_ord_y2$cl==13, 6, g_ord_y2$cl_ma)
g_ord_y2$cl_ma <- ifelse(g_ord_y2$cl==18, 7, g_ord_y2$cl_ma)
g_ord_y2$cl_ma <- ifelse(g_ord_y2$cl==14, 8, g_ord_y2$cl_ma)
g_ord_y2$cl_ma <- ifelse(g_ord_y2$cl==19, 9, g_ord_y2$cl_ma)
g_ord_y2$cl_ma <- ifelse(g_ord_y2$cl==15, 10, g_ord_y2$cl_ma)
g_ord_y2$cl_ma <- ifelse(g_ord_y2$cl==20, 11, g_ord_y2$cl_ma)


# Preparing conditions to work with function and loop for pre
conda <- list((g_ord_y2$cl==17 & g_ord_y2$matrix_yr2pre==5 & g_ord_y2$post==0), (g_ord_y2$cl==17 & g_ord_y2$matrix_yr2pre==6 & g_ord_y2$post==0), 
              (g_ord_y2$cl==11 & g_ord_y2$matrix_yr2pre==1 & g_ord_y2$post==0), (g_ord_y2$cl==11 & g_ord_y2$matrix_yr2pre==2 & g_ord_y2$post==0), 
              (g_ord_y2$cl==12 & g_ord_y2$post==0), (g_ord_y2$cl==13 & g_ord_y2$post==0), (g_ord_y2$cl==18 & g_ord_y2$post==0), 
              (g_ord_y2$cl==14 & g_ord_y2$post==0), (g_ord_y2$cl==19 & g_ord_y2$post==0), (g_ord_y2$cl==15 & g_ord_y2$post==0), (g_ord_y2$cl==20 & g_ord_y2$post==0))
condb <- list(CU.JK.pre.m5, CU.JK.pre.m6, CU.KK.pre.m1, CU.KK.pre.m2, JHU.pre, 
              Metro.pre.cl13, Metro.pre.cl18, STMU.pre.cl14, STMU.pre.cl19, UCD.pre, UGA.pre.post)


# creating a function that gets the descriptive data; x= classes/times, y=item indexes for pre
class_descy2pre <- function (a,b) {
  y2pre_total <- apply(g_ord_y2[a,b], 1, sum, na.rm=T)
  y2pre_mean <- round(mean(y2pre_total, na.rm = T),2)
  y2pre_max <- apply(g_ord_y2[a,b], 2, max, na.rm=T)
  y2pre_total_poss <- sum(y2pre_max, na.rm =T)
  y2pre_prop <- y2pre_total/y2pre_total_poss
  list2 <- list(y2pre_mean=y2pre_mean, y2pre_total_poss=y2pre_total_poss, y2pre_total=y2pre_total, y2pre_prop=y2pre_prop)
  return(list2)
}

# creating a loop with conditions based on a and b values for pre
resultsy2pre <- list(11)
for (i in 1:11) {
  mya <- conda[[i]]
  myb <- condb[[i]]
  temp <- class_descy2pre(a=mya, b=myb)
  resultsy2pre[[i]] <- temp
  g_ord_y2[mya,"year2_pre_total"] <- temp[["y2pre_total"]]
  g_ord_y2[mya,"year2_pre_prop"] <- temp[["y2pre_prop"]]
}

# Preparing conditions to work with function and loop for post
CU.JK.post5 <- CU.JK.post 
CU.JK.post6 <- CU.JK.post
CU.KK.post1 <- CU.KK.post
CU.KK.post2 <- CU.KK.post

condc <-  list((g_ord_y2$cl==17 & g_ord_y2$matrix_yr2pre==5 & g_ord_y2$post==1), (g_ord_y2$cl==17 & g_ord_y2$matrix_yr2pre==6 & g_ord_y2$post==1), 
               (g_ord_y2$cl==11 & g_ord_y2$matrix_yr2pre==1 & g_ord_y2$post==1), (g_ord_y2$cl==11 & g_ord_y2$matrix_yr2pre==2 & g_ord_y2$post==1), 
               (g_ord_y2$cl==12 & g_ord_y2$post==1), (g_ord_y2$cl==13 & g_ord_y2$post==1), (g_ord_y2$cl==18 & g_ord_y2$post==1), 
               (g_ord_y2$cl==14 & g_ord_y2$post==1), (g_ord_y2$cl==19 & g_ord_y2$post==1), (g_ord_y2$cl==15 & g_ord_y2$post==1), (g_ord_y2$cl==20 & g_ord_y2$post==1))

condd <- list(CU.JK.post5, CU.JK.post6, CU.KK.post1, CU.KK.post2, JHU.post, Metro.post.cl13, Metro.post.cl18, 
              STMU.post.cl14, STMU.post.cl19, UCD.post, UGA.pre.post)

#condc <-  list((g_ord_y2$cl==17 & g_ord_y2$post==1), (g_ord_y2$cl==11 & g_ord_y2$post==1), (g_ord_y2$cl==12 & g_ord_y2$post==1), 
 #              (g_ord_y2$cl==13 & g_ord_y2$post==1), (g_ord_y2$cl==18 & g_ord_y2$post==1), (g_ord_y2$cl==14 & g_ord_y2$post==1), 
#               (g_ord_y2$cl==19 & g_ord_y2$post==1), (g_ord_y2$cl==15 & g_ord_y2$post==1), (g_ord_y2$cl==20 & g_ord_y2$post==1))
#
#condd <- list(CU.JK.post, CU.KK.post, JHU.post, Metro.post.cl13, Metro.post.cl18, 
 #             STMU.post.cl14, STMU.post.cl19, UCD.post, UGA.pre.post)




# creating a function that gets the descriptive data; x= classes/times, y=item indexes for post
class_descy2post <- function (c,d) {
  y2post_total <- apply(g_ord_y2[c,d], 1, sum, na.rm=T)
  y2post_mean <- round(mean(y2post_total, na.rm = T),2)
  y2post_max <- apply(g_ord_y2[c,d], 2, max, na.rm=T)
  y2post_total_poss <- sum(y2post_max, na.rm =T)
  y2post_prop <- y2post_total/y2post_total_poss
  list3 <- list(y2post_mean=y2post_mean, y2post_total_poss=y2post_total_poss, y2post_total=y2post_total, y2post_prop=y2post_prop)
  return(list3)
}

# creating a loop with conditions based on a and b values for post
resultsy2post <- list(11)
for (i in 1:11) {
  myc <- condc[[i]]
  myd <- condd[[i]]
  temp <- class_descy2post(c=myc, d=myd)
  resultsy2post[[i]] <- temp
  g_ord_y2[myc,"year2_post_total"] <- temp[["y2post_total"]]
  g_ord_y2[myc,"year2_post_prop"] <- temp[["y2post_prop"]]
}


# getting item reliability for pre
CU.JK.pre.m5.reliability <- reliability(g_ord_y2[g_ord_y2$cl==17 & g_ord_y2$matrix_yr2pre==5 & g_ord_y2$post==0, CU.JK.pre.m5], NA.Delete = T)
CU.JK.pre.m5.rel <- round(CU.JK.pre.m5.reliability[[3]],2)

CU.JK.pre.m6.reliability <- reliability(g_ord_y2[g_ord_y2$cl==17  & g_ord_y2$matrix_yr2pre==6 & g_ord_y2$post==0, CU.JK.pre.m6], NA.Delete = T)
CU.JK.pre.m6.rel <- round(CU.JK.pre.m6.reliability[[3]],2)

CU.KK.pre.m1.reliability <- reliability(g_ord_y2[g_ord_y2$cl==11 & g_ord_y2$matrix_yr2pre==1 & g_ord_y2$post==0, CU.KK.pre.m1], NA.Delete = T)
CU.KK.pre.m1.rel <- round(CU.KK.pre.m1.reliability[[3]],2)

CU.KK.pre.m2.reliability <- reliability(g_ord_y2[g_ord_y2$cl==11  & g_ord_y2$matrix_yr2pre==2 & g_ord_y2$post==0, CU.KK.pre.m2], NA.Delete = T)
CU.KK.pre.m2.rel <- round(CU.KK.pre.m2.reliability[[3]],2)

JHU.pre.reliability <- reliability(g_ord_y2[g_ord_y2$cl==12 & g_ord_y2$post==0, JHU.pre], NA.Delete = T)
JHU.pre.rel <- round(JHU.pre.reliability[[3]],2)

Metro.pre.cl13.reliability <- reliability(g_ord_y2[g_ord_y2$cl==13 & g_ord_y2$post==0, Metro.pre.cl13], NA.Delete = T)
Metro.pre.cl13.rel <- round(Metro.pre.cl13.reliability[[3]],2)

Metro.pre.cl18.reliability <- reliability(g_ord_y2[g_ord_y2$cl==18 & g_ord_y2$post==0, Metro.pre.cl18], NA.Delete = T)
Metro.pre.cl18.rel <- round(Metro.pre.cl18.reliability[[3]],2)

STMU.pre.cl14.reliability <- reliability(g_ord_y2[g_ord_y2$cl==14 & g_ord_y2$post==0, STMU.pre.cl14], NA.Delete = T)
STMU.pre.cl14.rel <- round(STMU.pre.cl14.reliability[[3]],2)

STMU.pre.cl19.reliability <- reliability(g_ord_y2[g_ord_y2$cl==19 & g_ord_y2$post==0, STMU.pre.cl19], NA.Delete = T)
STMU.pre.cl19.rel <- round(STMU.pre.cl19.reliability[[3]],2)

UCD.pre.reliability <- reliability(g_ord_y2[g_ord_y2$cl==15 & g_ord_y2$post==0, UCD.pre], NA.Delete = T)
UCD.pre.rel <- round(UCD.pre.reliability[[3]],2)

UGA.pre.reliability <- reliability(g_ord_y2[g_ord_y2$cl==20 & g_ord_y2$post==0, UGA.pre.post], NA.Delete = T)
UGA.pre.rel <- round(UGA.pre.reliability[[3]],2)

pre_rel_list <- list(CU.JK.pre.m5.rel, CU.JK.pre.m6.rel, CU.KK.pre.m1.rel, CU.KK.pre.m2.rel, JHU.pre.rel, 
                       Metro.pre.cl13.rel, Metro.pre.cl18.rel, STMU.pre.cl14.rel, STMU.pre.cl19.rel, UCD.pre.rel, UGA.pre.rel)

# getting item reliability for post
CU.JK.post.m5.reliability <- reliability(g_ord_y2[g_ord_y2$cl==17 & g_ord_y2$matrix_yr2pre==5 & g_ord_y2$post==1, CU.JK.post5], NA.Delete = T)
CU.JK.post.m5.rel <- round(CU.JK.post.m5.reliability[[3]],2)

CU.JK.post.m6.reliability <- reliability(g_ord_y2[g_ord_y2$cl==17  & g_ord_y2$matrix_yr2pre==6 & g_ord_y2$post==1, CU.JK.post6], NA.Delete = T)
CU.JK.post.m6.rel <- round(CU.JK.post.m6.reliability[[3]],2)

CU.KK.post.m1.reliability <- reliability(g_ord_y2[g_ord_y2$cl==11 & g_ord_y2$matrix_yr2pre==1 & g_ord_y2$post==1, CU.KK.post1], NA.Delete = T)
CU.KK.post.m1.rel <- round(CU.KK.post.m1.reliability[[3]],2)

CU.KK.post.m2.reliability <- reliability(g_ord_y2[g_ord_y2$cl==11  & g_ord_y2$matrix_yr2pre==2 & g_ord_y2$post==1, CU.KK.post2], NA.Delete = T)
CU.KK.post.m2.rel <- round(CU.KK.post.m2.reliability[[3]],2)

JHU.post.reliability <- reliability(g_ord_y2[g_ord_y2$cl==12 & g_ord_y2$post==1, JHU.post], NA.Delete = T)
JHU.post.rel <- round(JHU.post.reliability[[3]],2)

Metro.post.cl13.reliability <- reliability(g_ord_y2[g_ord_y2$cl==13 & g_ord_y2$post==1, Metro.post.cl13], NA.Delete = T)
Metro.post.cl13.rel <- round(Metro.post.cl13.reliability[[3]],2)

Metro.post.cl18.reliability <- reliability(g_ord_y2[g_ord_y2$cl==18 & g_ord_y2$post==1, Metro.post.cl18], NA.Delete = T)
Metro.post.cl18.rel <- round(Metro.post.cl18.reliability[[3]],2)

STMU.post.cl14.reliability <- reliability(g_ord_y2[g_ord_y2$cl==14 & g_ord_y2$post==1, STMU.post.cl14], NA.Delete = T)
STMU.post.cl14.rel <- round(STMU.post.cl14.reliability[[3]],2)

STMU.post.cl19.reliability <- reliability(g_ord_y2[g_ord_y2$cl==19 & g_ord_y2$post==1, STMU.post.cl19], NA.Delete = T)
STMU.post.cl19.rel <- round(STMU.post.cl19.reliability[[3]],2)

UCD.post.reliability <- reliability(g_ord_y2[g_ord_y2$cl==15 & g_ord_y2$post==1, UCD.post], NA.Delete = T)
UCD.post.rel <- round(UCD.post.reliability[[3]],2)

UGA.post.reliability <- reliability(g_ord_y2[g_ord_y2$cl==20 & g_ord_y2$post==1, UGA.pre.post], NA.Delete = T)
UGA.post.rel <- round(UGA.post.reliability[[3]],2)

post_rel_list <- list(CU.JK.post.m5.rel, CU.JK.post.m6.rel, CU.KK.post.m1.rel, CU.KK.post.m2.rel, JHU.post.rel, 
                     Metro.post.cl13.rel, Metro.post.cl18.rel, STMU.post.cl14.rel, STMU.post.cl19.rel, UCD.post.rel, UGA.post.rel)

colnames(g_ord_y2)
# preparing for creation of table

by_course <- group_by(g_ord_y2,Institution,Semester,Course.Code,cl,cl_ma)
year2_results <- summarise(by_course,
                           count = n(),
                           Pre_mean_prop = round(mean(year2_pre_prop, na.rm = TRUE),2),
                           Pre_mprop_sd = round(sd(year2_pre_prop, na.rm = TRUE),2),
                           Post_mean_prop = round(mean(year2_post_prop, na.rm = TRUE),2),
                           Post_mprop_sd = round(sd(year2_post_prop, na.rm = TRUE),2))
year2_results<-as.data.frame(year2_results) 
# getting needed data from lists created in loop to add max, mean, and reliability columns to results df
# adding pre max colum
y2_pre_max <- unlist(lapply(resultsy2pre, "[[", "y2pre_total_poss"))
year2_results$Pre_max <- rep(NA,times=11)
year2_results$Pre_max <- y2_pre_max
# adding pre mean column
y2_pre_mean <- unlist(lapply(resultsy2pre, "[[", "y2pre_mean"))
year2_results$Pre_mean <- rep(NA,times=11)
year2_results$Pre_mean <- y2_pre_mean
# adding pre reliability column
y2_pre_alpha <- unlist(lapply(pre_rel_list, "[[", 1))
year2_results$Pre_alpha <- rep(NA, times=11)
year2_results$Pre_alpha <- y2_pre_alpha
# adding post max colum
y2_post_max <- unlist(lapply(resultsy2post, "[[", "y2post_total_poss"))
year2_results$Post_max <- rep(NA,times=11)
year2_results$Post_max <- y2_post_max
# adding post mean column
y2_post_mean <- unlist(lapply(resultsy2post, "[[", "y2post_mean"))
year2_results$Post_mean <- rep(NA,times=11)
year2_results$Post_mean <- y2_post_mean
# adding post reliability column
y2_post_alpha <- unlist(lapply(post_rel_list, "[[", 1))
year2_results$Post_alpha <- rep(NA, times=11)
year2_results$Post_alpha <- y2_post_alpha
# adding pre-post diff column
year2_results$Pre_Post_Diff <- year2_results$Post_mean_prop -  year2_results$Pre_mean_prop

# ordering columns
year2_results <- year2_results[,c(4,5,1,2,3,6,12,11,7,8,13,15,14,9,10,16,17)]
year2_results$cl_ma<- NULL

#exporting tables to Excel
setwd("./Keck Tables and Figures")
write.csv(format(year1_results, digits=2, nsmall=2),"year1_results.csv") 
write.csv(format(year2_results, digits=2, nsmall=2),"year2_results.csv") 

#--------------------------------------------------------------------------------------------------------
#                               Creating visuals
#--------------------------------------------------------------------------------------------------------
# year 1
# all courses Histograms
y1_hist <-  ggplot(g_ord_y1, aes(x=year1_prop)) + 
  geom_histogram(binwidth =.1,  color="black", fill="blue") +
  scale_x_continuous(name = "Proportion Correct of Local Items Post Year 1", breaks=seq(0, 1.0, by=.05)) +
  scale_y_continuous(name = "Number of Students")
y1_hist

# histogram by Course.Code -column
y1_hist_bycourse_tall <- ggplot(g_ord_y1, aes(x=year1_prop)) + 
  geom_histogram(binwidth=.1, color="black", aes(fill=Course.Code)) +
  facet_grid(Course.Code~., scales ="free_y") +
  scale_x_continuous(name = "Proportion Correct of Local Items Post Year 1", breaks=seq(0, 1, by=.05)) +
  scale_y_continuous(name = "Number of Students")
y1_hist_bycourse_tall

# histogram by Course.Code -rows
y1_hist_bycourse <- ggplot(g_ord_y1, aes(x=year1_prop)) + 
  geom_histogram(binwidth=.1, color="black", aes(fill=Course.Code)) +
  facet_grid(.~Course.Code, scales ="free_y") +
  scale_x_continuous(name = "Proportion Correct of Local Items Post Year 1", breaks=seq(0, 1, by=.05)) +
  scale_y_continuous(name = "Number of Students")
y1_hist_bycourse


# year 2
# all courses pre/post overlapping
y2_hist_pre_post <- ggplot(g_ord_y2, aes(x)) +
  geom_histogram(binwidth=.1, aes(x=year2_pre_prop), color="black", fill="blue", alpha=0.5)+
  geom_histogram(binwidth=.1, aes(x=year2_post_prop), color="black", fill="green", alpha=0.5)+
  scale_x_continuous(name = "Proportion Correct of Local Items Pre and Post Year 2", breaks=seq(0, 1, by=.05)) +
  scale_y_continuous(name = "Number of Students")
y2_hist_pre_post

# by courses pre/post overlapping -column
y2_hist_pre_post_tall <- ggplot(g_ord_y2, aes(x)) +
  geom_histogram(binwidth=.1, aes(x=year2_pre_prop), color="black", fill = "blue", alpha=0.5)+
  geom_histogram(binwidth=.1, aes(x=year2_post_prop), color="black", fill="green", alpha=0.5)+
  facet_grid(Course.Code~., scales = "free_y") +
  scale_x_continuous(name = "Proportion Correct of Local Items Pre and Post Year 2", breaks=seq(0, 1, by=.05)) +
  scale_y_continuous(name = "Number of Students")
y2_hist_pre_post_tall

# by courses pre/post overlapping -rows
y2_hist_pre_post_long <- ggplot(g_ord_y2, aes(x)) +
  geom_histogram(binwidth=.1, aes(x=year2_pre_prop), color="black", fill = "blue", alpha=0.5)+
  geom_histogram(binwidth=.1, aes(x=year2_post_prop), color="black", fill="green", alpha=0.5)+
  facet_grid(.~Course.Code, scales = "free_y") +
  scale_x_continuous(name = "Proportion Correct of Local Items Pre and Post Year 2", breaks=seq(0, 1, by=.05)) +
  scale_y_continuous(name = "Number of Students")
y2_hist_pre_post_long

