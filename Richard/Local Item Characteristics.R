# Richard Noone
# Keck Project
# Local Items frequencies, reliability, item-total correlations
# packages used: dplyr, CTT, recoder, ggplot2
# 10-11-2017
# Purpose:
#------------------------------------------------------------------
# this script will create a spreadsheet that has:
#                 1) category frequencies on the ordinal scale for all Local Items
#                 2) best available alpha reliability for all items
#                 3) best available Item-total correlations for all items
#---------------------------------------------------------------
setwd("/Users/richardnoone/Dropbox/2017 CU Fall/Github/Keck/Keck Analysis")

library(dplyr)
library(CTT)
library(recoder)
library(ggplot2)
# getting data
g <- read.csv("g_ord.csv", header=T)
# checking data structure
head(g)
str(g)
colnames(g)

# creating frequecy tables for every ordinal item
Freq_ord_scale <- apply(g[,c(403:753)], 2,function(x) table(x, useNA='always'))

# checking for max frequency categories to inform creation of dataframe
head(Freq_ord_scale,200)
tail(Freq_ord_scale,151)
max_cat <- 32

# getting item names to use as colnames in df
item_names <-colnames(g[-c(1:402)])

# making df to hold data from tables (# of rows determined by # of items, 
# # of columns determined by # of frequency categories + 1 for 0)
local_items_freq <- data.frame(matrix(ncol=33,nrow=351)) 
rownames(local_items_freq) <- item_names
colnames(local_items_freq) <- c(0:32)

# the loop extracts each scale for each item and pastes the data into the created df
for (i in 1:length(item_names)){
  temp <- data.frame(Freq_ord_scale[[i]])
  temp <- temp$Freq[-length(temp$Freq)]
  local_items_freq[i,c(1:length(temp))] <- temp
}

# three items repeat between classes. I'm removing them so that each item is represented only one time
rownames(local_items_freq)
local_items_freq <- local_items_freq[-c(37,38,75),]
# 10 items are not used in year 1 post or year 2 pre/post -removing
rownames(local_items_freq)
local_items_freq <- local_items_freq[-c(319:320,322:329),]

# ordering row.names so that reliability data can be matched up with other dfs
local_items_freq <- local_items_freq[order(row.names(local_items_freq)),]

# need these dfs prepared in "Local Item Descriptives -No Collapse" for reliability calculation
g_ord_y1 <- read.csv("g_ord_y1.csv", header=T)
g_ord_y2 <- read.csv("g_ord_y2.csv", header=T)

# creating item sets for calculating reliability by matrix, class, or Institution as needed for year 1, and year 2 post

#  UNIVERSITY OF COLORADO 


# MCDB2150_JK

CU.JK.y1 <- c("cl7_fc26", "cl7_fc27", "cl7_fc28", "cl7_fc29", "cl7_fc30", "cl7_fc31", "cl7_fc32",
              "cl7_fc33", "cl7_fc34", "cl7_fc35", "cl7_fc36", "cl7_fc37", "cl7_fc38", "cl7_fc39",              
              "cl7_fc40", "cl7_fcsa1a", "cl7_fcsa1b", "cl7_fcsa1c", "cl7_fcsa1d", "cl7_fcsa2a",
              "cl7_fcsa2b", "cl7_fcsa2c", "cl7_fcsa3a", "cl7_fcsa3b", "cl7_fcsa3c", "cl7_fcsa3d",            
              "cl7_fcsa4a", "cl7_fcsa4b", "cl7_fcsa5a", "cl7_fcsa5b", "cl7_fcsa5c", "cl7_fcsa6a",
              "cl7_fcsa6b", "cl7_fcsa6c", "cl7_fcsa6d", "cl7_fcsa6e")

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

UGA.post <- c("cl10_me1_2"  ,      "cl10_me1_44"  ,           "cl10_me2_40"  ,             "cl10_me4_mc17"  ,
              "cl10_me6_41") 


# recoded items have "x_ord", so pasting that to items listed above 
CU.JK.y1 <- paste(CU.JK.y1, "CUJK_ord", sep = "_")
CU.JK.post <- paste(CU.JK.post, "CUJK_ord", sep = "_")
CU.KK.y1 <- paste(CU.KK.y1, "CUKK_ord", sep = "_")
CU.KK.post <- paste(CU.KK.post, "CUKK_ord", sep = "_")
JHU.y1 <- paste(JHU.y1, "JHU_ord", sep = "_")
JHU.post <- paste(JHU.post, "JHU_ord", sep = "_")
Metro.y1.cl3 <- paste(Metro.y1.cl3, "Metro_ord", sep = "_")
Metro.y1.cl8 <- paste(Metro.y1.cl8, "Metro_ord", sep = "_")
Metro.post.cl13 <- paste(Metro.post.cl13, "Metro_ord", sep = "_")
Metro.post.cl18 <- paste(Metro.post.cl18, "Metro_ord", sep = "_")
STMU.y1.cl4 <- paste(STMU.y1.cl4, "STMU_ord", sep = "_")
STMU.y1.cl9 <- paste(STMU.y1.cl9, "STMU_ord", sep = "_")
STMU.post.cl14 <- paste(STMU.post.cl14, "STMU_ord", sep = "_")
STMU.post.cl19 <- paste(STMU.post.cl19, "STMU_ord", sep = "_")
UCD.y1.cl5 <- paste(UCD.y1.cl5, "UCD_ord", sep = "_")
UCD.y1.cl6 <- paste(UCD.y1.cl6, "UCD_ord", sep = "_")
UCD.post <- paste(UCD.post, "UCD_ord", sep = "_")
UGA.y1 <- paste(UGA.y1, "UGA_ord", sep = "_")
UGA.post <- paste(UGA.post, "UGA_ord", sep = "_")

Reliability_sets <- c(CU.JK.y1, CU.JK.post, CU.KK.y1, CU.KK.post, JHU.y1, JHU.post, Metro.y1.cl3,
                      Metro.y1.cl8, Metro.post.cl13, Metro.post.cl18, STMU.y1.cl4,
                      STMU.y1.cl9, STMU.post.cl14, STMU.post.cl19, UCD.y1.cl5, UCD.y1.cl6, UCD.post,
                      UGA.y1, UGA.post)

# creating item sets for best reliability of items. Each item is grouped on the test that had the 
# highest reliability for that item. This was determined by looking at year 1 and year 2
# results spreadsheets and item groups in script "Local Item Descriptives -No Collapse"

#  UNIVERSITY OF COLORADO 


# MCDB2150_JK

brCU.JK.y1 <- c("cl7_fc27", "cl7_fc29",  "cl7_fc31", "cl7_fc32",
              "cl7_fc35", "cl7_fc36", "cl7_fc37", "cl7_fc40", 
              "cl7_fcsa1a", "cl7_fcsa1b", "cl7_fcsa1c", "cl7_fcsa1d", 
              "cl7_fcsa4a", "cl7_fcsa4b", "cl7_fcsa6a",
              "cl7_fcsa6b", "cl7_fcsa6c", "cl7_fcsa6d", "cl7_fcsa6e")

brCU.JK.post <- c("cl7_fc26", "cl7_fc28", "cl7_fc30", "cl7_fc33", "cl7_fc34", "cl7_fc38", "cl7_fc39", 
                "cl7_fcsa2a", "cl7_fcsa2b","cl7_fcsa2c", "cl7_fcsa3a", "cl7_fcsa3b", "cl7_fcsa3c","cl7_fcsa3d",
                "cl7_fcsa5a", "cl7_fcsa5b", "cl7_fcsa5c", "cl17_fcsa2a", 	"cl17_fcsa2b",	"cl17_fcsa2c",
                "cl17_fcsa2d", "cl17_fcmc3",	"cl17_fcmc4",	"cl17_fcsa1a",	
                "cl17_fcsa1b",	"cl17_fcsa1c",	"cl17_fcsa1d", "cl17_fcsa1e",	"cl17_fcsa1f",	"cl17_fcsa3a",	
                "cl17_fcsa3b",	"cl17_fcsa3c",	"cl17_fcsa5")


# MCDB2150_KK

brCU.KK.y1 <- c("cl1_fc27","cl1_fc30", "cl1_fc31", "cl1_fc32" ,             
              "cl1_fc33", "cl1_fc34" ,  "cl1_fc35"  , "cl1_fc36",         
              "cl1_fc40" ,"cl1_fc41" , "cl1_fc43"  , "cl1_fc45")

brCU.KK.post <- c("cl1_fc26" , "cl1_fc28",  "cl1_fc29", "cl1_fc37" ,"cl1_fc38" , "cl1_fc39",  "cl1_fc42", 
                "cl1_fc44", "cl11_fcmc26" , "cl11_fcmc27", "cl11_fcmc28" ,
                "cl11_fcmc29", "cl11_fcmc31" , "cl11_fcmc38", "cl11_fcsa3a" , "cl11_fcsa3b" ,          
                "cl11_fcsa3c", "cl11_fcsa4_1"  , "cl11_fcsa4_2"  ,"cl11_fcsa4_3" ,         
                "cl11_fcsa4_4", "cl11_fcsa5a"   , "cl11_fcsa5b" , "cl11_fcsa5c",           
                "cl11_fcsa6a" , "cl11_fcsa6b"   ,  "cl11_fcsa7a"  , "cl11_fcsa7b",           
                "cl11_fcsa7c"  ,"cl11_fcsa7d"  ,"cl11_fcsa7e"  , "cl11_fcsa7f",           
                "cl11_fcsa8a"  ,"cl11_fcsa8b","cl11_fcsa8c" ,"cl11_fcsa8d",           
                "cl11_fcsa8e","cl11_fcsa8f")


## JHU: Genetics020.330

brJHU.y1 <-c("cl2_me1_1",           "cl2_me1_2"  ,         "cl2_me1_3"  ,            
           "cl2_me1_4"    ,             "cl2_me1_5"  ,         "cl2_me1_6"  ,           "cl2_me1_7"  ,            
           "cl2_me1_8"    ,             "cl2_me1_9"  ,         "cl2_me1_10" ,           "cl2_me1_11" ,           
           "cl2_me1_12"   ,             "cl2_me2_1"  ,         "cl2_me2_2"  ,           "cl2_me2_3"  ,            
           "cl2_me2_4"    ,             "cl2_me2_5"  ,         "cl2_me2_6"  ,           "cl2_me2_7"  ,           
           "cl2_me2_8"    ,             "cl2_me2_9"  ,         "cl2_me2_10" ,           "cl2_me2_11" ,          
           "cl2_me3_1"    ,             "cl2_me3_2"  ,         "cl2_me3_3"  ,           "cl2_me3_4"  ,          
           "cl2_me3_5"    ,             "cl2_me3_6"  ,         "cl2_me3_7"  ,           "cl2_me3_8")


brJHU.post <-c("cl2_me3_3arev",	"cl2_me3_3brev",	"cl2_me3_7arev",	"cl2_me3_7brev", 
             "cl12_me1_1"   ,             "cl12_me1_2" ,         "cl12_me1_3" ,           "cl12_me1_4" ,          
             "cl12_me1_5"   ,             "cl12_me1_6" ,          
             "cl12_me1_7"   ,             "cl12_me1_8" ,         "cl12_me1_9"  ,          "cl12_me1_10" ,          
             "cl12_me1_11"  ,             "cl12_me1_12",         "cl12_me2_1"  ,          "cl12_me2_2"  ,         
             "cl12_me2_3"   ,             "cl12_me2_4" ,         "cl12_me2_5"  ,          "cl12_me2_6"  ,        
             "cl12_me2_7"   ,             "cl12_me2_8" ,         "cl12_me2_9"  ,          "cl12_me2_10" ,         
             "cl12_me2_11"  ,             "cl12_me3_1" ,         "cl12_me3_2"  ,          "cl12_me3_3"  ,         
             "cl12_me3_4"   ,             "cl12_me3_5" ,         "cl12_me3_6"  ,          "cl12_me3_9")


## METRO: BIO3600

brMetro.y1.cl3 <- c("cl3_fc10"  ,     "cl3_fc11"   ,            "cl3_fc12"    ,           "cl3_fc13" ,             
                  "cl3_fc14"          ,     "cl3_fc15"   ,            "cl3_fc16"    ,           "cl3_fc17" ,             
                  "cl3_fc18"         ,      "cl3_fc19"   ,            "cl3_fc20"    ,           "cl3_fc21" ,             
                  "cl3_fc22"         ,      "cl3_fc23"   ,            "cl3_fc24"   ,            "cl3_fc25" ,             
                  "cl3_fc26"         ,      "cl3_fc27"   ,            "cl3_fc28"    ,           "cl3_fc29" ,             
                  "cl3_fc30"         ,      "cl3_fc31"   ,            "cl3_fc32")

brMetro.y1.cl8 <- c("cl8_fc3",                "cl8_fc4"    ,           
                  "cl8_fc5"          ,      "cl8_fc10"   ,            "cl8_fc11"   ,            "cl8_fc14" ,             
                  "cl8_fc17"         ,      "cl8_fc20"   ,            "cl8_fc22")

brMetro.post.cl13 <- c("cl13_fc6"   ,            "cl13_fc7"   ,       "cl13_fc8" ,             
                     "cl13_fc11"        ,      "cl13_fc12"  ,            "cl13_fc13"  ,       "cl13_fc14")

brMetro.post.cl18 <- c("cl13_fc2",                "cl13_sa1"   ,      
                     "cl13_sa2"   ,            "cl13_sa3"   ,           "cl18_fc1" ,
                     "cl18_fc2"         ,      "cl18_fc3"   ,            "cl18_fc9"   ,            "cl18_fc16" ,            
                     "cl18_fc17")


## ST MARYS: BL2330

brSTMU.y1.cl4 <- c("cl4_fcsa2a"    ,         "cl4_fcsa2b"     ,         "cl4_fcsa6a"  ,       "cl4_fcsa6b",            
                 "cl4_fcsa7"   ,           "cl4_fcsa8a"   ,          "cl4_fcsa8b"    ,          
                 "cl4_fcsa10a" ,           "cl4_fcsa10b" ,         "cl4_fcsa10c"     ,       "cl4_fcp1a"     ,        
                 "cl4_fcp1b"   ,           "cl4_fcp1c"   ,           "cl4_fcp1d"        ,          
                 "cl4_fcp3"    ,           "cl4_fcp4a"   ,           "cl4_fcp4b"     ,         "cl4_fcp5a"   ,          
                 "cl4_fcp5b"   ,           "cl4_fcp5c"   ,           "cl4_fcp5d"     ,         "cl4_fcp5e")

brSTMU.y1.cl9 <- c("cl9_fcsa4"   ,             "cl9_fcsa7" ,            
                 "cl9_fcp1"    ,           "cl9_fcp3"    ,           "cl9_fcp4"      ,         "cl9_fcp5"  ,            
                 "cl9_fcp6"    ,           "cl9_fcp7")

brSTMU.post.cl14 <- c("cl4_fcsa1"   ,       "cl4_fcsa3a"   ,          
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

brSTMU.post.cl19 <- c("cl9_fcsa2",              "cl9_fcsa3"   ,          "cl9_fcsa5"     ,           
                    "cl19_fcsa6a" ,           "cl19_fcsa6b" ,           "cl19_fcsa6c")


## UCD: BIOL3832

brUCD.y1.cl5 <- c("cl5_me4_1",             "cl5_me4_2",        	     "cl5_me4_3",   	         "cl5_me4_4",
                "cl5_me4_5")

brUCD.y1.cl6 <- c("cl6_me4_1",	             "cl6_me4_2",      	       "cl6_me4_3",
                "cl6_me4_4",               "cl6_me4_5")

brUCD.post <- c("cl5_me1_3a",             "cl5_me1_3b" ,            
              "cl5_me1_3c" ,            "cl5_me1_9a" ,            "cl5_me1_9b"  ,           "cl5_me3_4a" ,            
              "cl5_me3_4b")


## UGA: GENE3200

brUGA.y1 <- c("cl10_me1_44"  ,     "cl10_me2_39"  ,          "cl10_me2_40"  ,         
            "cl10_me3_tf2"   ,        "cl10_me3_mc12" ,         "cl10_me3_sa7" ,          "cl10_me4_tf1" ,         
            "cl10_me4_mc17"  ,        "cl10_me4_sa1" ,          "cl10_me4_sa4" ,          "cl10_me4_sa6" ,         
            "cl10_me5_tf20"  ,        "cl10_me5_mc29" ,         "cl10_me5_sa3" ,          "cl10_me6_20"  ,         
            "cl10_me6_25"    ,        "cl10_me6_32"   ,         "cl10_me6_41"  ,          "cl10_me6_46"  ,         
            "cl10_me6_49") 


brUGA.post <- c("cl10_me1_2") 


brCU.JK.y1 <- paste(brCU.JK.y1, "CUJK_ord", sep = "_")
brCU.JK.post <- paste(brCU.JK.post, "CUJK_ord", sep = "_")
brCU.KK.y1 <- paste(brCU.KK.y1, "CUKK_ord", sep = "_")
brCU.KK.post <- paste(brCU.KK.post, "CUKK_ord", sep = "_")
brJHU.y1 <- paste(brJHU.y1, "JHU_ord", sep = "_")
brJHU.post <- paste(brJHU.post, "JHU_ord", sep = "_")
brMetro.y1.cl3 <- paste(brMetro.y1.cl3, "Metro_ord", sep = "_")
brMetro.y1.cl8 <- paste(brMetro.y1.cl8, "Metro_ord", sep = "_")
brMetro.post.cl13 <- paste(brMetro.post.cl13, "Metro_ord", sep = "_")
brMetro.post.cl18 <- paste(brMetro.post.cl18, "Metro_ord", sep = "_")
brSTMU.y1.cl4 <- paste(brSTMU.y1.cl4, "STMU_ord", sep = "_")
brSTMU.y1.cl9 <- paste(brSTMU.y1.cl9, "STMU_ord", sep = "_")
brSTMU.post.cl14 <- paste(brSTMU.post.cl14, "STMU_ord", sep = "_")
brSTMU.post.cl19 <- paste(brSTMU.post.cl19, "STMU_ord", sep = "_")
brUCD.y1.cl5 <- paste(brUCD.y1.cl5, "UCD_ord", sep = "_")
brUCD.y1.cl6 <- paste(brUCD.y1.cl6, "UCD_ord", sep = "_")
brUCD.post <- paste(brUCD.post, "UCD_ord", sep = "_")
brUGA.y1 <- paste(brUGA.y1, "UGA_ord", sep = "_")
brUGA.post <- paste(brUGA.post, "UGA_ord", sep = "_")



br_item_groups <- c(brCU.JK.y1, brCU.JK.post, brCU.KK.y1, brCU.KK.post, 
                    brJHU.y1, brJHU.post, brMetro.y1.cl3, brMetro.y1.cl8, brMetro.post.cl13,
                    brMetro.post.cl18, brSTMU.y1.cl4, brSTMU.y1.cl9, brSTMU.post.cl14, brSTMU.post.cl19,
                    brUCD.y1.cl5, brUCD.y1.cl6, brUCD.post, brUGA.y1, brUGA.post)


# getting reliability estimates for each year 1
CU.JK.y1.reliability <- reliability(g_ord_y1[g_ord_y1$cl==7 & g_ord_y1$post==1, CU.JK.y1], NA.Delete = T)
CU.JK.y1.rel <- round(CU.JK.y1.reliability[[3]],2)
CU.JK.y1.itcor <- round(CU.JK.y1.reliability[[7]],2)
CU.JK.y1.nPer <- CU.JK.y1.reliability[[2]]
CU.JK.y1.nIt <- CU.JK.y1.reliability[[1]]

CU.KK.y1.reliability <- reliability(g_ord_y1[g_ord_y1$cl==1  & g_ord_y1$post==1, CU.KK.y1], NA.Delete = T)
CU.KK.y1.rel <- round(CU.KK.y1.reliability[[3]],2)
CU.KK.y1.itcor <- round(CU.KK.y1.reliability[[7]],2)
CU.KK.y1.nPer <- CU.KK.y1.reliability[[2]]
CU.KK.y1.nIt <- CU.KK.y1.reliability[[1]]

JHU.y1.reliability <- reliability(g_ord_y1[g_ord_y1$cl==2 & g_ord_y1$post==1, JHU.y1], NA.Delete = T)
JHU.y1.rel <- round(JHU.y1.reliability[[3]],2)
JHU.y1.itcor <- round(JHU.y1.reliability[[7]],2)
JHU.y1.nPer <- JHU.y1.reliability[[2]]
JHU.y1.nIt <- JHU.y1.reliability[[1]]

Metro.y1.cl3.reliability <- reliability(g_ord_y1[g_ord_y1$cl==3 & g_ord_y1$post==1, Metro.y1.cl3], NA.Delete = T)
Metro.y1.cl3.rel <- round(Metro.y1.cl3.reliability[[3]],2)
Metro.y1.cl3.itcor <- round(Metro.y1.cl3.reliability[[7]],2)
Metro.y1.cl3.nPer <- Metro.y1.cl3.reliability[[2]]
Metro.y1.cl3.nIt <- Metro.y1.cl3.reliability[[1]]

Metro.y1.cl8.reliability <- reliability(g_ord_y1[g_ord_y1$cl==8 & g_ord_y1$post==1, Metro.y1.cl8], NA.Delete = T)
Metro.y1.cl8.rel <- round(Metro.y1.cl8.reliability[[3]],2)
Metro.y1.cl8.itcor <- round(Metro.y1.cl8.reliability[[7]],2)
Metro.y1.cl8.nPer <- Metro.y1.cl8.reliability[[2]]
Metro.y1.cl8.nIt <- Metro.y1.cl8.reliability[[1]]

STMU.y1.cl4.reliability <- reliability(g_ord_y1[g_ord_y1$cl==4 & g_ord_y1$post==1, STMU.y1.cl4], NA.Delete = T)
STMU.y1.cl4.rel <- round(STMU.y1.cl4.reliability[[3]],2)
STMU.y1.cl4.itcor <- round(STMU.y1.cl4.reliability[[7]],2)
STMU.y1.cl4.nPer <- STMU.y1.cl4.reliability[[2]]
STMU.y1.cl4.nIt <- STMU.y1.cl4.reliability[[1]]

STMU.y1.cl9.reliability <- reliability(g_ord_y1[g_ord_y1$cl==9 & g_ord_y1$post==1, STMU.y1.cl9], NA.Delete = T)
STMU.y1.cl9.rel <- round(STMU.y1.cl9.reliability[[3]],2)
STMU.y1.cl9.itcor <- round(STMU.y1.cl9.reliability[[7]],2)
STMU.y1.cl9.nPer <- STMU.y1.cl9.reliability[[2]]
STMU.y1.cl9.nIt <- STMU.y1.cl9.reliability[[1]]

UCD.y1.cl5.reliability <- reliability(g_ord_y1[g_ord_y1$cl==5 & g_ord_y1$post==1, UCD.y1.cl5], NA.Delete = T)
UCD.y1.cl5.rel <- round(UCD.y1.cl5.reliability[[3]],2)
UCD.y1.cl5.itcor <- round(UCD.y1.cl5.reliability[[7]],2)
UCD.y1.cl5.nPer <- UCD.y1.cl5.reliability[[2]]
UCD.y1.cl5.nIt <- UCD.y1.cl5.reliability[[1]]

UCD.y1.cl6.reliability <- reliability(g_ord_y1[g_ord_y1$cl==6 & g_ord_y1$post==1, UCD.y1.cl6], NA.Delete = T)
UCD.y1.cl6.rel <- round(UCD.y1.cl6.reliability[[3]],2)
UCD.y1.cl6.itcor <- round(UCD.y1.cl6.reliability[[7]],2)
UCD.y1.cl6.nPer <- UCD.y1.cl6.reliability[[2]]
UCD.y1.cl6.nIt <- UCD.y1.cl6.reliability[[1]]

UGA.y1.reliability <- reliability(g_ord_y1[g_ord_y1$cl==10 & g_ord_y1$post==1, UGA.y1], NA.Delete = T)
UGA.y1.rel <- round(UGA.y1.reliability[[3]],2)
UGA.y1.itcor <- round(UGA.y1.reliability[[7]],2)
UGA.y1.nPer <- UGA.y1.reliability[[2]]
UGA.y1.nIt <- UGA.y1.reliability[[1]]


# getting item reliability for post
CU.JK.post.reliability <- reliability(g_ord_y2[g_ord_y2$cl==17 & g_ord_y2$post==1, CU.JK.post], NA.Delete = T)
CU.JK.post.rel <- round(CU.JK.post.reliability[[3]],2)
CU.JK.post.itcor <- round(CU.JK.post.reliability[[7]])
CU.JK.post.nPer <- CU.JK.post.reliability[[2]]
CU.JK.post.nIt <- CU.JK.post.reliability[[1]]

CU.KK.post.reliability <- reliability(g_ord_y2[g_ord_y2$cl==11 & g_ord_y2$post==1, CU.KK.post], NA.Delete = T)
CU.KK.post.rel <- round(CU.KK.post.reliability[[3]],2)
CU.KK.post.itcor <- round(CU.KK.post.reliability[[7]],2)
CU.KK.post.nPer <- CU.KK.post.reliability[[2]]
CU.KK.post.nIt <- CU.KK.post.reliability[[1]]

JHU.post.reliability <- reliability(g_ord_y2[g_ord_y2$cl==12 & g_ord_y2$post==1, JHU.post], NA.Delete = T)
JHU.post.rel <- round(JHU.post.reliability[[3]],2)
JHU.post.itcor <- round(JHU.post.reliability[[7]],2)
JHU.post.nPer <- JHU.post.reliability[[2]]
JHU.post.nIt <- JHU.post.reliability[[1]]

Metro.post.cl13.reliability <- reliability(g_ord_y2[g_ord_y2$cl==13 & g_ord_y2$post==1, Metro.post.cl13], NA.Delete = T)
Metro.post.cl13.rel <- round(Metro.post.cl13.reliability[[3]],2)
Metro.post.cl13.itcor <- round(Metro.post.cl13.reliability[[7]],2)
Metro.post.cl13.nPer <- Metro.post.cl13.reliability[[2]]
Metro.post.cl13.nIt <- Metro.post.cl13.reliability[[1]]

Metro.post.cl18.reliability <- reliability(g_ord_y2[g_ord_y2$cl==18 & g_ord_y2$post==1, Metro.post.cl18], NA.Delete = T)
Metro.post.cl18.rel <- round(Metro.post.cl18.reliability[[3]],2)
Metro.post.cl18.itcor <- round(Metro.post.cl18.reliability[[7]],2)
Metro.post.cl18.nPer <- Metro.post.cl18.reliability[[2]]
Metro.post.cl18.nIt <- Metro.post.cl18.reliability[[1]]

STMU.post.cl14.reliability <- reliability(g_ord_y2[g_ord_y2$cl==14 & g_ord_y2$post==1, STMU.post.cl14], NA.Delete = T)
STMU.post.cl14.rel <- round(STMU.post.cl14.reliability[[3]],2)
STMU.post.cl14.itcor <- round(STMU.post.cl14.reliability[[7]],2)
STMU.post.cl14.nPer <- STMU.post.cl14.reliability[[2]]
STMU.post.cl14.nIt <- STMU.post.cl14.reliability[[1]]

STMU.post.cl19.reliability <- reliability(g_ord_y2[g_ord_y2$cl==19 & g_ord_y2$post==1, STMU.post.cl19], NA.Delete = T)
STMU.post.cl19.rel <- round(STMU.post.cl19.reliability[[3]],2)
STMU.post.cl19.itcor <- round(STMU.post.cl19.reliability[[7]],2)
STMU.post.cl19.nPer <- STMU.post.cl19.reliability[[2]]
STMU.post.cl19.nIt <- STMU.post.cl19.reliability[[1]]

UCD.post.reliability <- reliability(g_ord_y2[g_ord_y2$cl==15 & g_ord_y2$post==1, UCD.post], NA.Delete = T)
UCD.post.rel <- round(UCD.post.reliability[[3]],2)
UCD.post.itcor <- round(UCD.post.reliability[[7]],2)
UCD.post.nPer <- UCD.post.reliability[[2]]
UCD.post.nIt <- UCD.post.reliability[[1]]

UGA.post.reliability <- reliability(g_ord_y2[g_ord_y2$cl==20 & g_ord_y2$post==1, UGA.post], NA.Delete = T)
UGA.post.rel <- round(UGA.post.reliability[[3]],2)
UGA.post.itcor <- round(UGA.post.reliability[[7]],2)
UGA.post.nPer <- UGA.post.reliability[[2]]
UGA.post.nIt <- UGA.post.reliability[[1]]


# getting nPerson
brCU.JK.y1_nPer <- rep(CU.JK.y1.nPer, times=length(brCU.JK.y1))
brCU.JK.post_nPer <- rep(CU.JK.post.nPer, times=length(brCU.JK.post))
brCU.KK.y1_nPer <- rep(CU.KK.y1.nPer, times=length(brCU.KK.y1))
brCU.KK.post_nPer <- rep(CU.KK.post.nPer, times=length(brCU.KK.post))
brJHU.y1_nPer <- rep(JHU.y1.nPer, times=length(brJHU.y1))
brJHU.post_nPer <- rep(JHU.post.nPer, times=length(brJHU.post))
brMetro.y1.cl3_nPer <- rep(Metro.y1.cl3.nPer, times=length(brMetro.y1.cl3))
brMetro.y1.cl8_nPer <- rep(Metro.y1.cl8.nPer, times=length(brMetro.y1.cl8))
brMetro.post.cl13_nPer <- rep(Metro.post.cl13.nPer, times=length(brMetro.post.cl13))
brMetro.post.cl18_nPer <- rep(Metro.post.cl18.nPer, times=length(brMetro.post.cl18))
brSTMU.y1.cl4_nPer <- rep(STMU.y1.cl4.nPer, times=length(brSTMU.y1.cl4))
brSTMU.y1.cl9_nPer <- rep(STMU.y1.cl9.nPer, times=length(brSTMU.y1.cl9))
brSTMU.post.cl14_nPer <- rep(STMU.post.cl14.nPer, times=length(brSTMU.post.cl14))
brSTMU.post.cl19_nPer <- rep(STMU.post.cl19.nPer, times=length(brSTMU.post.cl19))
brUCD.y1.cl5_nPer <- rep(UCD.y1.cl5.nPer, times=length(brUCD.y1.cl5))
brUCD.y1.cl6_nPer <- rep(UCD.y1.cl6.nPer, times=length(brUCD.y1.cl6))
brUCD.post_nPer <- rep(UCD.post.nPer, times=length(brUCD.post))
brUGA.y1_nPer <- rep(UGA.y1.nPer, times=length(brUGA.y1))
brUGA.post_nPer <- rep(UGA.post.nPer, times=length(brUGA.post))

nPer <- c(brCU.JK.y1_nPer, brCU.JK.post_nPer, brCU.KK.y1_nPer, brCU.KK.post_nPer, brJHU.y1_nPer,
          brJHU.post_nPer, brMetro.y1.cl3_nPer, brMetro.y1.cl8_nPer, brMetro.post.cl13_nPer,
          brMetro.post.cl18_nPer, brSTMU.y1.cl4_nPer, brSTMU.y1.cl9_nPer, brSTMU.post.cl14_nPer,
          brSTMU.post.cl19_nPer, brUCD.y1.cl5_nPer, brUCD.y1.cl6_nPer, brUCD.post_nPer, brUGA.y1_nPer,
          brUGA.post_nPer)

# getting nItems
brCU.JK.y1_nIt <- rep(CU.JK.y1.nIt, times=length(brCU.JK.y1))
brCU.JK.post_nIt <- rep(CU.JK.post.nIt, times=length(brCU.JK.post))
brCU.KK.y1_nIt <- rep(CU.KK.y1.nIt, times=length(brCU.KK.y1))
brCU.KK.post_nIt <- rep(CU.KK.post.nIt, times=length(brCU.KK.post))
brJHU.y1_nIt <- rep(JHU.y1.nIt, times=length(brJHU.y1))
brJHU.post_nIt <- rep(JHU.post.nIt, times=length(brJHU.post))
brMetro.y1.cl3_nIt <- rep(Metro.y1.cl3.nIt, times=length(brMetro.y1.cl3))
brMetro.y1.cl8_nIt <- rep(Metro.y1.cl8.nIt, times=length(brMetro.y1.cl8))
brMetro.post.cl13_nIt <- rep(Metro.post.cl13.nIt, times=length(brMetro.post.cl13))
brMetro.post.cl18_nIt <- rep(Metro.post.cl18.nIt, times=length(brMetro.post.cl18))
brSTMU.y1.cl4_nIt <- rep(STMU.y1.cl4.nIt, times=length(brSTMU.y1.cl4))
brSTMU.y1.cl9_nIt <- rep(STMU.y1.cl9.nIt, times=length(brSTMU.y1.cl9))
brSTMU.post.cl14_nIt <- rep(STMU.post.cl14.nIt, times=length(brSTMU.post.cl14))
brSTMU.post.cl19_nIt <- rep(STMU.post.cl19.nIt, times=length(brSTMU.post.cl19))
brUCD.y1.cl5_nIt <- rep(UCD.y1.cl5.nIt, times=length(brUCD.y1.cl5))
brUCD.y1.cl6_nIt <- rep(UCD.y1.cl6.nIt, times=length(brUCD.y1.cl6))
brUCD.post_nIt <- rep(UCD.post.nIt, times=length(brUCD.post))
brUGA.y1_nIt <- rep(UGA.y1.nIt, times=length(brUGA.y1))
brUGA.post_nIt <- rep(UGA.post.nIt, times=length(brUGA.post))

nIt <- c(brCU.JK.y1_nIt, brCU.JK.post_nIt, brCU.KK.y1_nIt, brCU.KK.post_nIt, brJHU.y1_nIt, brJHU.post_nIt,
         brMetro.y1.cl3_nIt, brMetro.y1.cl8_nIt, brMetro.post.cl13_nIt, brMetro.post.cl18_nIt, brSTMU.y1.cl4_nIt,
         brSTMU.y1.cl9_nIt, brSTMU.post.cl14_nIt, brSTMU.post.cl19_nIt, brUCD.y1.cl5_nIt, brUCD.y1.cl6_nIt, 
         brUCD.post_nIt, brUGA.y1_nIt, brUGA.post_nIt)

# getting best reliabilities
brCU.JK.y1_rel <- rep(CU.JK.y1.rel, times=length(brCU.JK.y1))
brCU.JK.post_rel <- rep(CU.JK.post.rel, times=length(brCU.JK.post))
brCU.KK.y1_rel <- rep(CU.KK.y1.rel, times=length(brCU.KK.y1))
brCU.KK.post_rel <- rep(CU.KK.post.rel, times=length(brCU.KK.post))
brJHU.y1_rel <- rep(JHU.y1.rel, times=length(brJHU.y1))
brJHU.post_rel <- rep(JHU.post.rel, times=length(brJHU.post))
brMetro.y1.cl3_rel <- rep(Metro.y1.cl3.rel, times=length(brMetro.y1.cl3))
brMetro.y1.cl8_rel <- rep(Metro.y1.cl8.rel, times=length(brMetro.y1.cl8))
brMetro.post.cl13_rel <- rep(Metro.post.cl13.rel, times=length(brMetro.post.cl13))
brMetro.post.cl18_rel <- rep(Metro.post.cl18.rel, times=length(brMetro.post.cl18))
brSTMU.y1.cl4_rel <- rep(STMU.y1.cl4.rel, times=length(brSTMU.y1.cl4))
brSTMU.y1.cl9_rel <- rep(STMU.y1.cl9.rel, times=length(brSTMU.y1.cl9))
brSTMU.post.cl14_rel <- rep(STMU.post.cl14.rel, times=length(brSTMU.post.cl14))
brSTMU.post.cl19_rel <- rep(STMU.post.cl19.rel, times=length(brSTMU.post.cl19))
brUCD.y1.cl5_rel <- rep(UCD.y1.cl5.rel, times=length(brUCD.y1.cl5))
brUCD.y1.cl6_rel <- rep(UCD.y1.cl6.rel, times=length(brUCD.y1.cl6))
brUCD.post_rel <- rep(UCD.post.rel, times=length(brUCD.post))
brUGA.y1_rel <- rep(UGA.y1.rel, times=length(brUGA.y1))
brUGA.post_rel <- rep(UGA.post.rel, times=length(brUGA.post))


br <- c(brCU.JK.y1_rel, brCU.JK.post_rel, brCU.KK.y1_rel, brCU.KK.post_rel, brJHU.y1_rel, brJHU.post_rel,
        brMetro.y1.cl3_rel, brMetro.y1.cl8_rel, brMetro.post.cl13_rel, brMetro.post.cl18_rel, brSTMU.y1.cl4_rel,
        brSTMU.y1.cl9_rel, brSTMU.post.cl14_rel, brSTMU.post.cl19_rel, brUCD.y1.cl5_rel, brUCD.y1.cl6_rel, 
        brUCD.post_rel, brUGA.y1_rel, brUGA.post_rel)

# getting best item-total correlations 
brCU.JK.y1_itcor <- CU.JK.y1.itcor
brCU.JK.post_itcor <- CU.JK.post.itcor
brCU.KK.y1_itcor <- CU.KK.y1.itcor
brCU.KK.post_itcor <- CU.KK.post.itcor
brJHU.y1_itcor <- JHU.y1.itcor
brJHU.post_itcor <- JHU.post.itcor
brMetro.y1.cl3_itcor <- Metro.y1.cl3.itcor
brMetro.y1.cl8_itcor <- Metro.y1.cl8.itcor
brMetro.post.cl13_itcor <- Metro.post.cl13.itcor
brMetro.post.cl18_itcor <- Metro.post.cl18.itcor
brSTMU.y1.cl4_itcor <- STMU.y1.cl4.itcor
brSTMU.y1.cl9_itcor <- STMU.y1.cl9.itcor
brSTMU.post.cl14_itcor <- STMU.post.cl14.itcor
brSTMU.post.cl19_itcor <- STMU.post.cl19.itcor
brUCD.y1.cl5_itcor <- UCD.y1.cl5.itcor
brUCD.y1.cl6_itcor <- UCD.y1.cl6.itcor
brUCD.post_itcor <- UCD.post.itcor
brUGA.y1_itcor <- UGA.y1.itcor
brUGA.post_itcor <- UGA.post.itcor

allcorr <- c(brCU.JK.y1_itcor, brCU.JK.post_itcor, brCU.KK.y1_itcor, brCU.KK.post_itcor, brJHU.y1_itcor, brJHU.post_itcor,
           brMetro.y1.cl3_itcor, brMetro.y1.cl8_itcor, brMetro.post.cl13_itcor, brMetro.post.cl18_itcor, brSTMU.y1.cl4_itcor,
           brSTMU.y1.cl9_itcor, brSTMU.post.cl14_itcor, brSTMU.post.cl19_itcor, brUCD.y1.cl5_itcor, brUCD.y1.cl6_itcor,
           brUCD.post_itcor, brUGA.y1_itcor, brUGA.post_itcor)

# creating df to hold corrs
item_cor_sort <- data.frame(matrix(ncol=2,nrow=456)) 
colnames(item_cor_sort) <- c("Item", "PtBis")
item_cor_sort$Item <- Reliability_sets
item_cor_sort$PtBis <- allcorr

# these corrs are for all items, not those selected for best
# need to determine which corr is on best list:
# making a variable that tells if the item is a "best" item
mCU.JK.y1 <- CU.JK.y1 %in% brCU.JK.y1
mCU.JK.post <- CU.JK.post %in% brCU.JK.post
mCU.KK.y1 <- CU.KK.y1 %in% brCU.KK.y1
mCU.KK.post <- CU.KK.post %in% brCU.KK.post
mJHU.y1 <- JHU.y1 %in% brJHU.y1
mJHU.post <- JHU.post %in% brJHU.post
mMetro.y1.cl3 <- Metro.y1.cl3 %in% brMetro.y1.cl3
mMetro.y1.cl8 <- Metro.y1.cl8 %in% brMetro.y1.cl8
mMetro.post.cl13 <- Metro.post.cl13 %in% brMetro.post.cl13
mMetro.post.cl18 <- Metro.post.cl18 %in% brMetro.post.cl18
mSTMU.y1.cl4 <- STMU.y1.cl4 %in% brSTMU.y1.cl4
mSTMU.y1.cl9 <- STMU.y1.cl9 %in% brSTMU.y1.cl9
mSTMU.post.cl14 <- STMU.post.cl14 %in% brSTMU.post.cl14
mSTMU.post.cl19 <- STMU.post.cl19 %in% brSTMU.post.cl19
mUCD.y1.cl5 <- UCD.y1.cl5 %in% brUCD.y1.cl5
mUCD.y1.cl6 <- UCD.y1.cl6 %in% brUCD.y1.cl6
mUCD.post <- UCD.post %in% brUCD.post
mUGA.y1 <- UGA.y1 %in% brUGA.y1
mUGA.post <- UGA.post %in% brUGA.post

btest <- c(mCU.JK.y1, mCU.JK.post, mCU.KK.y1, mCU.KK.post, mJHU.y1, mJHU.post, mMetro.y1.cl3,
           mMetro.y1.cl8, mMetro.post.cl13, mMetro.post.cl18, mSTMU.y1.cl4, mSTMU.y1.cl9, 
           mSTMU.post.cl14, mSTMU.post.cl19, mUCD.y1.cl5, mUCD.y1.cl6, mUCD.post, mUGA.y1, mUGA.post)
  
# adding variable to df
item_cor_sort$btest <- btest

#subsetting df to have only "best" items
item_cor_sort <- item_cor_sort[item_cor_sort$btest==T,]

# adding data on correlations to best reliability df
# this df is needed because the data is not sorted in a way that matches main df
best_rel <- data.frame(matrix(ncol=5,nrow=338)) 
colnames(best_rel) <- c("Item","nPersons", "nItems", "Rel", "PtBis")
best_rel$Item <- br_item_groups
best_rel$nPersons <- nPer
best_rel$nItems <- nIt
best_rel$Rel <- br
best_rel$PtBis <- item_cor_sort$PtBis 

# ordering by item so that reliability data can be matched up with local_items_freq df
best_rel <- best_rel[order(best_rel$Item),]

# adding best reliabilities to main df
local_items_freq$nPersons <- best_rel$nPersons
local_items_freq$nItems <- best_rel$nItems
local_items_freq$Best_alpha <- best_rel$Rel
local_items_freq$PtBis <- best_rel$PtBis

# exporting tables to Excel
setwd("./Keck Tables and Figures")
write.csv(format(local_items_freq, digits=2, nsmall=2),"Local_Items_Stats.csv") 

