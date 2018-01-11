# Richard Noone
# Keck Project
# Max possible on tests
# packages used: dplyr
# 12-01-2017
# Purpose:
#------------------------------------------------------------------
# this script will create a spreadsheet to indicate the maximum 
# possible points on each test for each institution by time period
# given Derek's recoding and exclusions in lines 1-376 of "2b_recode_data.R"

setwd("~/Dropbox/Github/Keck/Analysis Data")

# Keck_local_item is the result of Derek's recoding and exclusions
g<-read.csv("Keck_local_item_data.csv")
#str(g)
#head(g)
#colnames(g)
# Derek's used items:

CU.KK <- c("cl1_fc26" , "cl1_fc27","cl1_fc28",              
           "cl1_fc29" ,"cl1_fc30", "cl1_fc32" ,             
           "cl1_fc33", "cl1_fc34" ,  "cl1_fc35"  , "cl1_fc36",         
           "cl1_fc37" ,"cl1_fc38" , "cl1_fc39" ,  "cl1_fc40" , 
           "cl7_fc38","cl11_fcmc26" , "cl11_fcmc27", "cl11_fcmc28" ,
           "cl11_fcmc29", "cl11_fcmc31" , "cl11_fcmc38","cl1_fc42","cl1_fc43",
           "cl11_fcsa3a","cl11_fcsa3b","cl11_fcsa4_1", "cl11_fcsa4_2","cl11_fcsa4_3",
           "cl11_fcsa4_4", "cl11_fcsa5", "cl11_fcsa6a" , "cl11_fcsa6b","cl11_fcsa7a"  , "cl11_fcsa7b",           
           "cl11_fcsa7c"  ,"cl11_fcsa7d"  ,"cl11_fcsa7e" , "cl11_fcsa7f",           
           "cl11_fcsa8a"  ,"cl11_fcsa8b","cl11_fcsa8c" ,"cl11_fcsa8d",           
           "cl11_fcsa8e","cl11_fcsa8f")


CU.JK <- c("cl7_fc26", "cl7_fc27", "cl7_fc28", "cl7_fc29", "cl7_fc30", "cl7_fc31", "cl7_fc32",
           "cl7_fc33","cl7_fc34", "cl7_fc35", "cl7_fc36", "cl7_fc37", "cl7_fc38", "cl7_fc39",              
           "cl7_fc40","cl7_fcsa1a",  "cl7_fcsa1c","cl7_fcsa3b","cl7_fcsa4a", "cl7_fcsa5a",  "cl7_fcsa6a",
           "cl7_fcsa6c", "cl7_fcsa6d", "cl7_fcsa6e", "cl11_fcmc26", "cl11_fcmc27",
           "cl17_fcsa2a", "cl17_fcsa2b", "cl17_fcsa2c", "cl17_fcsa2d", 
           "cl17_fcmc3", "cl17_fcmc4" , "cl17_fcsa1a" , "cl17_fcsa1b" , "cl17_fcsa1c","cl17_fcsa1d" , 
           "cl17_fcsa1e", "cl17_fcsa1f" , "cl17_fcsa3a", "cl7_fcsa5c", "cl7_fcsa6b",
           "cl7_fcsa1b", "cl7_fcsa1d", "cl7_fcsa2a",
           "cl7_fcsa2b","cl7_fcsa2c", "cl7_fcsa3a", "cl7_fcsa3d", "cl7_fcsa5b", "cl17_fcsa3c",
           "cl7_fcsa3c", "cl7_fcsa4b", "cl17_fcsa3b")     


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
          "cl18_fc1", "cl13_sa1", "cl13_sa2", "cl13_sa3") # missing from original 


STMU<-c( "cl4_fcsa2a",    "cl4_fcsa3a"   ,  "cl4_fcsa6a"  ,"cl4_fcsa6b",            
         "cl4_fcsa8a"   ,  "cl4_fcsa10c"  , "cl4_fcp1b"   ,    "cl4_fcp1d"  ,        
         "cl4_fcp4b" ,   "cl4_fcp5b"   , "cl4_fcp5e" ,     "cl9_fcsa7" , 
         "cl4_fcp5c",  
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
         "cl9_fcsa5" ,    "cl9_fcp1"    ,   "cl9_fcp3", "cl9_fcp6", "cl9_fcp7"    ,
         "cl14_fcp1"   ,   "cl14_fcp2a"  ,   "cl14_fcp2b"    , "cl14_fcp2c"  ,  
         "cl14_fcp2d"  ,   "cl14_fcp2e"  ,    "cl14_fcp2f"    , "cl14_fcp3"   ,  
         "cl14_fcp4a"  ,  "cl14_fcp5", "cl14_fcp4b", "cl4_fcsa9") 


UCD<-c("cl5_me1_3a","cl5_me1_3b" ,"cl5_me1_3c" , "cl5_me1_9a", "cl5_me1_9b", 
       "cl5_me3_4a", "cl5_me3_4b","cl5_me4_2","cl5_me4_3", "cl5_me4_4", "cl5_me4_5")


UGA<-c("cl10_me1_2" ,"cl10_me1_30", "cl10_me2_2"  ,           
       "cl10_me2_11"  , "cl10_me2_17" , "cl10_me2_20"  ,  "cl10_me2_25"  ,        
       "cl10_me2_28"  ,  "cl10_me2_32"  , "cl10_me2_33" ,  "cl10_me3_tf2",       
       "cl10_me3_mc12" , "cl10_me4_tf1" , "cl10_me4_mc17"  ,  "cl10_me5_tf20"  ,       
       "cl10_me5_mc29" , "cl10_me6_20"  , "cl10_me6_25"    ,   "cl10_me6_32"   ,          
       "cl10_me6_41","cl10_me6_46", "cl10_me6_49", "cl10_me2_40", "cl10_me1_28","cl10_me1_44",
       "cl10_me3_sa7", "cl10_me4_sa6", "cl10_me5_sa3")                    

# removing overlapping items between CU.JK and CU.KK
# removing "cl11_fcmc26", "cl11_fcmc27",
CU.JK <- CU.JK[-c(25:26)]

# removing "cl7_fc38"
CU.KK <- CU.KK[-15]

# Confirming that number of items between Derek's list and Items Profile spreadsheet are the same
# because that spreadsheet will be used to calculate max poss; should be 259
total_Derek <- length(CU.JK)+length(CU.KK)+length(Metro)+length(STMU)+length(UCD)+length(UGA)

# creating dfs for each item set into which to place max poss 
df1 <- data.frame(matrix(ncol=2,nrow=length(CU.KK)))
colnames(df1) <- c("Item", "Max_Poss")

df2 <- df2 <- data.frame(matrix(ncol=2,nrow=length(CU.JK)))
colnames(df2) <- c("Item", "Max_Poss")

df3 <- df3 <- data.frame(matrix(ncol=2,nrow=length(Metro)))
colnames(df3) <- c("Item", "Max_Poss")

df4 <- df4 <- data.frame(matrix(ncol=2,nrow=length(STMU)))
colnames(df4) <- c("Item", "Max_Poss")

df5 <- df5 <- data.frame(matrix(ncol=2,nrow=length(UCD)))
colnames(df5) <- c("Item", "Max_Poss")

df6 <- df6 <- data.frame(matrix(ncol=2,nrow=length(UGA)))
colnames(df6) <- c("Item", "Max_Poss")

# creating tables of values for each item set
t1 <- apply(g[,CU.KK], 2, function (x) table (x))
t2 <- apply(g[,CU.JK], 2, function (x) table (x))
t3 <- apply(g[,Metro], 2, function (x) table (x))
t4 <- apply(g[,STMU], 2, function (x) table (x))
t5 <- apply(g[,UCD], 2, function (x) table (x))
t6 <- apply(g[,UGA], 2, function (x) table (x))

# filling in dfs for each item set
# loop puts in max possible based on item tables
for (i in 1:length(CU.KK)){
  table <- data.frame(t1[[i]])
  temp <- nrow(table)-1
  df1[i,2] <- temp
}
# adding item names
df1$Item <- CU.KK


for (i in 1:length(CU.JK)){
  table <- data.frame(t2[[i]])
  temp <- nrow(table)-1
  df2[i,2] <- temp
}
# adding item names
df2$Item <- CU.JK

# code different in this loop because of homogeneous scale among items
for (i in 1:length(Metro)){
  table <- data.frame(t3)
  temp <- nrow(table)-1
  df3[i,2] <- temp
}
# adding item names
df3$Item <- Metro


for (i in 1:length(STMU)){
  table <- data.frame(t4[[i]])
  temp <- nrow(table)-1
  df4[i,2] <- temp
}
# adding item names
df4$Item <- STMU


for (i in 1:length(UCD)){
  table <- data.frame(t5[[i]])
  temp <- nrow(table)-1
  df5[i,2] <- temp
}
# adding item names
df5$Item <- UCD


for (i in 1:length(UGA)){
  table <- data.frame(t6[[i]])
  temp <- nrow(table)-1
  df6[i,2] <- temp
}
# adding item names
df6$Item <- UGA


# combing dfs into one large one
MaxLocal <- rbind(df1,df2,df3,df4,df5,df6)

# ordering by Item names
MaxLocal <- MaxLocal[order(MaxLocal$Item),]
# confirming order
head(MaxLocal)
tail(MaxLocal)

# importing Item_Profile

setwd("~/Dropbox/Github/Keck/Original Data/Item Documentation")

IP <- read.csv("Items Profile_12_7.csv", header=T)
#str(IP)
#head(IP)

colnames(IP)
# Subsetting for Derek's Items only
IP <- IP[IP$Excluded==0,]
# ordering items by Item name
IP <- IP[order(IP$Item),]
# confirming ordering -esp that it matches MaxLocal
head(IP)
tail(IP)

# filling in Max possible on IP df
IP$Max_poss <- MaxLocal$Max_Poss

# creating df to hold Max possible values
Maxdf <- data.frame(matrix(ncol=5,nrow=8))
colnames(Maxdf) <- c("Institution", "Yr1_Fall", "Yr1_Spring", "Yr2_Fall", "Yr2_Spring")

# calculating Max possible for each test
# subsetting for CUJK year 1 post
CUJK_Y1S <- IP[IP$y1_post==1 & IP$CUJK==1,]
# calculating max
CUJK_Y1S_max <- sum(CUJK_Y1S$Max_poss)

CUJK_Y2S <- IP[IP$y2_post==1 & IP$CUJK==1,]
CUJK_Y2S_max <- sum(CUJK_Y2S$Max_poss)

CUKK_Y1F <- IP[IP$y1_post==1 & IP$CUKK==1,]
CUKK_Y1F_max <- sum(CUKK_Y1F$Max_poss)

CUKK_Y2F <- IP[IP$y2_post==1 & IP$CUKK==1,]
CUKK_Y2F_max <- sum(CUKK_Y2F$Max_poss)

Metro3_Y1F <- IP[IP$cl3==1,]
Metro3_Y1F_max <- sum(Metro3_Y1F$Max_poss)

Metro3_Y2F <- IP[IP$cl13pos==1,]
Metro3_Y2F_max <- sum(Metro3_Y2F$Max_poss)

Metro8_Y1S <- IP[IP$cl8==1,]
Metro8_Y1S_max <- sum(Metro8_Y1S$Max_poss)

Metro8_Y2S <- IP[IP$cl18pos==1,]
Metro8_Y2S_max <- sum(Metro8_Y2S$Max_poss)

STMU4_Y1F <- IP[IP$cl4==1,]
STMU4_Y1F_max <- sum(STMU4_Y1F$Max_poss) 

STMU4_Y2F <- IP[IP$cl14pos==1,]
STMU4_Y2F_max <- sum(STMU4_Y2F$Max_poss)

STMU9_Y1S <- IP[IP$cl9==1,]
STMU9_Y1S_max <- sum(STMU9_Y1S$Max_poss)

STMU9_Y2S <- IP[IP$cl19pos==1,]
STMU9_Y2S_max <- sum(STMU9_Y2S$Max_poss)

UCD_Y1F <- IP[IP$cl5==1,]
UCD_Y1F_max <- sum(UCD_Y1F$Max_poss)

UCD_Y2F <- IP[IP$y2_post==1 & IP$UCD==1,] 
UCD_Y2F_max <- sum(UCD_Y2F$Max_poss)

UGA_Y1S <- IP[IP$cl10==1,]
UGA_Y1S_nax <- sum(UGA_Y1S$Max_poss)

UGA7_Y2S <- IP[IP$matrix7==1,]
UGA7_Y2S_max <- sum(UGA7_Y2S$Max_poss)

UGA8_Y2S <- IP[IP$matrix8==1,]
UGA8_Y2S_max <- sum(UGA8_Y2S$Max_poss)

# organizing data into vectors to add to df
Fall1 <- c(CUKK_Y1F_max, NA, Metro3_Y1F_max, STMU4_Y1F_max, UCD_Y1F_max, NA, NA, NA)

Spring1 <- c(NA, CUJK_Y1S_max, Metro8_Y1S_max, STMU9_Y1S_max, NA, UGA_Y1S_nax, NA, NA) 

Fall2 <- c(CUKK_Y2F_max, NA, Metro3_Y2F_max, STMU4_Y2F_max, UCD_Y2F_max, NA, NA, NA)

Spring2 <- c(NA, CUJK_Y2S_max, Metro8_Y2S_max, STMU9_Y2S_max, NA, NA, UGA7_Y2S_max, UGA8_Y2S_max)

Inst <- c("CUKK", "CUJK", "Metro", "STMU", "UCD", "UGA", "UGA_m7", "UGA_m8")

# adding variables
Maxdf$Institution <- Inst
Maxdf$Yr1_Fall <- Fall1
Maxdf$Yr1_Spring <- Spring1
Maxdf$Yr2_Fall <- Fall2
Maxdf$Yr2_Spring <- Spring2


# exporting into Excel
setwd("~/Dropbox/Github/Keck/Analysis Data")
write.csv(Maxdf, "Max Possible_by_semester.csv")
write.csv(IP, "Items Profile_Used_items.csv")

