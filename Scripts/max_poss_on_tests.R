# Derek Briggs
# Max possible on tests
# Updated on 3/6/18 from Richard's original script of 12/1/17
# Purpose:
#------------------------------------------------------------------
# this script will create a institution by semester matrix with the maximum 
# possible points on each test for each institution by time period
# given Derek's recoding and item exclusions in lines 1-376 of "2b_recode_data.R"

setwd("~/Dropbox/Github/Keck/Analysis Data")
g<-read.csv("Keck_local_item_data.csv")
setwd("~/Dropbox/Github/Keck/Original Data/Item Documentation")
IP <- read.csv("Items Profile_12_20_17.csv", header=T)

IP$Item<-as.character(IP$Item)

CU.KK<-IP$Item[IP$Excluded==0 & IP$CUKK==1]
CU.JK<-IP$Item[IP$Excluded==0 & IP$CUJK==1]
Metro<-IP$Item[IP$Excluded==0 & IP$Metro==1]
STMU<-IP$Item[IP$Excluded==0 & IP$STMU==1]
UCD<-IP$Item[IP$Excluded==0 & IP$UCD==1]
UGA<-IP$Item[IP$Excluded==0 & IP$UGA==1]

length(CU.JK)+length(CU.KK)+length(Metro)+length(STMU)+length(UCD)+length(UGA)

# removing overlapping items between CU.JK and CU.KK
# removing "cl11_fcmc26", "cl11_fcmc27",
CU.JK <- CU.JK[!(CU.JK %in% c("cl11_fcmc26", "cl11_fcmc27"))]
# removing "cl7_fc38"
CU.KK <- CU.KK[!(CU.KK %in% "cl7_fc38")]

length(CU.JK)+length(CU.KK)+length(Metro)+length(STMU)+length(UCD)+length(UGA)

# creating dfs for each item set into which to place max poss 
df1 <- data.frame(matrix(ncol=2,nrow=length(CU.KK)))
df2  <- data.frame(matrix(ncol=2,nrow=length(CU.JK)))
df3  <- data.frame(matrix(ncol=2,nrow=length(Metro)))
df4  <- data.frame(matrix(ncol=2,nrow=length(STMU)))
df5 <- data.frame(matrix(ncol=2,nrow=length(UCD)))
df6 <- data.frame(matrix(ncol=2,nrow=length(UGA)))

# finding empirical max score for each item
m1 <- apply(g[,CU.KK], 2, max, na.rm=T)
m2 <- apply(g[,CU.JK], 2, max, na.rm=T)
m3 <- apply(g[,Metro], 2, max, na.rm=T)
m4 <- apply(g[,STMU], 2, max, na.rm=T)
m5 <- apply(g[,UCD], 2, max, na.rm=T)
m6 <- apply(g[,UGA], 2, max, na.rm=T)

df1[,2]<-m1
df2[,2]<-m2
df3[,2]<-m3
df4[,2]<-m4
df5[,2]<-m5
df6[,2]<-m6

# adding item names
df1[,1] <- CU.KK
df2[,1] <- CU.JK
df3[,1] <- Metro
df4[,1] <- STMU
df5[,1] <- UCD
df6[,1] <- UGA

# combing dfs into one large one
MaxLocal <- rbind(df1,df2,df3,df4,df5,df6)
colnames(MaxLocal)<-c("Item","Max_Poss")
setwd("~/Dropbox/Github/Keck/Analysis Data")
write.csv(MaxLocal, "Max_Possible_per_local_item.csv",row.names = FALSE)

#merge maxpossible with IP variables
IP<-merge(MaxLocal,IP,by="Item")

# creating df to hold Max Score on Local Exam Values by Institution and Semester
Maxdf <- data.frame(matrix(ncol=5,nrow=8))
colnames(Maxdf) <- c("Institution", "Yr1_Fall", "Yr1_Spring", "Yr2_Fall", "Yr2_Spring")

# calculating Max possible for each test

CUJK_Y1S_max <- sum(IP[IP$y1_post==1 & IP$CUJK==1,"Max_Poss"])
CUJK_Y2S_max <- sum(IP[IP$y2_post==1 & IP$CUJK==1,"Max_Poss"])
CUKK_Y1F_max <- sum(IP[IP$y1_post==1 & IP$CUKK==1,"Max_Poss"])
CUKK_Y2F_max <- sum(IP[IP$y2_post==1 & IP$CUKK==1,"Max_Poss"])

Metro_Y1F_max <- sum(IP[IP$cl3==1,"Max_Poss"])
Metro_Y2F_max <- sum(IP[IP$cl13pos==1,"Max_Poss"])
Metro_Y1S_max <- sum(IP[IP$cl8==1,"Max_Poss"])
Metro_Y2S_max <- sum(IP[IP$cl18pos==1,"Max_Poss"])

STMU_Y1F_max <- sum(IP[IP$cl4==1,"Max_Poss"])
STMU_Y2F_max <- sum(IP[IP$cl14pos==1,"Max_Poss"])
STMU_Y1S_max <- sum(IP[IP$cl9==1,"Max_Poss"])
STMU_Y2S_max <- sum(IP[IP$cl19pos==1,"Max_Poss"])

UCD_Y1F_max <- sum(IP[IP$cl5==1,"Max_Poss"])
UCD_Y2F_max <- sum(IP[IP$y2_post==1 & IP$UCD==1,"Max_Poss"]) 

UGA_Y1S_max <- sum(IP[IP$cl10==1,"Max_Poss"]) 
UGA7_Y2S_max <- sum(IP[IP$matrix7==1,"Max_Poss"]) 
UGA8_Y2S_max <- sum(IP[IP$matrix8==1,"Max_Poss"]) 

# organizing data into vectors to add to df
Fall1 <- c(CUKK_Y1F_max, NA, Metro_Y1F_max, STMU_Y1F_max, UCD_Y1F_max, NA, NA, NA)

Spring1 <- c(NA, CUJK_Y1S_max, Metro_Y1S_max, STMU_Y1S_max, NA, UGA_Y1S_max, NA, NA) 

Fall2 <- c(CUKK_Y2F_max, NA, Metro_Y2F_max, STMU_Y2F_max, UCD_Y2F_max, NA, NA, NA)

Spring2 <- c(NA, CUJK_Y2S_max, Metro_Y2S_max, STMU_Y2S_max, NA, NA, UGA7_Y2S_max, UGA8_Y2S_max)

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


