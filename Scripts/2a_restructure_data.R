##  The purpose of this script is to merge together the Keck data files
##  into a person-period (long) format.
##  Derek Briggs
##  Oct 2, 2017

# Coding for Missing Data: Only matrix sampled items in yr 2 are missing by design
# all other missing responses to GCA or year1 and year 2 post test items treated as incorrect

library(readxl)
library(dplyr)

setwd("~/Dropbox/Github/Keck/Original Data")

d1<-read_excel("GCAStudy_ALL DATA__09072017_working_yr1pre.xlsx",col_names=TRUE, skip=1,na=c("","NA"))
d2<-read_excel("GCAStudy_ALL DATA__09072017_working_yr1post.xlsx",col_names=TRUE, skip=1,na=c("","NA"))
d3<-read_excel("GCAStudy_ALL DATA__09072017_working_yr2pre.xlsx",col_names=TRUE, skip=1,na=c("","NA"))
d4<-read_excel("GCAStudy_ALL DATA__09072017_working_yr2post.xlsx",col_names=TRUE, skip=1,na=c("","NA"))

names(d1) <- gsub("Pre", "", names(d1))
d1[,19:43][is.na(d1[,19:43])]<-0
names(d2) <- gsub("Post", "", names(d2))
d2[,19:43][is.na(d2[,19:43])]<-0
#d2[,53:252][is.na(d2[,53:252])]<-0
names(d3) <- gsub("Pre", "", names(d3))
d3[,19:43][is.na(d3[,19:43])]<-0
names(d4) <- gsub("Post", "", names(d4))
d4[,19:43][is.na(d4[,19:43])]<-0
#d4[,53:260][is.na(d2[,53:260])]<-0

year1.data<-full_join(d1,d2)
year1.data$cl4_fcsa9<-as.numeric(year1.data$cl4_fcsa9)

year2.data<-full_join(d3,d4)
year2.data$Class<-factor(year2.data$Class)
year2.data$Class_other<-factor(year2.data$Class_other)

full.data<-full_join(year1.data,year2.data)

rm(d1,d2,d3,d4,year1.data,year2.data)

setwd("~/Dropbox/Github/Keck/Analysis Data")

full.data<-as.data.frame(full.data)

save(full.data, file="fulldata.RData")

write.csv(full.data,"GCA_data_09072017_long.csv")





