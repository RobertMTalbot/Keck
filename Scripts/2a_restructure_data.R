##  The purpose of this script is to merge together the Keck data files
##  into a person-period (long) format.
##  Derek Briggs
##  March 5, 2018

# Coding for Missing Data: Only matrix sampled items in yr 2 are missing by design
# all other missing responses to GCA or year1 and year 2 post test items treated as incorrect

library(readxl)
library(dplyr)

setwd("~/Dropbox/Github/Keck/Original Data")

d1<-read_excel("GCAStudy_ALL DATA__09072017_working_yr1pre.xlsx",col_names=TRUE, skip=1,na=c("","NA"))
d2<-read_excel("GCAStudy_ALL DATA__09072017_working_yr1post.xlsx",col_names=TRUE, skip=1,na=c("","NA"))
d3<-read_excel("GCAStudy_ALL DATA__09072017_working_yr2pre.xlsx",col_names=TRUE, skip=1,na=c("","NA"))
d4<-read_excel("GCAStudy_ALL DATA__09072017_working_yr2post.xlsx",col_names=TRUE, skip=1,na=c("","NA"))

exclude<-c("062F69BC895990333859F9358738E41EEBE142D2",
           "0A3DBCA13DE241B2E32AA7D0F4B163AF9F4F01A6",
           "3DE13BD49BC59339C6F15BD11429B4C572DA12DC",
           "7E8428D67990CA757894A0A7BE834BF524B073F0",
           "8ABB4558552443FF81C25F08DA39A859DA70512B",
           "900E678058835AEC1A571CDF1881534819A6C977",
           "93209ABF17D60F090C8D098DB5EB7ED37BED7593",
           "93A9B463E5609A75257E1C3F9F5F12DF69A80441",
           "96173A0144980BC2E6A3BEB866D08E85765E715B",
           "BDD0ACD881AEFBD0B5E9C4257D6ACC99D9FD8F18",
           "C548283C6DA7DC8344E3636DB49AB6F613DE3702",
           "CA636A1E2001CD1E86DBB51137EE4DA8B680E4B3",
           "D7EEB8034AF3E3AFEDA17C07646E597687504C8A",
           "E6A37E3ACB15377D012E63D10DC3BF51E1E0D46E")

d1<-d1[!(d1$StudentID %in% exclude),]
d2<-d2[!(d2$StudentID %in% exclude),]
d3<-d3[!(d3$StudentID %in% exclude),]
d4<-d4[!(d4$StudentID %in% exclude),]

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

#rm(d1,d2,d3,d4,year1.data,year2.data)

setwd("~/Dropbox/Github/Keck/Analysis Data")

full.data<-as.data.frame(full.data)

nrow(full.data)

full.data<-full.data[!is.na(full.data$StudentID),]

nrow(full.data)

save(full.data, file="fulldata.RData")

write.csv(full.data,"GCA_data_020518_long.csv")

##Sample Size Checks Against Original Data

setwd("~/Dropbox/Github/Keck/Original Data")

d<-read.csv("GCAStudy_ALL DATA__09072017_working.csv",header=T)

exclude<-c("062F69BC895990333859F9358738E41EEBE142D2",
           "0A3DBCA13DE241B2E32AA7D0F4B163AF9F4F01A6",
           "3DE13BD49BC59339C6F15BD11429B4C572DA12DC",
           "7E8428D67990CA757894A0A7BE834BF524B073F0",
           "8ABB4558552443FF81C25F08DA39A859DA70512B",
           "900E678058835AEC1A571CDF1881534819A6C977",
           "93209ABF17D60F090C8D098DB5EB7ED37BED7593",
           "93A9B463E5609A75257E1C3F9F5F12DF69A80441",
           "96173A0144980BC2E6A3BEB866D08E85765E715B",
           "BDD0ACD881AEFBD0B5E9C4257D6ACC99D9FD8F18",
           "C548283C6DA7DC8344E3636DB49AB6F613DE3702",
           "CA636A1E2001CD1E86DBB51137EE4DA8B680E4B3",
           "D7EEB8034AF3E3AFEDA17C07646E597687504C8A",
           "E6A37E3ACB15377D012E63D10DC3BF51E1E0D46E")

d<-d[!(d$StudentID %in% exclude),]

nrow(d)
d$StudentID<-as.character(d$StudentID)
#Only retain students with valid student IDs
d<-d[d$StudentID!="",]
nrow(d)

table(d$Local.pre.done,useNA="always")
table(d$Local.post.done,useNA="always")
table(d$GCA.pre.done,useNA="always")
table(d$GCA.post.done,useNA="always")

yr1<-subset(d,year==1)

table(yr1$GCA.pre.done)
table(yr1$GCA.post.done)

yr2<-subset(d,year==2)

table(yr2$GCA.pre.done)
table(yr2$GCA.post.done)
table(yr2$Local.pre.done)
table(yr2$Local.post.done)

y1_GCA_union<-subset(d,year==1 & GCA.pre.done==1 & GCA.post.done==1)
y2_GCA_union<-subset(d,year==2 & GCA.pre.done==1 & GCA.post.done==1)
y2_local_union<-subset(d,year==2 & Local.pre.done==1 & Local.post.done==1)
y2_full_union<-subset(d,year==2 & Local.pre.done==1 & Local.post.done==1 & GCA.pre.done==1 & GCA.post.done==1)

y12_local_post<-subset(d,Local.post.done==1)
table(y12_local_post$Institution)
y12_GCA_post<-subset(d, GCA.post.done==1)
table(y12_GCA_post$Institution)




