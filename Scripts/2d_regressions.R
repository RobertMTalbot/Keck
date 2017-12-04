##Regressions

#Read in original data with demographic vars in wide format

setwd("~/Dropbox/Github/Keck/Original Data")

#d<-read.csv("GCAStudy_ALL DATA__09072017_working.csv",header=T,na.strings=c(""))
# Don't use this import command, the na.strings option screws things up for GCA items

d<-read.csv("GCAStudy_ALL DATA__09072017_working.csv",header=T)

names(d)

nrow(d)
d$StudentID<-as.character(d$StudentID)
#Only retain students with valid student IDs
d<-d[d$StudentID!="",]
nrow(d)

#Read in data from restructured files, merge in thetas by student ID

setwd("~/Dropbox/Github/Keck/Analysis Data")
load("postdata.RData")

d2<-subset(post.data,select=c("StudentID","theta","se"))

rm(post.data)

d3<-merge(d,d2,by="StudentID",all.x=TRUE)

##Clean/transform covariates for regression analyses

#All GCA Prep items with missing data get scored as incorrect
d3[,18:67][is.na(d3[,18:67])]<-0

library(CTT)

#GCA Answer key
answer.key<-c(5,3,3,1,3,3,4,2,2,3,4,3,4,3,3,2,5,3,2,3,3,4,1,4,2)
# Get total GCA pre-test and post-test score across all students
out.pre<-score(d3[,18:42],output.scored = TRUE,answer.key,rel=TRUE)
out.pos<-score(d3[,43:67],output.scored = TRUE,answer.key,rel=TRUE)
# Use this pre-test score as covariate in raw score metric
d3$GCA.pre<-out.pre$score
# Use the post-test score as an alternate outcome variable
d3$GCA.pos<-out.pos$score
#Race/ethnicity variables
table(d3$Ethnicity,useNA = "always")
d3$Hispanic[d3$Ethnicity==1]<-1
d3$Hispanic[d3$Ethnicity==2]<-0
#Gender dummy var
table(d3$Sex,useNA = "always")
d3$Female<-NULL
d3$Female[d3$Sex==1]<-0
d3$Female[d3$Sex==2]<-1
#Standardized version of theta
d3$ztheta<-(d3$theta-mean(d3$theta,na.rm=T))/sd(d3$theta,na.rm=T)
#Turn undergraduate class variable into a factor
d3$Class<-factor(d3$Class)

#Trim down dataframe to relevant regression variables

data<-d3[,c(1:17,68:76,673:ncol(d3))]

#SDs of relevant continuous variables
sd(data$theta,na.rm=T)
sd(data$GCA.pre,na.rm=T)
sd(data$GCA.pos,na.rm=T)

#Regression models with overall theta as outcome (logit metric)

setwd("~/Dropbox/Github/Keck/Tables and Figures")

out1<-lm(theta~0+Course.Code+Demo4AmIndAl1+Demo4Asian2+Demo4Black3 +Demo4Haw4 + Hispanic +
        + Female +Class,data=data)
summary(out1)

out2<-lm(theta~0+Course.Code+Demo4AmIndAl1+Demo4Asian2+Demo4Black3 +Demo4Haw4 + Hispanic +
           + Female +Class + GCA.pre,data=data)
summary(out2)

library(stargazer)

stargazer(out1, out2, type="html",
          dep.var.labels=c("Theta (logits)" ),
          intercept.bottom = FALSE,
          intercept.top = TRUE,
          covariate.labels=c("Metro","StMu","UGA","CU","Am Ind","Asian",
                             "Black","Pac Isl","Hispanic","Female","Soph",
                             "Junior","Senior","5th Year","Other","GCA Pre"), 
          out="models.html")


#Regression models with GCA post as outcome (raw score--total out of 25--metric)

out3<-lm(GCA.pos~0+Course.Code+Demo4AmIndAl1+Demo4Asian2+Demo4Black3 +Demo4Haw4 + Hispanic +
           + Female +Class + GCA.pre,data=data)
summary(out3)




