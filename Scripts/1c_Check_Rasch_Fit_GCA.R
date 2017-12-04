# The purpose of this script is to check the fit of Rasch Model for GCA
# (item and person fit), (dimensionality), (parameter invariance)

## Might need to exclude items GCA items 3 and 19

library(TAM)
library(eRm)
library(sirt)

setwd("~/Dropbox/Github/Keck/Analysis Data")

d<-read.csv("GCA.Rasch.csv",header=T)

courses<-unique(d$Institution)
It_Names<-paste0("GCAPost",seq(1:25))

out<-RM(d[,It_Names[-c(3,19)]], sum0 = TRUE)
lrt<-LRtest(out,se=TRUE)
lrt
Waldtest(out)
#Items for which parameter invariance seems to hold
#when splitting sample by median raw score
# 2, 4, 6, 7, 8, 9, 11, 12, 13, 14, 18, 20, 21, 22, 23, 24, 25
plotGOF(lrt,conf= list())

it.subset<-c(2,4,6:8,11:14,18,21,25)

out2<-RM(d[,It_Names[it.subset]], sum0 = TRUE)
lrt2<-LRtest(out2,se=TRUE)
lrt2
Waldtest(out2)
plotGOF(lrt2,conf= list())

out3<-RM(d[,It_Names[-c(3,19)]], sum0 = TRUE)
lrt3<-LRtest(out3,se=TRUE,splitcr=d$Institution)
lrt3
#Items for which parameter invariance seems to hold
#when splitting sample by CU_JK vs CU_KK
# 2, 4, 6, 7, 8, 9, 11, 12, 13, 14, 18, 21, 22, 23, 25
Waldtest(out3)
plotGOF(lrt3,conf= list())

##Split by uga vs cu

d.sub<-d[(d$Institution=="CUB_JK" | d$Institution=="CUB_KK" | d$Institution=="Uga_NA"),]
out4<-RM(d.sub[,It_Names[-c(3,19)]], sum0 = TRUE)
lrt4<-LRtest(out4,se=TRUE,splitcr=d.sub$Course.Code)
lrt4
#Items for which parameter invariance seems to hold
#when splitting sample by CU_JK/CU_KK vs UGA
# old: 2, 4, 6, 7, 8, 9, 11, 12, 13, 14, 18, 21, 22, 23, 25
# new: 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 18, 22, 23, 25
Waldtest(out4)
plotGOF(lrt4,conf= list())

it.subset<-c(2,4,6:9,11:14,18,21:23,25)

##Checking Item fit and person fit across all courses

#Classical items stats

ctt_stats <- reliability(df)
ctt_table <- data.frame(
  Item_p               = ctt_stats$itemMean,
  Corrected_ItemTotal = ctt_stats$pBis,
  AlphaIfDeleted      = ctt_stats$alphaIfDeleted)

source("~/Dropbox/Courses/EDUC 8710/R Scripts Tutorials and Packages/educ8710_item_analysis_app_v06.R")

#Pretty good evidence here that items 3 and 19 (19 especially don't fit the Rasch Model)

setwd("~/Dropbox/Github/Keck/Analysis Data")

pf<-read.csv("GCA_post_rasch_person_stats.csv",header=T)

summary(pf$outfitMNSQ.z)
hist(pf$outfitMNSQ.z)
length(pf$outfitMNSQ.z[pf$outfitMNSQ.z>(1.99)])
length(pf$outfitMNSQ[pf$outfitMNSQ>(1.2)])

data<-d[,It_Names[-c(3,19)]]
mod<-tam.mml(resp=data,constraint="items", irtmodel="PCM")
fit.stats<-tam.fit(mod)
wle<-tam.wle(mod)
b1<-mod$AXsi[,-1]
fit1<-pcm.fit(b=b1,theta=wle$theta, data)

#person fit
fit1$personfit  #look at distributions by Institution

d<-cbind(d,fit1$personfit)

library(ggplot2)

png("outfit_by_institution.png")

ggplot(data=d,aes(x)) +
  geom_histogram(aes(x=outfit,y=..density..),binwidth=3,breaks=seq(0, 2.5, by=.25),fill="blue",color="black",alpha=0.5)+
  scale_x_continuous(name="Outfit MNSQ", breaks=seq(0, 2.5, by=.25)) +
  ylab("Density Scale") +
  facet_grid(Institution~.) +
  ggtitle("title") + theme(plot.title = element_text(hjust = 0.5))

dev.off()

png("outfit.t_by_institution.png")

ggplot(data=d,aes(x)) +
  geom_histogram(aes(x=outfit.t,y=..density..),binwidth=3,breaks=seq(-1.5, 3.5, by=.5),fill="blue",color="black",alpha=0.5)+
  scale_x_continuous(name="Outfit t", breaks=seq(-1.5, 2.5, by=.5)) +
  ylab("Density Scale") +
  facet_grid(Institution~.) +
  ggtitle("Outfit t by Institution") + theme(plot.title = element_text(hjust = 0.5))

dev.off()

summary(lm(fit1$personfit$outfit~d$Institution))

##Checking parameter invariance of GCA item parameters by unique course

results=list() 
thetas<-NULL 
items=list() 
fit=list()

for (i in 1:length(courses)){
  data<-d[d$Institution==courses[i],It_Names[it.subset]]
  mod<-tam.mml(resp=data,constraint="items", irtmodel="PCM")
  #Collect Item Difficulty Parameter Estimates
  Item.Pars<-mod$xsi
  #Item fit stats
  fit[[i]] <- tam.fit(mod)
  #Collect Item Difficulty Parameter Estimates
  Item.Pars<-mod$xsi
  #Theta Estimates
  Abil<-tam.wle(mod)
  Stud.Theta<-Abil$theta
  #Theta Estimates
  Abil<-tam.wle(mod)
  Stud.Theta<-cbind(Abil$theta,Abil$error)
  #Collect Results
  thetas<-rbind(thetas,Stud.Theta)
  items[[i]]<-Item.Pars
  results[[i]]<-mod
}

#save(results, file="rasch_results.RData")
#save(fit, file="rasch_fit.RData")

Item.Diff<-data.frame(items[[1]][,1],items[[2]][,1],items[[3]][,1],
                      items[[3]][,1],items[[5]][,1],items[[6]][,1],
                      items[[7]][,1],items[[8]][,1],items[[9]][,1])

names(Item.Diff)<-courses
cor(Item.Diff)
plot(Item.Diff[,1],Item.Diff[,2])
plot(Item.Diff[,1],Item.Diff[,7])

#Checking item fit by course

x<-fit[[1]][[1]]
x[x$Outfit_p < .10 | x$Outfit > 1.2,]

x<-fit[[2]][[1]]
x[x$Outfit_p < .10 | x$Outfit > 1.2,]

x<-fit[[3]][[1]]
x[x$Outfit_p < .10 | x$Outfit > 1.2,]

x<-fit[[4]][[1]]
x[x$Outfit_p < .10 | x$Outfit > 1.2,]

x<-fit[[5]][[1]]
x[x$Outfit_p < .10 | x$Outfit > 1.2,]

x<-fit[[6]][[1]]
x[x$Outfit_p < .10 | x$Outfit > 1.2,]

x<-fit[[7]][[1]]
x[x$Outfit_p < .10 | x$Outfit > 1.2,]

thetas<-data.frame(thetas)
names(thetas)<-c("theta","se")
names(items)<-courses
names(fit)<-courses
names(results)<-courses
