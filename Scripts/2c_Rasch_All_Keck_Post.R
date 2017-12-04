## The script calibrates locallly developed final exams onto a common scale
## using GCA as common item block and Partial Credit Model

setwd("~/Dropbox/Github/Keck/Scripts")

## Use full.data dataframe generated from "restructure data" script

#source("recode_data.R")

#Sourcing script above results in a dataframe "it.data" that will be
#basis for what follows below.

library(dplyr)
library(CTT)
library(TAM)

setwd("~/Dropbox/Github/Keck/Analysis Data")

#Import GCA Item Parameters created from script "Rasch_GCA.R"

gca_itpars<-read.csv("GCA_Item_Diff_Pars.csv",header=T)
gca_itpars[,1]<-as.character(gca_itpars[,1])

#Add in constrained value of 25th GCA item difficulty
gca_itpars[23,1]<-"GCAPost25"
gca_itpars[23,2]<-(-sum(gca_itpars[1:22,2]))
# define vector with fixed item difficulties
fixed <- cbind( c(1:2,4:18,20:25), gca_itpars[,2] )   
fixed

#Note: I'm excluding JHU. 

it.data$Inst.Course<-it.data$Institution
it.data$Inst.Course[it.data$Institution=="UCD_AJ" | it.data$Institution=="UCD_AJ1" 
              | it.data$Institution=="UCD_AJ2"]<-"UCD"

table(it.data$Inst.Course)

###-------------------------Calibrate Items from each unique course to GCA Scale-----------------

courses<-c("CUB_JK","CUB_KK","Metro_VM","StMU_CG","UCD","Uga_NA" )
It_Names<-list(CU.JK, CU.KK, Metro, STMU, UCD, UGA)
results=list() 
thetas<-NULL 
items=list() 
fit=list()

for (i in 1:length(courses)){
  data<-it.data[it.data$Inst.Course==courses[i],c(GCA,It_Names[[i]])]
  mod<-tam.mml(resp=data, xsi.fixed=fixed, irtmodel="PCM",
               control=list(maxiter = 1000))
  #Collect Item Difficulty Parameter Estimates
  Item.Pars<-mod$xsi
  #Item fit stats
  fit <- tam.fit(mod)
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
  fit[[i]]<-fit
  results[[i]]<-mod
}

thetas<-data.frame(thetas)
names(thetas)<-c("theta","se")
names(items)<-courses
names(fit)<-courses
names(results)<-courses

##Merge Thetas and SEs with source dataframe

post.data<-data.frame(it.data,thetas)

save(post.data, file="postdata.RData")

write.csv(post.data,"Keck_data_thetas_120517.csv")

##Descriptive Stats by Items and Persons

#By Persons

by_course <- group_by(post.data,Inst.Course)
stud_theta_by_course <- summarise(by_course,
                     count = n(),
                     mean_theta = mean(theta, na.rm = TRUE),
                     sd_theta = sd(theta, na.rm = TRUE),
                     se_mean = mean(se, na.rm = TRUE),
                     se_sd = sd(se, na.rm = TRUE))
                    

stud_theta_by_course<-as.data.frame(stud_theta_by_course) 

stud_theta_by_course

###By Items 

#GCA Item Stats: Will be same for each course

summary(items[[2]][1:25,1],na.rm=T)

##Local Item Stats on GCA Scale

stats<-NULL
for (i in 1:6){
x1<-summary(items[[i]][26:nrow(items[[i]]),1],na.rm=T)
x2<-sd(items[[i]][26:nrow(items[[i]]),1],na.rm=T)
stats<-rbind(stats,c(as.vector(x1),as.vector(x2)))
 }

stats<-data.frame(stats)
names(stats)<-c("Min","Q1","Median","Mean","Q3","Max","SD")
row.names(stats)<-courses
stats

#Get population mean and SD for each course

pop.theta<-matrix(nrow=6,ncol=2)

for (i in 1:6){
pop.theta[i,]<-c(results[[i]]$beta,sqrt(results[[i]]$variance))}

pop.theta<-data.frame(pop.theta)
names(pop.theta)<-c("mean","sd")
row.names(pop.theta)<-courses
pop.theta

##Use GGPlot to show boxplots of GCA item diff vs 5 sets of local items

## ************

matrix(data=NA, nrow=ncol(it.data[-(1:6)]),ncol=2)

diffs<-items[[1]][1:25,1]

for (i in 1:6){
diffs<-c(diffs,items[[i]][26:nrow(items[[i]]),1])}
class<-c(rep("GCA",25),
         rep(courses[1],nrow(items[[1]])-25),
         rep(courses[2],nrow(items[[2]])-25),
         rep(courses[3],nrow(items[[3]])-25),
         rep(courses[4],nrow(items[[4]])-25),
         rep(courses[5],nrow(items[[5]])-25),
         rep(courses[6],nrow(items[[6]])-25))

d<-data.frame(diffs,class)
names(d)<-c("difficulty","course")

#Boxplots comparing item distributions

setwd("~/Dropbox/Github/Keck/Tables and Figures")

library(ggplot2)

d$course <- factor(d$course, levels = c("GCA", courses))

png("local_item_diff_boxplots.png")

u <- ggplot(data=d,aes(y=difficulty, x=course, color=course))
u + stat_boxplot(geom="errorbar") + geom_jitter() + geom_boxplot(alpha=0.5) +
  ylab("Item Difficulty in Logits") + xlab("Item Set") + 
  geom_hline(aes(yintercept=0),colour="#990000", linetype="dashed") +
  theme(legend.title=element_blank()) + ggtitle("Distribution of Item Difficulty by Local Genetics Exams")

dev.off()

#Histograms comparing student performance

png("theta_by_institution.png")

ggplot(data=post.data,aes(x=theta)) +
  geom_histogram(aes(x=theta,y=..density..),binwidth=.5,breaks=seq(-6, 6, by=1),fill="blue",color="black",alpha=0.5)+
  scale_x_continuous(name="Theta Distribution", breaks=seq(-6, 6, by=1)) +
  ylab("Density Scale") +
  facet_grid(Institution~.) +
  ggtitle("") + theme(plot.title = element_text(hjust = 0.5))

dev.off()

### TOTAL SCORES in PERCENT OF TOTAL METRIC

#This is tricky. Easy enough to find the sum of item scores for each student
#but not so easy to figure out max score possible as this depends on items 
#administered in a particular class

#post.data$tot.score<-apply(post.data[,32:293],1, function(x) sum(x, na.rm=T))

tot.score<-apply(it.data[,32:ncol(it.data)],1, function(x) sum(x, na.rm=T))
#table(tot.score)
x<-apply(it.data[-(1:6)], 2,function(x) table(x, useNA='always'))

max.score<-rep(0,nrow(it.data))

#These results come from ....

max.score[post.data$Institution=="CUB_KK" & post.data$Semester=="Fa2015"]<-19
max.score[post.data$Institution=="CUB_KK" & post.data$Semester=="Fa2016"]<-38
max.score[post.data$Institution=="CUB_JK" & post.data$Semester=="Sp2016"]<-46
max.score[post.data$Institution=="CUB_JK" & post.data$Semester=="Sp2017"]<-43
max.score[post.data$Institution=="Metro_VM" & post.data$Semester=="Fa2015"]<-22
max.score[post.data$Institution=="Metro_VM" & post.data$Semester=="Sp2016"]<-18
max.score[post.data$Institution=="Metro_VM" & post.data$Semester=="Fa2016"]<-18
max.score[post.data$Institution=="Metro_VM" & post.data$Semester=="Sp2017"]<-20
max.score[post.data$Institution=="StMU_CG" & post.data$Semester=="Fa2015"]<-40
max.score[post.data$Institution=="StMU_CG" & post.data$Semester=="Sp2016"]<-21
max.score[post.data$Institution=="StMU_CG" & post.data$Semester=="Fa2016"]<-62
max.score[post.data$Institution=="StMU_CG" & post.data$Semester=="Sp2017"]<-61
max.score[post.data$Institution=="UCD_AJ" & post.data$Semester=="Fa2016"]<-8
max.score[post.data$Institution=="UCD_AJ1" | post.data$Institution=="UCD_AJ2" & post.data$Semester=="Fa2015"]<-12
max.score[post.data$Institution=="Uga_NA" & post.data$Semester=="Sp2016"]<-24
max.score[post.data$Institution=="Uga_NA" & post.data$Semester=="Sp2017"]<-7

#table(max.score)

post.data$per.total<-(tot.score/max.score)*100

summary(post.data$per.total)


