## The script calibrates locallly developed final exams onto a common scale
## using GCA as common item block and Partial Credit Model

setwd("~/Dropbox/Github/Keck/Analysis Data")
load("itemdata.RData")
load("item_names.RData")

library(dplyr)
library(CTT)
library(TAM)

#Import GCA Item Parameters created from script "Rasch_GCA.R"

#GCA items 3 and 19 have been excluded because they clearly don't fit the model

gca_itpars<-read.csv("GCA_Item_Diff_Pars.csv",header=T)
gca_itpars[,1]<-as.character(gca_itpars[,1])

#Add in constrained value of 25th GCA item difficulty
gca_itpars[23,1]<-"GCAPost25"
gca_itpars[23,2]<-(-sum(gca_itpars[1:22,2]))
# define vector 1 with fixed item difficulties--see "1c_Check_Rasch_Fit_GCA.R"
fixed <- cbind( c(1:2,4:18,20:25), gca_itpars[,2] )   
#alternate vector 2 with fixed item difficulties--see "1c_Check_Rasch_Fit_GCA.R"
xx<-c(2,4,6:8,11:14,18,21,25)
fixed2<-fixed[fixed[,1]%in%xx,]

#Note: I'm excluding JHU from local item calibrations 

it.data$Inst.Course<-it.data$Institution
it.data$Inst.Course[it.data$Institution=="UCD_AJ" | it.data$Institution=="UCD_AJ1" 
              | it.data$Institution=="UCD_AJ2"]<-"UCD"

table(it.data$Inst.Course)

###-------------------------Calibrate Items from each unique course to GCA Scale-----------------

courses<-c("CUB_JK","CUB_KK","Metro_VM","StMU_CG","UCD","Uga_NA" )
GCA<-paste0("GCA",seq(1,25,1))
It_Names<-list(CU.JK, CU.KK, Metro, STMU, UCD, UGA)
mod=list() 
thetas<-NULL 
items=list() 
fit=list()

for (i in 1:length(courses)){
  data<-it.data[it.data$Inst.Course==courses[i],c(GCA,It_Names[[i]])]
  mod[[i]]<-tam.mml(resp=data, xsi.fixed=fixed, irtmodel="PCM",            #Can change this by using fixed2 instead
               control=list(maxiter = 1000))
  #Collect Item Difficulty Parameter Estimates
  items[[i]]<-mod[[i]]$item
  #Get Item fit stats
  fit[[i]] <- tam.fit(mod[[i]])
  #Get Theta Estimates
  Abil<-tam.wle(mod[[i]])
  #Store Theta Estimates
  Stud.Theta<-cbind(Abil$theta,Abil$error)
  thetas<-rbind(thetas,Stud.Theta)
}

thetas<-data.frame(thetas)
names(thetas)<-c("theta","se")
names(items)<-courses
names(fit)<-courses
names(mod)<-courses

##----------Important note on PCM item parameter estimates from TAM-------
# If you just want to get the PCM threshold for each category
# you need to look at "Item Parameters Xsi" as in "mod[[i]]$xsi"
# If you look at "mod[[i]]$item" you get the Conquest parameterization 
# of the PCM, which provides the average of the category thresholds
# as "xsi.item" (confusing because this is NOT the same as the "xsi" noted above)
# and then you also get "AXsi_.Cat1" and "AXsi_.Cat2" which are NOT
# category thresholds but the deviation of each category threshold from the mean
# "xsi.item". 
##---------------------------------------------------------------------------

##Check Local Item Fit with script "Check_Fit_Local_Items.R"



##Compare item difficulty estimates to Bloom Level of Item

#Import Bloom's Level Info from Item Profile ("IP") Documentation
setwd("~/Dropbox/Github/Keck/Original Data/Item Documentation")
IP <- read.csv("Items Profile_12_20_17.csv", header=T)
#Bloom<-IP[,c("Item","Blooms","constructedresponse")]
setwd("~/Dropbox/Github/Keck/Analysis Data")
MaxLocal<-read.csv("Max_Possible_per_local_item.csv")
IP<-merge(IP,MaxLocal)

#Loop through items for each institution merge in item covariates

results=list()
ndat<-list()

for (i in 1:length(courses)){
ndat[[i]]<-merge(items[[i]][-(1:25),1:4],IP,by.x="item",by.y="Item")
by_Bloom <- group_by(ndat[[i]],Blooms)
temp <- summarise(by_Bloom,
                     count = n(),
                     Bloom_mean = mean(xsi.item, na.rm = TRUE),
                     Bloom_sd = sd(xsi.item, na.rm = TRUE))
results[[i]]<-as.data.frame(temp)
rm(temp)
}
names(results)<-courses

#Combine into one dataframe

all.items<-rbind(ndat[[1]],ndat[[2]],ndat[[3]],ndat[[4]],ndat[[5]],ndat[[6]])
all.items$pt<-all.items$M/all.items$Max_Poss

#Export as csv file for Jennifer Avena to Play With
write.csv(all.items,"item_regressions_data.csv",row.names=F)

#Regressions using logits as outcome

mod1<-lm(xsi.item~Blooms,data=all.items)
summary(mod1)
all.items$BF<-as.factor(all.items$Blooms)
mod2a<-lm(xsi.item~0 + BF,data=all.items)
summary(mod2a)
mod3<-lm(xsi.item~constructedresponse,data=all.items)
summary(mod3)
mod2b<-lm(xsi.item~BF,data=all.items)
summary(mod2b)
mod4<-lm(xsi.item~constructedresponse + BF ,data=all.items)
summary(mod4)

library(stargazer)

setwd("~/Dropbox/Github/Keck/Tables and Figures")

stargazer(mod3,mod2b,mod4, type="html",
          dep.var.labels=c("Item Difficulty in logits (Higher = Harder)"),
          intercept.bottom = FALSE,
          intercept.top = TRUE,
          covariate.labels=c("Intercept (Recall)","CR Format","Understand","Apply","Analyze","Create"), 
          out="Bloom_models_logits.html")


#Regressions using Raw Item Score (average score) as outcome

mod1.r<-lm(pt~Blooms,data=all.items)
summary(mod1.r)
all.items$BF<-as.factor(all.items$Blooms)
mod2a.r<-lm(pt~0 + BF,data=all.items)
summary(mod2a.r)
mod3.r<-lm(pt~constructedresponse,data=all.items)
summary(mod3.r)
mod2b.r<-lm(pt~BF,data=all.items)
summary(mod2b.r)
mod4.r<-lm(pt~BF + constructedresponse,data=all.items)
summary(mod4.r)

stargazer(mod3.r,mod2b.r,mod4.r, type="html",
          dep.var.labels=c("Prop of Max Possible on Item (Higher = Easier)"),
          intercept.bottom = FALSE,
          intercept.top = TRUE,
          covariate.labels=c("Intercept (Recall)","CR Format","Understand","Apply","Analyze","Create"), 
          out="Bloom_models_raw.html")

#lm(M~N,data=all.items)



for (i in 1:length(courses)){
  x<-merge(items[[i]][-(1:25),],IP,by.x="item",by.y="Item")
  by_Bloom <- group_by(x,Blooms)
  temp <- summarise(by_Bloom,
                    count = n(),
                    Bloom_mean = mean(xsi.item, na.rm = TRUE),
                    Bloom_sd = sd(xsi.item, na.rm = TRUE))
  results[[i]]<-as.data.frame(temp)
  rm(list = c('x','temp'))
}
names(results)<-courses




#merge maxpossible with IP variables
xx<-merge(x,IP,by.x="item",by.y="Item")


                     
results

##export all outfit stats to excel as one stacked dataframe

fitstats<-NULL

for (i in 1:length(courses)){

fitstats<-rbind(fitstats,fit[[i]]$itemfit[,1:4])
}

nrow(fitstats[fitstats$Outfit>=1.2,])/nrow(fitstats)

setwd("~/Dropbox/Github/Keck/Tables and Figures")
write.csv(fitstats,"itemfitstats.csv")

x<-results[[1]]

##Items with disordered thresholds from CU.JK
## cl7_fcsa1b, cl7_fcsa2a, cl7_fcsa2b, cl7_fcsa2c, cl7_fcsa3c
## cl7_fcsa4a , cl7_fcsa5b


plot(x, items=1:x$nitems, type="expected", low=-2, high=4, ngroups=10, 
     wle=NULL, export=TRUE, export.type="png", 
     export.args=list(), observed=TRUE, overlay=FALSE , 
     ask=FALSE,  package="lattice" ,  fix.devices=FALSE)


##Merge Thetas and SEs with source dataframe

post.data<-data.frame(it.data,thetas)

save(post.data, file="postdata.RData")

##Merge in raw scores in percent of total metric

temp<-read.csv("Keck_post_test_per_correct.csv",header=T)

attach(temp)

post.data<-cbind(post.data,local_tot_score,max.score,per.total)

detach(temp); rm(temp)

names(post.data)

write.csv(post.data,"Keck_data_thetas_030618.csv")

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




