## The script first calibrates locallly developed final exams onto a common scale
## using GCA as common item block and Partial Credit Model 
## Get difficulty estimates for all local items THEN run pre and post for each
## course separately so you can retrieve population mean and SD for pre and post data sets

setwd("~/Dropbox/Github/Keck/Analysis Data")
load("gaindata.RData")
load("gaindata_itemnames.RData")

library(dplyr)
library(TAM)

#Import GCA Item Parameters created from script "Rasch_GCA.R"

#GCA items 3 and 19 are not anchored because they clearly don't fit the model

gca_itpars<-read.csv("GCA_Item_Diff_Pars.csv",header=T)
gca_itpars[,1]<-as.character(gca_itpars[,1])

#Add in constrained value of 25th GCA item difficulty
gca_itpars[23,1]<-"GCAPost25"
gca_itpars[23,2]<-(-sum(gca_itpars[1:22,2]))
# define vector 1 with fixed item difficulties--see "1c_Check_Rasch_Fit_GCA.R"
fixed <- cbind( c(1:2,4:18,20:25), gca_itpars[,2] )   

#Note: I'm excluding JHU at STMU because of wonky items (JHU) and small sample size (STMU)

gain.data$Inst.Course<-gain.data$Institution
gain.data$Inst.Course[gain.data$Institution=="UCD_AJ" | gain.data$Institution=="UCD_AJ1" 
              | gain.data$Institution=="UCD_AJ2"]<-"UCD"

table(gain.data$Inst.Course)

###-------------------------Calibrate Items from each unique course to GCA Scale-----------------

courses<-c("CUB_JK","CUB_KK","Metro_VM","Uga_NA" )
GCA<-paste0("GCA",seq(1,25,1))
It_Names<-list(CU.JKs, CU.KKs, Metros, UGAs)

mod=list() 

for (i in 1:length(courses)){
  data<-gain.data[gain.data$Inst.Course==courses[i] & gain.data$post==1,c(GCA,It_Names[[i]])]
  mod[[i]]<-tam.mml(resp=data, xsi.fixed=fixed, irtmodel="PCM",            
               control=list(maxiter = 1000))
  }

names(mod)<-courses

###-------------------------Run Post Data with Just Fixed Local Items from each unique course to GCA Scale-----------------

mod.post=list()

for (i in 1:length(courses)){
  data<-gain.data[gain.data$Inst.Course==courses[i] & gain.data$post==1,It_Names[[i]]]
  temp<-mod[[i]]$xsi[-c(1:25),1]
  anchor<-matrix(c(seq(1,length(temp)),temp), ncol=2)
  mod.post[[i]]<-tam.mml(resp=data, xsi.fixed=anchor, irtmodel="PCM",            
                    control=list(maxiter = 2000))
  }

names(mod.post)<-courses

###-------------------------Run Pre Data with Just Fixed Local Items from each unique course to GCA Scale-----------------

mod.pre=list()

for (i in 1:length(courses)){
  data<-gain.data[gain.data$Inst.Course==courses[i] & gain.data$post==0,It_Names[[i]]]
  temp<-mod[[i]]$xsi[-c(1:25),1]
  anchor<-matrix(c(seq(1,length(temp)),temp), ncol=2)
  mod.pre[[i]]<-tam.mml(resp=data, xsi.fixed=anchor, irtmodel="PCM",            
                         control=list(maxiter = 2000))
}

apply(data, 2,function(x) table(x, useNA='always'))

#Abil <- tam.wle(mod.pre[[2]])
#ctt1 <- tam.ctt(data, Abil$theta)

names(mod.pre)<-courses

###-------------------------Retrieve Pop Means and Sds for Each Institution--------

theta.pre<-data.frame(matrix(data=NA,nrow=4,ncol=2))

for (i in 1:length(courses)){
  theta.pre[i,1]<-mod.pre[[i]]$beta
  theta.pre[i,2]<-sqrt(mod.pre[[i]]$variance)
   }

names(theta.pre)<-c("Pop Mean", "SD")

theta.post<-data.frame(matrix(data=NA,nrow=4,ncol=2))

for (i in 1:length(courses)){
  theta.post[i,1]<-mod.post[[i]]$beta
  theta.post[i,2]<-sqrt(mod.post[[i]]$variance)
}

names(theta.post)<-c("Pop Mean", "SD")

gains.l<-theta.post[,1]-theta.pre[,1]

gains.l

summary(mod.pre[[1]])

save(mod, file="post_all.RData")
save(mod.post, file="post_local.RData")
save(mod.pre, file="pre_local.RData")


###-------------------------Run Post Data with GCA Items-----------------

mod.gpost=list()

for (i in 1:length(courses)){
  data<-gain.data[gain.data$Inst.Course==courses[i] & gain.data$post==1,GCA]
  temp<-mod[[i]]$xsi[1:25,1]
  anchor<-matrix(c(seq(1,length(temp)),temp), ncol=2)
  mod.gpost[[i]]<-tam.mml(resp=data, xsi.fixed=anchor, irtmodel="PCM",            
                         control=list(maxiter = 2000))
}

names(mod.gpost)<-courses

###-------------------------Run Pre Data with GCA Items-----------------

mod.gpre=list()

for (i in 1:length(courses)){
  data<-gain.data[gain.data$Inst.Course==courses[i] & gain.data$post==0,GCA]
  temp<-mod[[i]]$xsi[1:25,1]
  anchor<-matrix(c(seq(1,length(temp)),temp), ncol=2)
  mod.gpre[[i]]<-tam.mml(resp=data, xsi.fixed=anchor, irtmodel="PCM",            
                        control=list(maxiter = 2000))
}

names(mod.gpre)<-courses

###-------------------------Retrieve Pop Means and Sds for Each Institution--------

theta.gpre<-data.frame(matrix(data=NA,nrow=4,ncol=2))

for (i in 1:length(courses)){
  theta.gpre[i,1]<-mod.gpre[[i]]$beta
  theta.gpre[i,2]<-sqrt(mod.gpre[[i]]$variance)
}

names(theta.gpre)<-c("Pop Mean", "SD")

theta.gpost<-data.frame(matrix(data=NA,nrow=4,ncol=2))

for (i in 1:length(courses)){
  theta.gpost[i,1]<-mod.gpost[[i]]$beta
  theta.gpost[i,2]<-sqrt(mod.gpost[[i]]$variance)
}

names(theta.gpost)<-c("Pop Mean", "SD")

gains.g<-theta.gpost[,1]-theta.gpre[,1]

gains.g

save(mod, file="post_all.RData")
save(mod.post, file="post_local.RData")
save(mod.pre, file="pre_local.RData")

