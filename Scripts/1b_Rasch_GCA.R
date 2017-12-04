## The purpose of this script is to calibrate the GCA with the Rasch Model
## and check for parameter invariance using post-test items with two different 
## samples: YR1-Fall 15 and Spring 16; YR2-Fall 16 and Spring 17

setwd("~/Dropbox/Github/Keck/Analysis Data")

d<-read.csv("GCA.Rasch.csv",header=T)

library(TAM)

## YEAR 1 & 2 GCA Post-Test Items------------------------------------------------------

##Calibrate year 1 and year 2 POSTtest data together, save item parameters

items.post<-d[,33:57]
#exclude GCA items 3 and 19--clear misfit to Rasch Model
items.post<-items.post[,-c(3,19)]
mod.post<-tam.mml(items.post,constraint="items")

#Collect Post-Test Difficulty Parameter Estimates
ItDiff.post<-mod.post$xsi$xsi
names(ItDiff.post)<-names(items.post)[1:22]
write.csv(ItDiff.post,"GCA_Item_Diff_Pars.csv")
#Post-Test Theta Estimates
Abil.Post<-tam.wle(mod.post)
Stud.Theta.Post<-Abil.Post$theta

##Now calibrate year 1 and year 2 PREtest data together using fixed item parameters

xsi.fixed <- cbind( 1:(length(ItDiff.post)) , ItDiff.post )   
# define vector with fixed item difficulties
mod.pre <- tam.mml(resp=d[,8:32] , xsi.fixed=xsi.fixed )
#Pre-Test Theta Estimates
Abil.Pre<-tam.wle(mod.pre)
Stud.Theta.Pre<-Abil.Pre$theta

#Compute Gain Scores
raw.gain<-Stud.Theta.Post-Stud.Theta.Pre
std.gain<-raw.gain/sqrt(mod.post$variance)

#Add to data.frame

d<-cbind(d,Stud.Theta.Pre,Stud.Theta.Post,raw.gain,std.gain)

##Compare Pre to Post Thetas by semester and course

library(ggplot2)

ggplot(data=d,aes(x)) +
  geom_histogram(aes(x=Stud.Theta.Pre,y=..density..),binwidth=.5,breaks=seq(-4, 4, by=.5),fill="blue",color="black",alpha=0.5)+
  scale_x_continuous(name="GCA Pre to Post Test Score Distribution", breaks=seq(-4, 4, by=1)) +
  ylab("Density Scale") +
  geom_histogram( aes(x = Stud.Theta.Post, y = ..density..), 
                  binwidth=3,breaks=seq(-4, 4, by=.5), color="black", fill= "green", alpha=0.5) +
  facet_grid(Course.Code~Semester) +
  ggtitle("Blue=Pre, Green=Post") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data=d,aes(x)) +
  geom_histogram(aes(x=Stud.Theta.Pre,y=..count..),binwidth=.5,breaks=seq(-4, 4, by=.5),fill="blue",color="black",alpha=0.5)+
  scale_x_continuous(name="GCA Pre to Post Test Score Distribution", breaks=seq(-4, 4, by=1)) +
  ylab("Count of Students") +
  geom_histogram( aes(x = Stud.Theta.Post, y = ..count..), 
                  binwidth=3,breaks=seq(-4, 4, by=.5), color="black", fill= "green", alpha=0.5) +
  facet_grid(Course.Code~Semester,scales="free") +
  ggtitle("Blue=Pre, Green=Post") + theme(plot.title = element_text(hjust = 0.5))



##WRIGHT MAP

install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("WrightMap")
library(WrightMap)

thetas <- cbind(Stud.Theta.Pre,Stud.Theta.Post)
wrightMap(thetas, ItDiff.post, item.side = itemClassic, item.prop = 0.3,
          breaks=seq(-4, 5, by=.25),
          dim.names = c("Pre GCA", "Post GCA"),
          dim.color = brewer.pal(3, "Set1"),
          main.title   = "",
          axis.logits  = "Logits",
          axis.persons = "Student Distribution",
          axis.items   = "GCA Items")



##Export Results by Semester, Institution, Course into csv file to compare to raw scores

library(dplyr)
by_course <- group_by(d,Semester,Institution,Course.Code,cl)
results1 <- summarise(by_course,
                     count = n(),
                     GCA_pre_mean = mean(Stud.Theta.Pre, na.rm = TRUE),
                     GCA_pre_sd = sd(Stud.Theta.Pre, na.rm = TRUE),
                     GCA_pos_mean = mean(Stud.Theta.Post, na.rm = TRUE),
                     GCA_pos_sd = sd(Stud.Theta.Post, na.rm = TRUE),
                     GCA_dif_mean = mean(raw.gain, na.rm = TRUE),
                     GCA_dif_sd = sd(raw.gain, na.rm = TRUE),
                     GCA_std_mean = mean(std.gain, na.rm = TRUE))

results1<-as.data.frame(results1) 

by_course <- group_by(d,Course.Code)
results2 <- summarise(by_course,
                     count = n(),
                     GCA_pre_mean = mean(Stud.Theta.Pre, na.rm = TRUE),
                     GCA_pre_sd = sd(Stud.Theta.Pre, na.rm = TRUE),
                     GCA_pos_mean = mean(Stud.Theta.Post, na.rm = TRUE),
                     GCA_pos_sd = sd(Stud.Theta.Post, na.rm = TRUE),
                     GCA_dif_mean = mean(raw.gain, na.rm = TRUE),
                     GCA_dif_sd = sd(raw.gain, na.rm = TRUE),
                     GCA_std_mean = mean(std.gain, na.rm = TRUE))

results2<-as.data.frame(results2) 


##EXPORT RESULTS FOR SUMMARY TABLE

#Export results as a csv file so you can create a summary table
#I use the format function as a wrapper so it only
#prints out to two digits, otherwise need to do by hand in excel
setwd("~/Dropbox/Github/Keck/Tables and Figures")
write.csv(format(results1, digits=2, nsmall=2),"GCA_results_theta.csv") 
write.csv(format(results2, digits=2, nsmall=2),"GCA_results_theta_agg.csv") 



#Descriptive statistics

summary(ItemDiff)
summary(PersonAbility)
sd(ItemDiff)
sd(PersonAbility)

#Change path to export output
setwd("~/Dropbox/Github/Keck/Tables and Figures")

#Statistics for items
Fit <- tam.fit(mod1)
write.csv(mod1$xsi,"gca_y1_rasch_item_stats.csv")
write.csv(Fit$itemfit,"gca_y1_item_fit.csv")

#plot histograms of ability and item parameters in the same graph

png("g6m_item_map.png")
layout(matrix(c(1,1,2),3,byrow=TRUE))
layout.show(2)
hist(PersonAbility,xlim=c(-3,3),breaks=20,main="GCA Posttest",
     xlab="Student Ability",ylab="Number of Students")
hist(ItemDiff,xlim=c(-3,3),breaks=20, main="",xlab="Item Difficulty",ylab="Number of Items")
dev.off()


##Check to see if parameter invariance holds

#Select just GCA Post Test for 
items_y1<-d[d$year==1,33:57]
items_y2<-d[d$year==2,33:57]

#Run Rasch Model using function "tam" from R package TAM
mod1<-tam.mml(resp=items_y1)
mod2<-tam.mml(resp=items_y2)

#Show item difficulty and SEs & create Item difficulties variable
mod1$xsi
ItemDiff.1 <- mod1$xsi$xsi
ItemDiff.2 <- mod2$xsi$xsi

plot(ItemDiff.1,ItemDiff.2)
cor(ItemDiff.1,ItemDiff.2)
