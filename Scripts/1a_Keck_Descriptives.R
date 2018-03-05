setwd("~/Dropbox/Github/Keck/Original Data")

#d<-read.csv("GCAStudy_ALL DATA__09072017_working.csv",header=T,na.strings=c(""))
# Don't use this import command, the na.strings option screws things up for GCA items

d<-read.csv("GCAStudy_ALL DATA__09072017_working.csv",header=T)

names(d)

nrow(d)
d$StudentID<-as.character(d$StudentID)
#Only retain students with valid student IDs
d<-d[d$StudentID!="",]
#Exclude following cases based on email exchange with JA, 2/5/18

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

###GCA Pre-Test and Post-Test Data by Class, Course and Institution

##----------Scoring GCA items with function "score" from CTT package

library(CTT)

# Basis for this answer key comes from the tab "GCA Details"
# in spreadsheet "GCAStudy_ALL DATA__06112017_working.xlsx"
answer.key<-c(5,3,3,1,3,3,4,2,2,3,4,3,4,3,3,2,5,3,2,3,3,4,1,4,2)

# Filter Students by those who attempted at least 15 of the 25 GCA items

#These two variables indicate the number of missing response on pre-test and post-test for each student
totMiss.pre<-apply(d[,18:42], MARGIN=1, function(x) {sum(is.na(x))}) 
totMiss.pos<-apply(d[,43:67], MARGIN=1, function(x) {sum(is.na(x))}) 

#d<-d[d$GCA.pre.done==1 & d$GCA.post.done==1,]

d<-d[totMiss.pre<10 & totMiss.pos<10,]

# Treat remaining missing responses as incorrect answers

d[,18:67][is.na(d[,18:67])]<-0

# Get reliability and total score across all students
out.pre<-score(d[,18:42],output.scored = TRUE,answer.key,rel=TRUE)
out.pos<-score(d[,43:67],output.scored = TRUE,answer.key,rel=TRUE)
rel.pre<-reliability(out.pre$scored,itemal=TRUE)
rel.pre
rel.pos<-reliability(out.pos$scored,itemal=TRUE)
rel.pos
#Total Score
GCA.pre<-out.pre$score
GCA.pos<-out.pos$score
#Gain from Pre to Post
GCA.dif<-GCA.pos-GCA.pre
#Std Gain
GCA.std.gain<-GCA.dif/sd(GCA.pos)
#Bind total score to original dataframe
d<-cbind(d,GCA.pre,GCA.pos,GCA.dif,GCA.std.gain)

#Export scored items for later use with Rasch Model

setwd("~/Dropbox/Github/Keck/Analysis Data")
GCA.Rasch<-data.frame(d[,1:6],out.pre$scored,out.pos$scored)
write.csv(GCA.Rasch,"GCA.Rasch.csv")

##Reliability by semester and course

#Vector with unique course id numbers
courses<-sort(unique(d$cl))
#initialize data.frame to collect reliability estimates
alpha<-data.frame(matrix(ncol=2,nrow=length(courses)))

#Reliability of GCA pre-tests by course
for (i in 1:length(courses)) {
  temp<-out.pre$scored[d$cl==courses[i],]
  out<-reliability(temp)
  alpha[i,1]<-out$alpha
}
#Reliability of GCA post-tests by course
for (i in 1:length(courses)) {
  temp<-out.pos$scored[d$cl==courses[i],]
  out<-reliability(temp)
  alpha[i,2]<-out$alpha
}

alpha<-cbind(courses,alpha)
names(alpha)<-(c("cl","Pre alpha","Post alpha"))
#This shows the estimates of reliability by course 
alpha

##----------Now create tables with GCA means and SDs 
##----------by course, institution and semester for pre, post and gain

#Use the dplyr package and the summarise function
#to get mean and SD of GCA scores by semester and course
#Note: this could also be done using the aggregate function

library(dplyr)
by_course <- group_by(d,Semester,Institution,Course.Code,cl)
results <- summarise(by_course,
                     count = n(),
                     GCA_pre_mean = mean(GCA.pre, na.rm = TRUE),
                     GCA_pre_sd = sd(GCA.pre, na.rm = TRUE),
                     GCA_pos_mean = mean(GCA.pos, na.rm = TRUE),
                     GCA_pos_sd = sd(GCA.pos, na.rm = TRUE),
                     GCA_dif_mean = mean(GCA.dif, na.rm = TRUE),
                     GCA_dif_sd = sd(GCA.dif, na.rm = TRUE),
                     GCA_std_mean = mean(GCA.std.gain, na.rm = TRUE))

results<-as.data.frame(results) 
# Really easy to play with in R. Here I use the "arrange" function
# also from the dplyr package to sort classes by those with
# largest pre to post GCA gains
arrange(results,desc(GCA_dif_mean))[c(1:4,10)]

##EXPORT RESULTS FOR SUMMARY TABLE

#Merge alpha to results using "cl" as common column
df<-merge(results,alpha)
#Export results as a csv file so you can create a summary table
#I use the format function as a wrapper so it only
#prints out to two digits, otherwise need to do by hand in excel
setwd("~/Dropbox/Github/Keck/Tables and Figures")
write.csv(format(df, digits=2, nsmall=2),"GCA_results.csv") 

##----------VISUALIZATIONS USING GGPLOT

#install.packages(ggplot2)
library(ggplot2)

setwd("~/Dropbox/Github/Keck/Tables and Figures")

##Let's look at some histograms first

#All Students, Two Histograms Overlapping on One Plot

g <-  ggplot(d, aes(x)) + geom_histogram(aes(x = GCA.pre, y = ..count..),
                                         binwidth=3,breaks=seq(0, 30, by=3), color="black", fill="blue", alpha=0.5) + 
  geom_histogram( aes(x = GCA.pos, y = ..count..), 
                  binwidth=3,breaks=seq(0, 30, by=3), color="black", fill= "green", alpha=0.5) +
  scale_x_continuous(name = "GCA Pre/Post Score",breaks=seq(0, 30, by=3)) +
  scale_y_continuous(name = "Number of Students",breaks=seq(0, 600, by=50)) +
  ggtitle("GCA Pre and Post Scores") + theme(plot.title = element_text(hjust = 0.5))
g

#Histograms By Class

#ggplot(data=d,aes(x)) +
#  geom_bar(aes(x=GCA.pre,y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]) ,fill="blue",color="black",alpha=0.5)+
#  scale_x_continuous(name="GCA Pre to Post Test Score Distribution", breaks=seq(0, 30, by=3)) +
#  ylab("Percent of Class per GCA Point") +
#  geom_bar(aes(x = GCA.pos, y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]),breaks=seq(0, 30, by=3), color="black", fill= "green", alpha=0.5) +
#  facet_grid(Course.Code~Semester) +
#  ggtitle("Using count and tapply") + theme(plot.title = element_text(hjust = 0.5))

png("gca_raw_hist_density_facets.png")

ggplot(data=d,aes(x)) +
  geom_histogram(aes(x=GCA.pre,y=..density..),binwidth=3,breaks=seq(0, 30, by=3),fill="blue",color="black",alpha=0.5)+
  scale_x_continuous(name="GCA Pre to Post Test Score Distribution", breaks=seq(0, 30, by=3)) +
  ylab("Density Scale") +
  geom_histogram( aes(x = GCA.pos, y = ..density..), 
                  binwidth=3,breaks=seq(0, 30, by=3), color="black", fill= "green", alpha=0.5) +
  facet_grid(Course.Code~Semester) +
  ggtitle("Blue=Pre, Green=Post") + theme(plot.title = element_text(hjust = 0.5))

dev.off()

png("gca_raw_hist_counts_facets.png")

ggplot(data=d,aes(x)) +
  geom_histogram(aes(x=GCA.pre,y=..count..),binwidth=3,breaks=seq(0, 30, by=3),fill="blue",color="black",alpha=0.5)+
  scale_x_continuous(name="GCA Pre to Post Test Score Distribution", breaks=seq(0, 30, by=3)) +
  ylab("Number of Students per bin") +
  geom_histogram( aes(x = GCA.pos, y = ..count..), 
                  binwidth=3,breaks=seq(0, 30, by=3), color="black", fill= "green", alpha=0.5) +
  facet_grid(Course.Code~Semester, scales="free") +
  ggtitle("Blue=Pre, Green=Post") + theme(plot.title = element_text(hjust = 0.5))

dev.off()

ggplot(data=d,aes(x=GCA.dif)) +
  geom_histogram(aes(y=..density..),binwidth=3,breaks=seq(-22, 22, by=4),fill="darkblue",color="black")+
  scale_x_continuous(name="GCA Gain Score Distribution", breaks=seq(-22, 22, by=4)) +
  facet_grid(Course.Code~Semester)

##Scatterplots of GCApre vs GCApost

#All Students

R<-cor(d$GCA.pre,d$GCA.pos)

ggplot(data=d,aes(x=GCA.pre, y= GCA.pos)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) +
  annotate("text",label=paste("r =",round(R,2)),x=20,y=5,color="black") + theme_bw() +
  scale_x_continuous(name="GCA Pre", breaks=seq(0, 25, by=5)) +
  scale_y_continuous(name="GCA Post", breaks=seq(0, 25, by=5)) 

#By Class

ggplot(data=d,aes(x=GCA.pre, y= GCA.pos,color=Course.Code)) + 
  geom_point() + geom_smooth(method=lm,se=TRUE) + 
  facet_grid(Course.Code~Semester) +theme_bw() +
  scale_x_continuous(name="GCA Pre", breaks=seq(0, 25, by=5)) +
  scale_y_continuous(name="GCA Post", breaks=seq(0, 25, by=5)) 


