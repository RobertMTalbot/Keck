#subset GCA pre and post responses from FSF all data- include cl, Sex and ethnicity codes
GCA_prepost <- subset(FSF_alldata, select=c(StudentID, Semester, cl, Pre1:Post25, Sex:Demo4White5))

#use lapply to find means of all GCA pre and post item responses, write to new list
#GCA_item_Means <- lapply(3:52, mean)

#calculate GCA pre and post total scores for each case add new vars to data frame
Pre_tot <- rowSums(GCA_prepost[4:28], na.rm = TRUE)
Post_tot <- rowSums(GCA_prepost[29:53], na.rm = TRUE)
GCA_prepost$Pre_tot <- Pre_tot
GCA_prepost$Post_tot <- Post_tot

#calculate raw gain and <g> for each case and append new vars to data frame
raw_gain <- (GCA_prepost[62] - GCA_prepost[61])
norm_gain <- raw_gain/(25 - GCA_prepost[61]) 
GCA_prepost$gain <- raw_gain
GCA_prepost$norm_gain <- norm_gain

#filter for cl = 1 and Semester = Fa2015 from GCA_prepost, calc ES and g
cl1f15 <- filter(GCA_prepost, Semester=="Fa2015", cl==1)
cl1f15_ES <- (mean(cl1f15$Post_tot, na.rm = TRUE) - mean(cl1f15$Pre_tot, na.rm = TRUE))/sd(cl1f15$Pre_tot, na.rm = TRUE)
cl1f15_g <- (mean(cl1f15$Post_tot, na.rm = TRUE) - mean(cl1f15$Pre_tot, na.rm = TRUE))/(25-mean(cl1f15$Pre_tot, na.rm = TRUE))
cl1f15gains <- c(cl1f15_ES, cl1f15_g)
#make this a function and then use lapply to iterate over classes and Semesters?

#calculate pre and post totals and gains (<g>, ES) for local items for each class

#code new var for GCA and local scores by learning goal, find gains for these item groupings






