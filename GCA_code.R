#subset GCA pre and post responses from FSF all data- include cl, Sex and ethnicity codes
GCA_prepost <- subset(FSF_alldata, select=c(StudentID, Semester, cl, Pre1:Post25, Sex:Demo4White5))

#use lapply to find means of all GCA pre and post item responses, write to new list
#GCA_item_Means <- lapply(3:52, mean)

#calculate GCA pre and post total scores for each case add new vars to data frame
Pre_tot <- rowSums(GCA_prepost[4:28], na.rm = TRUE)
Post_tot <- rowSums(GCA_prepost[29:53], na.rm = TRUE)
GCA_prepost$Pre_tot <- Pre_tot
GCA_prepost$Post_tot <- Post_tot

#define normalized change function. NEEDS WORK!
normchange <- function(x, y)
{
  if (y>x) 
  {(y-x)/(100-x)}
  else
  {if (y == 100 | y == 0 & x == 100 | x == 00)
  {NA}
    else
    {if (y == x)
    {0}
      else
      {if (y>x)
      {(y-x)/x}
      }}}
}

#calculate GCA pre and post %ages and normalized change c for each student, append vars to data frame
GCA_prepost$Pre_perc <- (GCA_prepost$Pre_tot/25)*100
GCA_prepost$Post_perc <- (GCA_prepost$Post_tot/25)*100
GCA_prepost$norm_c <- normchange(GCA_prepost$Pre_perc, GCA_prepost$Post_perc)


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



#histogram scraps for testing:

library(ggplot2)
df <-  data.frame(x = cl1f15$Pre_tot, x2 = cl1f15$Post_tot)

g2 <-  ggplot(df, aes(x)) + geom_histogram(aes(x = x, y = ..count..),
                  binwidth = 1, color="black", fill="blue", alpha=0.5) + 
  geom_histogram( aes(x = x2, y = ..count..), 
                  binwidth = 1, color="black", fill= "green", alpha=0.5) +
  scale_x_continuous(name = "GCA Pre/Post Score", 
                     limits=c(0, 25)) +
  scale_y_continuous(name = "Count") +
  ggtitle("Class 1 Fall 2015 GCA Pre and Post Scores")
print(g2)

##

g2 <- ggplot(df, aes(x = x, fill = x2)) + 
  geom_histogram(binwidth = 1, 
                 position="identity", alpha=0.6) + 
  scale_x_continuous(name = "GCA Pre/Post Score",                                                                                                                                   
                     breaks = seq(0, 175, 25),                                                                                                                           
                     limits=c(0, 25)) +
  scale_y_continuous(name = "Count") +
  ggtitle("Histogram of GCA Pre and Post Scores") +
  theme_bw() +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Arial", face = "bold"),
        text=element_text(family="Arial"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9)) +
  scale_fill_brewer(palette="Accent")
print(g2)

##
reg1 <- lm(norm_gain~Pre_tot, data = cl1f15)
summary(reg1)
prenorm1 <- ggplot(cl1f15, aes(x = Pre_tot, y = norm_gain)) + 
            geom_point(shape = 1) + geom_smooth(method = lm, formula = y~x, color = "darkred", fill = "blue") +
            (ggtitle("Class 1 Fall 2015 Individual <g> vs Pre-test score"))
print(prenorm1)

#normalized change function
normchange <- function(pre, post)
{
  if (post>pre) 
  {(post-pre)/(100-pre)}
  else
  {if (post&pre == 100 | 0)
  {NA}
    else
    {if (post == pre)
    {0}
      else
      {if (post>pre)
          {(post-pre)/pre}}}}
}
