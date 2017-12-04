## As a first comment, always make sure you provide a header that explains what your script 
## file is doing, provide your name and a date.
## Also need to make sure where data is coming from--what's the working directory you are using?
## Needs to be possible for me to replicate everything by running your code.

#reading in the data
GCAStudy_data <- read.csv("GCAStudy_ALL DATA__06112017_working.csv", header = TRUE)
# investigating the data, getting to know it
str(GCAStudy_data)
names(GCAStudy_data)
#renaming funky variable name -too long
colnames(GCAStudy_data)[12] <- "Local.post.done"
colnames(GCAStudy_data) #confirming change
#subsetting Pre-test
GCAPre <- GCAStudy_data[,20:44]

## Here's another approach using the function "select" from the pacakge "dplyr"
## nice because you can select a range of variables using column names instead of numbers

## install.packages(dplyr)
## library(dplyr)
## GCAPre<-select(GCAStudy_data,GCAPre1:GCAPre25)

head(GCAPre) 
is.data.frame(GCAPre)
#creating vector for means
GCAPre_Means <- colMeans(GCAPre, na.rm = TRUE)
GCAPre_Means 

## So first off, what you have above doesn't make sense as you are taking the means of each GCA item 
## over all respondents in the dataset.  But that's not what we want.
## We want to know the mean GCA score (mean of all items) by each distinct course
## So step 1 is to create a mean GCA score variable for each row
## Step 2 is to take the mean and SD of these means by course (variable = "cl")
## See if you can figure out how to accomplish this. Hint: consider using the aggregate function

## BUT, another key issue here is that it is clear that the GCA items have not been scored--
## it looks like the values 1 through 5 are indicating the item response option.
## If you go back to the original spreadsheet with source data, have a look at the sheet
## that reads "GCA Details".  This shows the scoring key. You'll need to apply this to transform
## the items into 1's (corect) and 0's (incorrect) BEFORE computing mean total score for each
## respondent. 


is.vector(GCAPre_Means)
#creating vector for sd
GCAPre_sd <- apply(GCAPre, 2, sd, na.rm = TRUE)
GCAPre_sd
#combining vectors into new dataframe
GCAPre_stats <- data.frame(GCAPre_Means, GCAPre_sd)
head(GCAPre_stats)
#subsetting post-test
GCAPost <- GCAStudy_data[,45:69]
head(GCAPost)
#creating vector for means
GCAPost_Means <- colMeans(GCAPost, na.rm = T)
head(GCAPost_Means)
#creating vector for sd
GCAPost_sd <- apply(GCAPost, 2, sd, na.rm = TRUE)
GCAPost_sd
#combining vectors into new dataframe
GCAPost_stats <- data.frame(GCAPost_Means, GCAPost_sd)
head(GCAPost_stats)
#combining all vectors into one dataframe
GCAPre_Post_stats <- data.frame(GCAPre_Means, GCAPre_sd, GCAPost_Means, GCAPost_sd)
GCAPre_Post_stats
#adding a Gains column (post - pre)
GCAPre_Post_stats$Gains <- GCAPre_Post_stats$GCAPost_Means - GCAPre_Post_stats$GCAPre_Means

# getting alpha
GCAPre_alpha <- alpha(GCAPre, check.keys=T)
GCAPre_alpha
