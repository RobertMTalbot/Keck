








sensitivity<-function(grade,test){  
  by_school <- group_by(d,eval(parse(text(paste0("schid",grade,"n")))),eval(parse(text(paste0("cltype",grade)))))
  results <- summarise(by_school,
                       count = n())
  
  results<-as.data.frame(results)  
  
  #Reshape this dataframe into wide format so that each school is a unique
  #row and count for each class size condition is unique column
  
  y<-spread(results,paste0("cltype",grade),count)
  
  #Create new dummy variable "screwy" that = 1 if a school has small class condition
  # where count of students is > than # in regular or reg + aide conditions
  
  y$screwy<-NULL
  y$screwy[(y$'1'>=y$'2') | (y$'1'>=y$'3')]<-1
  y$screwy[(y$'1'<y$'2') & (y$'1'<y$'3')]<-0
  
  ns.schools<-y[,paste0("schid",grade,"n")][y$screwy==0]
  ns.schools<-ns.schools[!is.na(ns.schools)]
  ns.schools<-as.character(ns.schools)
  
  ##Create small class dummy variable. 1 = small class, 0 = reg or reg + aide conditions
  
  ##Impact of Including School Fixed Effects
  
  d$small<-NULL
  d$small[d[,paste0("cltype",grade)]==1]<-1
  d$small[d[,paste0("cltype",grade)] != 1]<-0
  
  #Compare small class vs not small class, no school fixed effects
  
  out1<-lm(paste0("t",test,"ss",grade)~small,data=d)
  x1<-out1$coef[2]/sd(d[,paste0("t",test,"ss",grade)],na.rm=T)
  
  #Compare small class vs not small class, WITH school fixed effects
  
  out2<-lm(paste0("t",test,"ss",grade)~0 + small + paste0("schid",grade,"n"),data=d)
  x2<-out2$coef[1]/sd(d[,paste0("t",test,"ss",grade)],na.rm=T)
  
  #Now let's restrict this to only schools where data is not screwy!
  
  out3<-lm(paste0("t",test,"ss",grade)~0 + small+paste0("schid",grade,"n"),
           data=d[d[,paste0("schid",grade,"n")] %in% ns.schools,])
  x3<-out3$coef[1]/sd(d[,paste0("t",test,"ss",grade)],na.rm=T)
  
  return(c(x1,x2,x3,length(ns.schools)))
}


