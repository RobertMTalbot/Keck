## For Richard
## Other Approaches for Scoring MC items that don't 
## involve using the CTT package
## just to show how this could be done on your own


##--------------Approach #2 to scoring GCA items--using two for loops

y<-x<-data.frame(matrix(rep(0,nrow(d)*25),ncol=25,nrow=nrow(d)))
names(x)<-names(d)[20:44]
names(y)<-names(d)[45:69]

#Scoring responses to pre-test GCA items
for (i in 1:25){
  x[,i][d[,i+19]==answer.key[i]]<-1
}

#Scoring responses to post-test GCA items
for (i in 1:25){
  y[,i][d[,i+44]==answer.key[i]]<-1
}

##--------------Approach #3 to scoring GCA items--crudest approach

# Here's a less elegant (but more transparent) way to 
# accomplish the same thing if you take out comments below

# x$GCAPre1[d$GCAPre1==5] <-1 ; x$GCAPre2[d$GCAPre2==3] <-1
# x$GCAPre3[d$GCAPre3==3] <-1 ; x$GCAPre4[d$GCAPre4==1] <-1
# x$GCAPre5[d$GCAPre5==3] <-1 ; x$GCAPre6[d$GCAPre6==3] <-1
# x$GCAPre7[d$GCAPre7==4] <-1 ; x$GCAPre8[d$GCAPre8==2] <-1
# x$GCAPre9[d$GCAPre9==2] <-1 ; x$GCAPre10[d$GCAPre10==3] <-1
# x$GCAPre11[d$GCAPre11==4] <-1 ; x$GCAPre12[d$GCAPre12==3] <-1
# x$GCAPre13[d$GCAPre13==4] <-1 ; x$GCAPre14[d$GCAPre14==3] <-1
# x$GCAPre15[d$GCAPre15==3] <-1 ; x$GCAPre16[d$GCAPre16==2] <-1
# x$GCAPre17[d$GCAPre17==5] <-1 ; x$GCAPre18[d$GCAPre18==3] <-1
# x$GCAPre19[d$GCAPre19==2] <-1 ; x$GCAPre20[d$GCAPre20==3] <-1
# x$GCAPre21[d$GCAPre21==3] <-1 ; x$GCAPre22[d$GCAPre22==4] <-1
# x$GCAPre23[d$GCAPre23==1] <-1 ; x$GCAPre24[d$GCAPre24==4] <-1
# x$GCAPre25[d$GCAPre25==2] <-1 
#------------------------------------------------