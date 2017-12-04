summary(d$cl1_fc26)

table(d$cl1_fc26)

##Jenny's class, Spring 17

cl17<-d[,521:556]

cub<-subset(cl17,d$cl==17)

summary(cub)

#UGA class, Spring 17

cl20<-d[,618:647]
uga<-subset(cl20,d$cl==20)

names(uga)

apply(uga,2,table)

