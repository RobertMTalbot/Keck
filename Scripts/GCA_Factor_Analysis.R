## Factor Analysis of GCA 
## Derek Briggs, May 18, 2017

#d<-read.csv(1516GCA_FA.csv)

library(psych)

d<-GCA_prepost[29:53]

x<-seq(from=1, to=nrow(d), by=2)
y<-seq(from=2, to=nrow(d), by=2)

d1<-d[x,]
d2<-d[y,]

df1<-as.data.frame(d1)

##Item Descriptives and Alpha

alpha(df1,na.rm=T)

##Dimensionality Analyses

cm<-cor(d1,use="complete.obs")

ev <- eigen(cm)

set.seed(1234)  # set the seed so that you can reproduce your parallel analysis exactly

library(corrplot)

cor.plot(cm)

#Parallel Analysis

fa.parallel(cm, n.obs = nrow(d1), fm="minres", fa="both", 
            main = "Parallel Analysis Scree Plots",
            n.iter=100, error.bars=FALSE, SMC=FALSE,
            ylabel=NULL, show.legend=TRUE)

#EFA, using calibration sample

mod1<-fa(d1,nfactors=4,rotate="Promax",fm="pa",cor="tet")
mod2<-fa(d1,nfactors=3,rotate="Promax",fm="pa",cor="tet")

print(mod1,cut=.3)
print(mod2,cut=.3)

library(corrplot)

cor.plot(mod1)

##CFA based on EFA Solution, using validation sample

library("lavaan")

fac4_mod <- '
factor1 =~ 
Post2 + Post6 + Post10 + Post11 + Post16 + Post18
+ Post20 + Post23
factor2 =~ Post1 + Post2 + Post4 + Post5 + Post7 + Post8
+ Post11 + Post22 + Post24
factor3 =~ Post3 + Post5 + Post7 + Post8 + Post11 + Post22 + Post24
factor4 =~ Post19 + Post20'
fac4mod <- cfa(model = fac4_mod, data = d2, ordered = names(d2) ,estimator = "DWLS", se = "robust", test = "scaled.shifted", std.lv = TRUE)
summary(fac4mod,fit.measures = TRUE)
mi <- modindices(fac4mod)
mi[mi$op == "=~",]

#Dropping item 19, simple structure (no cross-loadings for items 5, 10)

fac3_mod <- '
factor1 =~ 
Post1 + Post4 + Post7 + Post8 + Post13
+ Post15 + Post22 + Post24
factor2 =~ Post2 + Post6 + Post10 + Post11 + Post12 + Post16
+ Post18 + Post20 + Post23
factor3 =~ Post3 + Post5 + Post9 + Post17 + Post21'
fac3mod <- cfa(model = fac3_mod, data = d2, ordered = names(d2) ,estimator = "DWLS", se = "robust", test = "scaled.shifted", std.lv = TRUE)
summary(fac3mod,fit.measures = TRUE)
mi <- modindices(fac3mod)
mi[mi$op == "=~",]


