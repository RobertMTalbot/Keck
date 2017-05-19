library(psych)

d<-GCA_prepost[29:53]

summary(d)

cm<-cor(d,use="complete.obs")

ev <- eigen(cm)

set.seed(1234)  # set the seed so that you can reproduce your parallel analysis exactly
fa.parallel(cm, n.obs = nrow(d), fm="minres", fa="both", 
            main = "Parallel Analysis Scree Plots",
            n.iter=100, error.bars=FALSE, SMC=FALSE,
            ylabel=NULL, show.legend=TRUE)

mod1<-fa(d,nfactors=4,rotate="Promax",fm="minres",cor="tet")

print(mod1,cut=.3)

df<-as.data.frame(d)

alpha(df,na.rm=T)
