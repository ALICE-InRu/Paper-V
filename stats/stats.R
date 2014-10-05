rm(list=ls(all=TRUE)) # clear workspace

library("ggplot2")
library("plyr")
library("xtable")

theme_set(theme_bw())

#----------------------------------------------------------
traininginfo <- read.csv('stepwisemodel.csv',header=TRUE,sep=",")
mistaTraining <- subset(traininginfo, dim==30 & (trainingdata=='jrnd' | trainingdata=='jrndn') & (track=='mwr' | track=='opt' | track=='cma' | track=='rnd' ))
head(mistaTraining)
summary(mistaTraining)

# reorder the appearance of rankings
mistaTraining$rank <- factor(mistaTraining$rank, c("b", "f", "p" , "a"))

# reorder the appearance of tracks
mistaTraining$track <- factor(mistaTraining$track, c("opt", "cma", "mwr" , "rnd" , "all"))

pl <- qplot(data=mistaTraining,step,l,geom=c("line"),linetype=rank) +
  ylab('size of preference set, l') + xlab('step, k')
# Divide with "trainingdata" vertical, "track" horizontal
pl <- pl + facet_grid( track~trainingdata)
# legend at bottom
pl <- pl + theme(legend.direction = "horizontal",legend.position = "bottom") 
# more descriptive name of ranking legends
pl <- pl + scale_linetype_discrete(name="Model",
  breaks=c("b", "f", "p" , "a"),
  labels=c("base ranking", "full pareto ranking", "partial pareto ranking" , "all rankings")) 
print(pl)
fname=paste('../figures/numTrainingData','.pdf',sep='')
ggsave(file=fname)#, width=6, height=5)
#----------------------------------------------------------


#----------------------------------------------------------
modelinfo <- read.csv('stepwisemodel.csv',header=TRUE,sep=",")
mistaModel <- subset(modelinfo, dim==30 & (trainingdata=='jrnd' | trainingdata=='jrndn') & (track=='mwr' | track=='opt' | track=='cma' | track=='rnd' | track=='all') & C==10)
head(mistaModel)
summary(mistaModel)

# reorder the appearance of rankings
mistaModel$rank <- factor(mistaModel$rank, c("b", "f", "p" , "a"))

# reorder the appearance of tracks
mistaModel$track <- factor(mistaModel$track, c("opt", "cma", "mwr" , "rnd", "all"))

pl <- qplot(data=mistaModel,step,acc,geom=c("line"),linetype=rank) +
  ylab('training accuracy (%)') + xlab('step, k')
# Divide with "trainingdata" horizontal, "track" vertical
pl <- pl + facet_grid(track ~ trainingdata )
# legend at bottom
pl <- pl + theme(legend.direction = "horizontal",legend.position = "bottom") 
# more descriptive name of ranking legends
pl <- pl + scale_linetype_discrete(name="Model",
   breaks=c("b", "f", "p" , "a"),
   labels=c("base ranking", "full pareto ranking", "partial pareto ranking" , "all rankings")) 
print(pl)
fname=paste('../figures/trainingAccuracy','.pdf',sep='')
ggsave(file=fname)#, width=6, height=12)
#----------------------------------------------------------




#----------------------------------------------------------
ratioinfo <- read.csv('stepwiseratio.csv',header=TRUE,sep=",")
mistaRatio <- subset(ratioinfo, dim==30 & ( data=='jrnd' | data=='jrndn' ))
mistaRatio <- subset(mistaRatio, track=='' | track=='mwr' | track=='opt' | track=='cma' | track=='rnd' | track=='all' ) 
#mistaRatio <- subset(mistaRatio, C==10 | C=='')
mistaRatio <- subset(mistaRatio, set=='test') # only report test-set
head(mistaRatio)
summary(mistaRatio)

# reorder the appearance of rankings
mistaRatio$rank <- factor(mistaRatio$rank, c("","b", "f", "p" , "a"))

# reorder the appearance of tracks
mistaRatio$track <- factor(mistaRatio$track, c("opt", "cma", "mwr" , "rnd", "all"))

mistaRatio$trackrank <- interaction(mistaRatio$track, mistaRatio$rank)

pl <- ggplot(data=mistaRatio , aes(y=rho, x=track , fill=rank)) + geom_boxplot() 
# Divide with "trainingdata" vertical, 
pl <- pl + facet_grid(data ~ .)
pl <- pl + xlab('') + ylab(paste('percentage relative deviation from optimality, ',expression(rho),'(%)'))                      
pl <- pl + scale_fill_manual(name="Model\n",
    values=c("grey20","grey40","grey60","grey80","grey100"),
    breaks=c("", "b", "f", "p" , "a"),
    labels=c("ref", "PREF_b", "PREF_f", "PREF_p" , "PREF_a")) 
pl <- pl + theme(legend.direction = "horizontal",legend.position = "bottom") 
print(pl)
fname=paste('../figures/boxplot','.pdf',sep='')
ggsave(file=fname)#, width=6, height=5)
#----------------------------------------------------------
mistaRatio$fullmodel <- interaction(mistaRatio$model, mistaRatio$trackrank)
for(d in unique(mistaRatio$data)) {
  tableMistaRatio <- subset(mistaRatio, data==d)
  model.stats <- ddply(tableMistaRatio, 'fullmodel',
                       function(x){
                         c(mean=mean(x$rho),med=median(x$rho),sd=sd(x$rho),min=min(x$rho),max=max(x$rho))
                      })
  # table 
  model.stats <- arrange(model.stats, mean)
  lbl<-paste('tablesresults',d,sep='')
  print(xtable(model.stats,label=(lbl),caption=paste('Main statistics for $\\mathcal{P}_{',d,'}$')),include.rownames = FALSE,file=paste(lbl,'.txt',sep=''))

  numModels<-length(model.stats$fullmodel)
  pdat <- matrix(c(rep(0,numModels*numModels)), nrow=numModels, ncol=numModels, byrow = TRUE,dimnames=list(model.stats$fullmodel,model.stats$fullmodel))
  for( x in model.stats$fullmodel ) {
    xrho <- tableMistaRatio$rho[which(tableMistaRatio$fullmodel==x)]
    for( y in model.stats$fullmodel ) {
      yrho <- tableMistaRatio$rho[which(tableMistaRatio$fullmodel==y)]
      ans <- ks.test(xrho,yrho)
      pdat[x,y] <- ans$p
    }
  }
  lbl2<-paste(lbl,'pvalues',sep='')
  print(xtable(pdat,label=(lbl2),caption=paste('p-values from Kolmogorov-Smirnov test for $\\mathcal{P}_{',d,'}$ models.',sep='')),rotate.colnames=TRUE,file=paste(lbl2,'.txt',sep=''),size="small")
  
  
  # skoða ranking vs. ranking
  pdat['PREF.opt.f','PREF.opt.p']
  pdat['PREF.cma.f','PREF.cma.p']
  pdat['PREF.mwr.f','PREF.mwr.p']
  pdat['PREF.rnd.f','PREF.rnd.p']

  pdat['PREF.opt.b','PREF.opt.p']
  pdat['PREF.cma.b','PREF.cma.p']
  pdat['PREF.mwr.b','PREF.mwr.p']
  pdat['PREF.rnd.b','PREF.rnd.p']
  
  pdat['PREF.opt.b','PREF.opt.f']
  pdat['PREF.cma.b','PREF.cma.f']
  pdat['PREF.mwr.b','PREF.mwr.f']
  pdat['PREF.rnd.b','PREF.rnd.f']
  
  pdat['PREF.opt.p','PREF.opt.a']
  pdat['PREF.cma.p','PREF.cma.a']
  pdat['PREF.mwr.p','PREF.mwr.a']
  pdat['PREF.rnd.p','PREF.rnd.a']
  
  pdat['PREF.opt.f','PREF.opt.a']
  pdat['PREF.cma.f','PREF.cma.a']
  pdat['PREF.mwr.f','PREF.mwr.a']
  pdat['PREF.rnd.f','PREF.rnd.a']
  
  pdat['PREF.opt.b','PREF.opt.a']
  pdat['PREF.cma.b','PREF.cma.a']
  pdat['PREF.mwr.b','PREF.mwr.a']
  pdat['PREF.rnd.b','PREF.rnd.a']
  
  
}




#====================== weights LION7
dat <- read.csv('weights.csv',header=TRUE)
dat$model <- factor(dat$model, levels=c("PREF","CMA-ES"))
mdat<-melt(subset(dat,trainingdata=='jrnd'),id.vars=c('trainingdata','model','track','rank','step'),value.name='weight')
p <- ggplot(mdat,aes(x=step,y=weight,linetype=model))+theme_bw()
p <- p +geom_line()
#p <- p + geom_smooth(stat='identity')
p <- p+facet_wrap(~variable,ncol=2)
p <- p + theme(legend.direction='horizontal',legend.position = c(0.75,0)) 
p
fname=paste('../figures/weights','.pdf',sep='')
ggsave(file=fname)#, width=6, height=5)