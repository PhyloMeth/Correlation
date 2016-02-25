library(geiger)
setwd("/Users/Hailee/Documents/School/Graduate_School/Spring_2016/PhyloMeth/Correlation")

source("CorrelationFunctions.R")
tree <- get_study_tree("pg_2346","tree4944")
plot(tree,cex=0.3)
discrete.data <- read.csv(file="/Users/Hailee/Desktop/taxa.csv", stringsAsFactors=FALSE,row.names=NULL)#death to factors.

latitude<- rnorm(128,mean=89,sd=0.5)
height<-rnorm(128,mean=2,sd=0.5)
continuous.data<-cbind(latitude,height)
rownames(continuous.data)<-tree$tip.label


cleaned.continuous <- CleanData(tree, continuous.data)
cleaned.discrete <- CleanData(tree, discrete.data)
VisualizeData(tree, cleaned.continuous)
VisualizeData(tree, cleaned.discrete)
contrasts.answer <- RunContrasts(tree, continuous.data)
save(list=ls(), file="CorrelationsResults.rda")
pagel94.answer <- RunPagel94(tree, discrete.data)
save(list=ls(), file="CorrelationsResults.rda")
