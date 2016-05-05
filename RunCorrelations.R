setwd("~/GitHub/Correlation")
source("CorrelationFunctions.R")
library(ape) #utility fns
library(geiger) #utilty fns
library(corHMM)
library(phylolm)
library(phytools)

# all of my hibiscus and trifolium trees have polytomies
# in an effort to make this exercise work, I am using a simulated tree


tree<-pbtree(n=20,scale=1,tip.label=(c("T.ochroleucum","T.palmeri","T.calocephalum","T.nanum","T.pallescens","T.repens","T.amabile","T.decorum","T.wigginsii","T.virginicum","T.vesiculosum","T.vernum","T.uniflorum","T.tomentosa","T.thalii","T.sylvatica","T.alpestre","T.argentinense","T.pratense","T.hybridum")))

dis.1<-c(0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1)
dis.2<-c(0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1)
discrete.data<-cbind.data.frame(row.names=tree$tip.label,dis.1,dis.2)


con.1<-c(8,  6, 14, 16, 22, 10, 23, 19,  7,  6, 12, 14, 18, 24, 20,  9, 28,  6, 22, 17)
con.2<-c(1, 2, 3, 4, 1, 5, 5, 1, 6, 3, 2, 4, 4, 2, 2, 5, 1, 4, 1, 5)
continuous.data<-cbind.data.frame(row.names=tree$tip.label,con.1,con.2)


#i decided to use the above method for putting data into this exercise, that way you won't need my data files
#discrete.data <- read.csv(file="DumdatDisc.csv", row.names=1, stringsAsFactors=FALSE) #death to factors.
#continuous.data <- read.csv(file="DumdatCont.csv", row.names=1, stringsAsFactors=FALSE) #death to factors.

cleaned.continuous <- CleanData(tree, continuous.data)
cleaned.discrete <- CleanData(tree, discrete.data)
VisualizeData(tree, cleaned.continuous)
VisualizeData(tree, cleaned.discrete)
contrasts.answer <- RunContrasts(tree, cleaned.continuous$data)
save(list=ls(), file="CorrelationsResults.rda")
pagel94.answer <- RunPagel94(tree, cleaned.discrete$data)
save(list=ls(), file="CorrelationsResults.rda")

