source("CorrelationFunctions.R")

## Read in a tree for the Eurycea bislineata complex, generated with SNP data. It has branch lengths.
tree <- read.tree("Eurycea_Tree")

## Read in fake, but plausible discrete data about the presence or absence of 'big heads' and presence or absence of 'bright colors' in these salamanders. I left out one taxa to demonstrate the utility of our 'CleanData' function later on.

## Actually, this is introducing some weird formatting error that I haven't yet been able to solve. So I'm going to bail on it for now.
discrete.data <- read.csv(file="Eurycea_Data_2.csv", stringsAsFactors=FALSE,header=FALSE) #death to factors

discrete.data[11,2]<-1

## Instead, I'm just going to simulate even faker data.
discrete.1<-round(runif(length(tree$tip.label)))
names(discrete.1)<-tree$tip.label

discrete.2<-round(runif(length(tree$tip.label)))
names(discrete.2)<-tree$tip.label

## Here I'm just totally making up continuous data for SVL (i.e., snout-vent length) and larval period. 

SVL <- rnorm(31,mean=25,sd=8)
larval.period <- rnorm(31,mean=1.2,sd=0.5)
continuous.data <- cbind(SVL,larval.period)
rownames(continuous.data) <- tree$tip.label

cleaned.continuous <- CleanData(tree, continuous.data)
cleaned.discrete <- CleanData(tree, discrete.data)

VisualizeData(cleaned.continuous)
VisualizeData(cleaned.discrete)
contrasts.answer <- RunContrasts(cleaned.continuous[[1]], cleaned.continuous[[2]])
save(list=ls(), file="CorrelationsResults.rda")
pagel94.answer <- RunPagel94(tree, discrete.1, discrete.2)
save(list=ls(), file="CorrelationsResults.rda")