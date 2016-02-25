source("CorrelationFunctions.R")

## Read in a tree for the Eurycea bislineata complex, generated with SNP data. It has branch lengths.
tree <- read.tree("Eurycea_Tree")

## Read in fake, but plausible discrete data about the presence or absence of 'big heads' and presence or absence of 'bright colors' in these salamanders. I left out one taxa to demonstrate the utility of our 'CleanData' function later on.
discrete.data <- read.csv(file="Eurycea_Data.csv", stringsAsFactors=FALSE, row.names=1) #death to factors

## Here I'm just totally making up continuous data for SVL (i.e., snout-vent length) and larval period. 

SVL <- rnorm(31,mean=25,sd=8)
larval.period <- rnorm(31,mean=1.2,sd=0.5)
continuous.data <- cbind(SVL,larval.period)
rownames(continuous.data) <- tree$tip.label

cleaned.continuous <- CleanData(tree, continuous.data)
cleaned.discrete <- CleanData(tree, discrete.data)
VisualizeData(tree, cleaned.continuous)
VisualizeData(tree, cleaned.discrete)
contrasts.answer <- RunContrasts(tree, continuous.data)
save(list=ls(), file="CorrelationsResults.rda")
pagel94.answer <- RunPagel(tree, discrete.data)
save(list=ls(), file="CorrelationsResults.rda")