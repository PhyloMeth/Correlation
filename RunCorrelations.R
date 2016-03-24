source("CorrelationFunctions.R")

setwd("~/Desktop/UTK/Spring_2016/PhyloMeth/Correlation")

## Read in a tree for the Eurycea bislineata complex, generated with SNP data. It has branch lengths.
tree <- read.tree("Eurycea_Tree")

## Read in fake, but plausible discrete data about the presence or absence of 'big heads' and presence or absence of 'bright colors' in these salamanders. I left out one taxa to demonstrate the utility of our 'CleanData' function later on.

## Actually, this is introducing some weird formatting error that I haven't yet been able to solve. So I'm going to bail on it for now.
#discrete.data <- read.csv(file="Eurycea_Data_2.csv", stringsAsFactors=FALSE,header=FALSE) #death to factors

## Instead, I'm just going to simulate even faker data. Here's a bad attempt at it. See below for a better attempt at simulating on a tree.
# discrete.1<-round(runif(length(tree$tip.label)))
# names(discrete.1)<-tree$tip.label

# discrete.2<-round(runif(length(tree$tip.label)))
# names(discrete.2)<-tree$tip.label

## Here I'm just totally making up continuous data for SVL (i.e., snout-vent length) and larval period. 

# SVL <- rnorm(31,mean=25,sd=8)
# larval.period <- rnorm(31,mean=1.2,sd=0.5)
# continuous.data <- cbind(SVL,larval.period)
# rownames(continuous.data) <- tree$tip.label

# Here I'm simulating a discrete trait.
q <- list(rbind(c(-12,12), c(12,-12)))
sim.discrete.1 <- sim.char(tree, q, model='discrete', n=1)
sim.discrete.2 <- sim.char(tree, q, model='discrete', n=1)
# Here I'm reformatting those trait data so that they work in downstream analyses. The format they were in had a weird string of text at the top that was causing problems.
new.discrete.1 <- as.vector(sim.discrete.1)
new.discrete.2 <- as.vector(sim.discrete.2)
names(new.discrete.1) <- row.names(sim.discrete.1)
names(new.discrete.2) <- row.names(sim.discrete.2)

full.discrete <- data.frame(new.discrete.1,new.discrete.2)

# We need this dumb format for the Pagel test.
pagel.discrete <- data.frame(rownames(full.discrete),full.discrete,row.names=NULL)

# Here I'm simulating a continuous trait.
sim.continuous.1 <- sim.char(tree, 1, n=1)
sim.continuous.2 <- sim.char(tree, 2, n=1)
# Here I'm reformatting those trait data so that they work in downstream analyses. The format they were in had a weird string of text at the top that was causing problems.
new.continuous.1 <- as.vector(sim.continuous.1)
new.continuous.2 <- as.vector(sim.continuous.2)
names(new.continuous.1) <- row.names(sim.continuous.1)
names(new.continuous.2) <- row.names(sim.continuous.2)

full.continuous <- data.frame(new.continuous.1,new.continuous.2)

cleaned.discrete <- CleanData(tree, full.discrete)
cleaned.continuous <- CleanData(tree, full.continuous)

VisualizeData(cleaned.continuous)
VisualizeData(cleaned.discrete)

contrasts.answer <- RunContrasts(cleaned.continuous[[1]], cleaned.continuous[[2]])
save(list=ls(), file="CorrelationsResults.rda")
pagel94.answer <- RunPagel94(tree, pagel.discrete)
save(list=ls(), file="CorrelationsResults.rda")