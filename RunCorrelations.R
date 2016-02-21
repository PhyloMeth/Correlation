source("CorrelationFunctions.R")
tree <- read.tree("eucTree.tre")
tree $tip.label <- gsub("_"," ", tree $tip.label)
discrete.data <- read.csv(file="eucDataDiscretized.csv", row.names=1, stringsAsFactors=FALSE) #death to factors.
continuous.data <- read.csv(file="eucData.csv", row.names=1, stringsAsFactors=FALSE) #death to factors.

cleaned.continuous <- CleanData(tree, continuous.data)
cleaned.discrete <- CleanData(tree, discrete.data)
VisualizeData.continuous(tree, cleaned.continuous)
VisualizeData.discrete(tree, cleaned.discrete)
contrasts.answer <- RunContrasts(tree, continuous.data)
save(list=ls(), file="CorrelationsResults.rda")
pagel94.answer <- RunPagel(tree, discrete.data)
save(list=ls(), file="CorrelationsResults.rda")

