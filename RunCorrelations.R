source("CorrelationFunctions.R")
tree <- read.tree("Eurycea_Tree")
discrete.data <- read.csv(file="Eurycea_Data.csv", stringsAsFactors=FALSE, row.names=1) #death to factors
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