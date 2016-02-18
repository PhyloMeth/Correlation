source("CorrelationFunctions.R")
tree <- read.tree("____PATH_TO_TREE_OR_SOME_OTHER_WAY_OF_GETTING_A_TREE____")
discrete.data <- read.csv(file="____PATH_TO_DATA_OR_SOME_OTHER_WAY_OF_GETTING_TRAITS____", stringsAsFactors=FALSE) #death to factors.
continuous.data <- read.csv(file="____PATH_TO_DATA_OR_SOME_OTHER_WAY_OF_GETTING_TRAITS____", stringsAsFactors=FALSE) #death to factors.

cleaned.continuous <- CleanData(tree, continuous.data)
cleaned.discrete <- CleanData(tree, discrete.data)
VisualizeData(tree, cleaned.continuous)
VisualizeData(tree, cleaned.discrete)
contrasts.answer <- RunContrasts(tree, continuous.data)
save(list=ls(), file="CorrelationsResults.rda")
pagel94.answer <- RunPagel(tree, discrete.data)
save(list=ls(), file="CorrelationsResults.rda")

