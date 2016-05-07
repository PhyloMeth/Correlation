source("E:/SCHOOL/Documents-School/Class/EEB_603_PhyloMeth/Repository2/Correlation/CorrelationFunctions.R")
#tree <- read.tree("____PATH_TO_TREE_OR_SOME_OTHER_WAY_OF_GETTING_A_TREE____")
# discrete.data <- read.csv(file="____PATH_TO_DATA_OR_SOME_OTHER_WAY_OF_GETTING_TRAITS____", stringsAsFactors=FALSE) #death to factors.
# continuous.data <- read.csv(file="____PATH_TO_DATA_OR_SOME_OTHER_WAY_OF_GETTING_TRAITS____", stringsAsFactors=FALSE) #death to factors.

data(whales)
tree <- whales
discrete.data1 <- rTraitDisc(tree, model = "ER", k=2, states=c(0,1))
discrete.data2 <- rTraitDisc(tree, model = "ER", k=2, states=c(0,1))
discrete.data <- cbind(discrete.data1, discrete.data2)

continuous.data1 <- rTraitCont(tree, model="BM")
continuous.data2 <- rTraitCont(tree, model="BM")
continuous.data <- cbind(continuous.data1, continuous.data2)


cleaned.continuous <- CleanData(tree, continuous.data)
cleaned.discrete <- CleanData(tree, discrete.data)
VisualizeData(tree, cleaned.continuous)
VisualizeData(tree, cleaned.discrete)
contrasts.answer <- RunContrasts(tree, continuous.data)
save(list=ls(), file="CorrelationsResults.rda")
pagel94.answer <- RunPagel94(tree, discrete.data)
save(list=ls(), file="CorrelationsResults.rda")
OtherMethod.answer <- RunOtherMethod(tree, ??????????.data)
