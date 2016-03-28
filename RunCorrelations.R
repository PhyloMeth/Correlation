source("/Users/orlandoschwery/Documents/UT/Courses&Workshops/Spring16/Phylometh/Correlation/CorrelationFunctions.R")
tree <- read.tree("/Users/orlandoschwery/Documents/UT/Courses&Workshops/Spring16/Phylometh/Correlation/dateddung2.tre")
#discrete.data <- read.csv(file="/Users/orlandoschwery/Documents/UT/Courses&Workshops/Spring16/Phylometh/Correlation/Correlation_traits_discrete.csv", stringsAsFactors=FALSE, header=TRUE, row.names=1) #death to factors.
#continuous.data <- read.csv(file="/Users/orlandoschwery/Documents/UT/Courses&Workshops/Spring16/Phylometh/Correlation/Correlation_traits_contin.csv", stringsAsFactors=FALSE, header=TRUE, row.names=1) #death to factors.

discrete.data1 <- rTraitDisc(tree, model = "ER", k=2, states=c(0,1))
discrete.data2 <- rTraitDisc(tree, model = "ARD", k=2, rate=c(0.3, 0.5), states=c(0,1))
discrete.data <- cbind(discrete.data1, discrete.data2)
discrete.data[discrete.data==1] <- 0
discrete.data[discrete.data==2] <- 1
discrete.data1[discrete.data1==1] <- 0
discrete.data1[discrete.data1==2] <- 1
continuous.data1 <- rTraitCont(tree, model="BM")
continuous.data2 <- rTraitCont(tree, model="OU")
continuous.data <- cbind(continuous.data1, continuous.data2)

# office mac
source("/Users/oschwery/Documents/UTK-Orlando/MiscSync/Correlation/CorrelationFunctions.R")
tree <- read.tree("/Users/oschwery/Documents/UTK-Orlando/MiscSync/Correlation/dateddung2.tre")
#discrete.data <- read.csv(file="/Users/oschwery/Documents/UTK-Orlando/MiscSync/Correlation/Correlation_traits_discrete.csv", stringsAsFactors=FALSE, header=TRUE, row.names=1) #death to factors.
#continuous.data <- read.csv(file="/Users/oschwery/Documents/UTK-Orlando/MiscSync/Correlation/Correlation_traits_contin.csv", stringsAsFactors=FALSE, header=TRUE, row.names=1) #death to factors.

#LaptopE
source("/Users/orlandoschwery/Documents/UT/MiscSync/Correlation/CorrelationFunctions.R")
tree <- read.tree("/Users/orlandoschwery/Documents/UT/MiscSync/Correlation/dateddung2.tre")
#discrete.data <- read.csv(file="/Users/orlandoschwery/Documents/UT/MiscSync/Correlation/Correlation_traits_discrete.csv", stringsAsFactors=FALSE, header=TRUE, row.names=1) #death to factors.
#continuous.data <- read.csv(file="/Users/orlandoschwery/Documents/UT/MiscSync/Correlation/Correlation_traits_contin.csv", stringsAsFactors=FALSE, header=TRUE, row.names=1) #death to factors.



cleaned.continuous <- CleanData(tree, continuous.data)
cleaned.discrete <- CleanData(tree, discrete.data)
VisualizeData(cleaned.continuous$phy, cleaned.continuous$data)
VisualizeData(cleaned.discrete$phy, cleaned.discrete$data)
contrasts.answer <- RunContrasts(cleaned.continuous$phy, cleaned.continuous$data)
save(list=ls(), file="CorrelationsResults.rda")
pagel94.answer <- RunPagel94(cleaned.discrete$phy, cleaned.discrete$data)
save(list=ls(), file="CorrelationsResults.rda")
other.answer <- RunOtherMethod(cleaned.discrete$phy, cleaned.discrete$data, cleaned.continuous$data)
save(list=ls(), file="CorrelationsResults.rda")
