library(ape) #utility fns
library(geiger) #utilty fns
library(corHMM)
library(phylolm)
library(phytools)

VisualizeData <- function(data) {
	pdf("Visualize.pdf")
	par(mfrow=c(1,2))
	plot(data[[1]])
	hist(data[[2]][,1])
	dev.off()
}

CleanData <- function(phy, data) {
	treedata(phy,data)
}

RunContrasts <- function(phy, data, output.pdf="PIC.pdf") {
	#Include here approaches to save plots, look at your data, regress through the origin, and return the results.
	pic.1 <- pic(data[,1],phy)
	pic.2 <- pic(data[,2],phy)
	pdf("PIC.pdf")
	plot(pic.2 ~ pic.1 -1, pch=19)
	dev.off()
	correlation <- cor.test(pic.1,pic.2)
	pic.model <- lm(pic.2 ~ pic.1 -1)
	return(list(correlation,pic.model))
}

RunPagel94 <- function(phy, data) {
	#Calculate the rate estimates under each model, the likelihood of the data under each model, and the model averaged rates. 	Return the results
	# pagel.model <- fitPagel(phy,data[,2],data[,3]) # Not working well.
  pagel.model <- corDISC(phy, data, ntraits = 2, model = "ARD")
}

RunOtherMethod <- function(phy, data) {
	
	# Haven't done this bit yet...
	
}