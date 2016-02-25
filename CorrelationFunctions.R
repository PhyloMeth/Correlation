library(ape) #utility fns
library(geiger) #utilty fns
library(corHMM)
library(phylolm)
library(phytools)

VisualizeData <- function(phy, data) {
	plot(phy)
	plot(data)
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
	
	# This isn't working. And I can't figure out why...
	fitPagel(phy,dat[[2]][,1],dat[[2]][,2])
	
}

RunOtherMethod <- function(phy, data) {
	
}