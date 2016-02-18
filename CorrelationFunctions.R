library(ape) #utility fns
library(geiger) #utilty fns
library(corHMM)
library(phylolm)
library(phytools)

VisualizeData <- function(phy, data) {
	#Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?
}

CleanData <- function(phy, data) {
	#treedata() in Geiger is probably my favorite function in R.
}

RunContrasts <- function(phy, data, output.pdf="PIC.pdf") {
	#Include here approaches to save plots, look at your data, regress through the origin, and return the results.
}

RunPagel94 <- function(phy, data) {
	#Calculate the rate estimates under each model, the likelihood of the data under each model, and the model averaged rates. 	Return the results
}

RunOtherMethod <- function(phy, data) {
	
}