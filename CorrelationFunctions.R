library(ape) #utility fns
library(geiger) #utilty fns
library(corHMM)
library(phylolm)
library(phytools)
library(rotl)
library(phylobase)

VisualizeData <- function(phy, data) {
	#Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?
}

CleanData <- function(phy, data) {
	treedata(phy,data)
	#treedata() in Geiger is probably my favorite function in R.
}


RunContrasts <- function(phy, data, output.pdf="PIC.pdf") {
	pic1<-pic(data[,1],phy)
	pic2<-pic(data[,2],phy)
	pdf=("PIC.pdf")
	plot(pic1~pic2)
	dev.off()
	#Include here approaches to save plots, look at your data, regress through the origin, and return the results.
}

RunPagel94 <- function(phy, data) {
	data<-discrete.data
	ard.result<-corDISC(phy,data,ntraits=2,model="ARD")
	print(ard.result)
	#Calculate the rate estimates under each model, the likelihood of the data under each model, and the model averaged rates. 	Return the results
}

#This didn't work
#RunPagel94 <- function(phy, data) {
	fitPagel(phy,data[,1],data[,2])
	#Calculate the rate estimates under each model, the likelihood of the data under each model, and the model averaged rates. 	Return the results
}

RunOtherMethod <- function(phy, data) {
	
}
