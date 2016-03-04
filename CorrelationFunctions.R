library(ape) #utility fns
library(geiger) #utilty fns
library(corHMM)
library(phylolm)
library(phytools)

VisualizeData <- function(phy, data) {  
	print(phy)
	plot(phy)
	print(data)
	names(data)
	str(data)
}

#Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?	

CleanData <- function(phy, data) { treedata(phy,data,sort=TRUE)
}


RunContrasts <- function(data, phy, output.pdf="PIC.pdf") {	
	pic1<-pic(data[,1],phy)
	pic2<-pic(data[,2],phy)
	pdf=("PIC.pdf")
	plot(pic1~pic2)
	cor.test(pic1,pic2)
}

#Include here approaches to save plots, look at your data, regress through the origin, and return the results.

RunPagel94 <- function(phy, data1,data2) { fitPagel(phy,data1,data2)
	}
	
#Calculate the rate estimates under each model, the likelihood of the data under each model, and the model averaged rates. 	Return the results

RunOtherMethod <- function(phy, data) {
	
}