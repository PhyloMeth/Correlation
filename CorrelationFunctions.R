library(ape) #utility fns
library(geiger) #utilty fns
library(corHMM)
library(phylolm)
library(phytools)


CleanData <- function(phy, data) {
  treedata(phy,data,sort=TRUE)
 
	#treedata() in Geiger is probably my favorite function in R.
}

VisualizeData <- function(phy, data) {
  plotTree(phy)
  print(data)
	#Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?
}

RunContrasts <- function(phy, data, output.pdf="PIC.pdf") {
  pic_trait1<-pic(data[,1], phy)
  pic_trait2<-pic(data[,2], phy)
  pdf=("PIC.pdf")
  plot(pic_trait1~pic_trait2)
  cor<-cor.test(pic_trait1, pic_trait2)
  print(cor$p.value)
  #Include here approaches to save plots, look at your data, regress through the origin, and return the results.
}

RunPagel94 <- function(phy, data) {
  x<-data[,1]
  y<-data[,2]
  tree<-phy
  run<-fitPagel(tree, x, y)
  print(run$P)
	#Calculate the rate estimates under each model, the likelihood of the data under each model, and the model averaged rates. 	Return the results
}



RunOtherMethod <- function(phy, data) {
	
}

