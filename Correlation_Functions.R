library(ape) #utility fns
library(geiger) #utilty fns
library(corHMM)
library(phylolm)
library(phytools)

MergeData <- function(data1,data2) {
	merged.data<-merge(data1,data2,all=FALSE)
	merged.data.withrows<-as.data.frame(merged.data, row.names=merged.data[,1])
	merged.data.cleaned<-merged.data.withrows[,-1]
	print(merged.data.cleaned)
}

VisualizeData <- function(phy, data) {
	plot.phylo(phy)
	print(data)
	#Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?
}


CleanData <- function(phy, data, sort) {
	phy.data<-treedata(phy,data,sort=sort)
	print(phy.data)
	#treedata() in Geiger is probably my favorite function in R.
}

RunContrasts <- function(phy, data, output.pdf="PIC.pdf") {
	#Include here approaches to save plots, look at your data, regress through the origin, and return the results.
	#phy.data<-treedata(phy,data,sort=TRUE)
	output.pic<-pic(x=data,phy=phy)
	print(output.pic)
	
}

RunPagel94 <- function(phy, x.data, y.data) {
	#Calculate the rate estimates under each model, the likelihood of the data under each model, and the model averaged rates. 	Return the results
	x <- as.factor(x.data)
	names(x) <- names(x.data)
	y <- as.factor(y.data)
	names(y) <- names(y.data)
	fitPagel(tree=phy,x=x,y=y)
	
}

phyloGLM <- function(formula, data, tree, model) {
	phyloglm(formula=formula,data=data,phy=tree, model)
	
}
