library(ape) #utility fns
library(geiger) #utilty fns
library(corHMM)
library(phylolm)
library(phytools)
library(fBasics)



VisualizeData.continuous <- function(phy, data) {
	#Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?
	quartz(height=7,width=5)
	layout(matrix(c(1,2,1,3,4,4), 3,2, byrow=T))
	plot.phylo(phy)
	if(phy$Nnode == length(phy$tip.label)-1){
		print("Tree is fully resolved")
	}else{
		print("Tree is not fully resolved")
		}
	hist(as.numeric(data$data[,1]), main="histogram of trait 1")
	print("Shapiro-Wilk Normality Test for trait 1:"); print(normalTest(data$data[,1]))
	hist(as.numeric(data$data[,2]), main="histogram of trait 2")
	print("Shapiro-Wilk Normality Test for trait 2:"); print(normalTest(data$data[,2]))
	plot(as.numeric(data$data[,1]), as.numeric(data$data[,2]), main="scatterplot of traits 1 and 2")
}

VisualizeData.discrete <- function(phy, data) {
	#Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?
	quartz(height=7,width=5)
	layout(matrix(c(1), 1,1)); par(mar=c(0.5,0.5,3,0.5))
	plot.phylo(phy, x.lim=c(0, 2.25), y.lim=c(0,25))
	tiplabels(data $data[,1], frame="n", adj=-15)
	tiplabels(data $data[,2], frame="n", adj=-21)
	text(c(1.7, 2), length(phy$tip.label)+1, c("trait 1", "trait 2"))
}


CleanData <- function(phy, data) {
	treedata(phy, data, sort=TRUE)
}

RunContrasts <- function(phy, data, output.pdf="PIC.pdf") {
	# Include here approaches to save plots, 
	quartz(height=5,width=5,type="pdf",dpi=600,file= output.pdf)
	# look at your data, 
	layout(matrix(c(1,2,3,3), 2,2, byrow=T))
	Dat <- treedata(phy, data, sort=TRUE)$data
	Phy <- treedata(phy, data, sort=TRUE)$phy
	pic1<-pic(Dat[,1], Phy); pic1pos <- pic1*sign(pic1); hist((pic1pos), main="Positivized trait 1 PIC's")
	print("Shapiro-Wilk Normality Test for positivized trait 1 PIC's:"); print(normalTest(pic1pos))
	pic2<-pic(Dat[,2], Phy); pic2pos <- pic2*sign(pic2); hist((pic2pos), main="Positivized trait 2 PIC's")
	print("Shapiro-Wilk Normality Test for positivized trait 2 PIC's:"); print(normalTest(pic2pos))
	# regress through the origin, 
	mod <- lm((pic2pos) ~ 0 +(pic1pos)) 
	plot(pic1pos, pic2pos, xlim=c(0, max((pic1pos))), ylim=c(0, max((pic2pos))), main="Regression of positivized phylogenetic independent contrasts"); abline(mod)
	dev.off()
	# and return the results.
		print("Regression results"); print(summary(mod))
}

RunPagel94 <- function(phy, data) {
	# Calculate the rate estimates under each model, the likelihood of the data under each model, and the model averaged rates.
	
	# First get data and discretize
	Dat <- treedata(phy, data, sort=TRUE)$data

	# Then get tree
	Phy <- treedata(phy, data, sort=TRUE)$phy
	# Run models
	fitPagelMod <- fitPagel(Phy, Dat[,2], Dat[,1], method="fitDiscrete")
	
	# Return results
	if(fitPagelMod $P > 0.05){
		print(paste("Hypothesis test returns a p-value of ", round(fitPagelMod $P,3), ", meaning that the evolution of traits is not correlated.", sep=""))
	}else{
		print(paste("Hypothesis test returns a p-value of ", round(fitPagelMod $P,3), ", meaning that the evolution of traits is correlated.", sep=""))
		}
}

RunOtherMethod <- function(phy, data) {
	Dat <- treedata(eucTree, eucData, sort=TRUE)$data
	Phy <- treedata(eucTree, eucData, sort=TRUE)$phy
	modBM <- phylolm(Dat[,2] ~ Dat[,1], phy=Phy, model="BM")
	
	if(summary(modBM)$coefficients[2,4] > 0.05){
		print(paste("Hypothesis test returns a p-value of ", round(summary(modBM)$coefficients[2,4],3), ", meaning that the evolution of traits is not correlated.", sep=""))
	}else{
		print(paste("Hypothesis test returns a p-value of ", round(summary(modBM)$coefficients[2,4],3), ", meaning that the evolution of traits is correlated.", sep=""))
		}
}










