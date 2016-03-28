library(ape) #utility fns
library(geiger) #utilty fns
library(corHMM)
library(phylolm)
library(phytools)
library(picante)

VisualizeData <- function(phy, data) {
	#Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?
is.rooted(phy)
is.binary.tree(phy)
is.ultrametric(phy)

print(data)

	if (length(unique(c(data[,1],data[,2]))) > 2) {
	quartz()
	plot(data[,1],data[,2])
	traitframe <- as.data.frame(data)
	traitframe[,3] <- rownames(traitframe)
	colnames(traitframe)[3] <- "names"
	quartz()
	color.plot.phylo(phy, traitframe, trait="continuous.data1", taxa.names="names")
	quartz()
	color.plot.phylo(phy, traitframe, trait="continuous.data2", taxa.names="names")
	} else {
	print(table(data[,1], data[,2]))
	quartz()
	barplot(c(table(data[,1]), table(data[,2])))
	quartz()
	plot(phy, label.offset = 3, cex=0.2)
	cols1 <- rep("yellow", length(data[,1]))
	names(cols1) <- row.names(data)
	cols1[data[,1]==1] <- "blue"
	cols2 <- rep("green", length(data[,2]))
	names(cols2) <- row.names(data)
	cols2[data[,2]==1] <- "red"
	tiplabels(pch = 23, col= cols1[phy$tip.label],  bg=cols1[phy$tip.label], adj = 1.4)
	tiplabels(pch = 22, col = cols2[phy$tip.label], bg=cols2[phy$tip.label], adj = 2.5)
	}
}

CleanData <- function(phy, data) {
	#treedata() in Geiger is probably my favorite function in R.
	dataclean <- treedata(phy,data)
	return(dataclean)
}

RunContrasts <- function(phy, data, output.pdf="PIC.pdf") {
	#Include here approaches to save plots, look at your data, regress through the origin, and return the results.
pic1 <- pic(data[,1], phy, scaled=TRUE, var.contrasts=TRUE)
pic2 <- pic(data[,2], phy, scaled=TRUE, var.contrasts=TRUE)
quartz()
plot(pic1[,1],sqrt(pic1[,2]))
quartz()
plot(pic2[,1],sqrt(pic2[,2]))
stand.test1 <- cor.test(pic1[,1],sqrt(pic1[,2]))
stand.test2 <- cor.test(pic2[,1],sqrt(pic2[,2]))
pic12 <- cbind(pic1[,1],pic2[,1])
pic1posed <- pic12
for (i in 1:length(pic1[,1])){
	if (pic1[i,1]<0) {
		pic1posed[i,] <- pic12[i,]*(-1)
	} else {
		pic1posed[i,] <- pic12[i,]
	}
}
pic21 <- cbind(pic2[,1],pic1[,1])
pic2posed <- pic21
for (i in 1:length(pic2[,1])){
	if (pic2[i,1]<0) {
		pic2posed[i,] <- pic21[i,]*(-1)
	} else {
		pic2posed[i,] <- pic21[i,]
	}
}

test12 <- cor.test(pic1posed[,1], pic1posed[,2])
test21 <- cor.test(pic2posed[,1], pic2posed[,2])

regr1 <- lm(pic1posed[,1]~pic1posed[,2] - 1)
regr2 <- lm(pic2posed[,1]~pic2posed[,2] -1)
sum1 <- summary(regr1)
sum2 <- summary(regr2)

quartz()
plot(pic1posed[,1]~pic1posed[,2])
abline(lm(pic1posed[,1]~pic1posed[,2] - 1))
quartz()
plot(pic2posed[,1]~pic2posed[,2])
abline(lm(pic2posed[,1]~pic2posed[,2] - 1))

pdf(output.pdf)
layout(matrix(c(1, 2, 3, 4), 2, 2, byrow=TRUE))
plot(pic1[,1],sqrt(pic1[,2]))
plot(pic2[,1],sqrt(pic2[,2]))
plot(pic1posed[,1]~pic1posed[,2])
abline(lm(pic1posed[,1]~pic1posed[,2] - 1))
plot(pic2posed[,1]~pic2posed[,2])
abline(lm(pic2posed[,1]~pic2posed[,2] - 1))
dev.off()  # close PDF device

return(list(pic1=pic1, pic2=pic2, stand.test1=stand.test1, stand.test2=stand.test2, pic1posed=pic1posed, pic2posed=pic2posed, test12=test12, test21=test21, regr1=regr1, sum1=sum1, regr2=regr2, sum2=sum2))
}

RunPagel94 <- function(phy, data) {
	#Calculate the rate estimates under each model, the likelihood of the data under each model, and the model averaged rates. 	Return the results
datafixed <- cbind(rownames(data), data[,1], data[,2])
pagelsolutionER <- corDISC(phy, datafixed, ntraits=2, model="ER")
pagelsolutionSYM <- corDISC(phy, datafixed, ntraits=2, model="SYM")
pagelsolutionARD <- corDISC(phy, datafixed, ntraits=2, model="ARD")
pagelsolution <- list(ER=pagelsolutionER, SYM=pagelsolutionSYM, ARD=pagelsolutionARD)
return(pagelsolution)
}

RunOtherMethod <- function(phy, data1, data2) {
dataUno <- data.frame(trait01=data1[,1], predictor1=data2[,1], predictor2=data2[,2])
dataDue <- data.frame(trait01=data1[,2], predictor1=data2[,1], predictor2=data2[,2])
glmresult1 <- phyloglm(formula=trait01~predictor1+predictor2, data=dataUno, phy=phy, btol=60)
glmresult2 <- phyloglm(formula=trait01~predictor1+predictor2, data=dataDue, phy=phy, btol=50)
glmresult <- list(GLM1=glmresult1, Summary1=summary(glmresult1), GlM2=glmresult2, Summary2=summary(glmresult2))
return(glmresult)
}
