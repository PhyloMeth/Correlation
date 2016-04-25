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
  pic(x, phy, scaled = TRUE, var.contrasts = TRUE #gives you contrasts and variances,
      rescaled.tree = FALSE
      #need to positivise the contrasts, not just using absolutes, scale data...but is the scaling appropriate. Look at Felsenstein 1985 to understand further.
      # run for each character
      # one loop to positivize
  )
  pic12 <- cbind(pic1[,1],pic2[,1])
  pic1posed <- pic12
  for (i in 1:length(pic1[,1])){
    if (pic1[i,1]<0) {
      pic1posed[i,] <- pic12[i,]*(-1)
    } else {
      pic1posed[i,] <- pic12[i,]
    }
  }
}

RunPagel94 <- function(phy, data) {
	#Calculate the rate estimates under each model, the likelihood of the data under each model, and the model averaged rates. 	Return the results
  corDISC(phy,data, ntraits=2, rate.mat=NULL, model=c("ER","SYM","ARD"), 
          node.states=c("joint", "marginal", "scaled"), p=NULL, root.p=NULL, ip=NULL, 
          lb=0, ub=100, diagn=FALSE) #three different models
}

RunOtherMethod <- function(phy, data) {
  corHMM(phy, data, rate.cat, rate.mat=NULL, node.states=c("joint", "marginal","scaled"), 
       optim.method=c("subplex"), p=NULL, root.p=NULL, ip=NULL, nstarts=10, n.cores=NULL, 
       sann.its=5000, diagn=FALSE)
	
}



phylolm(formula, data = list(), phy, model = c("BM", "OUrandomRoot",
                                               "OUfixedRoot", "lambda", "kappa", "delta", "EB", "trend"),
        lower.bound = NULL, upper.bound = NULL, 
        starting.value = NULL, ...)