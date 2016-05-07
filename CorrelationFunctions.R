library(ape) #utility fns
library(geiger) #utilty fns
library(corHMM)
library(phylolm)
library(phytools)
library(BAMMtools)




VisualizeData <- function(phy, data) {
	#Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?
  
  plot.phylo(phy, type="fan", cex=0.2)
  # note to self, don't put the object in quotes
  
    print(paste("The tree has ", Ntip(phy), " terminals and ", 
              Nnode(phy), " internal nodes out of ",Ntip(phy)-2,
              " possible, which means it is ", 
              round(100*(Nnode(phy)-1)/(Ntip(phy)-3), 2),
              "% resolved", sep=""))
  name.check(phy, data, data.names=NULL)
}

CleanData <- function(phy, data) {
	#treedata() in Geiger is probably my favorite function in R.
  treedata(phy, data, sort=FALSE, warnings=TRUE)
}

RunContrasts <- function(phy, data, output.pdf="PIC.pdf") {
	#Include here approaches to save plots, look at your data, regress through the origin, and return the results.
  pic1 <- pic(data[,1], phy, scaled = TRUE, var.contrasts = TRUE, #gives you contrasts and variances
      rescaled.tree = FALSE
      #need to positivise the contrasts, not just using absolutes, scale data...but is the scaling appropriate. Look at Felsenstein 1985 to understand further.
      # run for each character
      # one loop to positivize
  )
  pic2 <- pic(data[,2], phy, scaled = TRUE, var.contrasts = TRUE, #gives you contrasts and variances
                      rescaled.tree = FALSE)
 
  pictest1 <- cor.test(pic1[,1], pic1[,2])
  pictest2 <- cor.test(pic2[,1], pic2[,2])
    
  
   pic12 <- cbind(pic1[,1],pic2[,1])
  pic1posed <- pic12
  for (i in 1:length(pic1[,1])){
    if (pic1[i,1]<0) {
      pic1posed[i,] <- pic12[i,]*(-1)
    } else {
      pic1posed[i,] <- pic12[i,]
    }
  }
  test <- cor.test(pic1posed[,1], pic1posed[,2])
  return (list(pic=pic1posed, pictest1=pictest1, pictest2=pictest2, test=test))
  }

RunPagel94 <- function(phy, data) {
	#Calculate the rate estimates under each model, the likelihood of the data under each model, and the model averaged rates. 	Return the results
  datafixed <- cbind(rownames(data), data[,1], data[,2])
  PagelER <- corDISC(phy,datafixed, ntraits=2, model=c("ER")) 
  PagelSYM <- corDISC(phy,datafixed, ntraits=2, model=c("SYM")) 
  PagelARD <- corDISC(phy,datafixed, ntraits=2, model=c("ARD")) 
  # corDISC(phy,datafixed, ntraits=2, model=c("ER","SYM","ARD")) #three different models
  return (list(ER=PagelER, SYM=PagelSYM, ARD=PagelARD))
  }

RunOtherMethod <- function(phy, data) {
  datafixed <- cbind(rownames(data), data[,1], data[,2])
  Potato <- corHMM(phy, data, rate.cat, rate.mat=NULL, node.states=c("joint"), 
       optim.method=c("subplex"), p=NULL, root.p=NULL, ip=NULL, nstarts=10, n.cores=NULL, 
       sann.its=5000, diagn=FALSE)
	return (Potato)
}


#Functions I could potentially use:
# phylolm(formula, data = list(), phy, model = c("BM", "OUrandomRoot",
           #                                    "OUfixedRoot", "lambda", "kappa", "delta", "EB", "trend"),
       # lower.bound = NULL, upper.bound = NULL, 
        #starting.value = NULL, ...)