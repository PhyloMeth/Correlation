library(ape) #utility fns
library(geiger) #utilty fns
library(corHMM)
library(phylolm)
library(phytools)

discrete.data <- rTraitDisc(tree, model = "ER", k=2, states=c(0,1))

continuous.data <- rTraitCont(tree, model="BM")


VisualizeData <- function(phy, data) {
	#Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?
  plot.phylo(tree, type = "phylogram", use.edge.length = TRUE,
             node.pos = NULL, show.tip.label = TRUE, show.node.label = FALSE,
             edge.color = "black", edge.width = 1, edge.lty = 1, font = 3,
             cex = par("cex"), adj = NULL, srt = 0, no.margin = FALSE,
             root.edge = FALSE, label.offset = 0, underscore = FALSE,
             x.lim = NULL, y.lim = NULL, direction = "rightwards",
             lab4ut = NULL, tip.color = "black", plot = TRUE,
             rotate.tree = 0, open.angle = 0, node.depth = 1,
             align.tip.label = FALSE, ...)
  plot.phylo(tree, type="fan", cex=0.2)
  # note to self, don't put the object in quotes
  
    print(paste("The tree has ", Ntip(tree), " terminals and ", 
              Nnode(tree), " internal nodes out of ",Ntip(tree)-2,
              " possible, which means it is ", 
              round(100*(Nnode(tree)-1)/(Ntip(tree)-3), 2),
              "% resolved", sep=""))
  name.check(phy, data, data.names=NULL)
}

CleanData <- function(phy, data) {
	#treedata() in Geiger is probably my favorite function in R.
  treedata(phy, data, sort=FALSE, warnings=TRUE)
}

RunContrasts <- function(phy, data, output.pdf="PIC.pdf") {
	#Include here approaches to save plots, look at your data, regress through the origin, and return the results.
  pic(x, phy, scaled = TRUE, var.contrasts = TRUE, #gives you contrasts and variances
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


#Functions I could potentially use:
phylolm(formula, data = list(), phy, model = c("BM", "OUrandomRoot",
                                               "OUfixedRoot", "lambda", "kappa", "delta", "EB", "trend"),
        lower.bound = NULL, upper.bound = NULL, 
        starting.value = NULL, ...)