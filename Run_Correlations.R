source("~/Correlation/CorrelationFunctions.R")
tree <- read.tree("~/R/PTGR.ploid.tree.8January2015")
#discrete.data.x <- read.csv(file="~/R/temp.PTGR.ploid.data.txt", stringsAsFactors=FALSE, row.names=1) #death to factors.
#discrete.data.y <- 

continuous.data.x <- read.csv(file="~/Documents/Data/leitch.data.nodupes.txt", stringsAsFactors=FALSE) #death to factors.
continuous.data.y <- read.csv(file="~/Documents/Data/PTGR.data.1March2016.txt", stringsAsFactors=FALSE)

binary.data<-read.csv(file="~/Documents/Data/sample.binary.data", stringsAsFactors=FALSE)
merged.data.withrows<-as.data.frame(binary.data, row.names=binary.data[,1])
	binary.data.cleaned<-merged.data.withrows[,-1]

#My data sets are in individual files, so I wrote this function to merge together each variable and remove taxa with missing data
#discrete.data <-MergeData(discrete.data.x,discrete.data.y)
continuous.data <-MergeData(continuous.data.x,continuous.data.y)

cleaned.continuous <- CleanData(tree, continuous.data, sort=TRUE)
rand.tree <- pbtree(n=47,rooted=TRUE,tip.label=binary.data$ID)
cleaned.discrete <- CleanData(rand.tree, binary.data.cleaned, sort=TRUE)
plot.phylo(cleaned.discrete$phy)
VisualizeData(tree, cleaned.continuous)
VisualizeData(tree, cleaned.discrete)

contrasts.x <- RunContrasts(phy=cleaned.continuous$phy, data=cleaned.continuous$data[,1])
contrasts.y <- RunContrasts(phy=cleaned.continuous$phy, data=cleaned.continuous$data[,2])

save(list=ls(), file="CorrelationsResults.rda")
?fitPagel
pollen.nuclei<-as.vector(cleaned.discrete$data[,1])

pagel94.answer <- RunPagel94(phy=cleaned.discrete$phy, x.data=cleaned.discrete$data[,1], y.data=cleaned.discrete$data[,2])
save(list=ls(), file="CorrelationsResults.rda")

phyloGLM.results<-phyloGLM(formula=contrasts.y~contrasts.x, )

phyloglm(contrasts.y~contrasts.x, phy=cleaned.continuous$phy)
?phyloglm

#Make sure to add the data files to the cloned git repository and add them so they can be pushed back to github. It probably would have made more sense to do this first and then read the files in from the package folder...
system(paste("cd ~/Correlation"))

system(paste("cp ~/R/PTGR.ploid.tree.8January2015 ~/Correlation/PTGR.ploid.tree.8January2016"))
system(paste("git add PTGR.ploid.tree.8January2016"))

system(paste("cp ~/R/temp.PTGR.ploid.data.txt ~/Correlation/temp.PTGR.ploid.data.txt"))
system(paste("git add temp.PTGR.ploid.data.txt"))

system(paste("cp ~/Documents/Data/leitch.data.nodupes.txt ~/Correlation/leitch.data.nodupes.txt"))
system(paste("git add leitch.data.nodupes.txt"))

system(paste("cp ~/Documents/Data/PTGR.data.1March2016.txt ~/Correlation/PTGR.data.1March2016.txt"))
system(paste("git add PTGR.data.1March2016.txt"))

system(paste("cp ~/Documents/Data/sample.binary.data ~/Correlation/sample.binary.data"))
system(paste("git add sample.binary.data"))

