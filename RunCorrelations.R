set.seed(5)
source("CorrelationFunctions.R")
#setwd("~/desktop/PhyloMeth/Correlation")
tree <- read.tree("ZanneEtAlChronogram.tre")

dat<-c("Daucus_carota" , "Conyza_canadensis" , "Setaria_viridis" , "Andropogon_gerardii" , "Pueraria_montana" , "Ambrosia_trifida" , "Datura_stramonium" , "Smallanthus_uvedalia" , "Perilla_frutescens" , "Verbesina_jacksonii" , "Vernonia_gigantea" , "Lespedeza_cuneata" , "Ambrosia_artemisiifolia" , "Cichorium_intybus" , "Albizia_julibrissin" , "Cercis_canadensis" , "Sorghum_halepense" , "Sorghastrum_nutans" , "Ligustrum_sinense" , "Gleditsia_triacanthos" , "Microstegium_vimineum" , "Cyperus_strigosus" , "Mosla_chinensis" , "Symphyotrichum_pilosum" , "Erechtites_hieraciifolius" , "Lespedeza_bicolor" , "Helenium_autumnale" , "Trifolium_pratense" , "Acer_saccharum" , "Lonicera_maackii" , "Liriodendron_tulipifera" , "Ailanthus_altissima")

invasive<-c(1,0,1,0,1,0,1,0,1,0,0,1,0,1,1,0,1,0,1,0,1,0,1,0,0,1,0,1,0,1,0,1)
woody<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,0,0,0,0,1,0,0,0,1,1,1,1)

discrete.data<-cbind.data.frame(invasive,woody)
row.names(discrete.data)<-dat

continuous.data <- rnorm(nTips(new.tree$phy),4,1)
names(continuous.data)<-dat


cleaned.continuous <- CleanData(tree, continuous.data)
cleaned.discrete <- CleanData(tree, discrete.data)
VisualizeData(cleaned.continuous$phy, cleaned.continuous$data)
VisualizeData(cleaned.discrete$phy, cleaned.discrete$data)
contrasts.answer <- RunContrasts(cleaned.discrete$data, cleaned.discrete$phy)
save(list=ls(), file="CorrelationsResults.rda")
pagel94.answer <- RunPagel94(cleaned.discrete$phy,cleaned.discrete$data[,'woody'], cleaned.discrete$data[,'invasive'])
save(list=ls(), file="CorrelationsResults.rda")

