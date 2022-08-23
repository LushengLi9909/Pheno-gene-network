library(ggplot2)
library(tidyverse)
require(visNetwork)
Pheno_gene <- read.delim("data/Pheno_gene.txt",header = TRUE,sep=",")
mapping_all <- read.delim("data/ph_mp_mapping.txt",header = TRUE,sep=",")

phenos = function(pheno){
  if(pheno !=""){
    if(str_detect(strsplit(pheno, "")[[1]][1],"[[:upper:]]")){
      phenotypes = c(pheno,mapping_all[mapping_all$HPterm==pheno,"MPterm"])
    } else{
      phenotypes = c(pheno,mapping_all[mapping_all$MPterm==pheno,"HPterm"])
    }
    return(phenotypes)
  }}
pheno = "nephrolithiasis"
visnetwork = function(pheno){
  if(pheno !=""){
    phenotypes = phenos(pheno)
  
data = data.frame()
for(i in 1:length(phenotypes)){
  data1 = Pheno_gene[Pheno_gene$Phenotype==phenotypes[i],]
  data = rbind(data,data1)
}

Ngene = length(unique(data$Gene))
Npheno = length(unique(data$Phenotype))

nodes <- data.frame( id=c(unique(data$Phenotype),sort(unique(data$Gene))),
                     label=c(unique(data$Phenotype),sort(unique(data$Gene))),
                     group=c(rep('Phenotype',Npheno),
                             rep('Gene',Ngene)),
                     value =c(as.integer(c(table(unique(data$Phenotype)))+Npheno),
                              as.integer(c(table(data$Gene)))),
                     shape=c(rep("box",Npheno),rep("ellipse",Ngene)),
                     color=c(rep("orange",Npheno),rep("lightblue",Ngene)))

edges <- data.frame(from=c(data$Phenotype),to=c(data$Gene))
visNetwork(nodes, edges)
}}