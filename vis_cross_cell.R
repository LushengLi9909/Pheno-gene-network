library(ggplot2)
library(tidyverse)
require(visNetwork)

mapping_all <- read.delim("data/ph_mp_mapping.txt",header = TRUE,sep=",")
all_results_merged_mpo <- readRDS("data/mpo_results_merged.rds")
results <- readRDS("data/results_2022-07-17 23-06-35.rds")
all_results_merged_hpo = results
all_results_merged_hpo$Phenotype=gsub("-SLASH-","/",all_results_merged_hpo$Phenotype)
all_results_merged_hpo$CellType = gsub("mouse_TabulaMuris_","",all_results_merged_hpo$CellType)
all_results_merged_hpo$CellType = gsub("_"," ",all_results_merged_hpo$CellType)

phenos = function(pheno){
  if(pheno !=""){
    if(str_detect(strsplit(pheno, "")[[1]][1],"[[:upper:]]")){
      phenotypes = c(pheno,mapping_all[mapping_all$HPterm==pheno,"MPterm"])
    } else{
      phenotypes = c(pheno,mapping_all[mapping_all$MPterm==pheno,"HPterm"])
    }
    return(phenotypes)
  }}

cell_table = function(pheno){
  if(pheno !=""){
    phenotypes = phenos(pheno)
    cell_results = data.frame()
    for( p in phenotypes){
      if(str_detect(strsplit(p, "")[[1]][1],"[[:upper:]]")){
        cell_results = rbind(cell_results, all_results_merged_hpo[all_results_merged_hpo$Phenotype==p & all_results_merged_hpo$q <= 0.05,])
      } else{
        cell_results = rbind(cell_results, all_results_merged_mpo[all_results_merged_mpo$Phenotype==p & all_results_merged_mpo$q <= 0.05,])
      }}
    return(cell_results)
}}
visnetcell = function(pheno){
  if(pheno !=""){
    phenotypes = phenos(pheno)
    cell_h = c()
    cell_m = c()
    data = data.frame()
    for( p in phenotypes){
    if(str_detect(strsplit(p, "")[[1]][1],"[[:upper:]]")){
      cell = all_results_merged_hpo[all_results_merged_hpo$Phenotype==p & all_results_merged_hpo$q <= 0.05,"CellType"]
      cell_h <- append(cell_h, cell)
      for (c in cell){
        data = rbind(data,data.frame("from"=p,"to"=c))
      }
    } else{
      cell = all_results_merged_mpo[all_results_merged_mpo$Phenotype==p & all_results_merged_mpo$q <= 0.05,"CellType"]
      cell_m <- append(cell_m, cell)
      for (c in cell){
        data = rbind(data,data.frame("from"=p,"to"=c))
      }
    }
    }
    if(str_detect(strsplit(phenotypes, "")[[1]][1],"[[:upper:]]")){
      for( p in phenotypes[2:length(phenotypes)]){
        data = rbind(data,data.frame("from"=p,"to"=phenotypes[1]))
      }
    } else {
      for( p in phenotypes[2:length(phenotypes)]){
        data = rbind(data,data.frame("from"=phenotypes[1],"to"=p))
      }
    }
    
    Npheno = length(phenotypes)
    ncell = unique(data$to)
    ncell = ncell[which((ncell %in% phenotypes) == FALSE)]
    Ncell = length(ncell)
    nodes <- data.frame( id=c(phenotypes,ncell),
                         label=c(phenotypes,ncell),
                         group=c(rep('Phenotype',Npheno),
                                 rep('Celltype',Ncell)),

                         shape=c(rep("box",Npheno),rep("ellipse",Ncell)),
                         color=c(rep("lightblue",Npheno),rep("orange",Ncell)))
    
    edges <- data.frame(from=c(data$from),to=c(data$to),arrows = c("to"))
    visNetwork(nodes, edges)
  }}
