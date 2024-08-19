rm(list = ls())
pacman::p_load(openxlsx, rstudioapi, dplyr)
setwd(dirname(getActiveDocumentContext()$path))

# read micro species relative abundance data
species_abun_rel <- read.csv('../data/kraken_S_abd_species.csv', header = TRUE, row.names = 1)

# filter Bacteria kindom
kraken_tree <- read.xlsx('../data/kraken_tree0224.xlsx')
names <- filter(kraken_tree, kingdom=='Bacteria')$microname
species_abun_rel <- species_abun_rel[which(rownames(species_abun_rel) %in% names),]

sample_nums <- read.csv('filtered_sample_no.csv', header = TRUE)

filtered_col_index <- which(colnames(relative_abun) %in% sample_nums[[1]])
relative_abun <- relative_abun[,filtered_col_index]

species_filtered <- data.frame()
species_filtered <- relative_abun[apply(relative_abun, 1, mean)>0.0001,]
my_func <- function(var){
  return(length(which(var!=0))/length(var))
}
species_filtered <- species_filtered[apply(species_filtered, 1, my_func)>=0.1,]
write.csv(species_filtered, 'species_filtered.csv', quote = TRUE)