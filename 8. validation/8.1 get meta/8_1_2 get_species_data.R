rm(list = ls())
pacman::p_load(dplyr, openxlsx, rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

species_abun_abs <- read.xlsx('../data/species_abund_abs_original.xlsx', rowNames = T)
rel_func <- function(data){
  return(data/sum(data))
}
species_abun_rel <- as.data.frame(t(apply(species_abun_abs, 1, rel_func)))

# filter Bacteria kindom
kraken_tree <- read.xlsx('../data/metabolome_metadata.xlsx')
names <- filter(kraken_tree, kingdom=='Bacteria')$microname
species_filtered <- species_abun_rel[which(rownames(species_abun_rel) %in% names),]

df_sample <- read.csv('../data/metadata_complete.csv', header = T, row.names = 1)
species_filtered <- species_filtered[, which(colnames(species_filtered) %in% rownames(df_sample))]
write.csv(species_filtered, '../data/species_abund_rel_sample_filtered_name_kingdom.csv', quote = TRUE)

df_map <- read.csv('../data/species_id_map.csv', header = T)
rownames(df_map) <- df_map$NAME
df_species <- merge(species_filtered, df_map, all.x = T, by = 'row.names')
rownames(df_species) <- df_species$ID
df_species <- df_species[, c(-1,-grep('ID|NAME', colnames(df_species)))]
write.csv(df_species, '../data/species_abund_rel_sample_filtered_id_kingdom.csv', quote = TRUE)