rm(list=ls())
pacman::p_load(MetaboAnalystR, rstudioapi, dplyr, openxlsx, psych)
setwd(dirname(getActiveDocumentContext()$path))

result_dir <- 'result'
if(!dir.exists(result_dir)){
  dir.create(result_dir)
}

corr_func <- function(){
  df_species_id <- read.csv('../data/species_id_map.csv', header = T, row.names = 1)
  df_metobo_id <- read.xlsx('../data/metabolome_metadata.xlsx', rowNames = T)
  
  # all mediating results
  selected_species_id <- rownames(read.xlsx('source/sig_species.xlsx', rowNames = T))
  
  if(length(selected_species_id)==0){
    next
  }
  df_species <- as.data.frame(t(read.table('../3. differential gut microbiota/result/Dyslipidemia/features/filtered_data_norm_transformed.tsv', header = T, sep = '\t', row.names = 1)))
  df_species <- df_species[which(rownames(df_species) %in% selected_species_id),]
  df_species <- merge(df_species, df_species_id, all.x = T, by = 'row.names')
  rownames(df_species) <- df_species$NAME
  da <- df_species[,c(-1, -ncol(df_species))]
  #table(is.na(da))
  
  # all significant metabo
  selected_mets_id <- rownames(read.xlsx('source/sig_metabo.xlsx', rowNames = T))
  
  print(selected_mets_id)
  if(length(selected_mets_id)==0){
    next
  }
  metobo_path <- paste0('../5. differential metabolite/result/Dyslipidemia/data_normalized.csv')
  df_metobo <- read.csv(metobo_path, header = T, row.names = 1)
  df_metobo <- df_metobo[-1,]
  db <- df_metobo[which(rownames(df_metobo) %in% selected_mets_id),]
  db <- merge(db, df_metobo_id[, 'name', drop=F], all.x = T, by = 'row.names')
  rownames(db) <- db$name
  db <- db[, c(-1, -ncol(db))]
  #table(is.na(db))
  
  inter_samples <- intersect(colnames(da), colnames(db))
  da <- as.data.frame(t(da[, which(colnames(da) %in% inter_samples)]))
  db <- as.data.frame(t(db[, which(colnames(db) %in% inter_samples)]))
  
  da['sample'] <- rownames(da)
  da <- da %>%
    arrange(match(sample, rownames(db)))
  da <- da[, -ncol(da)]

  res <- corr.test(da, db, method = 'spearman',adjust = "BH")
  write.csv(res$p, paste0(result_dir, '/_res_p_all.csv'))
  write.csv(res$r, paste0(result_dir, '/_res_r_all.csv'))
}
corr_func()