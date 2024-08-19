rm(list=ls())
library(dplyr)
library(Maaslin2)
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

result_dir <- 'result'
if(!dir.exists(result_dir)){
  dir.create(result_dir)
}

df_meta <- read.csv('data/metadata.csv', header = T, row.names = 1)
for(col in colnames(df_meta)[1:ncol(df_meta)]){
  df_meta[,col] <- as.factor(df_meta[,col])
}

df_species <- read.csv('../data/species_abund_rel_sample_filtered_id_kingdom.csv', header = T, row.names = 1)
df_species <- as.data.frame(t(df_species))

#maaslin
maaslin_func <- function(cate){
  meta_filtered <- filter(df_meta, !(Dyslipidemia==1&get(cate)==0))
  Maaslin2(input_data = df_species, input_metadata = meta_filtered, output = paste0(result_dir, '/', cate),
           normalization = 'CSS',
           fixed_effects = c(cate), random_effects = c('age'),
           reference = c(paste0(cate,',0')))
}
cates <- c('Dyslipidemia', 'Decreased_HDL', 'Elevated_LDL', 'Elevated_TC', 'Elevated_TG')
lapply(cates, maaslin_func)
