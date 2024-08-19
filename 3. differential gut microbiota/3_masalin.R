rm(list=ls())
pacman::p_load(Maaslin2, rstudioapi, dplyr)
setwd(dirname(getActiveDocumentContext()$path))

result_dir <- 'result'
if(!dir.exists(result_dir)){
  dir.create(result_dir)
}

df_meta <- read.csv('../data/metadata.csv', header = T, row.names = 1, check.names = F)
for(col in colnames(df_meta)[2:17]){
  df_meta[,col] <- as.factor(df_meta[,col])
}
#str(df_meta)

df_abun <- read.csv('../data/species_abund_rel_sample_filtered_id.csv', header = T, row.names = 1)
df_abun_t <- as.data.frame(t(df_abun))

maaslin_func <- function(cate){
  meta_filtered <- filter(df_meta, !(Dyslipidemia==1&get(cate)==0))
  Maaslin2(input_data = df_abun_t, input_metadata = meta_filtered, output = paste0(result_dir,'/',cate),
           transform = "LOG", normalization = 'CSS', 
           fixed_effects = c(cate), random_effects = c('age', 'Sex'), reference = c(paste0(cate,',0')),#'Ethnicity', 'Education', 'Marrige'
           plot_heatmap = TRUE) 
}
cates <- c('Dyslipidemia', 'Elevated_TG', 'Elevated_TC', 'Elevated_LDL', 'Decreased_HDL')
lapply(cates, maaslin_func)
