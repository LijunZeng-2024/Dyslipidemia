rm(list=ls())
pacman::p_load(metagenomeSeq, tidyverse, microeco, magrittr, openxlsx, rstudioapi, ggtree, psych)
setwd(dirname(getActiveDocumentContext()$path))

result_dir <- 'result'
if(!dir.exists(result_dir)){
  dir.create(result_dir)
}

df_id <- read.csv('../data/species_id_map.csv', header = T)
df_meta <- read.csv('../data/metadata.csv', header = T, row.names = 1)
df_tax <- read.xlsx('../data/metabolome_metadata.xlsx')
rownames(df_tax) <- df_tax$microname

df_otu <- read.csv('source/otu.csv', header = T, row.names = 1)

data_mr_type <- newMRexperiment(df_otu)
p = cumNormStatFast(data_mr_type)
df_otu <- cumNorm(data_mr_type, p)
df_otu <- as.data.frame(MRcounts(df_otu, norm = F, log = T))

lefse_func <- function(cate){
  meta_filtered <- filter(df_meta, !(Dyslipidemia==1&get(cate)==0))
  dataset <- microtable$new(sample_table = meta_filtered,
                            otu_table = df_otu,#otu_filtered,
                            tax_table = df_tax)
  #dataset
  #用于执行差异丰度分析（Differential Abundance Analysis）
  lefse <- trans_diff$new(dataset = dataset,
                          method = "lefse",
                          group = 'Dyslipidemia',
                          alpha = 0.01,
                          p_adjust_method = 'none',
                          taxa_level = 'microname',
  )
  df_res <- lefse$res_diff[,c('LDA','P.adj')]
  df_res$NAME <- rownames(df_res)
  df_res <- merge(df_res, df_id, by='NAME')
  write.csv(df_res, paste0(result_dir, '/', cate, '_sig.csv'), row.names = F)
}
cates <- c('Dyslipidemia', 'Elevated_TG', 'Elevated_TC', 'Elevated_LDL', 'Decreased_HDL')
lapply(cates, lefse_func)