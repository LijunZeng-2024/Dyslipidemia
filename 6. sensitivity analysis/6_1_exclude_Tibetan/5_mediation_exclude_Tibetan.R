rm(list=ls())
pacman::p_load(metagenomeSeq, rstudioapi, dplyr, mediation, glmnet)
setwd(dirname(getActiveDocumentContext()$path))

result_dir <- 'result_mediating'
if(!dir.exists(result_dir)){
  dir.create(result_dir)
}

df_meta <- read.csv("source/metadata.csv", header = T, row.names = 1)
df_species_abund <- read.table(paste0('result_masslin/Dyslipidemia/features/filtered_data_norm_transformed.tsv'), header = T, sep = '\t' ,stringsAsFactors = F, row.names = 1)

mediation_func <- function(cate){
  print(cate)
  # filter sample from metadata by cate
  meta_filtered <- filter(df_meta, !(Dyslipidemia==1&get(cate)==0))  
  
  # filter species from res_x by bioname
  cate_species = (read.table(paste0('result_masslin/', cate, '/significant_results.tsv'), header = T, sep = '\t' ,stringsAsFactors = F))$feature
  print(length(cate_species))
  if(length(cate_species)==0){
    return()
  }
  
  df_abund_filtered <- df_species_abund[, colnames(df_species_abund) %in% cate_species, drop=FALSE]
  print(ncol(df_abund_filtered))
  df <- as.data.frame(merge(meta_filtered, df_abund_filtered, by='row.names'))
  rownames(df) <- df[,1]
  df <- df[,-1]

  res_df <- data.frame()
  for(species in cate_species){
    print(species)

    formular_M <- paste0(species, '~score+age+Sex+Education+Marriage')
    fitM <- lm(as.formula(formular_M), data=df)
    
    formular_Y <- paste0(cate, ' ~ score+', species,'+age+Sex+Education+Marriage')
    fitY <- glm(as.formula(formular_Y), data=df, family = 'binomial')
    
    fitMed <- mediate(fitM, fitY, treat="score", mediator=species, robustSE = TRUE)

    total_effect <- c(cate, species, fitMed$tau.coef, fitMed$tau.ci[1], fitMed$tau.ci[2], fitMed$tau.p)
    total_effect <- c(cate, species, 'TOTAL', fitMed$tau.coef, fitMed$tau.ci[1], fitMed$tau.ci[2], fitMed$tau.p)
    avg_d <- c(cate, species, 'ACME', fitMed$d.avg, fitMed$d.avg.ci[1], fitMed$d.avg.ci[2], fitMed$d.avg.p)
    avg_z <- c(cate, species, 'ADE', fitMed$z.avg, fitMed$z.avg.ci[1], fitMed$z.avg.ci[2], fitMed$z.avg.p)
    avg_n <- c(cate, species, 'PROP', fitMed$n.avg, fitMed$n.avg.ci[1], fitMed$n.avg.ci[2], fitMed$n.avg.p)
 
    res_df <- rbind(res_df, total_effect, avg_d, avg_z, avg_n)
  }
  colnames(res_df) <- c('cate','species','type','estimate','ci lower','ci higher','p')
  write.csv(res_df, paste0(result_dir, '/',cate,'.csv'), row.names = F)
}

cates <- c('Dyslipidemia', 'Elevated_TG', 'Elevated_TC', 'Elevated_LDL', 'Decreased_HDL')
lapply(cates,mediation_func)

df_map <- read.csv('../../data/species_id_map.csv', header = T)
colnames(df_map) <- c('species','NAME')
res_path <- paste0(getwd(), '/', result_dir)
setwd(res_path)
for(cate in cates){
  file_path <- paste0(cate, '.csv')
  if(!file.exists(file_path)){
    next
  }
  df <- read.csv(paste0(cate, '.csv'), header = T)
  df <- merge(df, df_map, by = 'species', all.x = T)
  
  df_acme <- filter(df, type=='ACME')
  acme <- paste0(round(df_acme$estimate,3), ' (', round(df_acme$ci.lower,3), ' to ', round(df_acme$ci.higher, 3), ')')
  df_ade <- filter(df, type=='ADE')
  ade <- paste0(round(df_ade$estimate,3), ' (', round(df_ade$ci.lower,3), ' to ', round(df_ade$ci.higher,3), ')')
  df_prop <- filter(df, type=='PROP')
  prop <- paste0(round(df_prop$estimate,3), ' (', round(df_prop$ci.lower,3), ' to ', round(df_prop$ci.higher,3), ')')
  df_species_id <- unique(df$NAME)
  df_res <- cbind(df_species_id, acme, ade, prop, round(df_prop$p,3))
  colnames(df_res) <- c('NAME', 'ACME', 'ADE', 'Prop', 'P_value')
  write.csv(df_res, paste0(cate, '_result.csv'), row.names=F)  
}
