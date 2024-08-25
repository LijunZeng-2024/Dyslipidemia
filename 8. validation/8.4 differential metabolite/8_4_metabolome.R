rm(list = ls())
pacman::p_load(MetaboAnalystR, ggplot2, ggrepel, openxlsx, dplyr, rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

result_dir <- 'result'
if(!dir.exists(result_dir)){
  dir.create(result_dir)
}

df_var <- read.csv('../data/metabo_metadata.csv', header=T, row.names = 1)

df_metobo <- read.xlsx('../data/metobo.xlsx', rowNames = T)
df_meta <- read.csv('../data/metadata.csv', header = T, row.names = 1)
df_metobo <- df_metobo[, which(colnames(df_metobo) %in% rownames(df_meta))]

combine_meta_cate <- function(work_path, cate){
  df <- filter(df_meta, !(Dyslipidemia==1&get(cate)==0))
  df <- merge(df[, cate, drop=F], t(df_metobo), by='row.names')
  colnames(df)[1] <- 'sample'
  write.csv(df, paste0(work_path, '/', cate, '_meta_cates.csv'), row.names = F)
}

# Statistical Analysis
sa_func <- function(cate){
  setwd(dirname(getActiveDocumentContext()$path))
  work_path <- paste0(result_dir, '/', cate)
  if(!dir.exists(work_path)){
    dir.create(work_path)
  }
  print(work_path)
  
  combine_meta_cate(work_path, cate)
  
  setwd(work_path)
  mSet<-InitDataObjects("pktable", "stat", FALSE)
  mSet<-Read.TextData(mSet, paste0(cate, '_meta_cates.csv'), "rowu", "disc");
  mSet<-SanityCheckData(mSet)
  mSet<-ReplaceMin(mSet);
  mSet<-SanityCheckData(mSet)
  mSet<-FilterVariable(mSet, "F", 25, "none", -1, "mean", 0)
  mSet<-PreparePrenormData(mSet)
  mSet<-Normalization(mSet, "NULL", "LogNorm", "NULL", ratio=FALSE, ratioNum=20) # log转换
  
  mSet<-OPLSR.Anal(mSet, reg=TRUE)
  p1 = as.data.frame(mSet$analSet$oplsda$scoreMN)
  o1 = as.data.frame(mSet$analSet$oplsda$orthoScoreMN)
  df_opls = merge(p1, o1, by='row.names')
  colnames(df_opls) <- c('sample', 'p1', 'o1')
  df_group <- read.csv(paste0(cate, '_meta_cates.csv'))[,c(1,2)]
  df_opls <- merge(df_opls, df_group, by='sample')
  colnames(df_opls) <- c('sample', 'p1', 'o1', 'group')
  write.csv(df_opls, 'opls.csv', row.names = F)
  
  mSet<-Volcano.Anal(mSet, FALSE, 2.0, 1, F, 0.1, TRUE, "raw")
  vip = as.data.frame(mSet$analSet$oplsda$vipVn)
  fc = as.data.frame(mSet$analSet$volcano$fc.all)
  fc.log = as.data.frame(mSet$analSet$volcano$fc.log)
  p = as.data.frame(mSet$analSet$volcano$p.value)
  p.log = as.data.frame(mSet$analSet$volcano$p.log)
  
  sig = as.data.frame(mSet$analSet$volcano$sig.mat)
  sig <- merge(sig, df_var[, 'name', drop=F], by='row.names', all.x = T)
  write.csv(sig, 'sig.csv')
  
  df_volcano <- as.data.frame(mSet$analSet$volcano[-14])
  df_volcano = merge(df_volcano, vip, by='row.names')
  rownames(df_volcano) = df_volcano[,1]
  df_volcano = df_volcano[,-1]
  colnames(df_volcano)[ncol(df_volcano)] <- 'vip'
  df_volcano <- merge(df_volcano, df_var[,'name',drop=F],by='row.names')
  rownames(df_volcano) <- df_volcano$Row.names
  df_volcano <- df_volcano[,-1]
  write.csv(df_volcano, 'volcano.csv')
  
  df_res <- df_volcano[,c('name', 'fc.all', 'fc.log', 'p.value', 'p.log')]
  df_res <- filter(df_res, (fc.all<=0.5 | fc.all>=2)&p.value<=0.05)
  df_res <- merge(df_res, df_var[,c('name','KEGG','KEGG_Pathway')], all.x = T, by = 'name')
  df_res[is.na(df_res)] <-  ''
  colnames(df_res) <- c('Name','FC','log2(FC)','P value','-log10(p)', 'KEGG', 'Pathway')
  write.csv(df_res, paste0(cate, '_result.csv'), row.names = F)
}

cates <- c('Dyslipidemia', 'Decreased_HDL', 'Elevated_LDL', 'Elevated_TC', 'Elevated_TG')
lapply(cates, sa_func)