rm(list = ls())
pacman::p_load(MetaboAnalystR, openxlsx, ggplot2, rstudioapi, dplyr, ggrepel)
setwd(dirname(getActiveDocumentContext()$path))

result_dir <- 'result'
if(!dir.exists(result_dir)){
  dir.create(result_dir)
}

df_var <- read.xlsx('../data/metabolome_metadata.xlsx', rowNames = T)

# volcano labels
flags <- list(Dyslipidemia = c('Vanillylmandelic acid','L-Arogenate','13-L-Hydroperoxylinoleic acid'),
              Decreased_HDL = c('8-Isoprostane','Shikimic acid','4-Hydroxytamoxifen','Protocatechuic acid','Traumatic Acid'),
              Elevated_LDL = c('3-Hydroxybenzoic acid','Vanillylmandelic acid','Beta-Tyrosine','Quinate'),
              Elevated_TC =  c('3-Hydroxybenzoic acid','Indinavir','Beta-Tyrosine','Vanillylmandelic acid','Quinate','L-Arogenate','Citalopram'),
              Elevated_TG = c('L-Arogenate','Glycocholic acid','3,4-Dihydroxybenzeneacetic acid','L-Carnitine','Lipoxin A4','Vanillylmandelic acid','8,9-DiHETrE', 'Citalopram','Indinavir','Beta-Tyrosine'))

volcano_plot <- function(df_res, cate){
  df_res$type <- 'insig'
  df_res$type[df_res$p.value <= 0.1 & df_res$fc.all>=2] <- 'up'
  df_res$type[df_res$p.value <= 0.1 & df_res$fc.all<=0.5] <- 'down'
  write.csv(df_res, 'res.csv')
  df_res$type <- factor(df_res$type, levels = c("up","down","insig"))
  
  threshold_p = 0.1
  threshold_FC = 2
  
  ##### fc-p plot #####
  p<-ggplot(df_res, aes(x = fc.log, y = p.log))+
    geom_point(aes(fill = type, size = vip), shape = 21, colour = 'black')+
    scale_fill_manual(values = c('insig'='grey', 'up'='#f46d43', 'down'='#66c2a5'))+
    geom_vline(xintercept = c(-log2(threshold_FC), log2(threshold_FC)), lty=3, lwd=0.8, color = "black")+
    geom_hline(yintercept = -log10(threshold_p), lty=3, lwd=0.8, color = 'black')+
    labs(title = '',
         x = 'log2(FC)',
         y = '-log10(pValue)')+
    theme_bw()+
    theme(panel.grid=element_blank(),
          axis.ticks.length = unit(0.15, "cm"),
          axis.line = element_line(color = "black"),
          axis.text = element_text(size = 14,color = "black"),
          axis.text.x = element_text(size = 12,face ="bold"),
          axis.text.y = element_text(size = 10,face ="bold"),
          legend.text = element_text(color='black',size=12))
  
  color_map <- read.csv('../../source/volcano_color.csv', header = T)
  flag_name <- unlist(flags[cate])
  df_res_filtered <- filter(df_res, df_res$name %in% flag_name)
  df_res_filtered <- merge(df_res_filtered, color_map, by='name')
  df_res_filtered <- df_res_filtered %>% arrange(type)
  
  p1 <- p + geom_label_repel(data = df_res_filtered, aes(label = name, colour = factor(color_group)),
                             box.padding = 2, min.segment.length = 1, max.overlaps = 20,
                             label.size = 0.5)+
    scale_colour_manual(values = c('#f667a0','#fd8d3c','#feb24c','#c994c7'))
  p1
  ggsave(paste0('volcano_',cate,'.pdf'), p1, units = "in", width = 10, height = 8, dpi = 300)
}

opls_plot <- function(modeldf, df_opls, cate){
  r2x_p1 <- modeldf$R2X[1]
  r2x_o1 <- modeldf$R2X[2]
  p <- ggplot(df_opls, aes(p1, o1, fill = factor(group)))+
    geom_point(colour = 'black', size = 3, shape = 21)+
    stat_ellipse(geom = 'polygon', type = 'norm', level = 0.95, linetype = 'solid', linewidth = 1, show.legend = F, alpha = 0.2)+
    scale_fill_manual(values = c('#e7aea9', '#afc5da'))+
    xlab(paste0('p1 (', r2x_p1*100, '%)'))+
    ylab(paste0('o1 (', r2x_o1*100, '%)'))+
    theme_bw()
  p
  ggsave(paste0('ellipse_',cate,'.pdf'), p, units = "in", width = 12,height = 8,dpi = 300)
}

sa_func <- function(cate){
  setwd(dirname(getActiveDocumentContext()$path))
  work_path <- paste0(result_dir, '/', cate)
  if(!dir.exists(work_path)){
    dir.create(work_path)
  }
  print(work_path)
  
  setwd(work_path)
  mSet<-InitDataObjects("pktable", "stat", FALSE)
  mSet<-Read.TextData(mSet, paste0('../../source/', cate, '_meta_cates.csv'), "rowu", "disc");
  mSet<-SanityCheckData(mSet)
  mSet<-ReplaceMin(mSet);
  mSet<-SanityCheckData(mSet)
  mSet<-FilterVariable(mSet, "F", 25, "none", -1, "mean", 0)
  mSet<-PreparePrenormData(mSet)
  mSet<-Normalization(mSet, "NULL", "LogNorm", "NULL", ratio=FALSE, ratioNum=20)
  mSet<-SaveTransformedData(mSet)
  
  # opls result
  mSet<-OPLSR.Anal(mSet, reg=TRUE)
  p1 = as.data.frame(mSet$analSet$oplsda$scoreMN)
  o1 = as.data.frame(mSet$analSet$oplsda$orthoScoreMN)
  df_opls = merge(p1, o1, by='row.names')
  colnames(df_opls) <- c('sample', 'p1', 'o1')
  df_group <- read.csv(paste0('../../source/', cate, '_meta_cates.csv'))[,c(1,2)]
  df_opls <- merge(df_opls, df_group, by='sample')
  colnames(df_opls) <- c('sample', 'p1', 'o1', 'group')
  write.csv(df_opls, 'opls.csv', row.names = F)
  
  modeldf <- mSet$analSet$oplsda$modelDF
  opls_plot(modeldf, df_opls, cate)
  
  # volcano result
  mSet<-Volcano.Anal(mSet, FALSE, 2.0, 1, F, 0.1, TRUE, "raw")
  vip = as.data.frame(mSet$analSet$oplsda$vipVn)
  fc = as.data.frame(mSet$analSet$volcano$fc.all)
  fc.log = as.data.frame(mSet$analSet$volcano$fc.log)
  p = as.data.frame(mSet$analSet$volcano$p.value)
  p.log = as.data.frame(mSet$analSet$volcano$p.log)
  
  # sig met
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
  
  volcano_plot(df_volcano, cate)
}

cates <- c('Dyslipidemia', 'Decreased_HDL', 'Elevated_LDL', 'Elevated_TC', 'Elevated_TG')
lapply(cates, sa_func)