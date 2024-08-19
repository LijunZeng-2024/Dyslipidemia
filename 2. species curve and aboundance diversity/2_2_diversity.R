rm(list = ls())
pacman::p_load(dplyr, ggplot2, vegan, rstudioapi, metagenomeSeq, magrittr, tidyverse, car)
setwd(dirname(getActiveDocumentContext()$path))

if(!dir.exists('result/alpha')){
  dir.create('result/alpha')
}
if(!dir.exists('result/beta')){
  dir.create('result/beta')
}

abund_rel <- read.csv('../data/species_abund_rel_sample_filtered_id.csv', header = T, row.names = 1)
data_mr_type <- newMRexperiment(abund_rel) # css normalization
p = cumNormStatFast(data_mr_type)
data_normed <- cumNorm(data_mr_type, p)
rel_normed = MRcounts(data_normed, norm = T, log = T) # log + css

df_t <- t(rel_normed)
alpha_data <- diversity(df_t, index = 'shannon') %>% as.data.frame() %>% 
  bind_cols(diversity(df_t, index = 'simpson')) %>%
  bind_cols(diversity(df_t,index = 'shannon')/log(specnumber(df_t),exp(1))) %>%
  set_colnames(c('shannon', 'simpson', 'pielou')) %>%
  rownames_to_column('sample')

df_maped <- read.csv('source/sample_cates_name.csv', header=T) %>% 
  left_join(.,alpha_data, by='sample')

for(col in colnames(df_maped)[2:6]){
  df_maped[,col] <- as.factor(df_maped[,col])
}

##### alpha diversity #####
violin_plot_fun <- function(cate){
  p <- ggplot(data = df_maped, aes(x = get(cate), y = get(index), colour =get(cate))) +
    geom_violin(scale = 'width', alpha=0.5, trim = TRUE) +
    geom_boxplot(aes(fill = get(cate)), alpha = 0.5, 
                 position = position_dodge(width = 0.2), 
                 width = 0.3, size = 0.8, outliers = T) +
    scale_fill_manual(limits=c('Control', quo_name(cate)), 
                      values =c("#e59698", "lightblue")) +
    scale_color_manual(limits=c('Control', quo_name(cate)), 
                       values=c("#e59698", "lightblue")) +
    theme_bw() +
    theme(legend.position = 'none')+
    labs(x=quo_name(cate), y=quo_name(index))
  ggsave(p, filename = paste0('result/alpha/',index, '_', cate, ".pdf"), width = 6, height = 8)
}

df <- data.frame()
alpha_index <- c('shannon', 'simpson', 'pielou')
cates <- colnames(df_maped)[2:6]
for(index in alpha_index){
  print(index)
  lapply(cates, violin_plot_fun)
  
  formulas1 <- lapply(cates, function(cate) {paste0(index,"~",cate)})
  res1 <- lapply(formulas1, function(f) leveneTest(as.formula(f), data=df_maped))
  leveneTest_func <- function(res){
    return(res$`Pr(>F)`[1])
  }
  leveneTest_res <- unlist(lapply(res1, leveneTest_func))
  
  formulas2 <- lapply(cates, function(cate) {paste0(index,"~",cate)})
  res2 <- lapply(formulas2, function(f) wilcox.test(as.formula(f), data=df_maped))
  t_test_func <- function(res){
    return(res$p.value)
  }
  tTest_res <- unlist(lapply(res2, t_test_func))
  
  col_cate <- rep(index, times = 10)
  col_type <- append(rep('leveneTest', 5), rep('tTest', 5))
  col_value <- append(leveneTest_res, tTest_res)
  df <- rbind(df, cbind(col_cate, col_type, col_value))
}
colnames(df) <- c('index', 'test_type', 'test_value')
write.csv(df, 'result/alpha/result.csv', row.names = F)

##### beta diversity #####
otu <- t(rel_normed)
otu.distance <- vegdist(otu)
pcoa <- cmdscale(otu.distance, eig=TRUE)
pc12 <- pcoa$points[,1:2]
pc <- round(pcoa$eig/sum(pcoa$eig)*100, digits=2)
pc12 <- as.data.frame(pc12)
pc12$sample <- row.names(pc12)

df_cates <- read.csv("../data/metadata.csv", header=T)[,c(1,14:18)]
df_beta <- merge(pc12, df_cates, by="sample")
cates <- colnames(df_beta)[4:8]
for(col in colnames(df_beta)[4:8]){
  df_beta[,col] <- as.factor(df_beta[,col])
}

beta_plot_func <- function(cate){
  print(cate)
  Adonis <- adonis2(as.formula(paste0('otu.distance~',cate)) ,data=df_beta, distance = "bray", permutations = 999)
  Adonis
  color=c("lightblue","#e59698")
  p1 <- ggplot(data=df_beta, aes(x=V1, y=V2, color=get(cate)))+
    geom_point(size=1.8)+
    theme(panel.grid = element_blank())+
    scale_color_manual(values = color)+
    scale_fill_manual(values = color)+
    stat_ellipse(data=df_beta, aes(fill=get(cate)),level=0.9,
                 geom = "polygon", linetype = 2, size=0.5, alpha=0.2, show.legend = T)+
    theme_bw()
  
  ggsave(paste0('result/beta/beta_',cate,'_',Adonis$`Pr(>F)`[1],'.pdf'), width = 6, height = 8)
}
lapply(cates, beta_plot_func)
