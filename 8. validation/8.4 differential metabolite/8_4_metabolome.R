rm(list=ls())
pacman::p_load(metagenomeSeq, tidyverse, microeco, magrittr, rstudioapi, openxlsx, ggtree, psych)
setwd(dirname(getActiveDocumentContext()$path))

df_sample <- read.csv('../data/metadata.csv', header = T, row.names = 1)
df_otu <- read.csv('source/otu_css_log.csv', header = T, row.names = 1)
df_tax <- read.xlsx('../data/metabolome_metadata.xlsx')
rownames(df_tax) <- df_tax$microname

data_mr_type <- newMRexperiment(df_otu)
p = cumNormStatFast(data_mr_type)
df_otu <- cumNorm(data_mr_type, p)
df_otu <- MRcounts(df_otu, norm = F, log = T)
write.csv(df_otu, 'source/otu_css_log.csv')

same_sample <- intersect(rownames(df_sample), colnames(df_otu))
df_sample <- df_sample[rownames(df_sample) %in% same_sample,]
df_otu <- df_otu[,which(colnames(df_otu) %in% same_sample)]

dataset <- microtable$new(sample_table = df_sample,
                          otu_table = df_otu,
                          tax_table = df_tax)
dataset
# Differential Abundance Analysis
lefse <- trans_diff$new(dataset = dataset,
                        method = "lefse",
                        group = 'Dyslipidemia',
                        alpha = 0.01,
                        lefse_subgroup = 'age',
                        p_adjust_method = 'none',
                        taxa_level = 'microname'
)

df_species <- read.xlsx('../data/species_log_css.xlsx', rowNames = T, sep.names = ' ')
df_metabo <- read.xlsx('../data/metabo_log_css.xlsx', rowNames = T, sep.names = ' ')

same_sample <- intersect(rownames(df_species), rownames(df_metabo))
df_species <- df_species[which(rownames(df_species) %in% same_sample),]
df_metabo <- df_metabo[which(rownames(df_metabo) %in% same_sample),]

# css
data_mr_type <- newMRexperiment(df_species)
p = cumNormStatFast(data_mr_type)
df_species <- cumNorm(data_mr_type, p)
df_species <- MRcounts(df_species, norm = T, log = T)

data_mr_type <- newMRexperiment(df_metabo)
p = cumNormStatFast(data_mr_type)
df_metabo <- cumNorm(data_mr_type, p)
df_metabo <- MRcounts(df_metabo, norm = T, log = T)

res <- corr.test(df_species, df_metabo, method = 'spearman',adjust = "BH")
write.csv(res$p, 'res_p_all_log.csv')
write.csv(res$r, 'res_r_all_log.csv')