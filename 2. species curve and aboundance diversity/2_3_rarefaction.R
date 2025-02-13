rm(list = ls())
pacman::p_load(dplyr, ggplot2, vegan, rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

abund_rel <- as.data.frame(t(read.csv('data/species_abund_rel_sample_filtered_name.csv', row.names=1, header = T)))
abs_rel <- as.data.frame(t(read.csv('data/kraken_S.csv', row.names=1, header = T)))
meta <- read.csv('data/metadata.csv', row.names=1, header = T)

rows_to_keep <- rownames(abs_rel) %in% rownames(abund_rel)
cols_to_keep <- colnames(abs_rel) %in% colnames(abund_rel)
abund_abs_filtered <- abs_rel[rows_to_keep, cols_to_keep]

otu <- rarecurve(abund_abs_filtered, step = 1000, col = rainbow(12), lwd = 1.5, xlab = "Sample Size", ylab = "Species Richness", label=FALSE)
otu