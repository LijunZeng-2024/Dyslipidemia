rm(list = ls())
pacman::p_load(vegan, openxlsx, ggplot2, rstudioapi, dplyr, reshape2, viridis)
setwd(dirname(getActiveDocumentContext()$path))
result_dir <- 'result'
if(!dir.exists(result_dir)){
  dir.create(result_dir)
}

curve_data <- read.xlsx('source/curve.xlsx')
sp <- specaccum(curve_data, method='random')

pdf('result/curve.pdf', width = 10, height = 8)
plot(sp, ylim=c(4000, 10000), 
     ci.type='poly', col='lightblue', 
     lwd=1, ci.lty=0, ci.col='lightblue')
dev.off()

species <- read.csv('../data/species_abund_rel_sample_filtered_name.csv', header = T)
rownames(species) <- species[,1]
species <- species[,-1]

species$sum <- rowSums(species)
species <- species[order(species$sum, decreasing = TRUE), ]
species <- species[, -ncol(species)]

top14_species <- species[1:14,]
species_names <- append(rownames(top14_species), 'Others')
top14_species['Others', ] <- colSums(species) - colSums(top14_species)

top14_species$Species <- factor(rownames(top14_species), levels = rev(rownames(top14_species)))
top14_species <- melt(top14_species, id = 'Species')
colnames(top14_species) <- c('species', 'sample', 'abundance')

df_group <- read.csv('source/sample_group.csv', header = T)
top14_species <- merge(top14_species, df_group, by = 'sample')
top14_species$group <- factor(top14_species$group, levels = c('Dyslipidemia', 'Decreased HDL', 'Elevated LDL', 'Elevated TC', 'Elevated TG', 'Health'))

num_colors <- length(unique(top14_species$species))-1
hsv_colors <- hsv(h=seq(0, 1, length=num_colors), s=0.6, v=0.9, alpha = 0.9)
hsv_colors <- append('#d3d3d3', hsv_colors)
hsv_colors[5] <- '#32CD32'
hsv_colors[6] <- '#98FB98'
hsv_colors[8] <- '#20B2AA'
hsv_colors[length(hsv_colors)-1] <- '#FA8072'

ggplot(top14_species, aes(x=group, y=abundance, fill = species)) +
  geom_bar(position = 'fill', stat = 'identity', width = 0.6) +
  scale_fill_manual(values =  hsv_colors) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave('result/stack.pdf', width = 10, height = 8)
