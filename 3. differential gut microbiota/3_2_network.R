rm(list = ls())
pacman::p_load(dplyr, ggplot2, vegan, rstudioapi, WGCNA, psych, reshape2, igraph)
setwd(dirname(getActiveDocumentContext()$path))

otu_table <- read.csv("otu_abs.csv",header=T,row.names=1)
sp <- read.csv("masslin_res.csv",header=T)
otu_table <- otu_table %>% filter(row.names(otu_table) %in% sp$Species)

rel_abundance <- apply(otu_table, 2, function(x) x/sum(x))
mean_rel_abundance <- rowMeans(rel_abundance) 
low_rel_abundance_otu <- rownames(otu_table)[mean_rel_abundance < 0.0001]
otu_table_filtered <- otu_table[!(rownames(otu_table) %in% low_rel_abundance_otu), ]
freq <- apply(otu_table_filtered, 1, function(x) sum(x > 0)/length(x))
keep <- freq >= 1/5
otu_table_filt <- otu_table_filtered[keep, ]
write.csv(otu_table_filt, 'otu.csv')

otu <- read.csv('otu.csv', header = T, row.names = 1)
cor <- corAndPvalue(t(otu),y=NULL,use = "pairwise.complete.obs", alternative='two.sided',method='spearman')

r <- cor$cor
p <- cor$p
p <- p.adjust(p, method = 'BH')
r[p > 0.001 | abs(r) < 0.60] = 0
write.csv(data.frame(r, check.names = FALSE), 'corr.matrix.csv')
g = graph_from_adjacency_matrix(r,mode="undirected",weighted=TRUE,diag = FALSE)
V(g)$Label <- rownames(r)

g = delete.vertices(g, names(degree(g)[degree(g) == 0]))
E(g)$corr = E(g)$weight
E(g)$weight = E(g)$weight

tax = read.csv('otu_tax.csv', row.names=1, header=T)
tax = tax[as.character(V(g)$name), ]
V(g)$Kingdom = tax$kingdom
V(g)$Phylum = tax$phyla
V(g)$Class = tax$class
V(g)$Order = tax$order
V(g)$Family = tax$family
V(g)$Genus = tax$genus
V(g)$Species = tax$species

node_list = data.frame(  
  label = names(V(g)),  
  kingdom = V(g)$Kingdom,  
  phylum = V(g)$Phylum,  
  class = V(g)$Class, 
  order = V(g)$Order, 
  family = V(g)$Family, 
  genus=V(g)$Genus, 
  species = V(g)$Species)
head(node_list)
write.csv(node_list, 'network.node_list.csv')

edge = data.frame(as_edgelist(g))
edge_list = data.frame(  
  source = edge[[1]],  
  target = edge[[2]],  
  weight = E(g)$weight,  
  correlation = E(g)$corr)
head(edge_list)
write.csv(edge_list, 'network.edge_list.csv')

write.graph(g, 'network.graphml', format = 'graphml')