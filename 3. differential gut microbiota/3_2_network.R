rm(list = ls())
pacman::p_load(dplyr, ggplot2, vegan, rstudioapi, igraph)
setwd(dirname(getActiveDocumentContext()$path))
library(psych)
library(reshape2)

otu_table <- read.csv("../otu_abs.csv", header=T, row.names=1)
sp <- read.csv("../masslin_res.csv", header=T)
otu_table <- otu_table %>% filter(row.names(otu_table) %in% sp$Species)
write.table(otu_table, file = "otu_abs.tsv", row.names=T, sep ="\t", quote = F, col.names = F)

cor_sparcc <- read.delim('cor_mat_sparcc_iter5_.out', row.names = 1, sep = '\t', check.names = FALSE)
pvals <- read.delim('pvals.two_sided_.txt', row.names = 1, sep = '\t', check.names = FALSE)

cor_sparcc[abs(cor_sparcc) < 0.3] <- 0
write.csv(as.data.frame(cor_sparcc), 'cor_sparcc.csv')

pvals[pvals>=0.05] <- -1
pvals[pvals<0.05 & pvals>=0] <- 1
pvals[pvals==-1] <- 0

adj <- as.matrix(cor_sparcc) * as.matrix(pvals)
diag(adj) <- 0
write.table(data.frame(adj, check.names = FALSE), 'neetwork.adj.txt', col.names = NA, sep = '\t', quote = FALSE)

network_adj <- read.delim('neetwork.adj.txt', row.names = 1, sep = '\t', check.names = FALSE)

g <- graph_from_adjacency_matrix(as.matrix(network_adj), mode = 'undirected', weighted = TRUE, diag = FALSE)
g <- delete_vertices(g, names(degree(g)[degree(g) == 0]))
g

E(g)$sparcc <- E(g)$weight
E(g)$weight <- abs(E(g)$weight)

tax = read.csv('../otu_tax.csv', row.names=1, header=T)
tax = tax[as.character(V(g)$name), ]
V(g)$Kingdom = tax$kingdom
V(g)$Phylum = tax$phyla
V(g)$Class = tax$class
V(g)$Order = tax$order
V(g)$Family = tax$family
V(g)$Genus = tax$genus
V(g)$Species = tax$species

write_graph(g, 'network.graphml', format = 'graphml')

edge <- data.frame(as_edgelist(g))
edge_list <- data.frame(
  source = edge[[1]],
  target = edge[[2]],
  weight = E(g)$weight,
  sparcc = E(g)$sparcc
)
write.table(edge_list, 'network.edge_list.txt', sep = '\t', row.names = FALSE, quote = FALSE)

node_list <- data.frame(
  nodes_id = V(g)$name,
  degree = degree(g)
)
write.table(node_list, 'network.node_list.txt', sep = '\t', row.names = FALSE, quote = FALSE)