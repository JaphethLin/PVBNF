
library(picante)
library(vegan)
library(tidyverse)
library(ape)

metadata = read.csv('metadata.csv', row.names = 'sample')
otutab = read.delim('ASVtable.txt', row.names = 1, sep = '\t')
meta = metadata %>% filter(habitat=='Rhizosphere' | habitat == 'Bulk')
meta$sample = rownames(meta)

tree <- prune.sample(t(otutab), tree)
tree

library(phyloseq)
phylo = phyloseq(otu_table(otutab, taxa_are_rows = TRUE),
                 sample_data(meta),
                 phy_tree(tree))
otutab = data.frame(otu_table(phylo))

rare.data = rarefy_even_depth(phylo,replace = TRUE,rngseed = 711)
sample_sums(phylo)
sample_sums(rare.data) #查看抽平前后的变化
meta = data.frame(sample_data(rare.data))
otutab = data.frame(otu_table(rare.data))
#write.table(cbind(rownames(otutab), otutab), 'otutab_rare.txt', quote = FALSE, sep = '\t', row.names = F, col.names = T)


distance_matrix <- beta(otutab, method = "bray")
distance_matrix <- as.matrix(distance_matrix)

meta = meta[rownames(distance_matrix),]

anosim <- anosim(distance_matrix, meta$habitat, permutations = 9999)
anosim

pcoa <- cmdscale(distance_matrix, 
                 k = (nrow(distance_matrix) - 1), 
                 eig = TRUE)          # 各 PCoA 轴的特征值
pcoa_eig <- pcoa$eig                  # 各 PCoA 轴的解释量
pcoa_exp <- pcoa$eig/sum(pcoa$eig)    # 样方排序坐标

# 前 3 轴解释量
pcoa1 <- paste('PCo1 (', round(100*pcoa_exp[1], 2), '%)')
pcoa2 <- paste('PCo2 (', round(100*pcoa_exp[2], 2), '%)')
pcoa3 <- paste('PCo3 (', round(100*pcoa_exp[3], 2), '%)')
pcoa4 <- paste('PCo4 (', round(100*pcoa_exp[4], 2), '%)')
pcoa5 <- paste('PCo5 (', round(100*pcoa_exp[5], 2), '%)')
PC_score <- data.frame(pcoa$point)[1:10]
PC_score$name <- rownames(PC_score)
score = cbind(PC_score, meta)

score$group = paste(score$habitat, score$type)
windowsFonts(Times=windowsFont("Times New Roman"))
(p1 = ggplot(score, aes(x=X2, y=X3, color=habitat)) +    # color == label
    geom_point(size=3,alpha=0.7) +
    xlab(c(pcoa2)) + ylab(c(pcoa3)) +
    theme_bw() +
    scale_colour_manual(
      name = NULL,
      values = c("#ffad21", "#980065", "#64B964","#648CC8"))+
    theme_classic() + 
    theme(legend.position = "top") +
    ggtitle("")) # + stat_ellipse(level = 0.75, size = 1, alpha = .4))
#geom_polygon(data = hulls, alpha = 0.3, aes(fill = factor(Treatment)),show.legend = F))

score$group = paste(score$habitat, score$type)
windowsFonts(Times=windowsFont("Times New Roman"))
(p2 = ggplot(score, aes(x=X2, y=X1, color=habitat)) +    # color == label
    geom_point(size=3,alpha=0.7) +
    xlab(c(pcoa2)) + ylab(c(pcoa1)) +
    theme_bw() +
    scale_colour_manual(
      name = NULL,
      values = c("#ffad21", "#980065", "#64B964","#648CC8"))+
    theme_classic() + 
    ggtitle("")) # + stat_ellipse(level = 0.75, size = 1, alpha = .4))
