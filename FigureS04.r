library(picante)
library(vegan)
library(tidyverse)
library(ape)

metadata = read.delim('metadata_large_scale.tsv', row.names = 1, sep = '\t')
table = read.csv('ASVTable_largescale.csv', row.names = 1)
otutab = table[,-((ncol(table)-7):ncol(table))]
tax = table[,(ncol(table)-7):ncol(table)]

meta = metadata %>% filter(site=='DY')
meta$sample = rownames(meta) 
tax = as.matrix(tax[rownames(otutab),])

library(phyloseq)
phylo = phyloseq(otu_table(otutab, taxa_are_rows = TRUE),
                 tax_table(tax),
                 sample_data(meta))

rare = rarefy_even_depth(phylo,replace = TRUE,rngseed = 711)
sample_sums(phylo)
sample_sums(rare) #查看抽平前后的变化
otu.rare = data.frame(otu_table(rare))

library(yolo)
distance_matrix <- beta(otu.rare, method = "bray")
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
pcoa6 <- paste('PCo6 (', round(100*pcoa_exp[6], 2), '%)')
pcoa7 <- paste('PCo7 (', round(100*pcoa_exp[7], 2), '%)')
pcoa8 <- paste('PCo8 (', round(100*pcoa_exp[8], 2), '%)')
PC_score <- data.frame(pcoa$point)[1:10]
PC_score$name <- rownames(PC_score)
score = cbind(PC_score, meta)

score$group = paste(score$habitat, score$type)
windowsFonts(Times=windowsFont("Times New Roman"))
(p13 = ggplot(score, aes(x=X2, y=X5, color=habitat)) +    # color == label
    geom_point(size=3,alpha=0.7) +
    xlab(c(pcoa2)) + ylab(c(pcoa5)) +
    theme_bw() +
    scale_colour_manual(
      name = NULL,
      values = c("#ffad21", "#980065", "#64B964","#648CC8"))+
    theme_classic() + 
    theme(legend.position = "none") +
    ggtitle("")) # + stat_ellipse(level = 0.75, size = 1, alpha = .4))
#geom_polygon(data = hulls, alpha = 0.3, aes(fill = factor(Treatment)),show.legend = F))

data = score %>% filter(habitat=='Rhizosphere')

(ggplot(data, aes(x=As/1000, y=X5))+
    geom_point(size=3, alpha=.5,color="#980065")+
    labs(x="As(g·kg-1)", y="PCo5")+
    geom_smooth(method=lm,level=0.95,color="#980065",alpha=.3,formula = y~poly(x, 1), size=.5)+#拟合线
    theme_classic()+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> p14)
















meta = metadata %>% filter(site=='ZZ')
meta$sample = rownames(meta) 
tax = as.matrix(tax[rownames(otutab),])

library(phyloseq)
phylo = phyloseq(otu_table(otutab, taxa_are_rows = TRUE),
                 tax_table(tax),
                 sample_data(meta))
rare = rarefy_even_depth(phylo,replace = TRUE,rngseed = 711)
sample_sums(phylo)
sample_sums(rare) #查看抽平前后的变化
otu.rare = data.frame(otu_table(rare))

distance_matrix <- beta(otu.rare, method = "bray")
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
pcoa6 <- paste('PCo6 (', round(100*pcoa_exp[6], 2), '%)')
pcoa7 <- paste('PCo7 (', round(100*pcoa_exp[7], 2), '%)')
pcoa8 <- paste('PCo8 (', round(100*pcoa_exp[8], 2), '%)')
PC_score <- data.frame(pcoa$point)[1:10]
PC_score$name <- rownames(PC_score)
score = cbind(PC_score, meta)

score$group = paste(score$habitat, score$type)
windowsFonts(Times=windowsFont("Times New Roman"))
(ggplot(score, aes(x=-X1, y=X3, color=habitat)) +    # color == label
    geom_point(size=3,alpha=0.7) +
    xlab(c(pcoa1)) + ylab(c(pcoa3)) +
    theme_bw() +
    scale_colour_manual(
      name = NULL,
      values = c("#ffad21", "#980065", "#64B964","#648CC8"))+
    theme_classic() + 
    theme(legend.position = "none") +
    ggtitle("") -> p11) # + stat_ellipse(level = 0.75, size = 1, alpha = .4))
#geom_polygon(data = hulls, alpha = 0.3, aes(fill = factor(Treatment)),show.legend = F))

data = score %>% filter(habitat=='Rhizosphere')

(ggplot(data, aes(x=As/1000, y=X3))+
    geom_point(size=3, alpha=.5,color="#980065")+
    labs(x="As(g·kg-1)", y="PCo3")+
    geom_smooth(method=lm,level=0.95,color="#980065",alpha=.3,formula = y~poly(x, 1), size=.5)+#拟合线
    theme_classic()+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> p12)















meta = metadata %>% filter(site=='WS')
meta$sample = rownames(meta) 
tax = as.matrix(tax[rownames(otutab),])


library(phyloseq)
phylo = phyloseq(otu_table(otutab, taxa_are_rows = TRUE),
                 tax_table(tax),
                 sample_data(meta))
rare = rarefy_even_depth(phylo,replace = TRUE,rngseed = 711)
sample_sums(phylo)
sample_sums(rare) #查看抽平前后的变化
otu.rare = data.frame(otu_table(rare))

distance_matrix <- beta(otu.rare, method = "bray")
distance_matrix <- as.matrix(distance_matrix)

meta = meta[rownames(distance_matrix),]

anosim <- anosim(distance_matrix, meta$habitat, permutations = 99999)
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
pcoa6 <- paste('PCo6 (', round(100*pcoa_exp[6], 2), '%)')
pcoa7 <- paste('PCo7 (', round(100*pcoa_exp[7], 2), '%)')
pcoa8 <- paste('PCo8 (', round(100*pcoa_exp[8], 2), '%)')
PC_score <- data.frame(pcoa$point)[1:10]
PC_score$name <- rownames(PC_score)
score = cbind(PC_score, meta)

score$group = paste(score$habitat, score$type)
windowsFonts(Times=windowsFont("Times New Roman"))
(p15 = ggplot(score, aes(x=X2, y=-X6, color=habitat)) +    # color == label
    geom_point(size=3,alpha=0.7) +
    xlab(c(pcoa2)) + ylab(c(pcoa6)) +
    theme_bw() +
    scale_colour_manual(
      name = NULL,
      values = c("#ffad21", "#980065", "#64B964","#648CC8"))+
    theme_classic() + 
    theme(legend.position = "none") +
    ggtitle("")) # + stat_ellipse(level = 0.75, size = 1, alpha = .4))
#geom_polygon(data = hulls, alpha = 0.3, aes(fill = factor(Treatment)),show.legend = F))

data = score %>% filter(habitat=='Rhizosphere')

(ggplot(data, aes(x=As/1000, y=-X6))+
    geom_point(size=3, alpha=.5,color="#980065")+
    labs(x="As(g·kg-1)", y="PCo6")+
    geom_smooth(method=lm,level=0.95,color="#980065",alpha=.3,formula = y~poly(x, 1), size=.5)+#拟合线
    theme_classic()+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> p16)
