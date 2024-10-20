
library(tidyverse)
library(ggplot2)

setwd("C:/Users/11830/Desktop/R.space/pv.N_work/Figure1")
metadata = read.csv('metadata.csv', row.names = 'raw.sample')



# change the feature
env = metadata %>% select('TN', 'type', 'habitat', 'align')

colnames(env) = c('value', 'group1', 'group2', 'align')
env$group3 = paste(env$group1, env$group2)
env$group3 = factor(env$group3, levels = c("Slight Bulk", "Slight Rhizosphere", "Severe Bulk", "Severe Rhizosphere"),
                    labels = c('BL', 'RL', 'BH', 'RH'))

cbPalette <- c("#FFDF7C", "#D0607D", "#ffad21", "#980065")
library(ggplot2)
(p1 <- ggplot(env, aes(x=group3, y=value, fill=group3)) + 
    geom_boxplot(alpha=0.5, width=0.2, color = "gray", position=position_dodge(width=0.8), size=0.1, outlier.colour = NA)+
    geom_violin(alpha=0.3, width=1, color = NA, position=position_dodge(width=0.8), size=0.1)+
    geom_point(aes(color = group3), alpha=0.4, size=.7, position=position_jitterdodge(jitter.width = 0.35, jitter.height = 0, dodge.width = 0.8))+
    scale_fill_manual(values = cbPalette)+
    scale_color_manual(values = cbPalette)+
    geom_line(aes(group = align), color = 'gray', lwd = 0.25, alpha = .2) +  #绘制样本连线，通过 aes(group) 参数指定配对样本信息
    theme_classic()+
    ylim(0.0,10.0) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(legend.position = "top", legend.title = element_blank()) +
    labs(x="", y='TN (g·kg-1)') +
    theme(axis.text=element_text(colour='black',size=9)))



# change the feature
env = metadata %>% select('DON', 'type', 'habitat', 'align')

colnames(env) = c('value', 'group1', 'group2', 'align')
env$group3 = paste(env$group1, env$group2)
env$group3 = factor(env$group3, levels = c("Slight Bulk", "Slight Rhizosphere", "Severe Bulk", "Severe Rhizosphere"),
                    labels = c('BL', 'RL', 'BH', 'RH'))

cbPalette <- c("#FFDF7C", "#D0607D", "#ffad21", "#980065")
library(ggplot2)
(p2 <- ggplot(env, aes(x=group3, y=value, fill=group3)) + 
    geom_boxplot(alpha=0.5, width=0.2, color = "gray", position=position_dodge(width=0.8), size=0.1, outlier.colour = NA)+
    geom_violin(alpha=0.3, width=1, color = NA, position=position_dodge(width=0.8), size=0.1)+
    geom_point(aes(color = group3), alpha=0.4, size=.7, position=position_jitterdodge(jitter.width = 0.35, jitter.height = 0, dodge.width = 0.8))+
    scale_fill_manual(values = cbPalette)+
    scale_color_manual(values = cbPalette)+
    geom_line(aes(group = align), color = 'gray', lwd = 0.25, alpha = .2) +  #绘制样本连线，通过 aes(group) 参数指定配对样本信息
    theme_classic()+
    ylim(0.0, 85.0) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(legend.position = "none", legend.title = element_blank()) +
    labs(x="", y='DTN (mg·kg-1)') +
    theme(axis.text=element_text(colour='black',size=9)))





# change the feature
env = metadata %>% select('NH4', 'type', 'habitat', 'align')

colnames(env) = c('value', 'group1', 'group2', 'align')
env$group3 = paste(env$group1, env$group2)
env$group3 = factor(env$group3, levels = c("Slight Bulk", "Slight Rhizosphere", "Severe Bulk", "Severe Rhizosphere"),
                    labels = c('BL', 'RL', 'BH', 'RH'))

cbPalette <- c("#FFDF7C", "#D0607D", "#ffad21", "#980065")
library(ggplot2)
(p3 <- ggplot(env, aes(x=group3, y=value, fill=group3)) + 
    geom_boxplot(alpha=0.5, width=0.2, color = "gray", position=position_dodge(width=0.8), size=0.1, outlier.colour = NA)+
    geom_violin(alpha=0.3, width=1, color = NA, position=position_dodge(width=0.8), size=0.1)+
    geom_point(aes(color = group3), alpha=0.4, size=.7, position=position_jitterdodge(jitter.width = 0.35, jitter.height = 0, dodge.width = 0.8))+
    scale_fill_manual(values = cbPalette)+
    scale_color_manual(values = cbPalette)+
    geom_line(aes(group = align), color = 'gray', lwd = 0.25, alpha = .2) +  #绘制样本连线，通过 aes(group) 参数指定配对样本信息
    theme_classic()+
    ylim(0.0, 80.0) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(legend.position = "none", legend.title = element_blank()) +
    labs(x="", y='NH4 (mg·kg-1)') +
    guides(fill="none") +
    theme(axis.text=element_text(colour='black',size=9)))





# change the feature
env = metadata %>% select('NO3', 'type', 'habitat', 'align')

colnames(env) = c('value', 'group1', 'group2', 'align')
env$group3 = paste(env$group1, env$group2)
env$group3 = factor(env$group3, levels = c("Slight Bulk", "Slight Rhizosphere", "Severe Bulk", "Severe Rhizosphere"),
                    labels = c('BL', 'RL', 'BH', 'RH'))

cbPalette <- c("#FFDF7C", "#D0607D", "#ffad21", "#980065")
library(ggplot2)
(p4 <- ggplot(env, aes(x=group3, y=value, fill=group3)) + 
    geom_boxplot(alpha=0.5, width=0.2, color = "gray", position=position_dodge(width=0.8), size=0.1, outlier.colour = NA)+
    geom_violin(alpha=0.3, width=1, color = NA, position=position_dodge(width=0.8), size=0.1)+
    geom_point(aes(color = group3), alpha=0.4, size=.7, position=position_jitterdodge(jitter.width = 0.35, jitter.height = 0, dodge.width = 0.8))+
    scale_fill_manual(values = cbPalette)+
    scale_color_manual(values = cbPalette)+
    geom_line(aes(group = align), color = 'gray', lwd = 0.25, alpha = .2) +  #绘制样本连线，通过 aes(group) 参数指定配对样本信息
    theme_classic()+
    ylim(0.0, 20.0) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(legend.position = "none", legend.title = element_blank()) +
    labs(x="", y='NO3 (mg·kg-1)') +
    guides(fill="none") +
    theme(axis.text=element_text(colour='black',size=9)))





setwd('C:/Users/11830/Desktop/R.space/pv.N_work/Figure2/nifHDK_50aa_abundance')
nifHDK <- read.csv('nifHDK.csv', row.names = 1)


nif_env <- nifHDK %>% 
  mutate(
    habitat = metadata[rownames(nifHDK), 'habitat'],
    group = metadata[rownames(nifHDK), 'type'],
    As5 = metadata[rownames(nifHDK), 'As5'],
    As3 = metadata[rownames(nifHDK), 'As3'],
    AsT = metadata[rownames(nifHDK), 'As.soil.'],
    AsL = metadata[rownames(nifHDK), 'As.leaf.'],
    AsR = metadata[rownames(nifHDK), 'As.root.'],
    pH = metadata[rownames(nifHDK), 'pH'],
    TN = metadata[rownames(nifHDK), 'TN'],
    DON = metadata[rownames(nifHDK), 'DON'],
    NH4 = metadata[rownames(nifHDK), 'NH4'],
    NO3 = metadata[rownames(nifHDK), 'NO3'])



nif_env_rhiz <- nif_env %>% filter(habitat == 'Rhizosphere')
nif_env_bulk <- nif_env %>% filter(habitat == 'Bulk')





################ Difference #################
nif_env_rhiz_l = nif_env_rhiz %>% filter(group == "Slight")
nif_env_rhiz_h = nif_env_rhiz %>% filter(group == "Severe")
nif_env_rhiz$group = factor(nif_env_rhiz$group, levels = c('Slight', 'Severe'))

cbPalette <- c("#D0607D", "#980065")
(px1 <- ggplot(nif_env_rhiz, aes(x=group, y=nifH_abun_50aa, fill=group)) + 
    geom_boxplot(alpha=0.3, width=0.1, color = "gray", position=position_dodge(width=0.8), size=0.1, outlier.colour = NA)+
    geom_violin(alpha=0.3, width=1, color = NA, position=position_dodge(width=0.8), size=0.1)+
    geom_point(aes(color = group), alpha=1, size=.7, position=position_jitterdodge(jitter.width = 0.35, jitter.height = 0, dodge.width = 0.8))+
    scale_fill_manual(values = cbPalette)+
    scale_color_manual(values = cbPalette)+
    theme_classic()+
    ylim(0.0, 600.0) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(legend.position = "none", legend.title = element_blank()) +
    labs(x="", y='RPKM(nifH)') +
    guides(fill="none") +
    theme(axis.text=element_text(colour='black',size=9)))



cbPalette <- c("#D0607D", "#980065")
(px2 <- ggplot(nif_env_rhiz, aes(x=group, y=nifD_abun_50aa, fill=group)) + 
    geom_boxplot(alpha=0.3, width=0.1, color = "gray", position=position_dodge(width=0.8), size=0.1, outlier.colour = NA)+
    geom_violin(alpha=0.3, width=1, color = NA, position=position_dodge(width=0.8), size=0.1)+
    geom_point(aes(color = group), alpha=1, size=.7, position=position_jitterdodge(jitter.width = 0.35, jitter.height = 0, dodge.width = 0.8))+
    scale_fill_manual(values = cbPalette)+
    scale_color_manual(values = cbPalette)+
    theme_classic()+
    ylim(100.0, 800.0) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(legend.position = "none", legend.title = element_blank()) +
    labs(x="", y='RPKM(nifD)') +
    guides(fill="none") +
    theme(axis.text=element_text(colour='black',size=9)))



cbPalette <- c("#D0607D", "#980065")
(px3 <- ggplot(nif_env_rhiz, aes(x=group, y=nifK_abun_50aa, fill=group)) + 
    geom_boxplot(alpha=0.3, width=0.1, color = "gray", position=position_dodge(width=0.8), size=0.1, outlier.colour = NA)+
    geom_violin(alpha=0.3, width=1, color = NA, position=position_dodge(width=0.8), size=0.1)+
    geom_point(aes(color = group), alpha=1, size=.7, position=position_jitterdodge(jitter.width = 0.35, jitter.height = 0, dodge.width = 0.8))+
    scale_fill_manual(values = cbPalette)+
    scale_color_manual(values = cbPalette)+
    theme_classic()+
    ylim(200.0, 800.0) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(legend.position = "none", legend.title = element_blank()) +
    labs(x="", y='RPKM(nifK)') +
    guides(fill="none") +
    theme(axis.text=element_text(colour='black',size=9)))






library(picante)
library(vegan)
library(tidyverse)
library(ape)
setwd('C:/Users/11830/Desktop/R.space/pv.N_work/Figure1')
metadata = read.csv('metadata.csv', row.names = 'sample')

setwd("C:/Users/11830/Desktop/R.space/pv.N_work/Figure1/amplicon/Second_sample")
otutab = read.delim('table.txt', row.names = 1, sep = '\t')
tree = read.tree('rooted_tree.nwk')
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


distance_matrix <- beta(otutab, method = "bray")
distance_matrix <- as.matrix(distance_matrix)
meta = meta[rownames(distance_matrix),]
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
score$AsA = score$As5 + score$As3
data = score %>% filter(habitat=='Rhizosphere')

(ggplot(data, aes(x=As5, y=X3))+
    geom_point(size=3, alpha=.5,color="#980065")+
    labs(x="As5(mg·kg-1)", y="PCo3")+
    geom_smooth(method=lm,level=0.95,color="#980065",alpha=.3,formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> pc4)




setwd('C:/Users/11830/Desktop/R.space/pv.N_work/Figure2')

plantN = read.csv('Pot1_plant_TCTN.csv', row.names = 'Name')

plantN$treatment = factor(plantN$treatment, levels = c("Control", "AsV"))

data = melt(plantN)

data = data %>% filter(variable!='TC.root' & variable!='TC.shoot')

(ggplot(data=data, aes(x=variable, y=value, fill=treatment, color=treatment))+
    stat_summary(fun=mean, geom = "bar", fun.args = list(mult=1), width=0.7, position=position_dodge(width=.7), fill = 'white', width=.6, size=.4, alpha = .5)+
    stat_summary(fun.data = mean_sdl, fun.args = list(mult=1), geom="errorbar", width=0.2, position=position_dodge(width=.7))+
    geom_point(size = 2, position=position_jitterdodge(dodge.width=0.7)) + 
    theme_bw()+
    scale_color_manual(values = c('#2e86c1', '#b9770e')) + 
    scale_y_continuous(expand = c(0,0), limits = c(0,2)) +
    labs(x = "", y="")+
    theme(axis.title.y.left = element_blank(),
          legend.position = c(0.3,0.7),
          legend.title = element_blank()) -> f2p8)





#################################################################
library(tidyverse)

setwd('C:/Users/11830/Desktop/R.space/pv.N_work/Figure2')

data = read.csv("pot_plant_biomass.csv")

data$treatment = factor(data$treatment, level = c('CK', 'AsV', 'NH4', 'NO3'))

data = data %>% filter(treatment != "NO3")

(ggplot(data, aes(treatment, biomass))+ 
    
    stat_summary(mapping = aes(color = treatment),fun=mean, fun.args = list(mult=1), geom='bar', fill = 'white', width=.6, size=.4) +
    
    geom_jitter(mapping = aes(color=treatment), position=position_jitter(0.17), size=3, alpha=.6)+
    
    stat_summary(fun.data = mean_se, fun.args = list(mult=1), geom='errorbar', color='black', width=.2) +    
    
    scale_color_manual(values=c("#DAF7A6", "#FFC300", "#FF5733"))+
    
    labs(x = "", y="biomass (g)")+
    
    scale_y_continuous(expand = c(0,0), limits = c(0,3), breaks = c(0,0.5,1.0,1.5,2.0,2.5,3.0)) +
    
    theme_classic()+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          legend.position = "none")+
    
    theme(axis.text=element_text(colour='black',size=9)) -> biomass)

####################################################

data = read.csv("qPCR_primer01.csv")

data$group = factor(data$group, level = c('BC', 'BA', 'RC', 'RA'))

(ggplot(data, aes(group, nifH))+ 
    
    stat_summary(mapping = aes(color = group),fun=mean, fun.args = list(mult=1), geom='bar', fill = 'white', width=.6, size=.4) +
    
    geom_jitter(mapping = aes(color = group), position=position_jitter(0.17), size=3, alpha=.6)+
    
    stat_summary(fun.data = mean_se, fun.args = list(mult=1), geom='errorbar', color='black', width=.2) +    
    
    scale_color_manual(values=c("#D1E5F0","#FDDBC7","#67A9CF","#EF8A62","#2166AC","#B2182B"))+
    
    labs(x = "", y="nifH (copies·mg-1 dry soil)")+
    
    scale_y_continuous(expand = c(0,0), limits = c(0,180000), labels = scales::scientific) +
    
    theme_classic()+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          legend.position = "none")+
    
    theme(axis.text=element_text(colour='black',size=9)) -> qpcr_nifH)



library(patchwork)

(p1 | p2 | p3 | p4) / (pc4 | px1 | px2 | px3) / (biomass | biomass | f2p8 | qpcr_nifH)
