library(ggalluvial)
library(ggplot2)
library(tidyverse)

data_mod_d1 = read.csv('As_gene_taxon.csv')

data_mod_d1 = data_mod[,-1]


# 低污染
data_ = data_mod_d1 %>% filter(ORF == 'arsM' & ind == 'rhiz_severe')

(arsM <- ggplot(as.data.frame(data_), aes(y = values/5, axis1 = ORF, axis2 = domain, axis3 = phylum, axis4 = class, axis5 = order)) + 
    
    geom_alluvium(aes(fill = order), width= 1/6, curve_type="sigmoid") +
    
    geom_stratum(width = 1/6, fill = "black", alpha= .3, colour = "#FFFFFF", size=.3) +
    
    geom_text(aes(label = after_stat(stratum)), stat = "stratum", size = 3) +
    
    scale_x_discrete(limits = c('ORF','domain', 'phylum', 'class', 'order'), expand = c(0.01, 0.01)) +
    
    scale_fill_manual(values = c('#FF5733', '#C70039', '#900C3F', '#581845', '#FFC300', '#F44336', '#E91E63', 
                                 '#9C27B0', '#673AB7', '#3F51B5', '#2196F3', '#03A9F4', '#00BCD4', '#009688',
                                 '#4CAF50', '#8BC34A', '#CDDC39', '#FFEB3B', '#FFC107', '#FF9800', '#FF5722',
                                 '#795548', '#9E9E9E', '#607D8B', '#FA8072', '#FFA07A', '#E9967A', '#F08080',
                                 '#CD5C5C', '#DC143C', '#FF1493', '#FF69B4', '#FFC0CB', '#FFB6C1', '#FFA07A', 
                                 '#FF8C00', '#FF7F50', '#FF4500', '#FFD700', '#FFFF00', '#FF6347', '#FF69B4', 
                                 '#FF00FF', '#9400D3', '#8B008B', '#800080', '#4B0082', '#0000FF', '#00BFFF', 
                                 '#00CED1', '#20B2AA', '#008080', '#008000', '#808000', '#A52A2A', '#B22222', 
                                 '#DAA520', '#D2691E', '#2F4F4F', '#008B8B', '#FF4500', '#FFA500', '#FF8C00', 
                                 '#FFC0CB', '#FF69B4', '#FFB6C1', '#FFA07A', '#FF7F50', '#FF6347', '#FFDAB9', 
                                 '#FAEBD7', '#FFEFD5', '#FFF8DC', '#F0E68C', '#EEE8AA', '#BDB76B', '#F5DEB3', 
                                 '#D2B48C', '#DEB887', '#BC8F8F', '#CD853F', '#A0522D', '#8B4513', '#D2691E', 
                                 '#A0522D', '#B8860B', '#FFDEAD', '#DFDFDF')) +
    #  scale_fill_brewer(type= "qual", palette = getPalette(colourCount)) +
    
    theme_classic() +
    
    guides(fill = "none") +
    
    ylab("RPKM") +
    ggtitle(""))





# 低污染
data_ = data_mod_d1 %>% filter(ORF == 'arsC_glut' & ind == 'rhiz_severe')

## color ##
library(RColorBrewer)
colourCount = length(unique(data_$order))
getPalette = colorRampPalette(brewer.pal(12, "Set3"))

(arsC <- ggplot(as.data.frame(data_), aes(y = values/5, axis1 = ORF, axis2 = domain, axis3 = phylum, axis4 = class, axis5 = order)) + 
    
    geom_alluvium(aes(fill = order), width= 1/6, curve_type="sigmoid") +
    
    geom_stratum(width = 1/6, fill = "black", alpha= .3, colour = "#FFFFFF", size=.3) +
    
    geom_text(aes(label = after_stat(stratum)), stat = "stratum", size = 3) +
    
    scale_x_discrete(limits = c('ORF','domain', 'phylum'), expand = c(0.01, 0.01)) +
    
    scale_fill_manual(values = c('#FF5733', '#C70039', '#900C3F', '#581845', '#FFC300', '#FF5733', '#C70039', '#900C3F', 
                                 '#581845', '#FFC300', '#F44336', '#E91E63', '#9C27B0', '#673AB7', '#3F51B5', '#2196F3', 
                                 '#03A9F4', '#00BCD4', '#009688', '#4CAF50', '#8BC34A', '#CDDC39', '#FFEB3B', '#FFC107', 
                                 '#FF9800', '#FF5722', '#795548', '#9E9E9E', '#607D8B', '#FA8072', '#FFA07A', '#E9967A', 
                                 '#F08080', '#CD5C5C', '#DC143C', '#FF1493', '#FF69B4', '#FFC0CB', '#FFB6C1', '#FFA07A', 
                                 '#FF8C00', '#FF7F50', '#FF4500', '#FFD700', '#FFFF00', '#FF6347', '#FF69B4', '#FF00FF', 
                                 '#9400D3', '#8B008B', '#800080', '#4B0082', '#0000FF', '#00BFFF', '#00CED1', '#20B2AA',
                                 '#008080', '#008000', '#808000', '#A52A2A', '#DFDFDF')) +
    #  scale_fill_brewer(type= "qual", palette = getPalette(colourCount)) +
    
    theme_classic() +
    
    guides(fill = "none") +
    
    ylab("RPKM") +
    ggtitle(""))

library(patchwork)
arsC / arsM
