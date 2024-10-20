
library(picante)
library(vegan)
library(tidyverse)
library(ape)

##############################
meta = read.csv('metadata.csv', row.names = 1)

meta$Treatment = factor(meta$Treatment, levels = c("CK", "Mes", "Cup", "Bra", "Rhi", "Mix"))

meta = meta %>% filter(Treatment!="Mix")

meta$Treatment = factor(meta$Treatment, levels = c("CK", "Mes", "Cup", "Bra", "Rhi"))

meta_filt = meta %>% select('dry_weight', 'Treatment')

meta_filt_melt = melt(meta_filt)


(ggplot(data=meta_filt_melt, aes(x=Treatment, y=value, fill=variable))+
    stat_summary(fun=mean, geom = "bar",fun.args = list(mult=1),width=0.7, position=position_dodge(width=.7), alpha = .7)+
    stat_summary(fun.data=mean_sdl,fun.args = list(mult=1),geom="errorbar",width=0.2, position=position_dodge(width=.7))+
    geom_point(position=position_jitterdodge(dodge.width=0.7), alpha=.5) + 
    theme_bw()+
    scale_y_continuous(expand = c(0,0), limits = c(0,0.71)) +
    labs(x = "")+
    theme(axis.title.y.left = element_blank(),
          legend.position = 'right',
          legend.title = element_blank()) -> pot2_p1)




ck = meta_filt_melt %>% filter(Treatment=='CK')
Bra = meta_filt_melt %>% filter(Treatment=='Bra')
Rhi = meta_filt_melt %>% filter(Treatment=='Rhi')

wilcox.test(Bra$value, ck$value)
wilcox.test(Rhi$value, ck$value)


(ggplot(data=meta, aes(x=Treatment, y=nifH_copy, fill = Treatment))+
    stat_summary(fill = "#e07233", fun=mean, geom = "bar",fun.args = list(mult=1),width=0.7)+
    stat_summary(fun.data=mean_sdl,fun.args = list(mult=1),geom="errorbar",width=0.2)+
    geom_point(position=position_jitterdodge(dodge.width=0.7), alpha=.5) + 
    theme_bw()+
    scale_y_continuous(expand = c(0,0), limits = c(0,2000000), labels = scales::scientific) +
    labs(x = "", y="")+
    theme(axis.title.y.left = element_blank(),
          legend.position = 'right',
          legend.title = element_blank()) -> pot2_p3)

meta_ck = meta %>% filter(Treatment=='CK')
meta_Bra = meta %>% filter(Treatment=='Bra')
meta_Rhi = meta %>% filter(Treatment=='Rhi')

wilcox.test(meta_Bra$nifH_copy, meta_ck$nifH_copy)
wilcox.test(meta_Rhi$nifH_copy, meta_ck$nifH_copy)


meta_N = meta %>% select('TN.root','TN.shoot','Treatment')

data = melt(meta_N)

data$Treatment = factor(data$Treatment, levels = c("CK", "Mes", "Cup", "Bra","Rhi"))


ck.shoot = data %>% filter(variable == 'TN.shoot' & Treatment=='CK')
Bra.shoot = data %>% filter(variable == 'TN.shoot' & Treatment=='Bra')
Rhi.shoot = data %>% filter(variable == 'TN.shoot' & Treatment=='Rhi')


cbpalette = c("#DDDDDD",
              "#8AAAB6",
              "#0089A4",
              "#FFCC00",
              "#ce2523")

(ggplot(data=data, aes(x=variable, y=value, fill=Treatment))+
    stat_summary(fun=mean, geom = "bar", fun.args = list(mult=1), width=0.7, position=position_dodge(width=.7), alpha = .7)+
    stat_summary(fun.data = mean_sdl, fun.args = list(mult=1), geom="errorbar", width=0.2, position=position_dodge(width=.7))+
    geom_point(position=position_jitterdodge(dodge.width=0.7), alpha=.5) + 
    theme_bw()+
    scale_fill_manual(values = cbpalette)+
    scale_y_continuous(expand = c(0,0), limits = c(0,3)) +
    labs(x = "", y="")+
    theme(axis.title.y.left = element_blank(),
          legend.position = 'right',
          legend.title = element_blank()) -> pot2_p2)





metadata = read.csv('metadata.csv', row.names = 'qiime2.sample')
otutab = read.delim('ASVtable.txt', row.names = 1, sep = '\t')
taxon = read.delim('taxon.txt', row.names = 1, sep = '\t')


library(phyloseq)
phylo = phyloseq(otu_table(otutab, taxa_are_rows = TRUE),
                 tax_table(tax),
                 sample_data(metadata))

rare = rarefy_even_depth(phylo,replace = TRUE,rngseed = 711)
sample_sums(phylo)
sample_sums(rare) #查看抽平前后的变化
# 47861

otu.rare = data.frame(otu_table(rare))

otu.rare.filt = otu.rare[c('b27191fcbe07f75bed6bf183ecb8c338',
                           'bcbb708a30cec667c1315494bd7c9f8b',
                           'ec25d169faef22bc9163a0a1987a92c8',
                           'a982a0f1091cb2e2cf172c626a0ae01a'),]


library(reshape2)

otu.rare.filt$id = rownames(otu.rare.filt)

otu.rare.filt.melt = melt(otu.rare.filt, id.vars = 'id')

index = data.frame(c('Bradyrhizobium', 'Mesorhizobium', 'Cupriavidus', 'Rhizobium'),
                   row.names = c('b27191fcbe07f75bed6bf183ecb8c338',
                                 'bcbb708a30cec667c1315494bd7c9f8b',
                                 'ec25d169faef22bc9163a0a1987a92c8',
                                 'a982a0f1091cb2e2cf172c626a0ae01a'))

colnames(index) = "taxon"

otu.rare.filt.melt$taxon = index[otu.rare.filt.melt$id,]

x = replicate(16, c("Brad", "Cup", "Control", "Mes", "Rhi"))

otu.rare.filt.melt$treatment = c(x[1,],x[2,],x[3,],x[4,],x[5,])

otu.rare.filt.melt$treatment = factor(otu.rare.filt.melt$treatment, levels = c('Control', 'Mes', 'Cup', 'Brad', 'Rhi'))


library(ggplot2)
library(ggpubr)
library(ggbreak)


cbpalette = c("#FFCC00",
              "#0089A4",
              "#8AAAB6",
              "#ce2523")


(
  ggplot(otu.rare.filt.melt, aes(x = treatment, y = value/47861 * 100, fill = taxon)) +
    geom_bar(stat = 'summary', fun = 'mean', position = position_dodge(), alpha = .7) + 
    stat_summary(fun.data = 'mean_sd', geom = "errorbar", colour = "black", width = .7, position = position_dodge(.9)) +
    
    geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .3), size = 1) + 
    theme_bw() +
    scale_fill_manual(values = cbpalette)+
    scale_y_continuous(expand = c(0,0), limits = c(0.0,8.0)) +
    labs(x = "", y="")+
    theme(axis.title.y.left = element_blank(),
          legend.position = 'right',
          legend.title = element_blank()) -> pot2_p4
)












library(ggalluvial)

data_mod_d1 = read.csv('As_gene_taxon.csv')

scaleFUN <- function(x) sprintf("%.2f", x)

## Rhiz ##
data_ = data_mod_d1 %>% filter(ORF == 'aioA' & ind == 'Rhiz')

scaleFUN <- function(x) sprintf("%.2f", x)
(ggplot(as.data.frame(data_), aes(y = values/5, axis1 = domain, axis2 = phylum, axis3 = class, axis4 = order)) + 
    
    geom_alluvium(aes(fill = order), width= 1/6, curve_type="sigmoid") +
    
    geom_stratum(width = 1/6, fill = "black", alpha= .3, colour = "#FFFFFF", size=.3) +
    
    geom_text(aes(label = after_stat(stratum)), stat = "stratum", size = 3) +
    
    scale_x_discrete(limits = c('domain', 'phylum', 'class', 'order'), expand = c(0.01, 0.01)) +
    
    scale_y_continuous(expand = expansion(mult = c(0.01,0.01)), labels=scaleFUN, limits = c(0,300))+
    
    scale_fill_manual(values = c("#D00A1C","#80B1D3","#B3DE69","#BC80BD","#FF7F00","#FFFFB3","#FDB462","#FCCDE5","#D9D9D9")) + 
    
    theme_classic() + 
    
    guides(fill=FALSE) +
    
    ylab("TPM") +
    
    ggtitle("") -> sankey.p)





library(patchwork)

sankey.p / ((pot2_p1 / pot2_p2) | (pot2_p3 / pot2_p4))

