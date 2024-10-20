library(yolo)
library(picante)
library(vegan)
library(tidyverse)
library(ape)

setwd("C:/Users/11830/Desktop/R.space/pv.N_work/Figure4")

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
otutab = read.delim('table.txt', row.names = 1, sep = '\t')
taxon = read.delim('taxon.txt', row.names = 1, sep = '\t')

taxon$id = rownames(taxon)

meta = metadata %>% filter(Treatment != 'Mix')
meta$sample = rownames(meta) 
otu = otutab[,rownames(meta)]
tax = as.matrix(taxon[rownames(otutab),])


library(phyloseq)
phylo = phyloseq(otu_table(otutab, taxa_are_rows = TRUE),
                 tax_table(tax),
                 sample_data(meta))
rare = rarefy_even_depth(phylo,replace = TRUE,rngseed = 711)
sample_sums(phylo)
sample_sums(rare) #查看抽平前后的变化
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

otu.rare.filt.melt %>% filter(treatment=='Brad'&taxon=='Bradyrhizobium') %>% summarise(mean(value)/49153*100)
otu.rare.filt.melt %>% filter(treatment=='Control'&taxon=='Bradyrhizobium') %>% summarise(mean(value)/49153*100)

otu.rare.filt.melt %>% filter(treatment=='Rhi'&taxon=='Rhizobium') %>% summarise(mean(value)/49153*100)
otu.rare.filt.melt %>% filter(treatment=='Control'&taxon=='Rhizobium') %>% summarise(mean(value)/49153*100)

otu.rare.filt.melt %>% filter(treatment=='Cup'&taxon=='Cupriavidus') %>% summarise(mean(value)/49153*100)
otu.rare.filt.melt %>% filter(treatment=='Control'&taxon=='Cupriavidus') %>% summarise(mean(value)/49153*100)

otu.rare.filt.melt %>% filter(treatment=='Mes'&taxon=='Mesorhizobium') %>% summarise(mean(value)/49153*100)
otu.rare.filt.melt %>% filter(treatment=='Control'&taxon=='Mesorhizobium') %>% summarise(mean(value)/49153*100)


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
library(ggplot2)
library(tidyverse)


setwd("C:/Users/11830/Desktop/R.space/pv.N_work/Figure1")
metadata = read.csv('metadata.csv', row.names = 'raw.sample')


setwd('D:/PV/PV_gene/Sankey')

gene_tax = read.csv('As_tax.tsv', sep = '\t')

##
colnames(gene_tax)[56] <- "ORF"
##
cn = colnames(gene_tax)
cn = gsub('X', 's', cn)
colnames(gene_tax) = cn

metadata = metadata_all %>% filter(habitat=='Bulk')
tmp = gene_tax[, metadata$map]
gene_tax$Bulk = rowMeans(tmp)

metadata = metadata_all %>% filter(habitat=='Rhizosphere')
tmp = gene_tax[, metadata$map]
gene_tax$Rhiz = rowMeans(tmp)

data_raw = gene_tax[,c('index', 'ORF', 'domain', 'phylum', 'class', 'order', 'Bulk', 'Rhiz')]

# statistics
data_raw_stat = data_raw %>% filter(ORF == 'aioA')
colSums(data_raw_stat[,7:8])
#

data_mod = cbind(data_raw[1:6], stack(data_raw[7:8]))

data_mod_d1 = data_mod



## color ##
library(RColorBrewer)
colourCount = length(unique(data_$order))
getPalette = colorRampPalette(brewer.pal(10, "Set3"))

scaleFUN <- function(x) sprintf("%.2f", x)


## Rhiz ##
#data_ = data_mod_d1 %>% filter(ORF == 'aioA' & ind == 'Rhiz')
data_ = data_mod_d1 %>% filter(ORF == 'aioA' & ind == 'Rhiz') # %>% filter(order!='unknow')
tmp = data_mod_d1 %>% filter(ORF == 'aioA' & ind == 'Rhiz')
proportion = sum(data_$values) / sum(tmp$values)

proportion = as.data.frame(rbind(proportion, 1-proportion))
proportion$group = c("Known", "Unkown")
colnames(proportion) = c("values", "group")
(ggplot(data = proportion, mapping = aes(x = 'Content', y = values, fill = group)) + 
    geom_bar(stat = 'identity', position = 'stack', width = 0.5) + 
    coord_polar(theta = 'y') + labs(x = '', y = '', title = '') + 
    scale_fill_manual(values = c("#980065", "#EEEEEE")) + 
    theme(panel.background = element_blank()) +
    theme(axis.text = element_blank()) + 
    theme(axis.ticks = element_blank()) + 
    theme(legend.position = "none") +
    geom_text(aes(label = round(values,4)*100)) -> pp2)

domain.proportion = aggregate(data_$values, by=list(data_$domain), FUN = sum)
domain.proportion$values = domain.proportion$x / sum(data_$values)
domain.label = domain.proportion %>% filter(values > 0)

phylum.proportion = aggregate(data_$values, by=list(data_$phylum), FUN = sum)
phylum.proportion$values = phylum.proportion$x / sum(data_$values)
phylum.label = phylum.proportion %>% filter(values > 0.01)

class.proportion = aggregate(data_$values, by=list(data_$class), FUN = sum)
class.proportion$values = class.proportion$x / sum(data_$values)
class.label = class.proportion %>% filter(values > 0.01)

order.proportion = aggregate(data_$values, by=list(data_$order), FUN = sum)
order.proportion$values = order.proportion$x / sum(data_$values)
order.label =order.proportion %>% filter(values > 0.01)

label_list2 = c(domain.label$Group.1, phylum.label$Group.1, class.label$Group.1, order.label$Group.1)

label.judge <- function(A, B) {
  A[!(A %in% B)] <- ""
  return(A)
} 

scaleFUN <- function(x) sprintf("%.2f", x)
(ggplot(as.data.frame(data_), aes(y = values/5, axis1 = domain, axis2 = phylum, axis3 = class, axis4 = order)) + 
    
    geom_alluvium(aes(fill = order), width= 1/6, curve_type="sigmoid") +
    
    geom_stratum(width = 1/6, fill = "black", alpha= .3, colour = "#FFFFFF", size=.3) +
    
    geom_text(aes(label = after_stat(stratum)), stat = "stratum", size = 3) +
    
    #geom_text(aes(label = label.judge(after_stat(stratum), label_list1)), stat = "stratum", size = 3) +
    
    scale_x_discrete(limits = c('domain', 'phylum', 'class', 'order'), expand = c(0.01, 0.01)) +
    
    scale_y_continuous(expand = expansion(mult = c(0.01,0.01)), labels=scaleFUN, limits = c(0,300))+
    
    scale_fill_manual(values = c("#D00A1C","#80B1D3","#B3DE69","#BC80BD","#FF7F00","#FFFFB3","#FDB462","#FCCDE5","#D9D9D9")) + 
    
    theme_classic() + 
    
    guides(fill=FALSE) +
    
    ylab("TPM") +
    
    ggtitle("") -> sankey.p)





library(patchwork)

sankey.p / ((pot2_p1 / pot2_p2) | (pot2_p3 / pot2_p4))

