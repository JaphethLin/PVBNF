


library(picante)
library(vegan)
library(tidyverse)
library(ape)
library(reshape2)
library(yolo)
library(patchwork)

metadata = read.csv('metadata.csv', row.names = 'sample')
metabolite = read.csv('metabolite.csv', row.names = 'ID')


# 高砷条件下
metadata_f = metadata %>% filter(type=='Severe' & habitat!='Root')
metabolite_data = metabolite[,row.names(metadata_f)]
metabolite_info = metabolite[,c(1:10,14)]


# Carbohydrates
plot_meta = metadata_f[colnames(metabolite_data),]
plot_info = metabolite_info %>% filter(group=='Carbohydrates')
plot_data = metabolite_data[rownames(plot_info),]

abun = abundance(as.matrix(plot_data), as.matrix(plot_meta), group = "habitat", taxon = as.matrix(plot_info), taxonomy.level = "name", attention = 100)
rela_abun = dcast(abun, Var1~Var2)
rownames(rela_abun) = rela_abun$Var1
rela_abun = rela_abun[,-1]
rela_abun.t = t(rela_abun)
rela_abun = as.data.frame(t(rela_abun.t / rowSums(rela_abun.t)))

rela_abun$name = rownames(rela_abun)
#rela_abun$name = factor(rela_abun$name, levels = ord)
rela_abun_plot = melt(rela_abun)

cbpalette <- c(  "#8486FF", "#B480FF", "#690091", "#7149af", "#FF92C9",
                 "#6E1F1F", "#ce2523", "#e07233", "#FF9E92", "#FFD8D3",
                 "#FF59BB", "#FF00F3", "#A00000", "#FF0000", "#FF8686",
                 "#BDB200", "#FFCC00", "#FFDF7C", "#69C000", "#A5FF00", "#CCFF6E",
                 "#00C08C", "#00FFBA", "#79FFDB", "#0089A4", "#00D5FF", "#7AE9FF",
                 "#0147C2", "#0078FF", "#00029E", "#0003FF")

ggplot(data=rela_abun_plot, aes(x=variable, y=value, fill = name))+
  geom_bar(stat="identity",
           position = "stack")+
  #coord_flip() +
  scale_fill_manual(values = cbpalette)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0)))+
  labs(x = "", y = "")+
  theme(axis.title.y.left = element_blank(),
        legend.position = 'top',
        legend.title = element_blank()) -> f5p4
f5p4


### Mannose ###
stat = rela_abun_plot %>% filter(name=='D-Mannose')
(stat[2,'value'] - stat[1,'value']) / stat[1,'value']
(stat[2,'value'] - stat[3,'value']) / stat[3,'value']
### ####### ###


# alpha amino acid
plot_meta = metadata_f[colnames(metabolite_data),]
plot_info = metabolite_info %>% filter(group=='alpha amino acid')
plot_data = metabolite_data[rownames(plot_info),]

abun = abundance(as.matrix(plot_data), as.matrix(plot_meta), group = "habitat", taxon = as.matrix(plot_info), taxonomy.level = "name", attention = 100)
rela_abun = dcast(abun, Var1~Var2)
rownames(rela_abun) = rela_abun$Var1
rela_abun = rela_abun[,-1]
rela_abun.t = t(rela_abun)
rela_abun = as.data.frame(t(rela_abun.t / rowSums(rela_abun.t)))

rela_abun$name = rownames(rela_abun)
#rela_abun$name = factor(rela_abun$name, levels = ord)
rela_abun_plot = melt(rela_abun)

cbpalette <- c( "#8486FF", "#B480FF", "#690091", "#7149af", "#FF92C9",
                "#931635", "#ce2523", "#e07233", "#FF9E92", "#FFD8D3",
                "#FF59BB", "#A0005E", "#A00000", "#FF0000", "#FF8686",
                "#BDB200", "#FFCC00", "#FFDF7C", "#69C000", "#A5FF00", "#CCFF6E",
                "#00C08C", "#00FFBA", "#79FFDB", "#0089A4", "#00D5FF", "#7AE9FF",
                "#0147C2", "#0078FF", "#00029E", "#0003FF", "#74B5FF")

ggplot(data=rela_abun_plot, aes(x=variable, y=value, fill = name))+
  geom_bar(stat="identity",
           position = "stack")+
  # coord_flip() +
  scale_fill_manual(values = cbpalette)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0)))+
  labs(x = "", y = "")+
  theme(axis.title.y.left = element_blank(),
        legend.position = 'top',
        legend.title = element_blank()) -> f5p5
f5p5


### Valine ###
stat = rela_abun_plot %>% filter(name=='L-Valine')
(stat[2,'value'] - stat[1,'value']) / stat[1,'value']
(stat[2,'value'] - stat[3,'value']) / stat[3,'value']
### ####### ###





# flavonoid
plot_meta = metadata_f[colnames(metabolite_data),]
plot_info = metabolite_info %>% filter(group=='flavonoid')
plot_data = metabolite_data[rownames(plot_info),]

abun = abundance(as.matrix(plot_data), as.matrix(plot_meta), group = "habitat", taxon = as.matrix(plot_info), taxonomy.level = "name", attention = 100)
rela_abun = dcast(abun, Var1~Var2)
rownames(rela_abun) = rela_abun$Var1
rela_abun = rela_abun[,-1]
rela_abun.t = t(rela_abun)
rela_abun = as.data.frame(t(rela_abun.t / rowSums(rela_abun.t)))

rela_abun$name = rownames(rela_abun)
#rela_abun$name = factor(rela_abun$name, levels = ord)
rela_abun_plot = melt(rela_abun)

cbpalette <- c( "#ACA5FF", "#7149af", "#FF92C9",
                "#931635", "#FF9E92", "#FFD8D3",
                "#FF59BB", "#FF0000", "#FF8686",
                "#BDB200", "#69C000", "#A5FF00", "#CCFF6E",
                "#00C08C", "#0089A4", "#00D5FF", "#7AE9FF",
                "#0147C2", "#74B5FF")

ggplot(data=rela_abun_plot, aes(x=variable, y=value, fill = name))+
  geom_bar(stat="identity",
           position = "stack")+
  # coord_flip() +
  scale_fill_manual(values = cbpalette)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0)))+
  labs(x = "", y = "")+
  theme(axis.title.y.left = element_blank(),
        legend.position = 'top',
        legend.title = element_blank()) -> f5p6
f5p6





plot_info = metabolite_info %>% filter(group=='Carbohydrates' | group=='alpha amino acid' | group=='flavonoid')

plot_info %>% select(c('name','enrichment','Rhiz_change','root_change','group')) %>% filter(group=='Carbohydrates')

plot_info %>% select(c('name','enrichment','Rhiz_change','root_change','group')) %>% filter(group=='alpha amino acid')

plot_info %>% select(c('name','enrichment','Rhiz_change','root_change','group')) %>% filter(group=='flavonoid')

(f5p4 | f5p5 | f5p6)
