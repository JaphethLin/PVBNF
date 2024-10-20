


library(picante)
library(vegan)
library(tidyverse)
library(ape)
library(reshape2)
library(yolo)
library(patchwork)



### 非靶向 ###
metadata = read.csv('Figure1_metadata.csv', row.names = 'sample')

metabolite = read.csv('metabolite_class.csv', row.names = 'ID')
metabolite_data = metabolite[,row.names(metadata)]
metabolite_info = metabolite[,c(1:10,14)]
metabolite_all = metabolite[,row.names(metadata)]


catechin = as.data.frame(sqrt(t(metabolite_all['M289T469',])))

meta_catechin = merge(metadata_all, catechin, by='row.names')

data_ = meta_catechin
data_$type = factor(data_$type, levels = c('Slight', 'Severe'))

(ggplot(data_, aes(x = habitat, y = M289T469, fill = type)) +
    
    stat_summary(fun=mean, geom = "bar",fun.args = list(mult=1), position = "dodge", alpha = .7)+
    geom_point(aes(color = type), position=position_jitterdodge(dodge.width=.9), alpha=1, size = 2) + 
    stat_summary(fun.data=mean_se, geom="errorbar", fun.args = list(mult=1), position = position_dodge( .9), width=.2, cex=.5)+
    theme_bw()+
    
    scale_fill_manual(values = c(
      "#FFB97F", "#df7976")) + 
    scale_color_manual(values = c(
      "#FFB97F", "#df7976", "#79C1FF","#1b79af")) + 
    
    scale_y_continuous(expand = c(0,0), labels = scales::scientific) +
    # facet_wrap(vars(Tissue), ncol = 2) +
    labs(x = "", y="")+
    theme(axis.title.y.left = element_blank(),
          legend.position = 'bottom',
          legend.title = element_blank()) -> pta2) 



metadata_f = metadata %>% filter(habitat=='Rhizosphere')
metabolite_data = metabolite[,row.names(metadata_f)]
metabolite_info = metabolite[,c(1:10,14)]

# Carbohydrates
plot_meta = metadata_f[colnames(metabolite_data),]
plot_info = metabolite_info %>% filter(group=='Carbohydrates')
plot_data = metabolite_data[rownames(plot_info),]

abun = abundance(as.matrix(plot_data), as.matrix(plot_meta), group = "type", taxon = as.matrix(plot_info), taxonomy.level = "name", attention = 100)
rela_abun = dcast(abun, Var1~Var2)
rownames(rela_abun) = rela_abun$Var1
rela_abun = rela_abun[,-1]

rela_abun$change = rela_abun$Severe / rela_abun$Slight
rela_abun = arrange(rela_abun, desc(change))

rela_abun$name = rownames(rela_abun)
rela_abun_plot = rela_abun

index = plot_info
rownames(index) = index$name
rela_abun_plot$enrichment = index[rela_abun_plot$name, "enrichment"]
rela_abun_plot$Rhiz_change = index[rela_abun_plot$name, "Rhiz_change"]
rela_abun_plot$root_change = index[rela_abun_plot$name, "root_change"]

rela_abun_plot$name = factor(rela_abun_plot$name, levels = rela_abun_plot$name[order(rela_abun_plot$change)])

library(ggbreak)

ggplot(data=rela_abun_plot, aes(x=name, y=log(change), fill = Rhiz_change))+
  geom_bar(stat="identity", color = "white")+
  theme_bw()+
  scale_fill_manual(values = c("#DDDDDD", "#901D61", "#901D61")) +
  coord_flip() +
  # scale_y_break(c(3, 3.5), ticklabels=c(4)) +
  #scale_y_break(c(4, 6.5), ticklabels=c(6.5,7), expand = expansion(mult = c(0,0)), space = 0.05) +
  scale_y_continuous(expand = expansion(mult = c(0,0)), limits = c(-2, 2))+
  theme(axis.title.y.left = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(),
        axis.title.x = element_blank()) -> f5p1
f5p1



# amino acid
plot_meta = metadata_f[colnames(metabolite_data),]
plot_info = metabolite_info %>% filter(group=='alpha amino acid')
plot_data = metabolite_data[rownames(plot_info),]

abun = abundance(as.matrix(plot_data), as.matrix(plot_meta), group = "type", taxon = as.matrix(plot_info), taxonomy.level = "name", attention = 100)
rela_abun = dcast(abun, Var1~Var2)
rownames(rela_abun) = rela_abun$Var1
rela_abun = rela_abun[,-1]

rela_abun$change = rela_abun$Severe / rela_abun$Slight
rela_abun = arrange(rela_abun, desc(change))

rela_abun$name = rownames(rela_abun)
rela_abun_plot = rela_abun

index = plot_info
rownames(index) = index$name
rela_abun_plot$enrichment = index[rela_abun_plot$name, "enrichment"]
rela_abun_plot$Rhiz_change = index[rela_abun_plot$name, "Rhiz_change"]
rela_abun_plot$root_change = index[rela_abun_plot$name, "root_change"]

rela_abun_plot$name = factor(rela_abun_plot$name, levels = rela_abun_plot$name[order(rela_abun_plot$change)])

library(ggbreak)

ggplot(data=rela_abun_plot, aes(x=name, y=log(change), fill = Rhiz_change))+
  geom_bar(stat="identity", color = "white")+
  theme_bw()+
  scale_fill_manual(values = c("#DDDDDD", "#901D61", "#901D61")) +
  coord_flip() +
  # scale_y_break(c(3, 3.5), ticklabels=c(4)) +
  #scale_y_break(c(4, 6.5), ticklabels=c(6.5,7), expand = expansion(mult = c(0,0)), space = 0.05) +
  scale_y_continuous(expand = expansion(mult = c(0,0)), limits = c(-1.5, 1.5))+
  theme(axis.title.y.left = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(),
        axis.title.x = element_blank()) -> f5p2
f5p2





# flavonoid
plot_meta = metadata_f[colnames(metabolite_data),]
plot_info = metabolite_info %>% filter(group=='flavonoid')
plot_data = metabolite_data[rownames(plot_info),]

abun = abundance(as.matrix(plot_data), as.matrix(plot_meta), group = "type", taxon = as.matrix(plot_info), taxonomy.level = "name", attention = 100)
rela_abun = dcast(abun, Var1~Var2)
rownames(rela_abun) = rela_abun$Var1
rela_abun = rela_abun[,-1]

rela_abun$change = rela_abun$Severe / rela_abun$Slight
rela_abun = arrange(rela_abun, desc(change))

rela_abun$name = rownames(rela_abun)
rela_abun_plot = rela_abun

index = plot_info
rownames(index) = index$name
rela_abun_plot$enrichment = index[rela_abun_plot$name, "enrichment"]
rela_abun_plot$Rhiz_change = index[rela_abun_plot$name, "Rhiz_change"]
rela_abun_plot$root_change = index[rela_abun_plot$name, "root_change"]

rela_abun_plot$name = factor(rela_abun_plot$name, levels = rela_abun_plot$name[order(rela_abun_plot$change)])

library(ggbreak)

ggplot(data=rela_abun_plot, aes(x=name, y=log(change), fill = Rhiz_change))+
  geom_bar(stat="identity", color = "white")+
  theme_bw()+
  scale_fill_manual(values = c("#DDDDDD", "#901D61", "#901D61")) +
  coord_flip() +
  # scale_y_break(c(3, 3.5), ticklabels=c(4)) +
  #scale_y_break(c(4, 6.5), ticklabels=c(6.5,7), expand = expansion(mult = c(0,0)), space = 0.05) +
  scale_y_continuous(expand = expansion(mult = c(0,0)), limits = c(-2, 2))+
  theme(axis.title.y.left = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(),
        axis.title.x = element_blank()) -> f5p3
f5p3







isca_res = read.csv('Figure4_ISCA_Results.csv')

isca_res$sig = ifelse(isca_res$Chemotactic.index>=0, "pos", "neg")

isca_res_rhi = isca_res %>% filter(Strain=='Rhizobium')
(ggplot(data=isca_res_rhi, aes(x=Material, y=Chemotactic.index, fill=sig))+
    stat_summary(fill = "#b95645", fun=mean, geom = "bar",fun.args = list(mult=1), position = "dodge", alpha = .6, width = .8)+
    geom_point(color = "#b0393e", position=position_jitterdodge(dodge.width=.9), alpha=.6, size = 2) + 
    stat_summary(fun.data=mean_se, geom="errorbar", fun.args = list(mult=1), position = position_dodge( .9), width=.2, cex=.5)+
    theme_bw()+
    scale_y_continuous(expand = c(0,0), limits = c(0,10)) +
    labs(x = "", y="")+
    # facet_wrap(vars(Strain), ncol = 2) +
    theme(axis.title.y.left = element_blank(),
          legend.position = 'none',
          legend.title = element_blank()) -> f4p4)

isca_res_bra = isca_res %>% filter(Strain=='Bradyrhizobium')
(ggplot(data=isca_res_bra, aes(x=Material, y=Chemotactic.index, fill=sig))+
    stat_summary(fill = "#b95645", fun=mean, geom = "bar",fun.args = list(mult=1), position = "dodge", alpha = .6, width = .8)+
    geom_point(color = "#b0393e", position=position_jitterdodge(dodge.width=.9), alpha=.6, size = 2) + 
    stat_summary(fun.data=mean_se, geom="errorbar", fun.args = list(mult=1), position = position_dodge( .9), width=.2, cex=.5)+
    theme_bw()+
    scale_y_continuous(expand = c(0,0), limits = c(0,10)) +
    labs(x = "", y="")+
    # facet_wrap(vars(Strain), ncol = 2) +
    theme(axis.title.y.left = element_blank(),
          legend.position = 'none',
          legend.title = element_blank()) -> f4p5)







### 靶向 ###

target_catechin = read.csv('Target_Catechin_Results.csv')

colnames(target_catechin)[3] = 'catechin'

target_catechin$Treatment = factor(target_catechin$Treatment, levels = c('CK', 'As'), labels = c('Control', 'Arsenic'))

(ggplot(target_catechin, aes(x = Treatment, y = catechin, fill = Treatment)) +
    stat_summary(fun=mean, geom = "bar",fun.args = list(mult=1), position = "dodge", alpha = .7, width = .5)+
    geom_point(aes(color = Treatment), position=position_jitterdodge(dodge.width=.9), alpha=1, size = 2) + 
    stat_summary(fun.data=mean_se, geom="errorbar", fun.args = list(mult=1), position = position_dodge( .9), width=.2, cex=.5)+
    theme_bw()+
    
    scale_fill_manual(values = c(
      "#79C1FF","#1b79af")) + 
    scale_color_manual(values = c(
      "#79C1FF","#1b79af")) + 
    
    scale_y_continuous(expand = c(0,0), limits = c(0, 0.25)) +
    # facet_wrap(vars(Tissue), ncol = 2) +
    labs(x = "", y="")+
    theme(axis.title.y.left = element_blank(),
          legend.position = 'none',
          legend.title = element_blank()) -> pta1) 






library(patchwork)
(f5p1 | f5p2 | f5p3) / (pta2 | pta1 | f4p4 | f4p5)














