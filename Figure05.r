


setwd('C:/Users/11830/Desktop/Research/Articles/PV_N.article/Nature Communications/返修1/Figures返修1/Figure5_revised')

library(tidyverse)

data = read.csv('15N_SILAC_Results.csv')

data$N15[data$N15<0] <- 0

library(reshape2)

data$Treatment = factor(data$Treatment, levels = c('CK', 'As', 'NonCub', 'Cub'), labels = c('Control', 
                                                                                            'Arsenic exposure', 
                                                                                            'Inactivated inoculation', 
                                                                                            'inoculation'))

(ggplot(data=data, aes(x=Treatment, y=N15, fill = Treatment))+
    stat_summary(fun=mean, geom = "bar",fun.args = list(mult=1), position = "dodge", alpha = .8)+
    geom_point(aes(color = Treatment), position=position_jitterdodge(dodge.width=0.7), alpha=.9, size = 2) + 
    stat_summary(fun.data=mean_se, fun.args = list(mult=1), geom="errorbar", width=.3, position = "dodge")+
    theme_bw()+

    scale_fill_manual(values = c(
      "#B2B2B2", "#B43220", "#FCE1AE","#DB9818")) + 
    scale_color_manual(values = c(
      "#B2B2B2", "#B43220", "#FCE1AE","#DB9818")) + 
    
    scale_y_continuous(expand = c(0,0), limits = c(0, 150)) +
    facet_wrap(vars(Tissue), ncol = 2) +
    labs(x = "", y="")+
    theme(axis.title.y.left = element_blank(),
          legend.position = 'right',
          legend.title = element_blank()) -> isoN15)


(ggplot(data=data, aes(x=Treatment, y=NFinTN, fill = Treatment))+
    geom_boxplot(position = "dodge", alpha = .8, size = .5)+
    geom_point(aes(color = Treatment), position=position_jitterdodge(dodge.width=0.7), alpha=.9, size = 2) + 
    theme_bw()+
    
    scale_fill_manual(values = c(
      "#B2B2B2", "#B43220", "#FCE1AE","#DB9818")) + 
    scale_color_manual(values = c(
      "#B2B2B2", "#B43220", "#FCE1AE","#DB9818")) + 
    
    scale_y_continuous(expand = c(0,0), limits = c(2.9, 3.35), breaks = seq(2.9, 3.3, 0.1)) +
    facet_wrap(vars(Tissue), ncol = 2) +
    labs(x = "", y="")+
    theme(axis.title.y.left = element_blank(),
          legend.position = 'right',
          legend.title = element_blank()) -> isoNF)

library(patchwork)
isoN15 / isoNF


data = read.csv('15N_SILAC_Results.csv')

data$N15[data$N15<0] <- 0
data$NFinTN[data$NFinTN<0] <- 0

data.root.ck = data %>% filter(Treatment == 'CK' & Tissue == 'root')
data.root.As = data %>% filter(Treatment == 'As' & Tissue == 'root')

data.shoot.ck = data %>% filter(Treatment == 'CK' & Tissue == 'shoot')
data.shoot.As = data %>% filter(Treatment == 'As' & Tissue == 'shoot')

data.root.NonCub = data %>% filter(Treatment == 'NonCub' & Tissue == 'root')
data.root.Cub = data %>% filter(Treatment == 'Cub' & Tissue == 'root')

data.shoot.NonCub = data %>% filter(Treatment == 'NonCub' & Tissue == 'shoot')
data.shoot.Cub = data %>% filter(Treatment == 'Cub' & Tissue == 'shoot')


wilcox.test(data.root.ck$N15, data.root.As$N15)

wilcox.test(data.shoot.ck$N15, data.shoot.As$N15)

wilcox.test(data.root.Cub$N15, data.root.NonCub$N15)

wilcox.test(data.shoot.Cub$N15, data.shoot.NonCub$N15)


wilcox.test(data.root.ck$NFinTN, data.root.As$NFinTN)

wilcox.test(data.shoot.ck$NFinTN, data.shoot.As$NFinTN)

wilcox.test(data.root.Cub$NFinTN, data.root.NonCub$NFinTN)

wilcox.test(data.shoot.Cub$NFinTN, data.shoot.NonCub$NFinTN)
