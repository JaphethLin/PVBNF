
library(tidyverse)

data = read.csv('greenhouse_exp01.csv')

data_long = reshape2::melt(data)

library(ggsci)

data_long = data_long %>% filter(variable!='DOC')

data_long$Trt = factor(data_long$Trt, levels = c('Bulk', 'Bulk_As', 'Control', 'As'), labels = c('Bulk soil(Control)', 'Bulk soil(As)', 'Rhiz.(Control)', 'Rhiz.(As)'))


(ggplot(subset(data_long, variable=='NH4'), aes(x=variable, y=value, fill=Trt))+
    stat_summary(fun=mean, geom = "bar", fun.args = list(mult=1), width=0.7, position=position_dodge(width=.7), alpha = .7)+
    stat_summary(fun.data = mean_sdl, fun.args = list(mult=1), geom="errorbar", width=0.2, position=position_dodge(width=.7))+
    geom_point(position=position_jitterdodge(dodge.width=0.7)) + 
    theme_bw()+
    scale_fill_manual(values = c('#1666D5', '#CB1F1F','#33D8E9', '#FF9452')) +
    scale_y_continuous(expand = c(0,0), limits = c(0,3)) +
    labs(x = "", y="")+
    theme(axis.title.y.left = element_blank(),
          legend.position = 'bottom',
          legend.title = element_blank()) -> p1)

(ggplot(subset(data_long, variable=='NO3'), aes(x=variable, y=value, fill=Trt))+
    stat_summary(fun=mean, geom = "bar", fun.args = list(mult=1), width=0.7, position=position_dodge(width=.7), alpha = .7)+
    stat_summary(fun.data = mean_sdl, fun.args = list(mult=1), geom="errorbar", width=0.2, position=position_dodge(width=.7))+
    geom_point(position=position_jitterdodge(dodge.width=0.7)) + 
    theme_bw()+
    scale_fill_manual(values = c('#1666D5', '#CB1F1F','#33D8E9', '#FF9452')) +
    scale_y_continuous(expand = c(0,0), limits = c(0,3)) +
    labs(x = "", y="")+
    theme(axis.title.y.left = element_blank(),
          legend.position = 'bottom',
          legend.title = element_blank()) -> p2)

library(gridExtra)
# 使用 grid.arrange() 将图组合在一起
grid.arrange(p1, p2, ncol = 2)
