
setwd('C:/Users/11830/Desktop/Research/Articles/PV_N.article/Nature Communications/返修1/Figures返修1/FigureS10_revised/Isolation')

exp3_As = read.csv('expriment3_As_species.csv')
exp3_od = read.csv('expriment3_OD.csv')


exp3_As[exp3_As<0] <- 0

library(tidyverse)

library(Rmisc)

data = exp3_As %>% filter(Medium=="NE" & Strain!="CK")

err <- summarySE(data, measurevar=c("AsV"), groupvars=c("Time","Strain","Medium"))

#err$ppm = factor(err$ppm, levels = c('AsV10', 'AsV5','AsIII10', 'AsIII5','CK'))

(ggplot(err, aes(x=Time, y=AsV, color=Strain, group = Strain, fill = Strain)) + 
    geom_errorbar(aes(ymin=AsV-se, ymax=AsV+se), width=.1, size=.3, alpha=.8) +
    geom_line(aes(color=Strain))+
    # geom_point(aes(color=Strain),size=2.5, alpha=.5)+
    scale_x_discrete(expand = expansion(mult = c(0.05,0.05))) +
    scale_y_continuous(limits = c(0,16)) +
    scale_color_manual(values = c('N11' = '#008080', 'J3' = '#FF7F50', 'G5' = '#808000', 'G7' = '#9c21c3', 'F3' = '#FFD700', 'M12' = '#CC5500',  'N2' = '#0331ff')) +
    theme_bw()+
    theme(legend.position = "none") -> e1)



data = exp3_As %>% filter(Medium=="NF" & Strain!="CK")

err <- summarySE(data, measurevar=c("AsV"), groupvars=c("Time","Strain","Medium"))

(ggplot(err, aes(x=Time, y=AsV, color=Strain, group = Strain, fill = Strain)) + 
    geom_errorbar(aes(ymin=AsV-se, ymax=AsV+se), width=.1, size=.3, alpha=.8) +
    geom_line(aes(color=Strain))+
    # geom_point(aes(color=Strain),size=2.5, alpha=.5)+
    scale_x_discrete(expand = expansion(mult = c(0.05,0.05))) +
    scale_y_continuous(limits = c(0,16)) +
    scale_color_manual(values = c('N11' = '#008080', 'J3' = '#FF7F50', 'G5' = '#808000', 'G7' = '#9c21c3', 'F3' = '#FFD700', 'M12' = '#CC5500',  'N2' = '#0331ff')) +
    theme_bw()+
    theme(legend.position = "none") -> e2)




data = exp3_od %>% filter(Medium=="NE"& Strain!="CK")

err <- summarySE(data, measurevar=c("OD"), groupvars=c("Time","Strain","Medium"))

(ggplot(err, aes(x=Time, y=OD, color=Strain, group = Strain, fill = Strain)) + 
    geom_errorbar(aes(ymin=OD-se, ymax=OD+se), width=.1, size=.3, alpha=.8) +
    geom_line(aes(color=Strain))+
    # geom_point(aes(color=Strain),size=2.5, alpha=.5)+
    scale_x_discrete(expand = expansion(mult = c(0.05,0.05))) +
    #scale_y_continuous(limits = c(0,15)) +
    scale_color_manual(values = c('N11' = '#008080', 'J3' = '#FF7F50', 'G5' = '#808000', 'G7' = '#9c21c3', 'F3' = '#FFD700', 'M12' = '#CC5500',  'N2' = '#0331ff')) +
    theme_bw()+
    theme(legend.position = "none") -> e3)



data = exp3_od %>% filter(Medium=="NF"& Strain!="CK")

err <- summarySE(data, measurevar=c("OD"), groupvars=c("Time","Strain","Medium"))

(ggplot(err, aes(x=Time, y=OD, color=Strain, group = Strain, fill = Strain)) + 
    geom_errorbar(aes(ymin=OD-se, ymax=OD+se), width=.1, size=.3, alpha=.8) +
    geom_line(aes(color=Strain))+
    # geom_point(aes(color=Strain),size=2.5, alpha=.5)+
    scale_x_discrete(expand = expansion(mult = c(0.05,0.05))) +
    scale_y_continuous(limits = c(0,0.3)) +
    scale_color_manual(values = c('N11' = '#008080', 'J3' = '#FF7F50', 'G5' = '#808000', 'G7' = '#9c21c3', 'F3' = '#FFD700', 'M12' = '#CC5500',  'N2' = '#0331ff')) +
    theme_bw()+
    theme(legend.position = c(0.1,0.8)) -> e4)




library(patchwork)
( e1 | e2 ) / (e3 | e4)
