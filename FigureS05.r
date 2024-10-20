
library(tidyverse)

metadata = read.csv('metadata.csv', row.names = 'raw.sample')

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



(ggplot(nif_env_rhiz, aes(x=As5, y=nifH_abun_50aa))+
    geom_jitter(position=position_jitter(0.17), size=3, alpha=.5,color="#980065")+
    labs(x="As(V) (mg kg-1)", y="nifH Homologues (RPKM)")+
    geom_smooth(method=lm,level=0.95,color="#980065", formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    ylim(0.0,600.0) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> p1)

(ggplot(nif_env_rhiz, aes(x=As5, y=nifD_abun_50aa))+
    geom_jitter(position=position_jitter(0.17), size=3, alpha=.5,color="#980065")+
    labs(x="As(V) (mg kg-1)", y="nifD Homologues (RPKM)")+
    geom_smooth(method=lm,level=0.95,color="#980065", formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    ylim(100.0,800.0) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> p2)

(ggplot(nif_env_rhiz, aes(x=As5, y=nifK_abun_50aa))+
    geom_jitter(position=position_jitter(0.17), size=3, alpha=.5,color="#980065")+
    labs(x="As(V) (mg kg-1)", y="nifK Homologues (RPKM)")+
    geom_smooth(method=lm,level=0.95,color="#980065", formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    ylim(200.0,800.0) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> p3)


## ---------------------------------------------------------- ##

(ggplot(nif_env_rhiz, aes(x=As3, y=nifH_abun_50aa))+
    geom_jitter(position=position_jitter(0.17), size=3, alpha=.5,color="#3498DB")+
    labs(x="As(III) (mg kg-1)", y="nifH Homologues (RPKM)")+
    geom_smooth(method=lm,level=0.95,color="#3498DB", formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    ylim(0.0,600.0) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> p4)

(ggplot(nif_env_rhiz, aes(x=As3, y=nifD_abun_50aa))+
    geom_jitter(position=position_jitter(0.17), size=3, alpha=.5,color="#3498DB")+
    labs(x="As(III) (mg kg-1)", y="nifD Homologues (RPKM)")+
    geom_smooth(method=lm,level=0.95,color="#3498DB", formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    ylim(100.0,800.0) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> p5)

(ggplot(nif_env_rhiz, aes(x=As3, y=nifK_abun_50aa))+
    geom_jitter(position=position_jitter(0.17), size=3, alpha=.5,color="#3498DB")+
    labs(x="As(III) (mg kg-1)", y="nifK Homologues (RPKM)")+
    geom_smooth(method=lm,level=0.95,color="#3498DB", formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    ylim(200.0,800.0) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> p6)


## ---------------------------------------------------------- ##

(ggplot(nif_env_rhiz, aes(x=AsT/1000, y=nifH_abun_50aa))+
    geom_jitter(position=position_jitter(0.17), size=3, alpha=.5,color="#E5C858")+
    labs(x="Total As (g kg-1)", y="nifH Homologues (RPKM)")+
    geom_smooth(method=lm,level=0.95,color="#E5C858", formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    ylim(0.0,600.0) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> p7)

(ggplot(nif_env_rhiz, aes(x=AsT/1000, y=nifD_abun_50aa))+
    geom_jitter(position=position_jitter(0.17), size=3, alpha=.5,color="#E5C858")+
    labs(x="Total As (g kg-1)", y="nifD Homologues (RPKM)")+
    geom_smooth(method=lm,level=0.95,color="#E5C858", formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    ylim(100.0,800.0) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> p8)

(ggplot(nif_env_rhiz, aes(x=AsT/1000, y=nifK_abun_50aa))+
    geom_jitter(position=position_jitter(0.17), size=3, alpha=.5,color="#E5C858")+
    labs(x="Total As (g kg-1)", y="nifK Homologues (RPKM)")+
    geom_smooth(method=lm,level=0.95,color="#E5C858", formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    ylim(200.0,800.0) +
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> p9)



cor.test(nif_env_rhiz$nifH_abun_50aa, nif_env_rhiz$As5)
cor.test(nif_env_rhiz$nifD_abun_50aa, nif_env_rhiz$As5)
cor.test(nif_env_rhiz$nifK_abun_50aa, nif_env_rhiz$As5)
cor.test(nif_env_rhiz$nifH_abun_50aa, nif_env_rhiz$As5, method = "spearman")
cor.test(nif_env_rhiz$nifD_abun_50aa, nif_env_rhiz$As5, method = "spearman")
cor.test(nif_env_rhiz$nifK_abun_50aa, nif_env_rhiz$As5, method = "spearman")


cor.test(nif_env_rhiz$nifH_abun_50aa, nif_env_rhiz$As3)
cor.test(nif_env_rhiz$nifD_abun_50aa, nif_env_rhiz$As3)
cor.test(nif_env_rhiz$nifK_abun_50aa, nif_env_rhiz$As3)
cor.test(nif_env_rhiz$nifH_abun_50aa, nif_env_rhiz$As3, method = "spearman")
cor.test(nif_env_rhiz$nifD_abun_50aa, nif_env_rhiz$As3, method = "spearman")
cor.test(nif_env_rhiz$nifK_abun_50aa, nif_env_rhiz$As3, method = "spearman")


cor.test(nif_env_rhiz$nifH_abun_50aa, nif_env_rhiz$AsT)
cor.test(nif_env_rhiz$nifD_abun_50aa, nif_env_rhiz$AsT)
cor.test(nif_env_rhiz$nifK_abun_50aa, nif_env_rhiz$AsT)
cor.test(nif_env_rhiz$nifH_abun_50aa, nif_env_rhiz$AsT, method = "spearman")
cor.test(nif_env_rhiz$nifD_abun_50aa, nif_env_rhiz$AsT, method = "spearman")
cor.test(nif_env_rhiz$nifK_abun_50aa, nif_env_rhiz$AsT, method = "spearman")


cor.test(nif_env_rhiz$nifH_abun_50aa, nif_env_rhiz$AsR)
cor.test(nif_env_rhiz$nifD_abun_50aa, nif_env_rhiz$AsR)
cor.test(nif_env_rhiz$nifK_abun_50aa, nif_env_rhiz$AsR)
cor.test(nif_env_rhiz$nifH_abun_50aa, nif_env_rhiz$AsR, method = "spearman")
cor.test(nif_env_rhiz$nifD_abun_50aa, nif_env_rhiz$AsR, method = "spearman")
cor.test(nif_env_rhiz$nifK_abun_50aa, nif_env_rhiz$AsR, method = "spearman")


cor.test(nif_env_rhiz$nifH_abun_50aa, nif_env_rhiz$AsL)
cor.test(nif_env_rhiz$nifD_abun_50aa, nif_env_rhiz$AsL)
cor.test(nif_env_rhiz$nifK_abun_50aa, nif_env_rhiz$AsL)
cor.test(nif_env_rhiz$nifH_abun_50aa, nif_env_rhiz$AsL, method = "spearman")
cor.test(nif_env_rhiz$nifD_abun_50aa, nif_env_rhiz$AsL, method = "spearman")
cor.test(nif_env_rhiz$nifK_abun_50aa, nif_env_rhiz$AsL, method = "spearman")


library(patchwork)

(p1|p2|p3)/(p4|p5|p6)/(p7|p8|p9)


