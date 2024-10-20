

library(tidyverse)

data = read.csv('metadata.csv')


datax = data %>% filter(habitat=='Rhizosphere')

(ggplot(datax, aes(x=As5, y=TN))+
    geom_point(size=3, alpha=.5,color="#980065")+
    labs(x="As5(mg·kg-1)", y="TN(g·kg-1)")+
    geom_smooth(method=lm,level=0.95,color="#980065",alpha=.3,formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    #scale_x_continuous(limits = c(0, 150)) + 
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> pc1)


(ggplot(datax, aes(x=As5, y=DON))+
    geom_point(size=3, alpha=.5,color="#980065")+
    labs(x="As5(mg·kg-1)", y="DON(mg·kg-1)")+
    geom_smooth(method=lm,level=0.95,color="#980065",alpha=.3,formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    #scale_x_continuous(limits = c(0, 150)) + 
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> pc2)

(ggplot(datax, aes(x=As5, y=NH4))+
    geom_point(size=3, alpha=.5,color="#980065")+
    labs(x="As5(mg·kg-1)", y="NH4(mg·kg-1)")+
    geom_smooth(method=lm,level=0.95,color="#980065",alpha=.3,formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    #scale_x_continuous(limits = c(0, 150)) + 
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> pc3)

(ggplot(datax, aes(x=As5, y=NO3))+
    geom_point(size=3, alpha=.5,color="#980065")+
    labs(x="As5(mg·kg-1)", y="NO3(mg·kg-1)")+
    geom_smooth(method=lm,level=0.95,color="#980065",alpha=.3,formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    #scale_x_continuous(limits = c(0, 150)) + 
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> pc4)


## ---------------------------------------------------------- ##
(ggplot(datax, aes(x=As3, y=TN))+
    geom_point(size=3, alpha=.5,color="#3498DB")+
    labs(x="As3(mg·kg-1)", y="TN(g·kg-1)")+
    geom_smooth(method=lm,level=0.95,color="#3498DB",alpha=.3,formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    scale_x_continuous(limits = c(0, 12), breaks = c(0,3,6,9,12)) + 
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> pc5)

(ggplot(datax, aes(x=As3, y=DON))+
    geom_point(size=3, alpha=.5,color="#3498DB")+
    labs(x="As3(mg·kg-1)", y="DON(mg·kg-1)")+
    geom_smooth(method=lm,level=0.95,color="#3498DB",alpha=.3,formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    scale_x_continuous(limits = c(0, 12), breaks = c(0,3,6,9,12)) + 
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> pc6)

(ggplot(datax, aes(x=As3, y=NH4))+
    geom_point(size=3, alpha=.5,color="#3498DB")+
    labs(x="As3(mg·kg-1)", y="NH4(mg·kg-1)")+
    geom_smooth(method=lm,level=0.95,color="#3498DB",alpha=.3,formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    scale_x_continuous(limits = c(0, 12), breaks = c(0,3,6,9,12)) + 
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> pc7)

(ggplot(datax, aes(x=As3, y=NO3))+
    geom_point(size=3, alpha=.5,color="#3498DB")+
    labs(x="As3(mg·kg-1)", y="NO3(mg·kg-1)")+
    geom_smooth(method=lm,level=0.95,color="#3498DB",alpha=.3,formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    scale_x_continuous(limits = c(0, 12), breaks = c(0,3,6,9,12)) + 
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> pc8)


## ---------------------------------------------------------- ##
(ggplot(datax, aes(x=As.soil., y=TN))+
    geom_point(size=3, alpha=.5,color="#E5C858")+
    labs(x="total As(mg·kg-1)", y="TN(g·kg-1)")+
    geom_smooth(method=lm,level=0.95,color="#E5C858",alpha=.3,formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    #scale_x_continuous(limits = c(0, 150)) + 
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> pc9)

(ggplot(datax, aes(x=As.soil., y=DON))+
    geom_point(size=3, alpha=.5,color="#E5C858")+
    labs(x="total As(mg·kg-1)", y="DON(mg·kg-1)")+
    geom_smooth(method=lm,level=0.95,color="#E5C858",alpha=.3,formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    #scale_x_continuous(limits = c(0, 150)) + 
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> pc10)

(ggplot(datax, aes(x=As.soil., y=NH4))+
    geom_point(size=3, alpha=.5,color="#E5C858")+
    labs(x="total As(mg·kg-1)", y="NH4(mg·kg-1)")+
    geom_smooth(method=lm,level=0.95,color="#E5C858",alpha=.3,formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    #scale_x_continuous(limits = c(0, 150)) + 
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> pc11)

(ggplot(datax, aes(x=As.soil., y=NO3))+
    geom_point(size=3, alpha=.5,color="#E5C858")+
    labs(x="total As(mg·kg-1)", y="NO3(mg·kg-1)")+
    geom_smooth(method=lm,level=0.95,color="#E5C858",alpha=.3,formula = y~poly(x, 1))+#拟合线
    theme_classic()+
    #scale_x_continuous(limits = c(0, 150)) + 
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.text=element_text(colour='black',size=9)) -> pc12)


cor.test(datax$As5, datax$TN, method = "spearman")
cor.test(datax$As5, datax$DON, method = "spearman")
cor.test(datax$As5, datax$NH4, method = "spearman")
cor.test(datax$As5, datax$NO3, method = "spearman")

cor.test(datax$As3, datax$TN, method = "spearman")
cor.test(datax$As3, datax$DON, method = "spearman")
cor.test(datax$As3, datax$NH4, method = "spearman")
cor.test(datax$As3, datax$NO3, method = "spearman")

cor.test(datax$As.soil., datax$TN, method = "spearman")
cor.test(datax$As.soil., datax$DON, method = "spearman")
cor.test(datax$As.soil., datax$NH4, method = "spearman")
cor.test(datax$As.soil., datax$NO3, method = "spearman")

## ---------------------------------------------------------- ##

(pc1|pc2|pc3|pc4) / (pc5|pc6|pc7|pc8) / (pc9|pc10|pc11|pc12) 
