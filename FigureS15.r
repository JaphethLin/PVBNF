



nr.stat = read.csv('nr.scan.results.csv', header = FALSE)
nr.stat$index = 'nr'

gbk.stat = read.csv('genebank.scan.results.csv', header = FALSE)
gbk.stat$index = 'gbk'

bact.refseq.stat = read.csv('bact.refseq.scan.results.csv', header = FALSE)
bact.refseq.stat$index = 'refseq'

arch.refseq.stat = read.csv('arch.refseq.scan.results.csv', header = FALSE)
arch.refseq.stat$index = 'refseq'

data=rbind(nr.stat, gbk.stat, bact.refseq.stat, arch.refseq.stat)


(ggplot(data=data, aes(x=V1, fill=index)) + 
    geom_histogram(data = filter(data, index=='nr'), 
                   binwidth=1, alpha=.5, color='black')+
    
    geom_histogram(data = filter(data, index=='gbk'), 
                   binwidth=1, alpha=.5, color='black')+
    
    geom_histogram(data = filter(data, index=='refseq'), 
                   binwidth=1, alpha=.5, color='black')+
    theme_bw()+
    
    scale_x_continuous(expand = c(0,0), limits = c(0,125)) +
    
    scale_y_continuous(expand = c(0,0), limits = c(0,2500)) +
    
    geom_density(aes(V1,y=..count..,color=index), 
                 fill='transparent',cex=1)+
    
    geom_vline(aes(xintercept=98.5), colour="#69C000", linetype="dashed", size=1) +
    
    scale_color_manual(values = c('#0089A4','#ce2523','#FFCC00')) + 
    
    scale_fill_manual(values = c('#0089A4','#ce2523','#FFCC00')) + 
    
    labs(x="",y="") -> ps1)



(ggplot(data=data, aes(x=V1, fill=index)) + 
    geom_histogram(data = filter(data, index=='nr'), 
                   binwidth=1, alpha=.5, color='white')+
    
    geom_histogram(data = filter(data, index=='gbk'), 
                   binwidth=1, alpha=.5, color='white')+
    
    geom_histogram(data = filter(data, index=='refseq'), 
                   binwidth=1, alpha=.5, color='white')+
    theme_bw()+
    
    scale_x_continuous(expand = c(0,0), limits = c(0,125)) +
    
    scale_y_continuous(expand = c(0,0), limits = c(0,2500)) +
    
    geom_density(aes(V1,y=..count..,color=index), 
                 fill='transparent',cex=1)+
    
    geom_vline(aes(xintercept=98.5), colour="#69C000", linetype="dashed", size=1) +
    
    scale_color_manual(values = c('#0147C2','#e07233','#BDB200')) + 
    
    scale_fill_manual(values = c('#0147C2','#e07233','#BDB200')) + 
    
    labs(x="",y="") -> ps1)

