

data = read.csv('NifHAbun_NifHPalmscan.csv', row.names = 1)
metadata = read.csv('metadata.csv', row.names = 'sample')
metadata$nifH = data[metadata$raw.sample,]

meta_rhiz = metadata %>% filter(habitat=='Rhizosphere')
meta_rhiz_l = meta_rhiz %>% filter(type=='Slight')
meta_rhiz_h = meta_rhiz %>% filter(type=='Severe')

meta_bulk = metadata %>% filter(habitat=='Bulk')
meta_bulk_l = meta_bulk %>% filter(type=='Slight')
meta_bulk_h = meta_bulk %>% filter(type=='Severe')

# wilcox.test(meta_rhiz$nifH, meta_bulk$nifH)
wilcox.test(meta_rhiz_l$nifH, meta_rhiz_h$nifH)


meta_rhiz$type = factor(meta_rhiz$type, levels = c('Slight', 'Severe'))
cbPalette <- c("#D0607D", "#980065")
(pnifH <- ggplot(meta_rhiz, aes(x=type, y=nifH, fill=type)) + 
    geom_boxplot(alpha=0.3, width=0.1, color = "gray", position=position_dodge(width=0.8), size=0.1, outlier.colour = NA)+
    geom_violin(alpha=0.3, width=1, color = NA, position=position_dodge(width=0.8), size=0.1)+
    geom_point(aes(color = type), alpha=1, size=.7, position=position_jitterdodge(jitter.width = 0.35, jitter.height = 0, dodge.width = 0.8))+
    scale_fill_manual(values = cbPalette)+
    scale_color_manual(values = cbPalette)+
    theme_classic()+
    ylim(0, 320) + 
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(legend.position = "none", legend.title = element_blank()) +
    labs(x="", y='RPKM (nifH)') +
    guides(fill="none") +
    theme(axis.text=element_text(colour='black',size=9)))

